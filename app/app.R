# setwd("C:/Users/abhay/OneDrive/Desktop/STAT/STAT_442/STAT_442_Final/app/")

library(shiny)
library(htmltools)
library(sportyR)
library(worldfootballR)
library(gt)
library(maps)
library(ggrepel)
library(ggplot2)
library(plotly)
library(scales)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(geosphere)

# Source pre-built data and visualizations
source("STAT_442_Final_Visuals.R")

teams <- unique(team_summary_stats$Team)
stadiums <- unique(stadiums.dat$Stadium)
results <- unique(epl_results$Match_Result)

# User interface component
ui <- fluidPage(
  # Formatting
  tags$head(
    tags$style(HTML("
      .conclusion-section {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 25px;
        margin-top: 30px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .conclusion-header {
        color: #2c3e50;
        border-bottom: 2px solid #3498db;
        padding-bottom: 10px;
        margin-bottom: 15px;
      }
      .full-size-image {
        margin-top: 25px;
        border: 1px solid #ddd;
        padding: 15px;
      }
      .shiny-output-error { color: red; }
    "))
  ),
  # Tab title
  title="EPL Dashboard",
  # Dashboard title
  titlePanel(title=div(
    icon("futbol"),
    tags$span("English Premier League (EPL) Dashboard for 2017–2018",
         style="margin-left:10px")
  )),
  br(),
  # Overview section
  wellPanel(
    h4("Dashboard Overview"),
    p("An interactive look at the 2017–2018 English Premier League season
      through 4 linked visualizations:"),
    tags$ul(
      tags$li("Bubble‑scatter plot of total expected goals (xG) vs actual
              goals, colored by performance and sized by average attendance."),
      tags$li("Geographic map showing straight‑line travel distances between a
              selected hub team and all other team stadiums."),
      tags$li("Violin‑box plot of matchday attendance distributions broken down
              by match outcome."),
      tags$li("Formatted team statistics table with conditional formatting for
              wins, draws, losses, goal difference, and average
              attendance per match.")
    ),
    tags$a("Download Full Analysis (PDF)", href="STAT_442_Final.pdf",
           target="_blank", class="btn btn-primary")
  ),
  br(),
  # Tabs for each visualization
  tabsetPanel(type="tabs",
              tabPanel(title=tagList(icon("chart-simple"),
                                     "Goals & Attendance by Team"),
                       sidebarLayout(sidebarPanel(
                         selectInput("selected_team1", "Select Team:",
                                     choices=c("All", teams)),
                         downloadButton("download_bubble", "Download Plot")),
                         mainPanel(plotlyOutput("viz1", height="500px"),
                                   imageOutput("static1", width="60%",
                                               height="60%"))
                       )
              ),
              tabPanel(title=tagList(icon("map-location-dot"),
                                     "Stadium Distance Map"),
                       sidebarLayout(sidebarPanel(
                         selectInput("selected_team2", "Team Stadium:",
                                       choices=c("All", stadiums)),
                         downloadButton("download_map", "Download Map")),
                         mainPanel(plotlyOutput("viz2", height="500px"),
                                   imageOutput("static2", width="60%",
                                               height="60%"))
                       )
              ),
              tabPanel(title=tagList(icon("ticket-simple"),
                                     "Attendance by Match Outcome"),
                       sidebarLayout(sidebarPanel(
                         selectInput("selected_result", "Match Result:",
                                     choices=c("All", results)),
                         downloadButton("download_violin", "Download Plot")),
                         mainPanel(plotlyOutput("viz3", height="500px"),
                                   imageOutput("static3", width="60%",
                                               height="60%"))
                       )
              ),
              tabPanel(title=tagList(icon("table"), "Team Performance Table"),
                       sidebarLayout(sidebarPanel(
                         selectInput("selected_team4", "Filter Table:",
                                     choices=c("All", teams)),
                         downloadButton("download_table", "Download Table")),
                         mainPanel(gt_output("viz4"))
                       )
              )
  ),
  br(),
  # Conclusion section
  wellPanel(
    h3(class="conclusion-header", "Key Analytical Findings"),
    tags$ul(
      tags$li(strong("Dominant Overall Performance: "),
              "Manchester City led the league with ", strong("32 wins"),
              " (84% win rate) and a goal difference of ", strong("+79"),
              ", outscoring their xG by ", strong("27.5 goals"),
              " (106 actual vs 78.5 expected)."),
      tags$li(strong("Attendance Edge: "),
      "The top 3 clubs by average attendance - Manchester United (56,225),
      ", "Tottenham (52,191), and Arsenal (48,852) - averaged ",
      strong("37% higher crowds"), " than the league average (38,274)."),
      tags$li(strong("Defensive Performance: "),
              "Burnley conceded only 39 goals against an xGA of 51.1, ",
              strong("24% fewer goals"),
              " than expected - one of the best defensive records in
              the league."),
      tags$li(strong("Relegation Struggles: "),
              "The 3 relegated sides (West Brom, Stoke, Swansea) scored ",
              strong("10.5% below their xG"),
              " and each conceded at least 55 goals.")
    ),
    p(em("Data Source: FBref.com / worldfootballR"),
      style="margin-top:20px; color: #7f8c8d;")
  )
)

# Server component
server <- function(input, output) {
  # Reactive filters for visuals
  filtered_bubble <- reactive({
    if (input$selected_team1 == "All") team_summary_stats
    else team_summary_stats %>% filter(Team == input$selected_team1)
  })
  filtered_map <- reactive({
    if (input$selected_team2 == "All") stadiums.dat
    else stadiums.dat %>% filter(Stadium == input$selected_team2)
  })
  filtered_violin <- reactive({
    if (input$selected_result == "All") epl_results
    else epl_results %>% filter(Match_Result == input$selected_result)
  })
  filtered_table <- reactive({
    if (input$selected_team4 == "All") team_summary_stats
    else team_summary_stats %>% filter(Team == input$selected_team4)
  })

  # Render interactive table and plots
  output$viz1 <- renderPlotly({
    p <- create_bubble_scatter_plot(filtered_bubble())
    ggplotly(p, tooltip="text") %>%
      layout(hoverlabel=list(bgcolor="black", font=list(size=12)))
  })
  output$viz2 <- renderPlotly({
    p <- create_map_plot(segments.dat, filtered_map())
    ggplotly(p, tooltip="text") %>%
      layout(hoverlabel=list(bgcolor="white", font=list(size=12)))
  })
  output$viz3 <- renderPlotly({
    p <- create_violin_box_plot(filtered_violin())
    ggplotly(p, tooltip="text")
  })
  output$viz4 <- render_gt({
    create_gt_table(filtered_table())
  })

  # Render image plots
  output$static1 <- renderImage({
    list(src="www/stat442-option1-bubble-scatter-plot.png",
         contentType="image/png", alt="Bubble scatter plot")
  }, deleteFile=FALSE)
  output$static2 <- renderImage({
    list(src="www/stat442-option2-map-plot.png", contentType="image/png",
         alt="Stadium map plot")
  }, deleteFile=FALSE)
  output$static3 <- renderImage({
    list(src="www/stat442-option3-violin-box-plot.png", contentType="image/png",
         alt="Hybrid violin-box plot")
  }, deleteFile=FALSE)

  # Create download option for all visuals
  output$download_bubble <- downloadHandler(
    filename=function() { "bubble_scatter_plot.png" },
    content=function(file) {
      ggsave(file, create_bubble_scatter_plot(filtered_bubble()), width=10,
             height=10)
    }
  )
  output$download_map <- downloadHandler(
    filename=function() { "stadium_map_plot.png" },
    content=function(file) {
      ggsave(file, create_map_plot(filtered_map()), width=10, height=10)
    }
  )
  output$download_violin <- downloadHandler(
    filename=function() { "violin_box_plot.png" },
    content=function(file) {
      ggsave(file, create_violin_box_plot(filtered_violin()), width=10,
             height=10)
    }
  )
  output$download_table <- downloadHandler(
    filename=function() { "team_stats_table.csv" },
    content=function(file) {
      write.csv(filtered_table(), file, row.names=FALSE)
    }
  )
}

# Call application component
shinyApp(ui, server)

