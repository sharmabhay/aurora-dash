# setwd("H:/Documents - Copy/GitHub/aurora-dash/app/")

library(shiny)
library(htmltools)
library(shinythemes)
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
source("visuals.R")

teams <- unique(team_summary_stats$Team)
stadiums <- unique(stadiums.dat$Stadium)
results <- unique(epl_results$Match_Result)

# User interface component
ui <- navbarPage(
  # Dashboard title
  title=div(icon("futbol"), "aurora-dash"),
  theme=shinytheme("flatly"),
  position="fixed-top",
  # Tab title
  windowTitle="EPL Dashboard",
  # Home/Overview section
  tabPanel("Home", icon=icon("home"),
           fluidRow(
             column(12,
                    tags$h2("Welcome to Aurora Dashboard"),
                    tags$p("Explore the 2017–2018 English Premier League (EPL)
                           in four interactive visualizations."),
                    tags$ul(
                      tags$li(
                        tags$img(
                          src="icons/scatter-plot.png", width="32px",
                          height="24px",
                          style="margin-right:8px; vertical-align:middle;"
                        ),
                        "Bubble‑scatter plot of total expected goals (xG) vs
                        actual goals, colored by performance and sized by
                        average attendance."
                      ),
                      tags$li(
                        tags$img(
                          src="icons/map.png", width="32px", height="24px",
                          style="margin-right:8px; vertical-align:middle;"
                        ),
                        "Geographic map showing straight‑line travel distances
                        between a selected hub team and all other team
                        stadiums."
                      ),
                      tags$li(
                        tags$img(
                          src="icons/box-plot.png", width="24px", height="24px",
                          style="margin-right:8px; vertical-align:middle;"
                        ),
                        "Violin‑box plot of matchday attendance distributions
                        broken down by match outcome."
                      ),
                      tags$li(
                        tags$img(
                          src="icons/performance.png", width="24px",
                          height="24px",
                          style="margin-right:8px; vertical-align:middle;"
                        ),
                        "Formatted team statistics table with conditional
                        formatting for wins, draws, losses, goal difference, and
                        average attendance per match."
                      )
                    ),
                    tags$div(style="margin-bottom: 25px;",
                             tags$a("Download Full Analysis (PDF)",
                                    href="EPL_Teams_Full_Analysis.pdf",
                                    target="_blank", class="btn btn-primary")
                    )
             )
           )
  ),
  # Visuals Section
  navbarMenu("Visualizations", icon=icon("bars"),
             tabPanel("Goals & Attendance by Team", icon=icon("chart-simple"),
                      style="margin-top: 25px; margin-bottom: 25px;",
                      sidebarLayout(sidebarPanel(
                        selectInput("selected_team1", "Select Team:",
                                    choices=c("All", teams)),
                        downloadButton("download_bubble", "Download Plot")),
                        mainPanel(plotlyOutput("viz1", height="500px"),
                                  imageOutput("static1", width="60%",
                                              height="60%"))
                      )
             ),
             tabPanel("Stadium Distance Map", icon=icon("map-location-dot"),
                      style="margin-top: 25px; margin-bottom: 25px;",
                      sidebarLayout(sidebarPanel(
                        selectInput("selected_team2", "Team Stadium:",
                                    choices=c("All", stadiums)),
                        downloadButton("download_map", "Download Map")),
                        mainPanel(plotlyOutput("viz2", height="500px"),
                                  imageOutput("static2", width="60%",
                                              height="60%"))
                      )
             ),
             tabPanel("Attendance by Match Outcome", icon=icon("ticket-simple"),
                      style="margin-top: 25px; margin-bottom: 25px;",
                      sidebarLayout(sidebarPanel(
                        selectInput("selected_result", "Match Result:",
                                    choices=c("All", results)),
                        downloadButton("download_violin", "Download Plot")),
                        mainPanel(plotlyOutput("viz3", height="500px"),
                                  imageOutput("static3", width="60%",
                                              height="60%"))
                      )
             ),
             tabPanel("Team Performance Table", icon=icon("table"),
                      style="margin-top: 25px; margin-bottom: 25px;",
                      sidebarLayout(sidebarPanel(
                        selectInput("selected_team4", "Filter Table:",
                                    choices=c("All", teams)),
                        downloadButton("download_table", "Download Table")),
                        mainPanel(gt_output("viz4"))
                      )
             )
  ),
  # Conclusion section
  footer=wellPanel(
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
    tags$p(tags$em("Data Source: FBref.com / worldfootballR"),
      style="margin-top:20px; color: #7f8c8d;")
  )
) %>%
# Formatting
tagList(
  tags$head(
    tags$style(HTML("
      .navbar,
      .navbar-fixed-top {
        min-height: 50px !important;
        margin-bottom: 0 !important;
        padding: 0 !important;
      }
      .navbar .navbar-brand {
        height: 50px !important;
        padding: 0 15px !important;
        line-height: 50px !important;
        font-size: 1.25em;
      }
      .navbar,
      .navbar-fixed-top {
        min-height: 50px !important;
        margin-bottom: 0 !important;
        padding: 0 !important;
      }
      .navbar .navbar-header,
      .navbar-fixed-top .navbar-header {
        float: left;
        height: 50px !important;
        padding: 0 !important;
        margin: 0 !important;
        line-height: 50px !important;
      }
      .navbar-nav > li > a {
        padding: 0 15px !important;
        height: 50px !important;
        line-height: 50px !important;
      }
      .navbar-nav .dropdown-menu {
        margin-top: 0 !important;
      }
      body {
        padding-top: 50px !important;
      }
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
    list(src="www/viz1-bubble-scatter-plot.png",
         contentType="image/png", alt="Bubble scatter plot")
  }, deleteFile=FALSE)
  output$static2 <- renderImage({
    list(src="www/viz2-map-plot.png", contentType="image/png",
         alt="Stadium map plot")
  }, deleteFile=FALSE)
  output$static3 <- renderImage({
    list(src="www/viz3-violin-box-plot.png", contentType="image/png",
         alt="Hybrid violin-box plot")
  }, deleteFile=FALSE)

  # Create download option for all visuals
  output$download_bubble <- downloadHandler(
    filename=function() { "bubble_scatter_plot.png" },
    content=function(file) {
      file.copy(from="www/viz1-bubble-scatter-plot.png", to=file,
                overwrite=TRUE)
    },
    contentType="image/png"
  )
  output$download_map <- downloadHandler(
    filename=function() { "stadium_map_plot.png" },
    content=function(file) {
      file.copy(from="www/viz2-map-plot.png", to=file, overwrite=TRUE)
    },
    contentType="image/png"
  )
  output$download_violin <- downloadHandler(
    filename=function() { "violin_box_plot.png" },
    content=function(file) {
      file.copy(from="www/viz3-violin-box-plot.png", to=file, overwrite=TRUE)
    },
    contentType="image/png"
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
