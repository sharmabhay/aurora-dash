# aurora-dash

An interactive Shiny dashboard for the 2017–2018 English Premier League (EPL) season, built with R. Explore expected vs actual goals, stadium travel distances, attendance patterns, and detailed team statistics - all in one place.

🔗 **Live Demo:** https://sharmabhay.shinyapps.io/EPL_2017-2018_Interactive_Dashboard/


## Features

- **Bubble Scatter Plot** of total Expect Goals vs Actual Goals, sized by Average Attendance and colored by finishing performance.
- **Geographic Map** showing straight-line Travel Distances from a selected hub club to all other Stadiums.
- **Violin-Box Plot** of matchday Attendance broken down by Match Outcome.
- **Formatted Table** with rich conditional formatting and performance metrics on Team Statistics.


## Installation

```bash
git clone https://github.com/sharmabhay/aurora-dash.git
cd aurora-dash
# Launch the app
cd app
Rscript -e "shiny::runApp('.')"
```
