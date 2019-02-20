#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(tsibble)
library(sugrrants)
library(glue)
library(emo)
library(bomrang)
library(viridis)
library(plotly)

# Data preparation
elec <- read_csv("data/di.csv", skip=1,
                 col_names = c("id", "date", paste0("d", 1:48), paste0("stuff", 1:5)),
                 col_types = "ccddddddddddddddddddddddddddddddddddddddddddddddddccccc")
load("data/temp.rda")

# Wrangle data
vic_holidays <- holiday_aus(2017:2019, state = "VIC")
elec <- elec %>% filter(id == 300)
elec <- elec %>%
  mutate(date = ymd(date)) %>%
  select(id:d48) %>%
  gather(halfhour, kwh, d1:d48) %>%
  mutate(halfhour = as.numeric(sub("d", "", halfhour))/2) %>%
  arrange(date, halfhour) %>%
  mutate(dt = ymd_hm(glue("{date} 12:00"),
                     tz = "Australia/Melbourne") +
           minutes(60*halfhour)) 

# Create a daily summary
elec_dly <- elec %>% 
  group_by(date) %>%
  summarise(kwh = sum(kwh)) %>%
  mutate(wday = wday(date, label = TRUE, abbr = TRUE,
                   week_start = 1),
       month = month(date, label = TRUE, abbr = TRUE),
       year = year(date)) %>%
  mutate(work = ifelse(wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "workday", "holiday")) %>%
  mutate(work = ifelse(date %in% vic_holidays$date, "holiday", work)) %>%
  left_join(maxtemp) 

# Add additional variables to day/time data
elec <- elec %>%
  mutate(wday = wday(date, label = TRUE, abbr = TRUE,
                     week_start = 1),
         month = month(date, label = TRUE, abbr = TRUE),
         year = year(date)) %>%
  mutate(work = ifelse(wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"), "workday", "holiday")) %>%
  mutate(work = ifelse(date %in% vic_holidays$date, "holiday", work)) %>%
  left_join(maxtemp) 

# Set up menu for X variable of boxplot
catvars <- c("wday", "month", "work")

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Exploring my energy usage"),
    tabsetPanel(
      # Set up the user interface for the calendar tab
      # to select variables to plot, labels and colouring
      tabPanel("Calendar",
               # Sidebar choosing variables, labels and colour
               sidebarLayout(
                 sidebarPanel(
                   dateInput("from", "From:", value = ymd(min(elec$date))),
                   dateInput("to", "To:", value = ymd(max(elec$date)))
                 ),

                 # Show the scatterplot, with a fixed height
                 mainPanel(
                   plotOutput("calendar", height="600px")
                 )
               )
    ),
    tabPanel("Boxplots",
             # Sidebar choosing variables, labels and colour
             sidebarLayout(
               sidebarPanel(
                 dateInput("from", "From:", value = ymd(min(elec$date))),
                 dateInput("to", "To:", value = ymd(max(elec$date))),
                 selectInput('x', "X:", catvars, catvars[1])
               ),
               
               # Show the scatterplot, with a fixed height
               mainPanel(
                 plotlyOutput("boxplots", height="400px")
               )
             )
    )
)
)

server <- function(input, output) {

  # Make the calendar plot
  output$calendar <- renderPlot({
    # Set up the calendar plot, for time perid provided,
    calendar_df <- elec %>%
      filter(date < input$to, date >= input$from) %>%
      frame_calendar(x = halfhour, y = kwh, date = date, ncol = 4) 
    p <- calendar_df %>%
      ggplot(aes(x = .halfhour, y = .kwh, group = date, colour=Max_temperature)) +
      geom_line() +
      scale_colour_viridis_c("temperature", option="inferno", direction=-1) +
      theme(legend.position = "bottom")
    prettify(p)
  })
  
  # Make the boxplot
  output$boxplots <- renderPlotly({
    # Set up the calendar plot, for time perid provided,
    p <- elec_dly %>%
      filter(year(date) == 2018) %>%
      ggplot(aes_string(x=input$x, y="kwh")) + geom_boxplot()
    ggplotly(p)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

