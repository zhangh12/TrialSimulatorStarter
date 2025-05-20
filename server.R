library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(glue)
library(DT)
library(shinyAce)
library(rclipboard)
library(jsonlite)

source('helpers.R')
source("server_config.R")
source("server_trial_info.R")
source("server_arms.R")
source("server_trial_events.R")
source("server_code_tab.R")


server <- function(input, output, session) {
  code_text <- reactiveVal("")
  
  vals <- reactiveValues(
    arms = list(),
    trial_events = list(),
    conditions = list(),
    condition_ids = LETTERS
  )
  
  # Call feature modules
  server_config(input, output, session, vals)
  server_trial_info(input, output, session, vals)
  server_arms(input, output, session, vals)
  server_trial_events(input, output, session, vals)
  server_code_tab(input, output, session, vals, code_text)
}
