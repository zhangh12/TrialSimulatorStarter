
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(glue)
library(DT)
library(shinyAce)
library(rclipboard)
library(jsonlite)

source('ui.R')
source('server.R')

shinyApp(ui = ui, server = server)

