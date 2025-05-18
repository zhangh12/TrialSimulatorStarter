library(shiny)
library(DT)
library(shinyAce)
library(rclipboard)

ui <- fluidPage(
  
  # ---- Title ----
  titlePanel("Trial Simulator"),
  
  # ---- Global Save/Load Buttons ----
  fluidRow(
    column(12, align = "right",
           div(style = "margin-bottom: 10px; display: flex; gap: 10px; justify-content: flex-end;",
               downloadButton("save_config", label = tagList(icon("save"), "Save Config"), class = "btn btn-primary"),
               tags$label(class = "btn btn-primary", tagList(icon("folder-open"), "Load Config"),
                          tags$input(id = "load_config", type = "file", style = "display: none;", accept = ".json")
               )
           )
    )
  ),
  
  # ---- Main Content Tabs ----
  tabsetPanel(
    
    # ---- Trial Event Tab ----
    tabPanel("Trial Event",
             fluidRow(
               
               # Left: Condition + Trial Event form
               column(3,
                      wellPanel(
                        textInput("event_name", "Trial Event Name"),
                        textInput("logic_expr", "Logic Expression (e.g. (A & B) | C)", value = ""),
                        
                        radioButtons("condition_type", "Triggering Condition",
                                     choices = c("Calendar Time", "Patient Number", "Event Number"),
                                     selected = character(0)
                        ),
                        
                        uiOutput("condition_ui"),
                        
                        div(style = "margin-top: 10px;",
                            actionButton("add_condition", "➕" ),
                            actionButton("delete_condition", "🗑")
                        ),
                        
                        hr(),
                        actionButton("add_trial_event", "➕ Add Trial Event")
                      )
               ),
               
               # Right: Condition and Trial Event tables
               column(9,
                      DTOutput("condition_table"),
                      hr(),
                      DTOutput("trial_event_table"),
                      uiOutput("trial_event_buttons_ui")
               )
             )
    ),
    
    # ---- Code Tab ----
    tabPanel("Code",
             rclipboard::rclipboardSetup(),
             fluidRow(
               column(12,
                      shinyAce::aceEditor("code", mode = "r", theme = "textmate", readOnly = TRUE, height = "300px"),
                      uiOutput("copy_code_button")
               )
             )
    )
    
    # Future: add tabPanel("Arm", ...) and others here
  )
)
