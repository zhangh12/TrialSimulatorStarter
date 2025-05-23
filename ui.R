library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(glue)
library(DT)
library(shinyAce)
library(rclipboard)
library(jsonlite)
library(shinyWidgets)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # ---- Title ----
  titlePanel("Trial Simulator"),
  
  # ---- Global Save/Load Buttons ----
  fluidRow(
    column(12, align = "right",
           div(style = "margin-bottom: 10px; display: flex; gap: 10px; justify-content: flex-end;",
               downloadButton("save_config", label = tagList(icon("save"), "Save Config"), class = "btn btn-primary"),
               tags$label(class = "btn btn-primary", tagList(icon("folder-open"), "Load Config"),
                          tags$input(id = "load_config", type = "file", style = "display: none;", accept = ".json")
               ),
               actionButton("reset_app", label = tagList(icon("redo"), "Restart"), class = "btn btn-danger")
           )
    )
  ),
  
  # ---- Main Content Tabs ----
  tabsetPanel(
    
    # ---- Trial Info Tab
    tabPanel(
      "Trial Info",
      fluidRow(
        hr(),
        column(6, 
               wellPanel(
                 textInput("trial_n", "Number of Patients"),
                 textInput("trial_duration", "Trial Duration"),
                 textAreaInput("accrual_rate", "Accrual Rate", rows = 2)
               )
        ),
        column(6,
               wellPanel(
                 textInput("dropout", "Dropout"),
                 textAreaInput("dropout_args", "Dropout Args", rows = 2)
               )
        )
      )
      # ,
      # fluidRow(
      #   column(12, align = "left",
      #          actionButton("update_trial_info", "Update", class = "btn btn-primary"))
      # )
    ),
    
    # ---- Arm Tab ----
    tabPanel("Arm",
             fluidRow(
               column(3,
                      wellPanel(
                        
                        # Arm Info (top)
                        textInput("arm_label", "Arm Label"),
                        textInput("arm_ratio", "Randomization Ratio"),
                        
                        # Endpoint Builder
                        hr(),
                        textInput("ep_name", "Endpoint Name(s), comma-separated"),
                        uiOutput("parsed_endpoint_ui"),
                        
                        textInput("ep_generator", "Generator"),
                        textAreaInput("ep_args", "Generator Arguments", rows = 2),
                        
                        uiOutput("add_or_update_ep_button"),
                        
                        # Arm Control
                        uiOutput("arm_main_button")
                      )
               ),
               
               column(9,
                      h4("Pending Endpoints"),
                      DTOutput("endpoint_table"),
                      
                      div(style = "margin-top: 10px;", uiOutput("edit_ep_ui")),
                      
                      div(style = "margin-top: 10px;", uiOutput("view_or_delete_ep_ui")),
                      
                      hr(),
                      h4("Defined Arms"),
                      DTOutput("arm_table"),
                      
                      div(style = "margin-top: 10px;", uiOutput("arm_table_buttons"))
                      
               )
             )
    ),
    
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
                        
                        checkboxInput("adapt_remove_arm", "Remove an arm", value = FALSE),
                        checkboxInput("adapt_add_arm", "Add an arm", value = FALSE),
                        checkboxInput("adapt_update_ratio", "Update sample ratio", value = FALSE),
                        checkboxInput("adapt_extend_duration", "Extend duration", value = FALSE),
                        checkboxInput("adapt_adjust_n", "Adjust sample size", value = FALSE),
                        
                        tags$script(HTML("
                          $(document).on('shiny:connected', function() {
                            $('#adapt_add_arm').prop('disabled', true);
                            $('#adapt_adjust_n').prop('disabled', true);
                          });
                        ")),
                        
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
               column(6,
                      div(style = "margin-top: 10px;",
                          switchInput(
                            inputId = "manual_edit_mode",
                            label = "Edit",
                            value = FALSE,
                            onLabel = "ON",
                            offLabel = "OFF",
                            size = "small"
                          )
                      )
               )
             ),
             
             fluidRow(
               column(12,
                      shinyAce::aceEditor("code", mode = "r", theme = "textmate", readOnly = TRUE, height = "300px"),
                      uiOutput("copy_code_button")
               )
             )
    ),
    
    tabPanel("Output",
             fluidRow(
               column(6,
                      div(style = "margin-top: 20px; border: 1px solid #ccc; padding: 10px; height: 300px; overflow-y: auto;",
                          h4("Console Output:"),
                          verbatimTextOutput("console_output")
                      )
               ),
               column(6,
                      div(style = "margin-top: 20px; border: 1px solid #ccc; padding: 10px; height: 300px;",
                          h4("Plot Output:"),
                          plotOutput("code_plot")
                      )
               )
             )
    )
  )
)
