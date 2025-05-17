
ui <- 
  fluidPage(
    
    titlePanel('Trial Simulator'),
    
    fluidRow(
      column(12, align = "right",
             tags$div(style = "margin-bottom: 10px; display: flex; gap: 10px; justify-content: flex-end;",
                      downloadButton("save_config", label = tagList(icon("save"), "Save Config"), class = "btn-primary"),
                      
                      tags$label(class = "btn btn-primary", tagList(icon("folder-open"), "Load Config"),
                                 tags$input(id = "load_config", type = "file", style = "display: none;", accept = ".json")
                      )
             )
      )
    ),
    
    tabsetPanel(
      
      tabPanel(
        'Trial Event', 
        fluidRow(
          
          column(3, wellPanel(
            
            textInput('event_name', 'Triel Event', value = ''),
            
            textInput("logic_expr", "Logic Expression (e.g. (A & B) | C)", value = ''), 
            
            radioButtons(inputId = 'condition_type', 
                         label = 'Triggering Condition', 
                         choices = c('Calendar Time', 
                                     'Patient Number', 
                                     'Event Number'), 
                         selected = character(0)),
            
            uiOutput('condition_ui'), 
            
            actionButton('add_condition', '➕'),
            #actionButton('edit_condition', '✏️'),
            actionButton('delete_condition', '🗑️'),
            
            hr(),
            
            actionButton('add_trial_event', 'Add Trial Event')
            
          )),
          
          column(9, 
                 DTOutput('condition_table'), 
                 hr(), 
                 DTOutput('trial_event_table'), 
                 uiOutput('trial_event_buttons_ui'))
        )
      ),
      
      tabPanel(
        'Code',
        rclipboard::rclipboardSetup(),  # initialize clipboard JS
        
        fluidRow(
          column(12,
                 shinyAce::aceEditor('code', mode = 'r', theme = 'textmate', readOnly = TRUE, height = '300px'),
                 uiOutput('copy_code_button')
          )
        )
      )
      
    )
  )

