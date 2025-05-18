source("server_config.R")
source("server_trial_events.R")
source("server_code_tab.R")


server <- function(input, output, session) {
  code_text <- reactiveVal("")
  
  vals <- reactiveValues(
    conditions = list(),
    condition_ids = LETTERS,
    trial_events = list()
  )
  
  # Call feature modules
  server_config(input, output, session, vals)
  server_trial_events(input, output, session, vals)
  server_code_tab(input, output, session, vals, code_text)
}
