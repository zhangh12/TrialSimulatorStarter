source('packages.R')
source('R/helpers.R')
source("R/server_config.R")
source("R/server_trial_info.R")
source("R/server_arms.R")
source("R/server_trial_events.R")
source("R/server_code_tab.R")


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
