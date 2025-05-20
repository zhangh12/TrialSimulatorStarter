
server_config <- function(input, output, session, vals) {
  
  observeEvent(input$update_trial, {
    if (!nzchar(input$trial_n) || !nzchar(input$trial_duration)) {
      showNotification("❌ Patient number and trial duration are required.", type = "error")
      return()
    }
    
    if (nzchar(input$dropout_args) && !nzchar(input$dropout)) {
      showNotification("❌ Please specify Dropout type when Dropout Args is provided.", type = "error")
      return()
    }
    
    vals$trial_info <- list(
      name = input$trial_name,
      n = input$trial_n,
      duration = input$trial_duration,
      accrual_rate = input$accrual_rate,
      dropout = input$dropout,
      dropout_args = input$dropout_args
    )
    
    showNotification("✅ Trial information updated.", type = "message")
  })
  
}


