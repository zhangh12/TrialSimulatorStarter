server_trial_info <- function(input, output, session, vals) {
  
  observeEvent(input$update_trial_info, {
    if (!nzchar(input$trial_n)) {
      showNotification("❌ Please enter the number of patients.", type = "error")
      return()
    }
    if (!nzchar(input$trial_duration)) {
      showNotification("❌ Please enter the trial duration.", type = "error")
      return()
    }
    if (nzchar(input$dropout_args) && !nzchar(input$dropout)) {
      showNotification("❌ Dropout type is required when Dropout Args is provided.", type = "error")
      return()
    }
    
    vals$trial_info <- list(
      n = input$trial_n,
      duration = input$trial_duration,
      accrual_rate = input$accrual_rate,
      dropout = input$dropout,
      dropout_args = input$dropout_args
    )
    
    showNotification("✅ Trial information updated.", type = "message")
  })
}
