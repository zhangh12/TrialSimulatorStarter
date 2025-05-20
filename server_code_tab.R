server_code_tab <- function(input, output, session, vals, code_text) {
  
  observe({
    if (length(vals$trial_events) == 0 && length(vals$trial_info) == 0) {
      updateAceEditor(session, "code", value = "")
      return()
    }
    
    # ---- Trial Info ----
    trial_info_block <- generate_trial_info_codes(input)
    
    # ---- Trial Event ----
    trial_events_block <- generate_trial_event_codes(vals$trial_events)
    
    final_code <- glue::glue("\n{trial_info_block}\n\n", 
                             "{paste0(trial_events_block, collapse = '\n\n')}\n\n", 
                             .trim = FALSE)
    
    event_names <- paste(sapply(vals$trial_events, function(event) gsub("\\s+", "_", event$name)), collapse = ", ")
    
    final_code <- glue::glue("{final_code}\n\n",
                             "listener <- listener()\n",
                             "listener$add_events({event_names})\n\n",
                             "controller <- controller(trial, listener)\n",
                             "controller$run(n = 1, plot_event = TRUE)\n\n", 
                             .trim = FALSE)
    
    updateAceEditor(session, "code", value = final_code)
    code_text(final_code)
  })
  
  output$copy_code_button <- renderUI({
    req(code_text())
    rclipboard::rclipButton(
      inputId = "copy_code",
      label = "Copy Code",
      clipText = code_text(),
      icon = icon("clipboard")
    )
  })
  
}
