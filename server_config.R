server_config <- function(input, output, session, vals) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  observeEvent(input$load_config, {
    req(input$load_config)
    
    tryCatch({
      path <- input$load_config$datapath
      stopifnot(is.character(path), file.exists(path))
      
      loaded <- jsonlite::read_json(path, simplifyVector = FALSE)
      if (!is.list(loaded)) stop("Invalid configuration structure")
      
      # 🔄 FULL RESET of all state
      vals$arms <- list()
      vals$trial_events <- list()
      vals$conditions <- list()
      vals$condition_ids <- LETTERS
      vals$pending_endpoints <- list()
      vals$ep_table_raw <- NULL
      vals$editing_arm_id <- NULL
      vals$editing_ep_id <- NULL
      
      # 🧹 Clear all UI inputs
      updateTextInput(session, "arm_label", value = "")
      updateTextInput(session, "arm_ratio", value = "")
      updateTextInput(session, "ep_name", value = "")
      updateTextInput(session, "ep_generator", value = "")
      updateTextInput(session, "ep_args", value = "")
      
      updateTextInput(session, "event_name", value = "")
      updateTextInput(session, "logic_expr", value = "")
      updateRadioButtons(session, "condition_type", selected = character(0))
      
      updateTextInput(session, "trial_n", value = "")
      updateTextInput(session, "trial_duration", value = "")
      updateTextAreaInput(session, "accrual_rate", value = "")
      updateTextInput(session, "dropout", value = "")
      updateTextAreaInput(session, "dropout_args", value = "")
      
      # ✅ Load config into now-clean environment
      if (!is.null(loaded$trial_events)) {
        vals$trial_events <- loaded$trial_events
      }
      
      if (!is.null(loaded$arms)) {
        vals$arms <- loaded$arms
      }
      
      if (!is.null(loaded$trial_info)) {
        vals$trial_info <- loaded$trial_info
      }
      
      showNotification("✅ Config loaded successfully", type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Failed to load config:", e$message), type = "error")
    })
  })
  
  output$save_config <- downloadHandler(
    filename = function() {
      paste0("trial_config_", Sys.Date(), ".json")
    },
    content = function(file) {
      jsonlite::write_json(
        list(
          trial_events = vals$trial_events,
          arms = vals$arms,
          trial_info = vals$trial_info
        ),
        path = file,
        pretty = TRUE,
        auto_unbox = TRUE
      )
    }
  )
  
  # Show confirmation modal when user clicks Restart
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Confirm Restart",
      "Are you sure you want to reset the app? All unsaved progress will be lost.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Yes, Reset", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  # Perform reset if confirmed
  observeEvent(input$confirm_reset, {
    removeModal()
    
    # reset all states
    vals$arms <- list()
    vals$trial_events <- list()
    vals$conditions <- list()
    vals$condition_ids <- LETTERS
    vals$pending_endpoints <- list()
    vals$ep_table_raw <- NULL
    vals$editing_arm_id <- NULL
    vals$editing_ep_id <- NULL
    vals$trial_info <- list()
    
    # clear all UI inputs
    updateTextInput(session, "arm_label", value = "")
    updateTextInput(session, "arm_ratio", value = "")
    updateTextInput(session, "ep_name", value = "")
    updateTextInput(session, "ep_generator", value = "")
    updateTextInput(session, "ep_args", value = "")
    
    updateTextInput(session, "event_name", value = "")
    updateTextInput(session, "logic_expr", value = "")
    updateRadioButtons(session, "condition_type", selected = character(0))
    
    updateTextInput(session, "trial_n", value = "")
    updateTextInput(session, "trial_duration", value = "")
    updateTextAreaInput(session, "accrual_rate", value = "")
    updateTextInput(session, "dropout", value = "")
    updateTextAreaInput(session, "dropout_args", value = "")
    
    showNotification("🔁 App reset successfully", type = "message")
  })
  
}
