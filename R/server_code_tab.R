server_code_tab <- function(input, output, session, vals, code_text) {
  
  manual_mode <- reactiveVal(FALSE)
  
  # --- Modal confirmation if toggling off manual edit mode
  observeEvent(input$manual_edit_mode, ignoreInit = TRUE, {
    if (!input$manual_edit_mode && manual_mode()) {
      # User wants to turn OFF manual mode → confirm first
      showModal(modalDialog(
        title = "⚠️ Confirm Overwrite",
        "Your manual edits will be lost and replaced by auto-generated code. Continue?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_disable_manual", "Yes, overwrite", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
      
      # Revert UI toggle immediately
      updateSwitchInput(session, "manual_edit_mode", value = TRUE)
    } else if (input$manual_edit_mode) {
      # User turned ON manual mode
      manual_mode(TRUE)
      shinyAce::updateAceEditor(session, "code", readOnly = FALSE)
    }
  })
  
  # --- Confirmation accepted: revert to auto-generated code
  observeEvent(input$confirm_disable_manual, {
    removeModal()
    manual_mode(FALSE)
    updateSwitchInput(session, "manual_edit_mode", value = FALSE)
    shinyAce::updateAceEditor(session, "code", readOnly = TRUE)
    regenerate_code()
  })

  # --- Code generation logic
  regenerate_code <- function() {
    if (manual_mode()) return()
    
    if (length(vals$trial_events) == 0 && length(vals$arms) == 0) {
      updateAceEditor(session, "code", value = "")
      return()
    }
    
    # ---- Arm ----
    arm_block <- generate_arm_codes(vals$arms)
    
    # ---- Trial Info ----
    trial_info_block <- generate_trial_info_codes(input)
    
    # ---- Trial Event ----
    trial_events_block <- generate_trial_event_codes(vals$trial_events)
    
    final_code <- glue::glue("\n{arm_block}\n\n", 
                             "#-----------------------------------------\n\n", 
                             "{trial_info_block}",
                             "#-----------------------------------------\n\n", 
                             "{trial_events_block}",
                             .trim = FALSE)
    
    event_names <- paste(
      sapply(vals$trial_events, function(event) gsub("\\s+", "_", event$name)),
      collapse = ", "
    )
    
    final_code <- glue::glue("{final_code}\n\n",
                             "#-----------------------------------------\n\n", 
                             "listener <- listener()\n",
                             "listener$add_events({event_names})\n\n",
                             "controller <- controller(trial, listener)\n",
                             "controller$run(n = 1, plot_event = TRUE)\n\n", 
                             .trim = FALSE)
    
    updateAceEditor(session, "code", value = final_code)
    code_text(final_code)
  }
  
  # --- Observe all config changes and update if not in manual mode
  observe({
    regenerate_code()
  })
  
  observeEvent(input$code, {
    if (input$manual_edit_mode) {
      code_text(input$code)
    }
  })
  
  # --- Copy button
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
