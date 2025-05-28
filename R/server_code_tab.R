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
    
    info <- vals$dropout_info()
    
    if ((length(vals$trial_events) == 0 && length(vals$arms) == 0) || !info$ready) {
      updateAceEditor(session, "code", value = "")
      return()
    }
    
    dropout_code <- info$string
    
    # ---- Package ----
    package_block <- generate_packages_codes()
    
    # ---- Arm ----
    arm_block <- generate_arm_codes(vals$arms)
    
    # ---- Trial Info ----
    trial_info_block <- generate_trial_info_codes(input, vals$arms, info$string)
    
    # ---- Trial Event ----
    trial_events_block <- generate_trial_event_codes(vals$trial_events)
    
    final_code <- glue::glue("\n{package_block}\n\n", 
                             "\n{arm_block}\n\n", 
                             "#-----------------------------------------\n\n", 
                             "{trial_info_block}\n\n",
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
  
  output$code_buttons <- renderUI({
    req(code_text())
    tagList(
      actionButton(
        inputId = "run_code",
        label = tagList(icon("play"), "Run Code"),
        class = "btn btn-primary"
      ),
      
      rclipboard::rclipButton(
        inputId = "copy_code",
        label = tagList(icon("clipboard"), "Copy Code"),
        clipText = code_text(),
        class = "btn btn-primary"
      )
    )
  })
  
  # --- Run Code Button Logic ---
  observeEvent(input$run_code, {
    req(code_text())
    
    updateTabsetPanel(session, inputId = "tabs", selected = "Output")
    
    output$console_output <- renderPrint({
      eval_env <- new.env(parent = globalenv())
      
      out_text <- NULL
      msg_text <- NULL
      warn_text <- NULL
      
      tryCatch({
        output$code_plot <- renderPlot({
          eval(parse(text = code_text()), envir = eval_env)
        })
        
        # Capture printed output
        out_text <- capture.output({
          withCallingHandlers(
            eval(parse(text = code_text()), envir = eval_env),
            message = function(m) {
              msg_text <<- c(msg_text, paste0("", conditionMessage(m)))
              invokeRestart("muffleMessage")
            },
            warning = function(w) {
              warn_text <<- c(warn_text, paste0("Warning: ", conditionMessage(w)))
              invokeRestart("muffleWarning")
            }
          )
        })
      }, error = function(e) {
        output$code_plot <- renderPlot(NULL)
        out_text <<- paste("Error:", e$message)
      })
      
      cat(
        paste(
          c(out_text, msg_text, warn_text),
          collapse = "\n"
        )
      )
    })
  })
  
  
}
