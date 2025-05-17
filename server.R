
server <- 
  function(input, output, session) {
    
    code_text <- reactiveVal('')
    
    vals <- reactiveValues(
      conditions = list(), 
      condition_ids = LETTERS,
      trial_events = list()
    )
    
    observeEvent(input$load_config, {
      req(input$load_config)
      
      tryCatch({
        loaded <- jsonlite::read_json(input$load_config$datapath, simplifyVector = TRUE)
        validate(
          need(is.list(loaded), "Invalid configuration file format.")
        )
        vals$trial_events <- loaded
        showNotification("Config loaded successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Failed to load config:", e$message), type = "error")
      })
    })
    
    output$save_config <- downloadHandler(
      filename = function() {
        paste0("trial_config_", Sys.Date(), ".json")
      },
      content = function(file) {
        # Save only trial events (you can expand this if needed)
        jsonlite::write_json(
          vals$trial_events,
          path = file,
          pretty = TRUE,
          auto_unbox = TRUE
        )
      }
    )
    
    
    
    output$condition_ui <- renderUI({
      req(input$condition_type)
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$condition_type,
             'Calendar Time' = textInput(inputId = 'time', 
                                         label = 'Time', 
                                         value = ''),
             'Patient Number' = textInput(inputId = 'patient_number', 
                                          label = 'Number of patients',
                                          value = ''), 
             'Event Number' = tagList(textInput(inputId = 'endpoint_name', 
                                                label = 'Endpoint',
                                                value = ''), 
                                      textInput(inputId = 'event_number', 
                                                label = 'Number of events',
                                                value = '')
             )
      )
    })
    
    ## add condition
    observeEvent(input$add_condition, {
      
      req(input$condition_type)
      
      ## make sure non-empty inputs when adding a condition
      if(input$condition_type == 'Calendar Time'){
        req(nzchar(input$time))
      }
      
      if(input$condition_type == 'Patient Number'){
        req(nzchar(input$patient_number))
      }
      
      if(input$condition_type == 'Event Number'){
        req(nzchar(input$endpoint_name))
        req(nzchar(input$event_number))
      }
      
      if(length(vals$condition_ids) == 0){
        showNotification('Too many conditions for a trial event', type = 'error')
        return()
      }
      
      new_id <- vals$condition_ids[1]
      vals$condition_ids <- vals$condition_ids[-1]
      
      condition <- switch(input$condition_type,
                          'Calendar Time' = list(ID = new_id, Type = "Calendar Time", Value = input$time),
                          'Patient Number' = list(ID = new_id, Type = "Patient Number", Value = input$patient_number),
                          'Event Number' = list(ID = new_id, Type = "Event Number", Value = glue::glue('{input$endpoint_name}:{input$event_number}'))
      )
      vals$conditions[[new_id]] <- condition
      
      ## reset inputs
      if(input$condition_type == 'Calendar Time'){
        updateTextInput(session, 'time', value = '')
      }
      
      if(input$condition_type == 'Patient Number'){
        updateTextInput(session, 'patient_number', value = '')
      }
      
      if(input$condition_type == 'Event Number'){
        updateTextInput(session, 'endpoint_name', value = '')
        updateTextInput(session, 'event_number', value = '')
      }
    })
    
    # Render condition table
    output$condition_table <- renderDT({
      if (length(vals$conditions) == 0) return(DT::datatable(data.frame()))
      df <- bind_rows(vals$conditions) %>% 
        arrange(ID)
      datatable(df, selection = "single", rownames = FALSE)
    })
    
    # Delete condition
    observeEvent(input$delete_condition, {
      selected <- input$condition_table_rows_selected
      if (length(selected) == 0) return()
      cond_ids <- names(vals$conditions)
      remove_id <- cond_ids[selected]
      
      vals$condition_ids <- sort(c(vals$condition_ids, remove_id))
      vals$conditions[[remove_id]] <- NULL
    })
    
    # add a trial event
    observeEvent(input$add_trial_event, {
      req(nzchar(input$event_name))
      
      # Check for duplicates (case-sensitive)
      existing_names <- sapply(vals$trial_events, function(e) e$name)
      if (input$event_name %in% existing_names) {
        showNotification('A trial event with this name already exists.', type = 'error')
        return()
      }
      
      if (length(vals$conditions) == 0) {
        showNotification('Please add at least one condition before saving a trial event.', type = 'error')
        return()
      }
      
      ## make sure that logic expression only use existing condition IDs and logic operators
      logic_raw <- input$logic_expr
      valid_logic <- grepl('^\\s*([A-Z]|\\(|\\)|\\&|\\||\\s)+\\s*$', logic_raw)
      
      if (!nzchar(logic_raw) || !valid_logic) {
        showNotification('Logic expression must be non-empty and contain only A-Z, &, |, and parentheses.', type = 'error')
        return()
      }
      
      used_ids <- unique(unlist(strsplit(gsub('[^A-Z]', '', logic_raw), '')))
      defined_ids <- names(vals$conditions)
      
      if (!setequal(used_ids, defined_ids)) {
        if(!all(used_ids %in% defined_ids)){
          showNotification(glue::glue('Unknown condition ID(s): {paste(setdiff(used_ids, defined_ids), collapse = ", ")}'), type = 'error')
          return()
        }
        
        if(!all(defined_ids %in% used_ids)){
          showNotification(glue::glue('Missing condition ID(s): {paste(setdiff(defined_ids, used_ids), collapse = ", ")}'), type = 'error')
          return()
        }
        
      }
      
      # Save the current trial event
      event_id <- paste0("event_", length(vals$trial_events) + 1)
      vals$trial_events[[event_id]] <- list(
        name = input$event_name,
        conditions = vals$conditions,
        logic = input$logic_expr
      )
      
      # Reset condition builder
      vals$conditions <- list()
      vals$condition_ids <- LETTERS
      updateTextInput(session, "event_name", value = "")
      updateTextInput(session, "logic_expr", value = "")
      updateRadioButtons(session, "condition_type", selected = character(0))
    })
    
    # render trial event table
    output$trial_event_table <- renderDT({
      if (length(vals$trial_events) == 0) {
        return(DT::datatable(data.frame()))
      }
      
      df <- bind_rows(lapply(vals$trial_events, function(event) {
        data.frame(
          Name = event$name,
          Conditions = length(event$conditions),
          stringsAsFactors = FALSE
        )
      }))
      
      datatable(df, selection = 'single', rownames = FALSE)
    })
    
    # manage buttons of viewing and deleting a trial event
    output$trial_event_buttons_ui <- renderUI({
      if (length(vals$trial_events) == 0) return(NULL)
      
      fluidRow(
        column(6, actionButton("view_trial_event", "🔍 View Conditions", width = "100%")),
        column(6, actionButton("delete_trial_event", "🗑️ Delete Trial Event", width = "100%"))
      )
    })
    
    
    # render condition table of selected trial event
    observeEvent(input$view_trial_event, {
      selected <- input$trial_event_table_rows_selected
      if (length(selected) == 0) return()
      
      event_ids <- names(vals$trial_events)
      selected_event <- vals$trial_events[[event_ids[selected]]]
      
      cond_df <- bind_rows(selected_event$conditions) %>%
        arrange(ID)
      
      showModal(modalDialog(
        title = paste("Conditions in", selected_event$name),
        renderTable(cond_df),
        easyClose = TRUE,
        size = "l"
      ))
    })
    
    
    # delete a trial event
    observeEvent(input$delete_trial_event, {
      selected <- input$trial_event_table_rows_selected
      if (length(selected) == 0) return()
      
      event_ids <- names(vals$trial_events)
      delete_id <- event_ids[selected]
      
      vals$trial_events[[delete_id]] <- NULL
    })
    
    
    # render code editor
    observe({
      if(length(vals$trial_events) == 0){
        updateAceEditor(session, 'code', value = '')
        return()
      }
      
      codes <- sapply(vals$trial_events, function(event){
        
        cond_map <- sapply(event$conditions, function(cond){
          
          switch(cond$Type, 
                 'Calendar Time' = glue::glue('calendarTime(time = {cond$Value})'), 
                 'Patient Number' = glue::glue('enrollment(n = {cond$Value})'), 
                 'Event Number' = {
                   parts <- strsplit(cond$Value, ':')[[1]]
                   glue::glue('eventNumber(endpoint = "{parts[1]}", n = {parts[2]})')
                 }
          )
        }, USE.NAMES = TRUE)
        
        logic_expr <- event$logic
        for (id in names(cond_map)) {
          logic_expr <- gsub(paste0("\\b", id, "\\b"), cond_map[[id]], logic_expr)
        }
        
        event_variable <- gsub('\\s+', '_', event$name)
        
        glue::glue(
          '{event_variable}_action <- function(trial, event_name){{\n\n',
          '  locked_data <- trial$get_locked_data(event_name)\n\n',
          '  NULL\n\n}}\n\n',
          '{event_variable} <- event(\n',
          '  name = "{event$name}", \n',
          '  trigger_condition = {logic_expr}, \n',
          '  action = {event_variable}_action\n)'
        )
      })
      
      final_code <- paste(codes, collapse = '\n\n')
      
      event_names <- lapply(vals$trial_events, function(event){gsub('\\s+', '_', event$name)}) %>%
        paste(collapse = ',')
      
      final_code <- glue::glue('{final_code}\n\n', 
                               'listener <- listener()\n', 
                               'listener$add_events(',
                               '{event_names}',
                               ')\n')
      
      final_code <- glue::glue('{final_code}\n\n', 
                               'controller <- controller(trial, listener)\n',
                               'controller$run(n = 1, plot_event = TRUE)\n\n')
      
      updateAceEditor(session, 'code', value = final_code)
      code_text(final_code)  # store for clipboard
      
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

