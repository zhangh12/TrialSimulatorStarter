server_trial_events <- function(input, output, session, vals) {
  output$condition_ui <- renderUI({
    req(input$condition_type)
    switch(input$condition_type,
           "Calendar Time" = textInput("time", "Time", ""),
           "Patient Number" = textInput("patient_number", "Number of patients", ""),
           "Event Number" = tagList(
             textInput("endpoint_name", "Endpoint", ""),
             textInput("event_number", "Number of events", "")
           )
    )
  })
  
  observeEvent(input$add_condition, {
    req(input$condition_type)
    
    # Check required fields
    if (input$condition_type == "Calendar Time") req(nzchar(input$time))
    if (input$condition_type == "Patient Number") req(nzchar(input$patient_number))
    if (input$condition_type == "Event Number") {
      req(nzchar(input$endpoint_name))
      req(nzchar(input$event_number))
    }
    
    if (length(vals$condition_ids) == 0) {
      showNotification("Too many conditions for a trial event", type = "error")
      return()
    }
    
    new_id <- vals$condition_ids[1]
    vals$condition_ids <- vals$condition_ids[-1]
    
    condition <- switch(input$condition_type,
                        "Calendar Time" = list(ID = new_id, Type = "Calendar Time", Value = input$time),
                        "Patient Number" = list(ID = new_id, Type = "Patient Number", Value = input$patient_number),
                        "Event Number" = list(ID = new_id, Type = "Event Number", Value = glue::glue("{input$endpoint_name}:{input$event_number}"))
    )
    
    vals$conditions[[new_id]] <- condition
    
    # Reset inputs
    if (input$condition_type == "Calendar Time") updateTextInput(session, "time", value = "")
    if (input$condition_type == "Patient Number") updateTextInput(session, "patient_number", value = "")
    if (input$condition_type == "Event Number") {
      updateTextInput(session, "endpoint_name", value = "")
      updateTextInput(session, "event_number", value = "")
    }
  })
  
  output$condition_table <- renderDT({
    if (length(vals$conditions) == 0) return(datatable(data.frame()))
    df <- bind_rows(vals$conditions) %>% arrange(ID)
    datatable(df, selection = "single", rownames = FALSE)
  })
  
  observeEvent(input$delete_condition, {
    selected <- input$condition_table_rows_selected
    if (length(selected) == 0) return()
    cond_ids <- names(vals$conditions)
    remove_id <- cond_ids[selected]
    vals$condition_ids <- sort(c(vals$condition_ids, remove_id))
    vals$conditions[[remove_id]] <- NULL
  })
  
  observeEvent(input$add_trial_event, {
    req(nzchar(input$event_name))
    
    if (input$event_name %in% sapply(vals$trial_events, function(e) e$name)) {
      showNotification("A trial event with this name already exists.", type = "error")
      return()
    }
    
    if (length(vals$conditions) == 0) {
      showNotification("Please add at least one condition before saving a trial event.", type = "error")
      return()
    }
    
    logic_raw <- input$logic_expr
    valid_logic <- grepl("^\\s*([A-Z]|\\(|\\)|\\&|\\||\\s)+\\s*$", logic_raw)
    
    if (!nzchar(logic_raw) || !valid_logic) {
      showNotification("Logic expression must be non-empty and contain only A-Z, &, |, and parentheses.", type = "error")
      return()
    }
    
    used_ids <- unique(unlist(strsplit(gsub("[^A-Z]", "", logic_raw), "")))
    defined_ids <- names(vals$conditions)
    
    if (!setequal(used_ids, defined_ids)) {
      if (!all(used_ids %in% defined_ids)) {
        showNotification(glue::glue("Unknown condition ID(s): {paste(setdiff(used_ids, defined_ids), collapse = ', ')}"), type = "error")
        return()
      }
      if (!all(defined_ids %in% used_ids)) {
        showNotification(glue::glue("Missing condition ID(s): {paste(setdiff(defined_ids, used_ids), collapse = ', ')}"), type = "error")
        return()
      }
    }
    
    event_id <- paste0("event_", length(vals$trial_events) + 1)
    vals$trial_events[[event_id]] <- list(
      name = input$event_name,
      conditions = vals$conditions,
      logic = input$logic_expr
    )
    
    vals$conditions <- list()
    vals$condition_ids <- LETTERS
    updateTextInput(session, "event_name", value = "")
    updateTextInput(session, "logic_expr", value = "")
    updateRadioButtons(session, "condition_type", selected = character(0))
  })
  
  output$trial_event_table <- renderDT({
    if (length(vals$trial_events) == 0) return(datatable(data.frame()))
    df <- bind_rows(lapply(vals$trial_events, function(event) {
      data.frame(Name = event$name, Conditions = length(event$conditions), stringsAsFactors = FALSE)
    }))
    datatable(df, selection = "single", rownames = FALSE)
  })
  
  output$trial_event_buttons_ui <- renderUI({
    if (length(vals$trial_events) == 0) return(NULL)
    fluidRow(
      column(6, actionButton("view_trial_event", "🔍 View Conditions", width = "100%")),
      column(6, actionButton("delete_trial_event", "🗑️ Delete Trial Event", width = "100%"))
    )
  })
  
  observeEvent(input$view_trial_event, {
    selected <- input$trial_event_table_rows_selected
    if (length(selected) == 0) return()
    selected_event <- vals$trial_events[[names(vals$trial_events)[selected]]]
    cond_df <- bind_rows(selected_event$conditions) %>% arrange(ID)
    showModal(modalDialog(
      title = paste("Conditions in", selected_event$name),
      renderTable(cond_df),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  observeEvent(input$delete_trial_event, {
    selected <- input$trial_event_table_rows_selected
    if (length(selected) == 0) return()
    remove_id <- names(vals$trial_events)[selected]
    vals$trial_events[[remove_id]] <- NULL
  })
}
