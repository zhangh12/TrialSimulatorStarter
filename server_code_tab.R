server_code_tab <- function(input, output, session, vals, code_text) {
  observe({
    if (length(vals$trial_events) == 0) {
      updateAceEditor(session, "code", value = "")
      return()
    }
    
    codes <- sapply(vals$trial_events, function(event) {
      cond_map <- sapply(event$conditions, function(cond) {
        switch(cond$Type,
               "Calendar Time" = glue::glue("calendarTime(time = {cond$Value})"),
               "Patient Number" = glue::glue("enrollment(n = {cond$Value})"),
               "Event Number" = {
                 parts <- strsplit(cond$Value, ":")[[1]]
                 glue::glue('eventNumber(endpoint = "{parts[1]}", n = {parts[2]})')
               }
        )
      }, USE.NAMES = TRUE)
      
      logic_expr <- event$logic
      for (id in names(cond_map)) {
        logic_expr <- gsub(paste0("\\b", id, "\\b"), cond_map[[id]], logic_expr)
      }
      
      event_variable <- gsub("\\s+", "_", event$name)
      
      glue::glue(
        "{event_variable}_action <- function(trial, event_name){{\n\n",
        "  locked_data <- trial$get_locked_data(event_name)\n\n",
        "  NULL\n\n}}\n\n",
        "{event_variable} <- event(\n",
        '  name = "{event$name}", \n',
        "  trigger_condition = {logic_expr}, \n",
        "  action = {event_variable}_action\n)"
      )
    })
    
    final_code <- paste(codes, collapse = "\n\n")
    event_names <- paste(sapply(vals$trial_events, function(event) gsub("\\s+", "_", event$name)), collapse = ", ")
    final_code <- glue::glue("{final_code}\n\n",
                             "listener <- listener()\n",
                             "listener$add_events({event_names})\n\n",
                             "controller <- controller(trial, listener)\n",
                             "controller$run(n = 1, plot_event = TRUE)\n")
    
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
