server_arms <- function(input, output, session, vals) {
  
  vals$pending_endpoints <- list()
  vals$ep_table_raw <- NULL  # parsed from comma-separated input
  
  # ---- Reset dynamic inputs when ep_name changes ----
  observeEvent(input$ep_name, {
    isolate({
      num_fields <- length(unlist(strsplit(input$ep_name, ",\\s*")))
      for (i in seq_len(num_fields)) {
        updateCheckboxInput(session, paste0("is_tte_", i), value = FALSE)
        updateTextInput(session, paste0("readout_", i), value = "")
      }
    })
  })
  
  # ---- Dynamic UI for Parsed Endpoint Table ----
  output$parsed_endpoint_ui <- renderUI({
    req(nzchar(input$ep_name))
    
    ep_names <- unlist(strsplit(input$ep_name, ",\\s*"))
    vals$ep_table_raw <- ep_names
    
    if (length(ep_names) == 0) return(NULL)
    
    header <- fluidRow(
      column(4, strong("Endpoint")),
      column(2, strong("TTE")),
      column(6, strong("Readout"))
    )
    
    rows <- lapply(seq_along(ep_names), function(i) {
      tte_id <- paste0("is_tte_", i)
      readout_id <- paste0("readout_", i)
      
      tte_value <- input[[tte_id]] %||% FALSE
      readout_value <- input[[readout_id]] %||% ""
      
      readout_input <- tags$input(
        id = readout_id,
        type = "text",
        class = "form-control",
        value = if (isTRUE(tte_value)) "" else readout_value,
        placeholder = "Readout",
        disabled = if (isTRUE(tte_value)) "disabled" else NULL
      )
      
      fluidRow(
        column(4, div(ep_names[i], style = "margin-top: 6px;")),
        column(2, checkboxInput(tte_id, label = NULL, value = tte_value)),
        column(6, readout_input)
      )
    })
    
    tagList(header, rows)
  })
  
  # ---- Add Endpoint(s) ----
  observeEvent(input$add_ep, {
    
    if (!nzchar(input$ep_name)) {
      showNotification("Please specify at least one endpoint name.", type = "error")
      return()
    }
    
    if (!nzchar(input$ep_generator)) {
      showNotification("Please specify a generator before adding.", type = "error")
      return()
    }
    
    
    ep_names <- vals$ep_table_raw
    type_flags <- c()
    readout_map <- c()
    
    for (i in seq_along(ep_names)) {
      is_tte <- isolate(input[[paste0("is_tte_", i)]]) %||% FALSE
      readout <- if (!is_tte) isolate(input[[paste0("readout_", i)]]) %||% "" else ""
      
      type_flags <- c(type_flags, if (is_tte) "tte" else "non-tte")
      
      if (!is_tte && nzchar(readout)) {
        readout_map <- c(readout_map, sprintf("%s = %s", ep_names[i], readout))
      }
    }
    
    if (any(type_flags == "non-tte") && length(readout_map) < sum(type_flags == "non-tte")) {
      showNotification("Please specify readout for all non-TTE endpoints", type = "error")
      return()
    }
    
    entry <- list(
      name = sprintf("c(%s)", paste(shQuote(ep_names), collapse = ", ")),
      type = sprintf("c(%s)", paste(shQuote(type_flags), collapse = ", ")),
      readout = if (length(readout_map) > 0) sprintf("c(%s)", paste(readout_map, collapse = ", ")) else "",
      generator = input$ep_generator,
      args = input$ep_args
    )
    
    ep_id <- paste0("ep_", as.integer(Sys.time()))
    vals$pending_endpoints[[ep_id]] <- entry
    
    updateTextInput(session, "ep_name", value = "")
    updateTextInput(session, "ep_generator", value = "")
    updateTextInput(session, "ep_args", value = "")
    vals$ep_table_raw <- NULL
  })
  
  # ---- Delete Endpoint ----
  observeEvent(input$delete_ep, {
    selected <- input$endpoint_table_rows_selected
    if (length(selected) != 1) return()
    
    ep_ids <- names(vals$pending_endpoints)
    delete_id <- ep_ids[selected]
    vals$pending_endpoints[[delete_id]] <- NULL
  })
  
  # ---- Add Arm ----
  observeEvent(input$add_arm, {
    req(nzchar(input$arm_label), nzchar(input$arm_ratio))
    
    arm_id <- paste0("arm_", as.integer(Sys.time()))
    vals$arms[[arm_id]] <- list(
      label = input$arm_label,
      ratio = input$arm_ratio,
      endpoints = vals$pending_endpoints
    )
    
    vals$pending_endpoints <- list()
    updateTextInput(session, "arm_label", value = "")
    updateTextInput(session, "arm_ratio", value = "1")
  })
  
  # ---- Duplicate Arm ----
  observeEvent(input$duplicate_arm, {
    selected <- input$arm_table_rows_selected
    if (length(selected) != 1) return()
    
    arm_ids <- names(vals$arms)
    selected_id <- arm_ids[selected]
    original <- vals$arms[[selected_id]]
    
    new_label <- paste(original$label, "Copy")
    new_id <- paste0("arm_", as.integer(Sys.time()))
    
    vals$arms[[new_id]] <- list(
      label = new_label,
      ratio = original$ratio,
      endpoints = original$endpoints
    )
  })
  
  # ---- Delete Arm ----
  observeEvent(input$delete_arm, {
    selected <- input$arm_table_rows_selected
    if (length(selected) != 1) return()
    
    arm_ids <- names(vals$arms)
    delete_id <- arm_ids[selected]
    vals$arms[[delete_id]] <- NULL
  })
  
  # ---- Render Arm Table ----
  output$arm_table <- renderDT({
    if (length(vals$arms) == 0) return(datatable(data.frame()))
    
    df <- bind_rows(lapply(vals$arms, function(arm) {
      data.frame(Label = arm$label, Ratio = arm$ratio, Endpoints = length(arm$endpoints))
    }))
    datatable(df, selection = "single", rownames = FALSE)
  })
  
  # ---- Render Endpoint Table (pending only) ----
  output$endpoint_table <- renderDT({
    if (length(vals$pending_endpoints) == 0) return(datatable(data.frame()))
    
    df <- bind_rows(vals$pending_endpoints)
    datatable(df, selection = "single", rownames = FALSE)
  })
  
  # ---- Render View Endpoint Button ----
  output$view_ep_ui <- renderUI({
    if (length(vals$pending_endpoints) == 0) return(NULL)
    actionButton("view_ep", "🔍 View Endpoint", width = "100%")
  })
  
  # ---- View Endpoints ----
  observeEvent(input$view_ep, {
    selected <- input$endpoint_table_rows_selected
    if (length(selected) != 1) return()
    
    ep_ids <- names(vals$pending_endpoints)
    entry <- vals$pending_endpoints[[ep_ids[selected]]]
    
    showModal(modalDialog(
      title = "Endpoint Details",
      tagList(
        strong("Endpoint Name:"), p(entry$name),
        strong("Type:"), p(gsub("'", '"', entry$type)),
        strong("Readout:"), p(gsub("'", '"', entry$readout %||% "")),
        strong("Generator:"), p(entry$generator),
        strong("Arguments:"), p(entry$args)
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  
}
