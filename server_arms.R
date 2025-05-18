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
      
      fluidRow(
        column(4, div(ep_names[i], style = "margin-top: 6px;")),
        column(2, checkboxInput(tte_id, label = NULL, value = FALSE)),
        column(6, textInput(readout_id, label = NULL, value = "", placeholder = "Readout"))
      )
    })
    
    tagList(header, rows)
  })
  
  # ---- Add Endpoint(s) ----
  observeEvent(input$add_ep, {
    req(vals$ep_table_raw)
    
    for (i in seq_along(vals$ep_table_raw)) {
      ep_name <- vals$ep_table_raw[i]
      is_tte <- input[[paste0("is_tte_", i)]] %||% FALSE
      readout <- input[[paste0("readout_", i)]] %||% ""
      
      ep_id <- paste0("ep_", as.integer(Sys.time()), "_", i)
      vals$pending_endpoints[[ep_id]] <- list(
        name = ep_name,
        type = "",
        readout = readout,
        generator = "",
        args = "",
        is_tte = is_tte
      )
    }
    
    updateTextInput(session, "ep_name", value = "")
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
}
