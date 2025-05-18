server_arms <- function(input, output, session, vals) {
  
  # Store endpoints before arm is finalized
  vals$pending_endpoints <- list()
  
  # ---- Add Endpoint ----
  observeEvent(input$add_ep, {
    req(nzchar(input$ep_name))
    
    ep_id <- paste0("ep_", as.integer(Sys.time()))
    vals$pending_endpoints[[ep_id]] <- list(
      name = input$ep_name,
      type = input$ep_type,
      readout = input$ep_readout,
      generator = input$ep_generator,
      args = input$ep_args
    )
    
    updateTextInput(session, "ep_name", value = "")
    updateTextInput(session, "ep_type", value = "")
    updateTextInput(session, "ep_readout", value = "")
    updateTextInput(session, "ep_generator", value = "")
    updateTextAreaInput(session, "ep_args", value = "")
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
    req(nzchar(input$arm_label), input$arm_ratio > 0)
    
    arm_id <- paste0("arm_", as.integer(Sys.time()))
    vals$arms[[arm_id]] <- list(
      label = input$arm_label,
      ratio = input$arm_ratio,
      endpoints = vals$pending_endpoints
    )
    
    vals$pending_endpoints <- list()
    updateTextInput(session, "arm_label", value = "")
    updateNumericInput(session, "arm_ratio", value = 1)
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
