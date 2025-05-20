server_arms <- function(input, output, session, vals) {
  
  vals$editing_arm_id <- NULL
  vals$editing_ep_id <- NULL
  
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
    
    if (is.null(vals$editing_ep_id)) {
      if (!nzchar(input$ep_name)) {
        showNotification("Please specify at least one endpoint name.", type = "error")
        return()
      }
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
    
    ep_names_ <- if (is.null(vals$editing_ep_id)) {
      sprintf("c(%s)", paste(shQuote(ep_names), collapse = ", "))
    } else {
      vals$pending_endpoints[[vals$editing_ep_id]]$name
    }
    
    type_flags_ <- if (is.null(vals$editing_ep_id)) {
      sprintf("c(%s)", paste(shQuote(type_flags), collapse = ", "))
    } else {
      vals$pending_endpoints[[vals$editing_ep_id]]$type
    }
    
    readout_ <- if (is.null(vals$editing_ep_id)) {
      if (length(readout_map) > 0) sprintf("c(%s)", paste(readout_map, collapse = ", ")) else ""
    } else {
      vals$pending_endpoints[[vals$editing_ep_id]]$readout
    }
    
    entry <- list(
      name = ep_names_,
      type = type_flags_,
      readout = readout_,
      generator = input$ep_generator,
      args = input$ep_args
    )
    
    ep_id <- paste0("ep_", as.integer(Sys.time()))
    if (is.null(vals$editing_ep_id)) {
      # Normal add
      ep_id <- paste0("ep_", as.integer(Sys.time()))
      vals$pending_endpoints[[ep_id]] <- entry
    } else {
      # Update
      vals$pending_endpoints[[vals$editing_ep_id]] <- entry
      vals$editing_ep_id <- NULL
    }
    
    updateTextInput(session, "ep_name", value = "")
    updateTextInput(session, "ep_generator", value = "")
    updateTextInput(session, "ep_args", value = "")
    vals$ep_table_raw <- NULL
  })
  
  observeEvent(input$edit_ep, {
    selected <- input$endpoint_table_rows_selected
    if (length(selected) != 1) return()
    
    ep_ids <- names(vals$pending_endpoints)
    ep <- vals$pending_endpoints[[ep_ids[selected]]]
    
    vals$editing_ep_id <- ep_ids[selected]
    
    updateTextInput(session, "ep_generator", value = ep$generator)
    updateTextInput(session, "ep_args", value = ep$args)
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
    
    if (!is.null(vals$editing_arm_id)) {
      # ✅ Save arm being edited
      if (!nzchar(input$arm_label)) {
        showNotification("Please provide arm label.", type = "error")
        return()
      }
      if (!nzchar(input$arm_ratio)) {
        showNotification("Please provide randomization ratio.", type = "error")
        return()
      }
      
      vals$arms[[vals$editing_arm_id]]$label <- input$arm_label
      vals$arms[[vals$editing_arm_id]]$ratio <- input$arm_ratio
      vals$arms[[vals$editing_arm_id]]$endpoints <- vals$pending_endpoints
      
      vals$editing_arm_id <- NULL
      vals$pending_endpoints <- list()
      vals$ep_table_raw <- NULL
      
      updateTextInput(session, "arm_label", value = "")
      updateTextInput(session, "arm_ratio", value = "")
      updateTextInput(session, "ep_name", value = "")
      updateTextInput(session, "ep_generator", value = "")
      updateTextInput(session, "ep_args", value = "")
      
      showNotification("✅ Arm updated", type = "message")
    } else {
      # ✅ Check if we're duplicating instead of adding
      if (length(input$arm_table_rows_selected) == 1 && length(vals$arms) > 0) {
        selected_id <- names(vals$arms)[input$arm_table_rows_selected]
        original <- vals$arms[[selected_id]]
        
        base_label <- paste0(original$label, " Copy")
        all_labels <- sapply(vals$arms, function(a) a$label)
        label <- base_label
        i <- 2
        while (label %in% all_labels) {
          label <- paste0(base_label, " ", i)
          i <- i + 1
        }
        
        new_id <- paste0("arm_", as.integer(Sys.time()))
        vals$arms[[new_id]] <- list(
          label = label,
          ratio = original$ratio,
          endpoints = original$endpoints
        )
        
        showNotification(glue::glue("✅ Arm duplicated: {label}"), type = "message")
        return()  # ❗ Do not continue to manual add path
      }
      
      # ✅ Add arm manually
      if (!nzchar(input$arm_label)) {
        showNotification("Please provide arm label.", type = "error")
        return()
      }
      if (!nzchar(input$arm_ratio)) {
        showNotification("Please provide randomization ratio.", type = "error")
        return()
      }
      
      new_id <- paste0("arm_", as.integer(Sys.time()))
      vals$arms[[new_id]] <- list(
        label = input$arm_label,
        ratio = input$arm_ratio,
        endpoints = vals$pending_endpoints
      )
      
      vals$pending_endpoints <- list()
      vals$ep_table_raw <- NULL
      
      updateTextInput(session, "arm_label", value = "")
      updateTextInput(session, "arm_ratio", value = "")
      updateTextInput(session, "ep_name", value = "")
      updateTextInput(session, "ep_generator", value = "")
      updateTextInput(session, "ep_args", value = "")
      
      showNotification("✅ Arm added", type = "message")
    }
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
  
  # ---- Render View and Delete Endpoint Buttons ----
  output$view_or_delete_ep_ui <- renderUI({
    if (length(vals$pending_endpoints) == 0) return(NULL)
    
    div(style = "margin-top: 100px;", uiOutput("pending_ep_buttons"))
    fluidRow(
      column(6, actionButton("delete_ep", "🗑️ Delete Endpoint", width = "100%")),
      column(6, actionButton("view_ep", "🔍 View Endpoint", width = "100%"))
    )
  })
  
  output$edit_ep_ui <- renderUI({
    if (length(vals$pending_endpoints) == 0 || is.null(vals$editing_arm_id)) return(NULL)
    if (length(input$endpoint_table_rows_selected) != 1) return(NULL)
    
    actionButton("edit_ep", "✏️ Edit Endpoint", width = "100%")
  })
  
  output$add_or_update_ep_button <- renderUI({
    label <- if (is.null(vals$editing_ep_id)) "➕ Add Endpoint" else "💾 Update Endpoint"
    actionButton("add_ep", label)
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
  
  # ---- Dynamic Main Arm Button ----
  output$arm_main_button <- renderUI({
    label <- if (length(vals$arms) == 0) "➕ Add Arm" else "📄 Duplicate Arm"
    actionButton("add_arm", label)
  })
  
  # ---- Dynamic Table Buttons (Edit/Delete) ----
  output$arm_table_buttons <- renderUI({
    if (length(vals$arms) == 0 || length(input$arm_table_rows_selected) != 1) return(NULL)
    
    fluidRow(
      column(6, uiOutput("edit_or_save_arm_button")),
      column(6, actionButton("delete_arm", "🗑️ Delete Arm", width = "100%"))
    )
  })
  
  # ---- Edit Arm ----
  observeEvent(input$edit_arm, {
    selected <- input$arm_table_rows_selected
    if (length(selected) != 1) return()
    
    arm_ids <- names(vals$arms)
    selected_id <- arm_ids[selected]
    arm <- vals$arms[[selected_id]]
    
    # Store edit mode
    vals$editing_arm_id <- selected_id
    vals$pending_endpoints <- arm$endpoints
    
    # Populate UI
    updateTextInput(session, "arm_label", value = arm$label)
    updateTextInput(session, "arm_ratio", value = arm$ratio)
  })
  
  output$edit_or_save_arm_button <- renderUI({
    if (length(vals$arms) == 0 || length(input$arm_table_rows_selected) != 1) return(NULL)
    
    if (is.null(vals$editing_arm_id)) {
      actionButton("edit_arm", "✏️ Edit Arm", width = "100%")
    } else {
      actionButton("save_arm", "💾 Save Arm", width = "100%")
    }
  })
  
  observeEvent(input$save_arm, {
    if (!nzchar(input$arm_label)) {
      showNotification("Please provide arm label.", type = "error")
      return()
    }
    if (!nzchar(input$arm_ratio)) {
      showNotification("Please provide randomization ratio.", type = "error")
      return()
    }
    
    # Update arm
    if (!is.null(vals$editing_arm_id)) {
      vals$arms[[vals$editing_arm_id]]$label <- input$arm_label
      vals$arms[[vals$editing_arm_id]]$ratio <- input$arm_ratio
      vals$arms[[vals$editing_arm_id]]$endpoints <- vals$pending_endpoints
      
      vals$editing_arm_id <- NULL
      vals$pending_endpoints <- list()
      vals$ep_table_raw <- NULL
      
      updateTextInput(session, "arm_label", value = "")
      updateTextInput(session, "arm_ratio", value = "")
      updateTextInput(session, "ep_name", value = "")
      updateTextInput(session, "ep_generator", value = "")
      updateTextInput(session, "ep_args", value = "")
      
      showNotification("✅ Arm updated", type = "message")
    }
  })
  
  
  
  
  # ---- Disable Arm Inputs When Arms Exist ----
  observe({
    is_disabled <- length(vals$arms) > 0 && is.null(vals$editing_arm_id)
    
    input_ids <- c("arm_label", "arm_ratio", "ep_name", "ep_generator", "ep_args")
    
    for (id in input_ids) {
      if (is_disabled) {
        shinyjs::disable(id)
      } else {
        shinyjs::enable(id)
      }
    }
    
    if (is_disabled) {
      # also disable endpoint-specific fields
      for (i in seq_along(vals$ep_table_raw)) {
        shinyjs::disable(paste0("is_tte_", i))
        shinyjs::disable(paste0("readout_", i))
      }
    }
    
    if (!is.null(vals$editing_ep_id)) {
      shinyjs::disable("ep_name")
    } else if (!is_disabled) {
      shinyjs::enable("ep_name")
    }
    
    if (!is.null(vals$editing_arm_id)) {
      shinyjs::disable("add_arm")
    } else {
      shinyjs::enable("add_arm")
    }
    
  })
  
  
  
  
}
