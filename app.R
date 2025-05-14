
library(shiny)
library(DT)
library(glue)
library(shinyFiles)
library(fs)

ui <- fluidPage(
  titlePanel("Trial Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("arm_label", "Arm Label"),
      numericInput("arm_ratio", "Randomization Ratio", value = 1, min = 0.1),
      actionButton("add_arm", "➕ Add Arm"),
      hr(),
      textInput("ep_name", "Endpoint Name"),
      textInput("ep_type", "Endpoint Type"),
      textInput("ep_readout", "Readout"),
      textInput("ep_generator", "Generator"),
      textAreaInput("ep_args", "Generator Arguments", rows = 2),
      actionButton("add_ep", "Add Endpoint"),
      actionButton("delete_ep", "Delete Selected Endpoint"),
      hr(),
      textInput("new_event_name", "New Event Name"),
      actionButton("add_event", "➕ Add Event"),
      textInput("event_logic", "Logic (e.g., A and B)"),
      selectInput("cond_type", "Condition Type", c("calendar", "enrollment", "event")),
      textInput("cond_endpoint", "Endpoint"),
      textInput("cond_arms", "Arms"),
      numericInput("cond_value", "N or Time", 0),
      actionButton("add_condition", "Add Condition"),
      actionButton("delete_event", "Delete Selected Event"),
      hr(),
      shinySaveButton("save_code_btn", "💾 Save Code to File", "Save Code As...", filetype = list(R = "R"))
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Trial",
                           textInput("trial_name", "Trial Name"),
                           numericInput("trial_n", "Patient Number", value = 1000),
                           numericInput("trial_duration", "Trial Duration", value = 52),
                           textInput("recruitment_arg", "Recruitment Argument"),
                           textInput("dropout", "Dropout"),
                           textInput("dropout_arg", "Dropout Argument")
                  ),
                  tabPanel("Arms",
                           DTOutput("arm_table"),
                           DTOutput("endpoint_table")
                  ),
                  tabPanel("Events",
                           DTOutput("event_table"),
                           DTOutput("condition_table")
                  ),
                  tabPanel("Code",
                           verbatimTextOutput("code_preview")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  arms <- reactiveValues(data = data.frame(Label = character(), Ratio = numeric(), stringsAsFactors = FALSE),
                         endpoints = list(),
                         selected = NULL)
  events <- reactiveValues(data = data.frame(Name = character(), Logic = character(), stringsAsFactors = FALSE),
                           conditions = list(),
                           selected = NULL,
                           cond_id = 1)
  
  volumes <- c(Home = fs::path_home(), Working = getwd())
  shinyFileSave(input, "save_code_btn", roots = volumes, session = session)
  
  observeEvent(input$add_arm, {
    if (nzchar(input$arm_label)) {
      arms$data <- rbind(arms$data, data.frame(Label = input$arm_label, Ratio = input$arm_ratio))
      arms$endpoints[[input$arm_label]] <- data.frame(Name = character(), Type = character(), Readout = character(),
                                                      Generator = character(), Args = character(), stringsAsFactors = FALSE)
    }
  })
  
  output$arm_table <- renderDT({
    datatable(arms$data, selection = "single", editable = FALSE)
  })
  
  observe({
    sel <- input$arm_table_rows_selected
    if (length(sel) > 0) {
      arms$selected <- arms$data$Label[sel]
    } else {
      arms$selected <- NULL
    }
  })
  
  observeEvent(input$add_ep, {
    sel <- arms$selected
    if (!is.null(sel)) {
      df <- arms$endpoints[[sel]]
      df <- rbind(df, data.frame(Name = input$ep_name, Type = input$ep_type, Readout = input$ep_readout,
                                 Generator = input$ep_generator, Args = input$ep_args))
      arms$endpoints[[sel]] <- df
    }
  })
  
  output$endpoint_table <- renderDT({
    sel <- arms$selected
    if (!is.null(sel)) {
      datatable(arms$endpoints[[sel]], selection = "single")
    }
  })
  
  observeEvent(input$delete_ep, {
    sel <- arms$selected
    ep_row <- input$endpoint_table_rows_selected
    if (!is.null(sel) && length(ep_row) > 0) {
      df <- arms$endpoints[[sel]]
      df <- df[-ep_row, ]
      arms$endpoints[[sel]] <- df
    }
  })
  
  observeEvent(input$add_event, {
    if (nzchar(input$new_event_name)) {
      events$data <- rbind(events$data, data.frame(Name = input$new_event_name, Logic = "", stringsAsFactors = FALSE))
      events$conditions[[input$new_event_name]] <- list()
    }
  })
  
  output$event_table <- renderDT({
    datatable(events$data, selection = "single")
  })
  
  observe({
    sel <- input$event_table_rows_selected
    if (length(sel) > 0) {
      events$selected <- events$data$Name[sel]
    } else {
      events$selected <- NULL
    }
  })
  
  observeEvent(input$add_condition, {
    name <- events$selected
    if (!is.null(name)) {
      id <- LETTERS[events$cond_id]
      cond <- list(
        type = input$cond_type,
        endpoint = input$cond_endpoint,
        arms = input$cond_arms
      )
      if (input$cond_type == "calendar") {
        cond$time <- input$cond_value
      } else {
        cond$n <- input$cond_value
      }
      events$conditions[[name]][[id]] <- cond
      events$cond_id <- events$cond_id + 1
      
      ix <- which(events$data$Name == name)
      events$data$Logic[ix] <- input$event_logic
    }
  })
  
  output$condition_table <- renderDT({
    name <- events$selected
    if (!is.null(name)) {
      conds <- events$conditions[[name]]
      df <- do.call(rbind, lapply(names(conds), function(id) {
        cbind(ID = id, as.data.frame(conds[[id]], stringsAsFactors = FALSE))
      }))
      datatable(df)
    }
  })
  
  observeEvent(input$delete_event, {
    name <- events$selected
    if (!is.null(name)) {
      events$data <- events$data[events$data$Name != name, ]
      events$conditions[[name]] <- NULL
      events$selected <- NULL
    }
  })
  
  reactiveCode <- reactive({
    arm_code <- mapply(function(label, ratio) {
      eps <- arms$endpoints[[label]]
      ep_code <- apply(eps, 1, function(ep) {
        glue("define_endpoint(name = '{ep[1]}', type = '{ep[2]}', readout = '{ep[3]}', generator = '{ep[4]}', args = '{ep[5]}')")
      })
      glue("define_arm(name = '{label}', label = '{label}', ratio = {ratio}, endpoints = list(
  {paste(ep_code, collapse = ',\n  ')}
))")
    }, arms$data$Label, arms$data$Ratio, SIMPLIFY = FALSE)
    
    event_code <- apply(events$data, 1, function(row) {
      name <- row["Name"]
      logic <- row["Logic"]
      conds <- events$conditions[[name]]
      cond_lines <- mapply(function(id, cond) {
        common <- glue("type = '{cond$type}', endpoint = '{cond$endpoint}', arms = '{cond$arms}'")
        extra <- if (cond$type == "calendar") glue("time = {cond$time}") else glue("n = {cond$n}")
        glue("{id} = define_condition({common}, {extra})")
      }, names(conds), conds, SIMPLIFY = TRUE)
      glue("define_event(name = '{name}', logic = '{logic}', conditions = list(
  {paste(cond_lines, collapse = ',\n  ')}
))")
    })
    
    trial_code <- glue("define_trial(name = '{input$trial_name}', patients = {input$trial_n}, duration = {input$trial_duration}, enroller = '{input$recruitment_arg}', dropout = '{input$dropout}', dropout_args = '{input$dropout_arg}')")
    
    paste(c(arm_code, event_code, trial_code), collapse = "\n\n")
  })
  
  output$code_preview <- renderText({ reactiveCode() })
  
  observeEvent(input$save_code_btn, {
    fileinfo <- parseSavePath(volumes, input$save_code_btn)
    if (nrow(fileinfo) > 0) {
      filepath <- as.character(fileinfo$datapath)
      code <- isolate(reactiveCode())
      writeLines(code, filepath)
      showModal(modalDialog("R code saved successfully.", easyClose = TRUE))
    }
  })
}

shinyApp(ui, server)
