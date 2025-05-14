library(shiny)
library(glue)
library(shinyFiles)
library(fs)

# ---- Arm Module UI ----
armModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("label"), "Arm Label"),
    uiOutput(ns("endpoint_selector")),
    textInput(ns("endpoint_name"), "Endpoint Name"),
    textInput(ns("endpoint_type"), "Endpoint Type"),
    textInput(ns("endpoint_readout"), "Readout"),
    textInput(ns("endpoint_generator"), "Generator"),
    textAreaInput(ns("endpoint_args"), "Generator Arguments", rows = 3),
    actionButton(ns("add_endpoint"), "Add Endpoint"),
    actionButton(ns("delete_endpoint"), "Delete Endpoint")
  )
}

# ---- Arm Module Server ----
armModuleServer <- function(id, label) {
  moduleServer(id, function(input, output, session) {
    state <- reactiveValues(endpoints = list(), selected = NULL)
    ns <- session$ns
    
    observeEvent(input$add_endpoint, {
      name <- input$endpoint_name
      if (nzchar(name)) {
        state$endpoints[[name]] <- list(
          name = name,
          type = input$endpoint_type,
          readout = input$endpoint_readout,
          generator = input$endpoint_generator,
          generator_args = input$endpoint_args
        )
        state$selected <- name
        updateTextInput(session, "endpoint_name", value = "")
        updateTextInput(session, "endpoint_type", value = "")
        updateTextInput(session, "endpoint_readout", value = "")
        updateTextInput(session, "endpoint_generator", value = "")
        updateTextAreaInput(session, "endpoint_args", value = "")
      }
    })
    
    observeEvent(input$delete_endpoint, {
      ep <- state$selected
      if (!is.null(ep)) {
        state$endpoints[[ep]] <- NULL
        state$selected <- NULL
        updateTextInput(session, "endpoint_name", value = "")
        updateTextInput(session, "endpoint_type", value = "")
        updateTextInput(session, "endpoint_readout", value = "")
        updateTextInput(session, "endpoint_generator", value = "")
        updateTextAreaInput(session, "endpoint_args", value = "")
      }
    })
    
    observeEvent(input$endpoint_selector, {
      ep <- input$endpoint_selector
      if (!is.null(ep) && ep %in% names(state$endpoints)) {
        ep_data <- state$endpoints[[ep]]
        updateTextInput(session, "endpoint_name", value = ep_data$name)
        updateTextInput(session, "endpoint_type", value = ep_data$type)
        updateTextInput(session, "endpoint_readout", value = ep_data$readout)
        updateTextInput(session, "endpoint_generator", value = ep_data$generator)
        updateTextAreaInput(session, "endpoint_args", value = ep_data$generator_args)
        state$selected <- ep
      }
    })
    
    output$endpoint_selector <- renderUI({
      if (length(state$endpoints) == 0) return(NULL)
      selectInput(ns("endpoint_selector"), "Select Endpoint", choices = names(state$endpoints), selected = state$selected)
    })
    
    return(reactive({
      list(name = label, label = input$label, endpoints = state$endpoints)
    }))
  })
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Modular Trial Designer"),
  sidebarLayout(
    sidebarPanel(
      textInput("new_arm", "New Arm Name"),
      actionButton("add_arm", "➕ Add Arm"),
      textInput("new_event_name", "New Event Name"),
      actionButton("add_event", "➕ Add Event"),
      shinySaveButton("save_code_btn", "💾 Save Code to File", "Save Code As...", filetype = list(R = "R"))
    ),
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Code", verbatimTextOutput("code_preview")),
                  tabPanel("Trial",
                           textInput("trial_name", "Trial Name"),
                           dateInput("trial_start", "Start Date"),
                           numericInput("trial_n", "Max Sample Size", value = 1000)
                  ),
                  tabPanel("Events", uiOutput("events_ui"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  arms <- reactiveValues(labels = character(), modules = list())
  events <- reactiveValues(all = list())
  
  volumes <- c(Home = fs::path_home(), Working = getwd())
  shinyFileSave(input, "save_code_btn", roots = volumes, session = session)
  
  observeEvent(input$add_arm, {
    label <- input$new_arm
    id <- paste0("arm_", label)
    if (nzchar(label) && !(label %in% arms$labels)) {
      insertTab("main_tabs", tabPanel(label, armModuleUI(id)), target = "Code", position = "before")
      arms$labels <- c(arms$labels, label)
      arms$modules[[label]] <- armModuleServer(id, label)
    }
  })
  
  observeEvent(input$add_event, {
    name <- input$new_event_name
    if (nzchar(name) && !(name %in% names(events$all))) {
      events$all[[name]] <- reactiveValues(
        name = name,
        logic = "",
        conditions = list(),
        id_counter = 1
      )
    }
  })
  
  output$events_ui <- renderUI({
    tagList(
      lapply(names(events$all), function(e_name) {
        ev <- events$all[[e_name]]
        ns <- NS(e_name)
        cond_ui <- lapply(names(ev$conditions), function(id) {
          cond <- ev$conditions[[id]]
          fluidRow(
            column(2, strong(id)),
            column(2, cond$type),
            column(2, cond$endpoint),
            column(2, cond$arms),
            column(2, ifelse(cond$type == "calendar", paste("Time:", cond$time), paste("N:", cond$n))),
            column(2, actionButton(ns(paste0("del_", id)), "X", class = "btn-danger btn-sm"))
          )
        })
        tagList(
          h4(glue("Event: {e_name}")),
          fluidRow(
            column(3, selectInput(ns("type"), "Type", c("calendar", "enrollment", "event"))),
            column(3, textInput(ns("endpoint"), "Endpoint")),
            column(3, textInput(ns("arms"), "Arms")),
            column(3, numericInput(ns("value"), "N or Time", 0))
          ),
          actionButton(ns("add_condition"), "Add Condition"),
          br(),
          do.call(tagList, cond_ui),
          textInput(ns("logic_input"), "Logic (e.g., A and B)", value = isolate(ev$logic)),
          actionButton(ns("delete_event"), "Delete Event", class = "btn-warning"),
          hr()
        )
      })
    )
  })
  
  observe({
    lapply(names(events$all), function(e_name) {
      ns <- function(x) paste0(e_name, "-", x)
      ev <- events$all[[e_name]]
      
      observeEvent(input[[ns("add_condition")]], {
        id <- LETTERS[ev$id_counter]
        typ <- input[[ns("type")]]
        endpoint <- input[[ns("endpoint")]]
        arms <- input[[ns("arms")]]
        val <- input[[ns("value")]]
        cond <- list(type = typ, endpoint = endpoint, arms = arms)
        if (typ == "calendar") cond$time <- val else cond$n <- val
        ev$conditions[[id]] <- cond
        ev$id_counter <- ev$id_counter + 1
      })
      
      observeEvent(input[[ns("logic_input")]], {
        ev$logic <- input[[ns("logic_input")]]
      }, ignoreInit = FALSE)
      
      observe({
        lapply(names(ev$conditions), function(id) {
          observeEvent(input[[ns(paste0("del_", id))]], {
            ev$conditions[[id]] <- NULL
          }, ignoreInit = TRUE)
        })
      })
      
      observeEvent(input[[ns("delete_event")]], {
        events$all[[e_name]] <- NULL
      }, ignoreInit = TRUE)
    })
  })
  
  reactiveCode <- reactive({
    arm_code <- lapply(arms$modules, function(mod) {
      dat <- mod()
      eps <- dat$endpoints
      ep_code <- vapply(eps, function(ep) {
        glue("define_endpoint(name = '{ep$name}', type = '{ep$type}', readout = '{ep$readout}', generator = '{ep$generator}', args = '{ep$generator_args}')")
      }, character(1))
      glue("define_arm(name = '{dat$name}', label = '{dat$label}', endpoints = list(
  {paste(ep_code, collapse = ',\n  ')}
))")
    })
    
    event_code <- lapply(events$all, function(ev) {
      cond_lines <- vapply(names(ev$conditions), function(id) {
        cond <- ev$conditions[[id]]
        common <- glue("type = '{cond$type}', endpoint = '{cond$endpoint}', arms = '{cond$arms}'")
        extra <- if (cond$type == "calendar") glue("time = {cond$time}") else glue("n = {cond$n}")
        glue("{id} = define_condition({common}, {extra})")
      }, character(1))
      glue("define_event(name = '{ev$name}', logic = '{ev$logic}', conditions = list(
  {paste(cond_lines, collapse = ',\n  ')}
))")
    })
    
    trial_code <- glue("define_trial(name = '{input$trial_name}', start_date = '{input$trial_start}', max_sample_size = {input$trial_n})")
    
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
