library(shiny)
library(glue)

# ---- Arm Module UI ----
armModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("dose"), "Dose"),
    hr(),
    h4("Endpoints"),
    uiOutput(ns("endpoint_selector")),
    textInput(ns("endpoint_name"), "Endpoint Name"),
    textInput(ns("endpoint_type"), "Endpoint Type"),
    textInput(ns("endpoint_unit"), "Endpoint Unit"),
    actionButton(ns("add_endpoint"), "➕ Add / Update Endpoint"),
    actionButton(ns("delete_endpoint"), "🗑️ Delete Endpoint")
  )
}

# ---- Arm Module Server ----
armModuleServer <- function(id, label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- reactiveValues(
      endpoints = list(),
      selected_ep = NULL
    )
    
    observeEvent(input$add_endpoint, {
      name <- input$endpoint_name
      if (nzchar(name)) {
        state$endpoints[[name]] <- list(
          name = name,
          type = input$endpoint_type,
          unit = input$endpoint_unit
        )
        state$selected_ep <- name
      }
    })
    
    observeEvent(input$delete_endpoint, {
      ep <- state$selected_ep
      if (!is.null(ep)) {
        state$endpoints[[ep]] <- NULL
        state$selected_ep <- NULL
        updateTextInput(session, "endpoint_name", value = "")
        updateTextInput(session, "endpoint_type", value = "")
        updateTextInput(session, "endpoint_unit", value = "")
      }
    })
    
    observeEvent(input$endpoint_selector, {
      ep <- input$endpoint_selector
      if (nzchar(ep) && ep %in% names(state$endpoints)) {
        endpoint <- state$endpoints[[ep]]
        updateTextInput(session, "endpoint_name", value = endpoint$name)
        updateTextInput(session, "endpoint_type", value = endpoint$type)
        updateTextInput(session, "endpoint_unit", value = endpoint$unit)
        state$selected_ep <- ep
      }
    })
    
    output$endpoint_selector <- renderUI({
      if (length(state$endpoints) == 0) return(NULL)
      selectInput(ns("endpoint_selector"), "Select Endpoint:", choices = names(state$endpoints), selected = state$selected_ep)
    })
    
    return(reactive({
      list(
        name = label,
        dose = input$dose,
        endpoints = isolate(state$endpoints)
      )
    }))
  })
}

# ---- Main App ----
ui <- fluidPage(
  titlePanel("Modular Clinical Trial Designer"),
  sidebarLayout(
    sidebarPanel(
      textInput("new_arm", "New Arm Name"),
      actionButton("add_arm", "➕ Add Arm"),
      uiOutput("arm_removal_ui")
    ),
    mainPanel(
      tabsetPanel(id = "arm_tabs",
                  tabPanel("Code", value = "code_tab", h3("Generated Code"), verbatimTextOutput("combined_code"))
      )
    )
  )
)

server <- function(input, output, session) {
  arm_list <- reactiveValues(labels = character(), modules = list())
  
  # ---- Add Arm ----
  observeEvent(input$add_arm, {
    label <- input$new_arm
    id <- paste0("arm_", label)
    if (nzchar(label) && !(label %in% arm_list$labels)) {
      insertTab("arm_tabs",
                tabPanel(title = label, value = id, armModuleUI(id)),
                target = "code_tab", position = "before", select = TRUE
      )
      arm_list$labels <- c(arm_list$labels, label)
      arm_list$modules[[label]] <- armModuleServer(id = id, label = label)
    }
  })
  
  # ---- Remove Arm UI ----
  output$arm_removal_ui <- renderUI({
    if (length(arm_list$labels) == 0) return(NULL)
    tagList(
      selectInput("remove_arm", "Select Arm to Remove", choices = arm_list$labels),
      actionButton("delete_arm", "🗑️ Remove Selected Arm")
    )
  })
  
  # ---- Delete Arm ----
  observeEvent(input$delete_arm, {
    label <- input$remove_arm
    id <- paste0("arm_", label)
    if (!is.null(label) && label %in% arm_list$labels) {
      removeTab("arm_tabs", target = id)
      arm_list$labels <- setdiff(arm_list$labels, label)
      arm_list$modules[[label]] <- NULL
    }
  })
  
  # ---- Combined Code ----
  output$combined_code <- renderText({
    codes <- lapply(arm_list$modules, function(mod) {
      dat <- mod()
      eps <- dat$endpoints
      ep_code <- if (length(eps) == 0) "" else {
        vapply(eps, function(ep) {
          glue("define_endpoint(name = '{ep$name}', type = '{ep$type}', unit = '{ep$unit}')")
        }, character(1))
      }
      glue("define_arm(name = '{dat$name}', dose = '{dat$dose}', endpoints = list(\n  {paste(ep_code, collapse = ',\n  ')}\n))")
    })
    paste(codes, collapse = "\n\n")
  })
}

shinyApp(ui, server)
