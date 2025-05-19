server_config <- function(input, output, session, vals) {
  
  observeEvent(input$load_config, {
    req(input$load_config)
    
    tryCatch({
      path <- input$load_config$datapath
      stopifnot(is.character(path), file.exists(path))
      
      loaded <- jsonlite::read_json(path, simplifyVector = FALSE)
      if (!is.list(loaded)) stop("Invalid configuration structure")
      
      if (!is.null(loaded$trial_events)) {
        vals$trial_events <- loaded$trial_events
      } else {
        vals$trial_events <- list()
      }
      
      if (!is.null(loaded$arms)) {
        vals$arms <- loaded$arms
      } else {
        vals$arms <- list()
      }
      
      showNotification("✅ Config loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Failed to load config:", e$message), type = "error")
    })
  })
  
  output$save_config <- downloadHandler(
    filename = function() {
      paste0("trial_config_", Sys.Date(), ".json")
    },
    content = function(file) {
      jsonlite::write_json(
        list(
          trial_events = vals$trial_events,
          arms = vals$arms
        ),
        path = file,
        pretty = TRUE,
        auto_unbox = TRUE
      )
    }
  )
}
