
server_trial_info <- function(input, output, session, vals) {

  # ---- Render dynamic dropout argument UI ----
  observeEvent(input$dropout, {
    output$dropout_args_ui <- renderUI({
      req(input$dropout)

      if (input$dropout == "") return(NULL)

      if (input$dropout == "Custom") {
        return(textAreaInput("dropout_custom_args", "Custom Arguments (e.g., arg1 = 1, arg2 = 2)", rows = 2))
      }

      dropout_choices <- c(
        "Beta"        = "beta",
        "Binomial"    = "binom",
        "Cauchy"      = "cauchy",
        "Chi-squared" = "chisq",
        "Exponential" = "exp",
        "F"           = "f",
        "Gamma"       = "gamma",
        "Geometric"   = "geom",
        "Log-normal"  = "lnorm",
        "Logistic"    = "logis",
        "Multinomial" = "multinom",
        "Neg. Binom." = "nbinom",
        "Normal"      = "norm",
        "Poisson"     = "pois",
        "t"           = "t",
        "Uniform"     = "unif",
        "Weibull"     = "weibull"
      )

      dist_key <- dropout_choices[[input$dropout]]
      if (is.null(dist_key)) return(NULL)

      rng_fun <- get(paste0("r", dist_key), "package:stats", inherits = TRUE)
      args <- formals(rng_fun)
      args$n <- NULL

      lapply(names(args), function(arg_name) {
        default <- if (!is.null(args[[arg_name]])) deparse(args[[arg_name]]) else ""
        textInput(
          inputId = paste0("dropout_arg_", arg_name),
          label = arg_name,
          value = default
        )
      })
    })
  })

  # ---- Reactive: dropout_args_string and readiness flag ----
  observe({
    vals$dropout_info <- reactive({
      dist_input <- input$dropout %||% ""

      dropout_choices <- c(
        "Beta"        = "beta",
        "Binomial"    = "binom",
        "Cauchy"      = "cauchy",
        "Chi-squared" = "chisq",
        "Exponential" = "exp",
        "F"           = "f",
        "Gamma"       = "gamma",
        "Geometric"   = "geom",
        "Log-normal"  = "lnorm",
        "Logistic"    = "logis",
        "Multinomial" = "multinom",
        "Neg. Binom." = "nbinom",
        "Normal"      = "norm",
        "Poisson"     = "pois",
        "t"           = "t",
        "Uniform"     = "unif",
        "Weibull"     = "weibull"
      )

      # Case 1: No dropout
      if (dist_input == "") {
        return(list(string = "", ready = TRUE))
      }

      # Case 2: Custom
      if (dist_input == "Custom") {
        custom_args <- trimws(input$dropout_custom_args %||% "")
        out_string <- if (nzchar(custom_args)) {
          paste0(", dropout = custom, ", custom_args)
        } else {
          ", custom"
        }
        return(list(string = out_string, ready = TRUE))
      }

      # Case 3: Predefined
      dist_func <- dropout_choices[[dist_input]]
      if (is.null(dist_func)) return(list(string = "", ready = FALSE))

      rng_name <- paste0("r", dist_func)
      rng_fun <- get(rng_name, "package:stats", inherits = TRUE)
      args <- formals(rng_fun)
      args$n <- NULL

      arg_values <- sapply(names(args), function(arg_name) {
        val <- input[[paste0("dropout_arg_", arg_name)]] %||% ""
        trimws(val)
      }, USE.NAMES = TRUE)

      if (any(arg_values == "")) return(list(string = "", ready = FALSE))

      arg_str <- paste(paste0(names(arg_values), " = ", arg_values), collapse = ", ")
      return(list(string = paste0(", dropout = ", rng_name, ", ", arg_str), ready = TRUE))
    })
  })
}
