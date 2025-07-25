

generate_packages_codes <- function(){
  glue::glue("library(dplyr)\n", 
             "# library(rpact, quietly = TRUE)\n",
             "library(ggplot2)\n", 
             "library(survival)\n",
             "library(TrialSimulator)"
  )
}

generate_arm_codes <- function(arms){
  
  codes <- 
    sapply(arms, 
           function(arm){
             
             arm_variable <- gsub("\\s+", "_", arm$label)
             arm_codes <- glue::glue("# define arm: {arm$label}\n", 
                                     "{arm_variable} <- arm(name = \"{arm$label}\")\n\n")
             
             ep_codes <- 
               sapply(seq_along(arm$endpoints), 
                      function(idx){
                        endpoint <- arm$endpoints[[idx]]
                        
                        ep_idx <- glue::glue("ep_", idx)
                        ep_codes <- glue::glue("{ep_idx} <- endpoint(\n", 
                                               "  name = {endpoint$name}, \n", 
                                               "  type = {endpoint$type}, ", 
                                               .trim = FALSE)
                        
                        if (endpoint$readout != "") {
                          ep_codes <- glue::glue("{ep_codes}\n", 
                                                 "  readout = {endpoint$readout}, ", 
                                                 .trim = FALSE)
                        }
                        
                        ep_codes <- glue::glue("{ep_codes}\n", 
                                               "  generator = {endpoint$generator}", 
                                               .trim = FALSE)
                        
                        if (endpoint$args != "") {
                          ep_codes <- glue::glue("{ep_codes}, \n", 
                                                 "  {endpoint$args}\n)\n", 
                                                 .trim = FALSE)
                        } else {
                          ep_codes <- glue::glue("{ep_codes}\n)\n", 
                                                 .trim = FALSE)
                        }
                        
                        ep_codes
                      })
             
             arm_codes <- glue::glue("{arm_codes}\n", 
                                     "{paste0(ep_codes, collapse = '\n')}\n", 
                                     "{arm_variable}$add_endpoints({paste0(paste0('ep_', seq_along(arm$endpoints)), collapse = ', ')})\n\n")
             
             arm_codes
           })
  
  codes <- paste0(codes, 
                  collapse = '\n#-----------------------------------------\n\n')
  codes
  
}

generate_trial_info_codes <- function(input, arms, dropout){
  
  `%|%` <- function(a, b) if (a != "") a else b
  
  comment1_trial_n <- ifelse(input$trial_n == "", "  # n_patients is not specified by users. Use a hypothetical value. \n", "")
  comment2_trial_n <- ifelse(input$trial_n == "", "  # This can cause an issue if it is not compatible to other settings.\n", "")
  trial_n <- input$trial_n %|% 2000
  
  comment1_trial_duration <- ifelse(input$trial_duration == "", "  # duration is not specified by users. Use a hypothetical value. \n", "")
  comment2_trial_duration <- ifelse(input$trial_duration == "", "  # This can cause an issue if it is not compatible to other settings.\n", "")
  trial_duration <- input$trial_duration %|% 52
  
  comment_accrual_rate <- ifelse(input$accrual_rate == "", "  # accrual rate is not specified by users. Use a default one.\n", "")
  accrual_rate <- input$accrual_rate %|% "    data.frame(\n      end_time = c(6, 12, Inf), \n      piecewise_rate = c(30, 40, 50)\n    )"
  
  codes <- glue::glue(
    "trial <- trial(\n", 
    "  name = \"trial\",\n", 
    "{comment1_trial_n}", 
    "{comment2_trial_n}", 
    "  n_patients = {trial_n}, \n", 
    "{comment1_trial_duration}", 
    "{comment2_trial_duration}", 
    "  duration = {trial_duration}, \n", 
    "  enroller = StaggeredRecruiter, \n", 
    "{comment_accrual_rate}", 
    "  accrual_rate = \n    {accrual_rate}",
    "{dropout})\n", 
    .trim = FALSE
  )
  
  arms_info <- 
    lapply(arms, 
           function(arm){
             data.frame(arm_variable = gsub("\\s+", "_", arm$label), ratio = arm$ratio)
           }) %>% 
    do.call(rbind, .)
  
  add_arms_block <- glue::glue("trial$add_arms(sample_ratio = c({paste0(arms_info$ratio, collapse = \", \")}), {paste0(arms_info$arm_variable, collapse = \", \")})")
  
  codes <- glue::glue("{codes}\n", 
                      "{add_arms_block}", 
                      .trim = FALSE)
  
  codes

}

generate_trial_event_codes <- function(trial_events){
  
  codes <- sapply(trial_events, function(event) {
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
    
    adapt_codes <- ""
    
    if(event$adaptation$add_arm){
      adapt_codes <- glue::glue("{adapt_codes}\n", 
                                "  # call trial$add_arms(). See add_arms() in TrialSimulator::Trials", 
                                .trim = FALSE)
    }
    
    if(event$adaptation$remove_arm){
      adapt_codes <- glue::glue("{adapt_codes}\n", 
                                "  # call trial$remove_arms(). See remove_arms() in TrialSimulator::Trials", 
                                .trim = FALSE)
    }
    
    if(event$adaptation$update_ratio){
      adapt_codes <- glue::glue("{adapt_codes}\n", 
                                "  # call trial$update_sample_ratio(). See update_sample_ratio() in TrialSimulator::Trials", 
                                .trim = FALSE)
    }
    
    if(event$adaptation$extend_duration){
      adapt_codes <- glue::glue("{adapt_codes}\n", 
                                "  # call trial$set_duration(). See set_duration() in TrialSimulator::Trials", 
                                .trim = FALSE)
    }
    
    if(event$adaptation$adjust_n){
      adapt_codes <- glue::glue("{adapt_codes}\n", 
                                "  # call trial$set_sample_size(). See set_sample_size() in TrialSimulator::Trials", 
                                .trim = FALSE)
    }
    
    glue::glue(
      "{event_variable}_action <- function(trial, milestone_name){{\n\n",
      "  locked_data <- trial$get_locked_data(milestone_name)\n\n",
      "  # custom codes below this line\n", 
      "  {adapt_codes}\n\n", 
      "  NULL\n\n}}\n\n",
      "{event_variable} <- milestone(\n",
      "  name = \"{event$name}\", \n",
      "  when = {logic_expr}, \n",
      "  action = {event_variable}_action\n)", 
      .trim = FALSE
    )
  })
  
  codes <- paste0(codes, collapse = '\n\n#-----------------------------------------\n\n')
  
  codes
  
}

