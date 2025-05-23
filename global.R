if (!requireNamespace("TrialSimulator", quietly = TRUE)) {
  remotes::install_github(
    "zhangh12/TrialSimulator", 
    build_manual = TRUE, 
    build_vignettes = TRUE, 
    force = TRUE
  )
}
library(TrialSimulator)
