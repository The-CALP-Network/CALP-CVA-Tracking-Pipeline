# utils.R — shared helpers loaded by all pipeline scripts
#
# If run interactively from inside RStudio, source this file first to set
# the working directory to the project root automatically.

enforce_project_root <- function() {
  # When running via Rscript the working directory is wherever the user ran
  # the command from.  We detect if we are NOT at the root by checking for a
  # known sentinel file.
  if (!file.exists("code/utils.R")) {
    # Try to find the root by walking up from the current script location
    args <- commandArgs(trailingOnly = FALSE)
    file_flag <- grep("^--file=", args, value = TRUE)
    if (length(file_flag)) {
      script_path <- normalizePath(sub("^--file=", "", file_flag))
      root <- dirname(dirname(script_path))   # code/ -> project root
      setwd(root)
    } else {
      # Interactive / RStudio: use rstudioapi if available
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable()) {
        root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
        setwd(root)
      }
    }
  }
  invisible(getwd())
}

# Ensure required packages are installed and loaded silently
load_packages <- function(...) {
  pkgs <- c(...)
  needed <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(needed)) install.packages(needed, quiet = TRUE)
  invisible(lapply(pkgs, library, character.only = TRUE, quietly = TRUE,
                   warn.conflicts = FALSE))
}