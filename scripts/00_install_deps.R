options(stringsAsFactors = FALSE)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Use a user library so installs work on shared/remote machines
user_lib <- Sys.getenv("R_LIBS_USER", unset = "")
if (!nzchar(user_lib)) {
  user_lib <- file.path(Sys.getenv("HOME"), "R", "library")
}
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

message("Using R_LIBS_USER: ", user_lib)
message("Current .libPaths():\n  - ", paste(.libPaths(), collapse = "\n  - "))

pkgs_needed <- c(
  # for Figure 2
  "dplyr",
  # for Figure 1 + Figure 2
  "ggplot2", "scales", "patchwork",
  # for Figure 1 (often already installed, but check)
  "survival"
)

missing <- pkgs_needed[!vapply(pkgs_needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, quiet = TRUE)
}

still_missing <- pkgs_needed[!vapply(pkgs_needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) {
  stop("Still missing packages after install: ", paste(still_missing, collapse = ", "))
}

message("All required packages are installed.")
