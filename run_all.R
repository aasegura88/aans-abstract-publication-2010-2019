# Run all manuscript tables/figures (headless).
data_path <- Sys.getenv("AANS_DATA_PATH", unset = file.path("data", "AANScleanedv3 JMR 2.25.csv"))
out_dir   <- Sys.getenv("AANS_OUT_DIR",   unset = "outputs")

if (!file.exists(data_path)) {
  stop("Data file not found. Set AANS_DATA_PATH or place CSV at: ", data_path)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Ensure scripts see the same paths
Sys.setenv(AANS_DATA_PATH = data_path, AANS_OUT_DIR = out_dir)

message("Using data: ", data_path)
message("Writing outputs to: ", out_dir)

# Ensure required packages exist (installs into user library if needed)
source("scripts/00_install_deps.R")

scripts <- c(
  "scripts/01_table1.R",
  "scripts/02_table2.R",
  "scripts/03_table3.R",
  "scripts/04_figure1.R",
  "scripts/05_figure2.R"
)

missing <- scripts[!file.exists(scripts)]
if (length(missing)) stop("Missing scripts:\n- ", paste(missing, collapse = "\n- "))

for (s in scripts) {
  message("\nRunning: ", s)
  env <- new.env(parent = baseenv())
  source(s, local = env)
  rm(env)
  invisible(gc())
}

message("\nDone.")
