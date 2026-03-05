# Driver to regenerate manuscript tables/figures.
# Expects data path via AANS_DATA_PATH or ./data/
data_path <- Sys.getenv("AANS_DATA_PATH", unset = "data/AANScleanedv3 JMR 2.25.csv")
out_dir   <- Sys.getenv("AANS_OUT_DIR", unset = "outputs")

if (!file.exists(data_path)) {
  stop("Data file not found. Set AANS_DATA_PATH or place CSV at: ", data_path)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

message("Using data: ", data_path)
message("Writing outputs to: ", out_dir)

scripts <- c(
  "scripts/01_table1.R",
  "scripts/02_table2.R",
  "scripts/03_table3.R",
  "scripts/04_figure1.R",
  "scripts/05_figure2.R"
)

missing <- scripts[!file.exists(scripts)]
if (length(missing)) {
  message("Missing scripts:\n- ", paste(missing, collapse = "\n- "))
  message("If you are using only the notebook, you can ignore run_all.R.")
  quit(status = 0)
}

for (s in scripts) {
  message("\nRunning: ", s)
  source(s, local = TRUE)
}

message("\nDone.")
