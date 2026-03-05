# AANS abstract publication analysis (2010 vs 2019)

Reproducible analysis code and notebook to generate tables and figures for a study of AANS podium abstract publication outcomes comparing the 2010 and 2019 annual meetings (podium-to-print publication).

This repository is intended to support a manuscript submission.

## Contents

- `notebooks/`: analysis notebook(s)
- `scripts/`: standalone R scripts for manuscript tables/figures (optional but recommended)
- `data/`: expected location for the analysis-ready CSV (not tracked)
- `outputs/`: generated tables/figures (not tracked)

## Data

The analysis expects an analysis-ready CSV file (not included in the repository).

Set the input path via environment variable:

```bash
export AANS_DATA_PATH="/path/to/AANScleanedv3 JMR 2.25.csv"

exit
