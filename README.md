# AANS abstract publication analysis (2010 vs 2019)

<p align="center">
  <img src="/AANS_github0.png" alt="Time to podium-to-print publication (2010 vs 2019)" width="100%">
</p>

Time-to-publication distribution; complements the binary 36-month endpoint below.

## Key result
<p align="center">
  <img src="/AANS_github1.png" alt="Medical-student first authorship increased while podium-to-print publication declined (2010 vs 2019)." width="100%">
</p>

- Medical-student first authorship: 6.6% (9/136) in 2010 vs 47.5% (103/217) in 2019.
- Publication within 36 months: 57.0% (77/135) in 2010 vs 41.9% (91/217) in 2019.
- Error bars show exact 95% binomial confidence intervals.

<details>
<summary>Statistics</summary>

Student-share Fisher p < 0.001; 36-month publication Fisher p = 0.006; year × student interaction not significant (LRT p ≈ 0.93).  
Within-36-month publication uses time-to-publication when available; published abstracts with missing time-to-publication were excluded from this endpoint.
</details>

Counts are shown as numerator/denominator; error bars are exact 95% binomial confidence intervals.
For the current analysis dataset: student-share Fisher p < 0.001; publication-within-36-months Fisher p = 0.006; no evidence the decline differs by student status (year × student interaction LRT p ≈ 0.93).

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

