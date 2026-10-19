# Supplementary analyses

Analyses that are **not** part of the data-loading pipeline (`01-load_data.R`).
They are kept here as self-contained, run-on-demand scripts because they are
exploratory, heavy, or only support the main conclusions as a complement.

## Why this folder exists

The Relative Interaction Index (RII) was originally computed on survival data
(binary, 0/1). Adapting RII to binomial data is hard, and for survival it does
not lead to conclusions that differ substantially from the original GLMM fitted
directly on survival. Therefore RII-on-survival is **retired from the main
pipeline** and kept here as a small supplement that *demonstrates the
concordance* with the GLMM approach.

RII may still be appropriate on **growth** data (continuous), where the index's
assumptions hold. That would live here too (see `RII_growth.R`, stub).

## Contents

| File | Purpose |
|---|---|
| `RII_survival.R` | Classic RII on survival + comparison against a GLMM on survival. |
| `RII_survival.qmd` | Consolidated supplementary report (replaces the old `Results03/04/05_RII`). |
| `RII_growth.R` | (Stub) RII on growth data, to be developed later. |
| `_archive/` | Superseded scripts and reports preserved for reference (`01.2e-RII.R`, `03.3-RII.R`, `Results03/04/05_RII`). |

## How to run

```r
# from the project root
source("05-supplementary/RII_survival.R")
```

Each script sources only the light loader(s) it needs (e.g.
`01-scripts/01.2a-survival_growth.R`) so it can run independently of the full
pipeline. Outputs are written to `08-img/` (and any tables next to the data).
