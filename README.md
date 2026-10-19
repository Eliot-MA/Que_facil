# Que_facil
Analyses conducted within the framework of the QueVadis Project on the facilitative capacity of Mediterranean shrub species, how this effect changes depending on target species with contrasting ecologies, and whether this effect can in turn be modulated by the presence of a tree canopy.

## Project structure

The project is organized hierarchically: a main script orchestrates a set of
thematic modules, and each module sources the individual analysis scripts.

```text
01-load_data.R
├── 01.1-abiotic.R
│   ├── 01.1a-microclimate.R
│   │   ├── 01.1a.1-merge_dataloggers.R
│   │   ├── 01.1a.2-correct_errors.R
│   │   ├── 01.1a.3-add_microsite_environment.R
│   │   └── 01.1a.4-compute_derived_variables.R
│   ├── 01.1b-elevation.R
│   └── 01.1c-PAR_extintion.R
└── 01.2-biotic.R
    ├── 01.2a-survival_growth.R
    ├── 01.2b-biomass.R
    ├── 01.2c-SLA.R
    └── 01.2d-shrub_size.R

02-eda.R        # exploratory analyses (orchestrator, WIP)
├── 02.1-elevation.R
├── 02.2-shrub_size.R
└── 02.3-extinction.R

03-models.R     # statistical models (orchestrator, WIP)
├── 03.1-elevation.R
├── 03.2-shrub_size.R
├── 03.4-surv_models.R
├── 03.5-SLA.R
└── 03.6-wood_density.R
```

> `01.2f-wood_density.R` and `_functions.R` (shared plotting helpers) also live
> in `01-scripts/`.
>
> Supplementary analyses that are **not** part of the pipeline live in
> `05-supplementary/` (see its own README).

### Workflow

- **`01-load_data.R`** loads the datasets and initializes the pipeline.
- **`01.1-abiotic.R`** loads abiotic variables: microclimate, elevation, and PAR/extinction.
- **`01.2-biotic.R`** loads biotic variables: survival/growth, biomass, SLA, and size.
- **`02-eda.R`** runs the exploratory analyses.
- **`03-models.R`** fits the statistical models.

This modular organization keeps each analysis self-contained while allowing the complete workflow to be reproduced from a single entry-point script.

## Data

Raw inputs live in `00-data/raw/` and pipeline outputs in
`00-data/processed/`; the whole `00-data/` tree is **not versioned**
(large files and a separate data license in `LICENSE-data`). See
`00-data/README.md` for the list of expected input files, which scripts read or
write each one, and the note on latin1-encoded CSVs.


## Dependencies

The scripts rely on a number of R packages. Most are available on CRAN, but the
less obvious ones are called out here so a fresh environment can be set up:

- Data handling: `dplyr`, `tidyr`, `purrr`, `readr`, `tibble`, `janitor`, `stringr`, `lubridate`, `tidylog`
- Reading Excel inputs: `readxl`
- **Fuzzy matching of datalogger time windows: `fuzzyjoin`** (used in `01.1a.3-add_microsite_environment.R`)
- Modelling: `glmmTMB`, `mgcv`, `car`, `performance`, `DHARMa`, `broom.mixed`
- Plotting & reporting: `ggplot2`, `patchwork`, `ggeffects`, `kableExtra`, `gtsummary`

Some raw CSV inputs are stored in **latin1** (e.g.
`00-data/raw/datos_humedad_temperatura_tamajon.csv`, whose bytes are not valid
UTF-8). These are read with `fileEncoding = "latin1"`, which is intentional and
should be preserved for correct accented characters.


