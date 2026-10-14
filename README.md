# Que_facil
Analyses conducted within the framework of the QueVadis Project on the facilitative capacity of Mediterranean shrub species, how this effect changes depending on target species with contrasting ecologies, and whether this effect can in turn be modulated by the presence of a tree canopy.

## Project structure

The project is organized hierarchically, with a main script responsible for loading and orchestrating the execution of thematic modules. Each module groups analyses related to a specific component of the study and calls a set of independent scripts implementing individual analyses.

```text
01-load-data.R
├── 01.1-abiotic.R
│   ├── 01.1.a-microclimate.R
│   ├── 01.1.b-elevation.R
│   └── 01.1.c-PAR_extintion.R
└── 01.2-biotic.R
    ├── 01.2.a-surv-growth.R
    ├── 01.2.b-biomass.R
    ├── 01.2.c-SLA.R
    ├── 01.2.d-size.R
    └── 01.2.e-RII.R

```

### Workflow

- **`01-load-data.R`** loads the required datasets and initializes the analysis workflow.
- **`01.1-biotic.R`** runs analyses based on biotic variables, including biomass, specific leaf area (SLA), plant size, survival and growth, and Relative Interaction Index (RII).
- **`01.2-abiotic.R`** runs analyses based on abiotic variables, including microclimate, elevation, photosynthetically active radiation (PAR), and extinction-related analyses.

This modular organization keeps each analysis self-contained while allowing the complete workflow to be reproduced from a single entry-point script.

## Dependencies

The scripts rely on a number of R packages. Most are available on CRAN, but the
less obvious ones are called out here so a fresh environment can be set up:

- Data handling: `dplyr`, `tidyr`, `purrr`, `readr`, `tibble`, `janitor`, `stringr`, `lubridate`, `tidylog`
- Reading Excel inputs: `readxl`
- **Fuzzy matching of datalogger time windows: `fuzzyjoin`** (used in `01.1a.3-add_microsite_environment.R`)
- Modelling: `glmmTMB`, `mgcv`, `car`, `performance`, `DHARMa`, `broom.mixed`
- Plotting & reporting: `ggplot2`, `patchwork`, `ggeffects`, `kableExtra`, `gtsummary`

Some raw CSV inputs are stored in **latin1** (e.g.
`00-data/datos_humedad_temperatura_tamajon.csv`, whose bytes are not valid
UTF-8). These are read with `fileEncoding = "latin1"`, which is intentional and
should be preserved for correct accented characters.


