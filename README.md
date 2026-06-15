# Que_facil
Analyses conducted within the framework of the QueVadis Project on the facilitative capacity of Mediterranean shrub species, how this effect changes depending on target species with contrasting ecologies, and whether this effect can in turn be modulated by the presence of a tree canopy.

# Workflow

```mermaid
flowchart LR

  subgraph S1["01-load_data.R"]
    direction TB
    A[01.1-gps.R]
    B[01.2-surv_par_vol.R]
    C[01.3-Microclimate.R]
    D[01.3.1-merge_dataloggers.R]
    E[01.3.2-correct_errors.R]
    F[01.3.3-add_microsite_environment.R]
    G[01.3.4-compute_derived_variables.R]
    H[01.4-RII.R]
    I[01.5-predict_biomass.R]
  end

  subgraph S2["02-eda.R"]
    direction TB
    J[02.1-elevation.R]
    K[02.2-shrub_size.R]
    L[02.3-extinction.R]
  end

  subgraph S3["03-models.R"]
    direction TB
    M[03.1-elevation.R]
    N[03.2-shrub_size.R]
  end

  D --> C
  E --> C
  F --> C
  G --> C
  H --> B
  I --> B
  J --> A
  K --> B
  L --> B
  M --> A
  N --> B
```
