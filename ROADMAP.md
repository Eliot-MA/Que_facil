# Roadmap — Que_facil

Guion de trabajo para ir arreglando el proyecto paso a paso. Ordenado por dependencias:
primero que funcione y sea reproducible, luego completar análisis, y al final la síntesis.

Estados: `[ ]` pendiente · `[~]` en progreso · `[x]` hecho.

---

## FASE 0 — Congelar y ordenar el repo
- [x] **0.1** Commitear lo pendiente: la línea `## ACTUALIZAR PARA INCLUIR FECHA DE MEDICIÓN` (`01.2a-survival_growth.R:85`) y añadir el guion `02-reports/guion_estancia_investigacion.qmd` (hoy *untracked*).
- [x] **0.2** Sincronizar `main` con `origin/main` (hay 1 commit local sin pushear).
- [x] **0.3** Crear rama de trabajo `fix/pipeline` para ir acumulando los arreglos.

## FASE 1 — Pipeline ejecutable (bloqueante)
Objetivo: que el proyecto corra de principio a fin sin errores de `source()`.
- [x] **1.1** Inventariar todos los `source()` del repo y contrastarlos con los nombres reales de `01-scripts/`.
- [x] **1.2** `01-load_data.R` ahora sourcea `01.1-abiotic.R` y `01.2-biotic.R`.
- [x] **1.3** Referencias corregidas (`01.2-biotic.R:7`, `01.1a-microclimate.R:4`, `02.1`, `02.2`, `02.3`, `03.1`, `03.2`, `03.3`, `03.4`, `.qmd`).
- [x] **1.4** Decisión: **reconectar** `03.3-RII.R`→`01.2e-RII.R` y `03.4-surv_models.R`→`01.2b-biomass.R` + `df.biomass` (no archivar).
- [x] **1.5** Verificación (estática + ejecución modular):
  - Los 28 scripts parsean sin error; todas las llamadas `source()` vivas resuelven.
  - Cada loader dedicado crea su BD: `df.surv`(1441×6), `df.sla`(60×6), `df.vol`(481×8), `df.biomass`(481×10)+`rD.biomass`(30×7), `df.wood_density`(59×9), `df.shrubs.gps`(240×7), `df.par.ext`(333×9), microclima OK.
  - `01.2e-RII.R` ejecuta completo (validado a 50+20 iteraciones; el bootstrap real es 5000+500).
  - `02.1` y `02.3` corren OK; `03.1` llega al bug conocido de Fase 2 (`gam1`).
  - **Bugs descubiertos y arreglados aquí:** (a) la BD de compatibilidad `df.surv.par.vol` explotaba many-to-many (5761 filas) y cruzaba `quercus_sp` al unir por `Individual`; corregido uniendo por `(Individual, quercus_sp)` en `02.2`, `03.1`, `03.2`, `Presentation01.qmd` → 1441 filas, 0 NAs en datos modelados. (b) el plot de convergencia de `01.2e-RII.R:289` perdía las columnas de facet con `summarise()`; corregido reteniéndolas.

## FASE 2 — Bugs y robustez
> Nota: los números de línea del plan original quedaron obsoletos tras el refactor de Fase 1;
> abajo se indican las ubicaciones reales.
- [x] **2.1** `03.1-elevation.R:207-208` usaba `summary(gam1)`/`plot(gam1)` sobre un objeto inexistente → corregido a `gam` (el modelo con término suave `s(Elevation_c)`, cuyo edf/p comenta el script).
- [x] **2.2** `03.2-shrub_size.R`: `modelo` era un re-ajuste manual **idéntico** a `m22` (mismos términos y AIC = 430.24) → reemplazado por `modelo <- m22` (elimina duplicación y riesgo de divergencia).
- [x] **2.3** `03.4-surv_models.R:110-117` bloque duplicado (`modelo <- m2` … `m3 <- update(...)`) eliminado.
- [x] **2.4** `p_boot <- 2*min(...)` está en **`03.3-RII.R:172`** (y su copia en `Results04_RII_2.qmd`), no en `01.2e`: operaba sobre un valor lógico. Ahora es un p-valor bootstrap bilateral `min(1, 2*min(P(v≤0), P(v≥0)))` sobre los replicados. Verificado: rango [0,1].
- [x] **2.5** `01.2e-RII.R`: la convergencia sobre el subconjunto fijo se agrupa ahora solo por lo que varía (`iteration`; y `ind_A` en la versión por individuo) y se eliminaron los `facet_grid` sobre columnas constantes.
- [x] **2.6** `01.2e-RII.R`: sustituido el `write.csv2` comentado por un **exportador** que corre `bootstrap_once_v3` sobre `df.surv` completo y agrega por `(surv_date,type_RII,Interacting_species,quercus_sp,iteration)` → escribe `00-data/boot_RII.csv`. Verificado (5 iter): 42 grupos/iter y mismo esquema que el CSV legado (210000 filas = 5000×42).
- [x] **2.7** Dependencias declaradas en `README.md` (sección *Dependencies*, con `fuzzyjoin` destacado). El `fileEncoding="latin1"` de `01.1a.3` se verificó correcto (CSV con bytes no-UTF8) y se mantiene, documentado en el README.

## FASE 3 — Reproducibilidad y entorno
- [ ] **3.1** Crear `renv` + `renv.lock` (hoy no hay versiones fijadas).
- [ ] **3.2** Corregir `.gitignore:55` (`.pdf` → `*.pdf`) para no trackear salidas.
- [ ] **3.3** Actualizar `README.md` al árbol real de scripts.
- [ ] **3.4** Documentar dónde viven los datos no versionados (`00-data/`).
- [x] **3.5** (adelantado) Instalados los paquetes que faltaban en el entorno: `here`, `tidylog`, `kableExtra`, `gtsummary` y `waffle` (este último desde el archivo CRAN + `extrafont`, al estar retirado de CRAN).

## FASE 4 — Completar análisis pendientes
- [ ] **4.1** Terminar SLA (`01.2c-SLA.R`, hoy "Working on it…") + reporte.
- [ ] **4.2** Integrar el 3er censo (mayo 2026) en los resultados temporales.
- [ ] **4.3** Convergencia y nº óptimo de iteraciones del bootstrap → materializar `Results05_RII_3.qmd`.
- [ ] **4.4** Modelos multinomiales mixtos para RII ∈ {−1,0,1} (`Results04:71`, "deferred").
- [ ] **4.5** Validar modelos de biomasa Gamma vs otras familias (`Results02:126`).
- [ ] **4.6** Integrar la **pendiente** (extraída del DTM, nunca usada) en los modelos.
- [ ] **4.7** Integrar las variables **microclimáticas** (dataloggers procesados pero no usados).

## FASE 5 — Cierre y publicación
- [ ] **5.1** Rellenar los orquestadores vacíos `02-eda.R` y `03-models.R`.
- [ ] **5.2** Síntesis en formato artículo a partir del guion.
- [ ] **5.3** PR de `fix/pipeline` → `main`.
