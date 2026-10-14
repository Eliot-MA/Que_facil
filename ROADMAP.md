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
- [ ] **1.1** Inventariar todos los `source()` del repo y contrastarlos con los nombres reales de `01-scripts/`.
- [ ] **1.2** Reescribir `01-load_data.R:10-12` para que invoque los módulos actuales (`01.1-abiotic.R`, `01.2-biotic.R`).
- [ ] **1.3** Corregir referencias rotas:
  - `01.2-biotic.R:7` → `01.2b-biomass.R` (typo `01.2b.biomass.R`)
  - `01.1a-microclimate.R:4` → `01.1a.4-compute_derived_variables.R`
  - `02.1-elevation.R:1`, `02.2-shrub_size.R:10-12`, `02.3-extinction.R:3`
  - `03.1`, `03.2`, `03.3:4-5`, `03.4:48`
  - `.qmd`: `Results03_RII.qmd:188`, `Presentation01.qmd:112,181-183`
- [ ] **1.4** Decidir qué hacer con `03.3-RII.R` y `03.4-surv_models.R` (parecen duplicar lógica vieja de `01.2e`/`03.5`): recuperar, fusionar o eliminar.
- [ ] **1.5** Verificación: ejecutar el pipeline completo desde `01-load_data.R` sin errores.

## FASE 2 — Bugs y robustez
- [ ] **2.1** `03.1-elevation.R:198` usa `gam1` que nunca se crea → usar el objeto real (`glm`/`gam`).
- [ ] **2.2** `03.2-shrub_size.R:520,526,527` usa `modelo` donde el ajuste se llama `m22`.
- [ ] **2.3** `03.4-surv_models.R:110-117` bloque duplicado de `modelo <- m2…m3`.
- [ ] **2.4** `01.2e-RII.R:173` `p_boot <- 2*min(...)`: operación sobre lógicos, probable bug → revalidar.
- [ ] **2.5** `01.2e-RII.R:212-222` bootstrap de convergencia sobre subconjunto fijo pero agrupado como si hubiera varios → corregir filtro/agrupación.
- [ ] **2.6** `01.2e-RII.R:237` `write.csv2(...)` comentado: los `boot_RII*.csv` se leen como entrada → asegurar reproducibilidad en cadena.
- [ ] **2.7** Declarar dependencias no obvias (`fuzzyjoin`) y arreglar el `fileEncoding="latin1"` de `01.1a.3`.

## FASE 3 — Reproducibilidad y entorno
- [ ] **3.1** Crear `renv` + `renv.lock` (hoy no hay versiones fijadas).
- [ ] **3.2** Corregir `.gitignore:55` (`.pdf` → `*.pdf`) para no trackear salidas.
- [ ] **3.3** Actualizar `README.md` al árbol real de scripts.
- [ ] **3.4** Documentar dónde viven los datos no versionados (`00-data/`).

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
