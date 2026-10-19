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
- [x] **2.6** `01.2e-RII.R`: sustituido el `write.csv2` comentado por un **exportador** que corre `bootstrap_once_v3` sobre `df.surv` completo y agrega por `(surv_date,type_RII,Interacting_species,quercus_sp,iteration)` → escribe `00-data/processed/_archive/boot_RII.csv`. Verificado (5 iter): 42 grupos/iter y mismo esquema que el CSV legado (210000 filas = 5000×42).
- [x] **2.7** Dependencias declaradas en `README.md` (sección *Dependencies*, con `fuzzyjoin` destacado). El `fileEncoding="latin1"` de `01.1a.3` se verificó correcto (CSV con bytes no-UTF8) y se mantiene, documentado en el README.

## FASE 3 — Reproducibilidad y entorno
- [x] **3.1** Creado `renv` + `renv.lock` (`renv::init()` hidrata la librería del proyecto desde la del usuario; `renv.lock` fija 252 paquetes y R 4.5.0). Nota: `waffle` (retirado de CRAN) queda fijado por versión (1.0.2); `renv::restore()` en una máquina nueva requerirá el archivo CRAN.
- [x] **3.2** Corregido `.gitignore` (`.pdf` → `*.pdf`) para no trackear salidas.
- [x] **3.3** Actualizado `README.md` al árbol real de scripts.
- [x] **3.4** Documentados los datos no versionados en `00-data/README.md` (más sección *Data* en la raíz).
- [x] **3.5** (adelantado) Instalados los paquetes que faltaban en el entorno: `here`, `tidylog`, `kableExtra`, `gtsummary` y `waffle` (este último desde el archivo CRAN + `extrafont`, al estar retirado de CRAN).

## FASE 4 — Completar análisis pendientes
> Nota: esta fase agrupa análisis heterogéneos que quedaron a medias. **Cada punto debe revisarse y
> validarse individualmente**, uno por uno, antes de darlo por hecho; no se abordan todos de golpe.

> **Decisión sobre el RII (supervivencia).** El RII sobre datos binarios de
> supervivencia se retira del pipeline y pasa a un **análisis suplementario**
> (`05-supplementary/`: `RII_survival.R` + `RII_survival.qmd`). En datos binarios
> RII ≡ diferencia de probabilidades de supervivencia, justo lo que estima el
> GLMM, por lo que el suplemento se limita a demostrar la concordancia entre
> ambos métodos. Los scripts/reportes antiguos (`01.2e-RII.R`, `03.3-RII.R`,
> `Results03/04/05_RII`) quedan archivados en `05-supplementary/_archive/`. El
> RII sobre **crecimiento** (continuo) queda como posible análisis futuro
> (`05-supplementary/RII_growth.R`).
- [ ] **4.1** Terminar SLA (`01.2c-SLA.R`, hoy "Working on it…") + reporte.
- [ ] **4.2** Integrar el 3er censo (mayo 2026) en los resultados temporales.
- [x] **4.3** Convergencia y nº óptimo de iteraciones del bootstrap → **superado por la decisión RII**: el RII de supervivencia sale del pipeline; el material de convergencia queda archivado en `05-supplementary/_archive/`.
- [x] **4.4** Modelos multinomiales mixtos para RII ∈ {−1,0,1} → **descartado**: pierde sentido al retirar el RII de supervivencia.
- [ ] **4.5** Validar modelos de biomasa Gamma vs otras familias (`Results02:126`).
- [ ] **4.6** Integrar la **pendiente** (extraída del DTM, nunca usada) en los modelos.
- [ ] **4.7** Integrar las variables **microclimáticas** (dataloggers procesados pero no usados).

## FASE 5 — Cierre y publicación
- [ ] **5.1** Rellenar los orquestadores vacíos `02-eda.R` y `03-models.R`.
- [ ] **5.2** Síntesis en formato artículo a partir del guion.
- [ ] **5.3** PR de `fix/pipeline` → `main`.

## FASE 6 — Rediseño analítico (plan revisado)
> Plan revisado tras la evaluación del repositorio. **Sustituye y refina los pendientes de la FASE 4**
> (4.2 integración temporal, 4.5 biomasa, 4.6 pendiente, 4.7 microclima) y añade suelo, PCA y crecimiento.
>
> **Decisiones estructurales:**
> - **Tres unidades de análisis** (artículo/dataset por unidad, no un único dataset con todo).
> - **Dos modelos** de supervivencia y crecimiento: (a) claros vs. arbustos, (b) dentro de arbustos
>   (la biomasa/SLA/suelo no existen en los claros; resuelve el problema de `filter(microsite != "gap")`).
> - **PCA/MFA a nivel de arbusto etiquetado** (n = 60: 30 pinar + 30 abierto), no a nivel de plantón.
> - **Selección de modelos por candidatos a priori + AICc**; las cadenas `drop1`/`stepAIC`/`dredge`
>   actuales pasan a **análisis de sensibilidad**, no a método principal.
> - Variables de suelo/microclima/PAR son **de una sola campaña** (foto fija), no un régimen temporal.
>
> **Datos ya disponibles pero no migrados:** humedad del suelo, textura y nutrientes (no es una
> debilidad, es una tarea de migración pendiente: 6.0.1).

### 6.0 — Migración de suelo y arreglos previos
- [ ] **6.0.1** Crear `01-scripts/01.1d-edaphic.R` (loader de suelo: humedad, textura, nutrientes)
  siguiendo la convención `raw/`→`processed/`; engancharlo a `01.1-abiotic.R`; actualizar `00-data/README.md`.
- [ ] **6.0.2** Arreglar la fecha de `t1` de crecimiento (`01.2a-survival_growth.R:85`, `TODO`).
- [ ] **6.0.3** Corregir la fila con `Fecha segunda medicion = jul-26` y las filas sin especie/ambiente en `growth.csv`;
  revisar las 8 filas vacías de `vol_par_surv.csv`.
- [ ] **6.0.4** Homogeneizar recodificaciones de factores entre scripts (`zone`/`Environment`,
  `gap`/`open`/`open area`, `abierto`/`pinar`).
- [ ] **6.0.5** Verificar la fecha real de plantación y corregir "286 días" en
  `02-reports/guion_estancia_investigacion.qmd:363` (12/06/2025 → 27/05/2026 = 349 días).

### 6.A — Datasets maestros
- [ ] **6.A.1** `df.surv.analysis` — 1 fila = plantón × fecha (480 × 3 censos): factores, `survival` (2→1),
  `V_t1`, PAR, elevación/pendiente.
- [ ] **6.A.2** `df.growth.analysis` — 1 fila = plantón vivo a t2 (~225): `V_t1`, `V_t2`, factores, controles.
- [ ] **6.A.3** `df.mechanistic` — 1 fila = arbusto etiquetado con datos completos (n=60): biomasa, SLA,
  microclima (Tª/RH/VPD), PAR, humedad del suelo, textura, nutrientes.
- [ ] **6.A.4** Un único script de construcción que los guarde en `processed/` (evitar los `left_join`
  frágiles repetidos en cada `03.x`).

### 6.B — Supervivencia en tres niveles (candidatos a priori + AICc)
- [ ] **6.B.1** **Claros vs. arbustos** (n=480): `survival ~ surv_date * (Environment * microsite * quercus_sp) + V_t1 + Elevation + (1|id_quercus) + (1|Individual)`; el contraste `date × predictores`
  responde a "¿cambian los patrones con el tiempo?".
- [ ] **6.B.2** **Dentro de arbustos** (n=360): añadir `log(biomasa)` e interacciones `microsite × biomasa`,
  `Environment × biomasa`.
- [ ] **6.B.3** **Mecanístico** (subconjunto de 60 arbustos): scores del PCA/MFA o variables edáficas
  sueltas como predictores (exploratorio, declarar n).
- [ ] **6.B.4** Chequear separación en celdas de baja supervivencia (gap × *Genista* en claro ≈ 12%);
  reducir interacciones a priori o, si hace falta, `brms` con priors débiles.
- [ ] **6.B.5** Mantener un único criterio de selección principal; documentar el resto como sensibilidad.

### 6.C — Crecimiento
- [ ] **6.C.1** ANCOVA en supervivientes a t2: `V_t2 ~ V_t1 + Environment * microsite * quercus_sp + (1|Individual)`
  (respuesta primaria `Vtallo`; análogo a 6.B.2 dentro de arbustos).
- [ ] **6.C.2** Sensibilidad de supervivientes: comparar `V_t1` de supervivientes vs. muertos (el sesgo
  de supervivientes es la mayor limitación metodológica del análisis de crecimiento).
- [ ] **6.C.3** Declarar explícitamente que la longitud/diámetro solo se mide en vivos.

### 6.D — PCA/MFA del microhábitat (n=60)
- [ ] **6.D.1** Matriz de cobertura (variables × individuos) y **verificación del balance de**
  **especies y ambiente** en los 60 (por diseño, todas las especies están igualmente representadas en pinar y abierto).
- [ ] **6.D.2** MFA/PCA sobre los 60 con `Environment` como variable **suplementaria** (≤7 variables:
  biomasa, SLA, VPD, extinción PAR, humedad del suelo, un eje de textura —composicional—, 1-2 nutrientes).
  MFA con bloques para que el bloque de suelo no domine por número de variables.
- [ ] **6.D.3** FAMD (Environment activo) y/o dos PCA de 30 como **complementario/sensibilidad**;
  usar solo si los gradientes difieren entre ambientes.
- [ ] **6.D.4** Densidad de madera **fuera** del PCA individual (individuos distintos por método destructivo);
  se queda en `03.6`.
- [ ] **6.D.5** Exportar loadings y scores a `processed/` para usarlos en 6.B.3.

### 6.E — Predictivo y descriptivo
- [ ] **6.E.1** `PAR/extinción ~ biomasa` en los 240 micrositios (único modelo predictivo realista;
  base física Beer-Lambert).
- [ ] **6.E.2** Microclima y suelo: análisis **descriptivo** por tratamiento (foto de una campaña), no predictivo.

### 6.F — Funciones y orquestación
- [ ] **6.F.1** `01-scripts/_functions.R`: `fit_survival_model()`, `fit_growth_model()` (DHARMa +
  colinealidad + R² embebidos) y helper de selección si se mantiene `drop1`.
- [ ] **6.F.2** Rellenar `03-models.R` como orquestador que declara los modelos candidatos (6.B, 6.C, 6.D).
- [ ] **6.F.3** Nuevos scripts: `02.4-pca_mechanistic.R`, `03.7-growth_models.R`, `03.8-mechanistic_models.R`.
