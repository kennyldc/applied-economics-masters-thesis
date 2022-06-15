# Script for running some models for my master's thesis at the student level

pacman::p_load(tidyverse, janitor, broom, fixest, factoextra, lfe)

# # Information about results and crime for each student
panel_secundarias_alumnos <- readRDS("panel_secundarias_alumnos.rds")
panel_primarias_alumnos <- readRDS("panel_primarias_alumnos.rds")
caracteristicas_2018 <- readRDS("caracteristicas_2018.rds")

# Dictionary of the Social Context questionnaire database  --------------------------------------------------
# The dictionary is not intended to  make data analysis, however I will bring into R the question and the code (variable for each year)
diccionario <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-f3QSQ8YRVnQfU0xf522z3KrRQX_POR_RS65WnoBE0g/edit?usp=sharing")
# Getting all the questions that were made at least one year
diccionario <- diccionario |> select(pregunta, categoria, cues17, cues18, cues19) |> remove_empty()
# For the sake of comparability in each year of the analysis I will only use questions that were made every year.
preguntas_todos <- diccionario |> drop_na()
# The following questions are cancelled from the analysis because are poorly designed
preguntas_todos <- preguntas_todos |> filter((pregunta != "¿En la escuela le dan clases en la lengua indígena?") & 
                                               (pregunta != "Frecuencia falta tiempo para terminar tarea"))

# To print tables let use 
definiciones <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1pTBmuH9eY8ch8s5lGjEIAPbObtJ0fXR6-tU4dFpP0aY/edit?usp=sharing")

# Functions ---------------------------------------------------------------
# Function to run multiple regressions with fixed effects, and if wanted: weights. The function returns a summary table
reg_felm <-  function(seccion_examen, tipo_delito, datos){
  f <- formula(paste(seccion_examen, "~", tipo_delito, "| ID_UNICO + YEAR | 0 | ID_UNICO")) # specification with fixed effects and class formula
  regresion <- felm(f, data = datos) # the function to run fixed effects regressions without weights
  tabla <- summary(regresion)$coefficients # gets the coefficients info (coef, sd, p value)
  tabla <- tibble::rownames_to_column(as.data.frame(tabla), "termino") # format the results to dataframe
  adjr <- tibble(adj_r_squared = summary(regresion)$r.squared) # gets the adjusted R2 of the regression
  tabla <- cbind(tabla, adjr) # adds the adjusted R to the dataframe in order to get a summary
  # adding format to get the specification, the type of crime, distance and the number of days into the summary table
  tabla <- tabla |> mutate(especificacion = paste(seccion_examen, "~", tipo_delito),
                           delito = case_when(grepl("TIPO1", especificacion) ~ "PUBLICO",
                                              grepl("TIPO2", especificacion) ~ "PRIVADO",
                                              grepl("TIPO3", especificacion) ~ "HOMICIDIO",
                                              TRUE ~ "TODOS"),
                           delito = as.factor(delito),
                           distancia = case_when(grepl("D250", especificacion) ~ "250",
                                                 grepl("D500", especificacion) ~ "500",
                                                 TRUE ~ "1000"),
                           dias = case_when(grepl("T10", especificacion) ~ "10",
                                            grepl("T29", especificacion) ~ "29",
                                            TRUE ~ "90"))
  tabla <- tabla |> mutate(across(where(is.numeric), ~ round(., digits = 4)),
                           across(`Pr(>|t|)`, ~ round(., digits = 3)),
                           astericos = case_when(`Pr(>|t|)` <= 0.01 ~ "***",
                                                 (`Pr(>|t|)` > 0.01 & `Pr(>|t|)` <= 0.05) ~ "**",
                                                 (`Pr(>|t|)` > 0.05 & `Pr(>|t|)` <= 0.1) ~ "*",
                                                 TRUE ~ ""), .after = Estimate)
  tabla <- tabla |> mutate(distancia = as.numeric(distancia),
                           dias = as.numeric(dias))
  tabla <- tabla |> filter(grepl("INC", termino))
  tabla <- tabla |> select(-`t value`,-`Cluster s.e.`)
  tabla <- tabla |> select(especificacion, termino, delito:last_col(), Estimate:adj_r_squared)
  return(tabla)
}

my_pca <- function(data, cuestionario, proportion_of_variance){
  eco <- data |> select(all_of(matches(preguntas_todos |> filter(categoria == "económica") |> 
                                         pull({{cuestionario}}))))
  aca <- data |> select(all_of(matches(preguntas_todos |> filter(categoria == "académica") |> 
                                         pull({{cuestionario}}))))
  soc <- data |> select(all_of(matches(preguntas_todos |> filter(categoria == "social") |> 
                                         pull({{cuestionario}}))))
  comp_eco <- prcomp(eco, scale. = TRUE)
  comp_aca <- prcomp(aca, scale. = TRUE)
  comp_soc <- prcomp(soc, scale. = TRUE)
  # Select a cut-off point of proportion of variance explained of (x)
  importance_eco <-  as_tibble(summary(comp_eco)$importance, rownames = "importance_of_components")
  importance_aca <-  as_tibble(summary(comp_aca)$importance, rownames = "importance_of_components")
  importance_soc <-  as_tibble(summary(comp_soc)$importance, rownames = "importance_of_components")
  pc_number_eco <- pivot_longer(importance_eco, cols = 2:last_col(), names_to = "PC", values_to = "value") |>
    filter(importance_of_components == "Cumulative Proportion" & value < proportion_of_variance) |> tail(1) |> pull(PC)
  pc_number_aca <- pivot_longer(importance_aca, cols = 2:last_col(), names_to = "PC", values_to = "value") |>
    filter(importance_of_components == "Cumulative Proportion" & value < proportion_of_variance) |> tail(1) |> pull(PC)
  pc_number_soc <- pivot_longer(importance_soc, cols = 2:last_col(), names_to = "PC", values_to = "value") |>
    filter(importance_of_components == "Cumulative Proportion" & value < proportion_of_variance) |> tail(1) |> pull(PC)
  variance_explained_eco <- importance_eco |> select(1:all_of(pc_number_eco)) |>
    rename_with( ~ gsub("PC", "ECO_PC_", .x, fixed = TRUE))
  variance_explained_aca <- importance_aca |> select(1:all_of(pc_number_aca)) |>
    rename_with( ~ gsub("PC", "ACA_PC_", .x, fixed = TRUE))
  variance_explained_soc <- importance_soc |> select(1:all_of(pc_number_soc)) |>
    rename_with( ~ gsub("PC", "SOC_PC_", .x, fixed = TRUE))
  contribution_to_variance_eco <- as_tibble(get_pca_var(comp_eco)$contrib, rownames = "preguntas") |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_eco)) |>
    rename_with( ~ gsub("PC", "ECO_PC_", .x, fixed = TRUE)) |>
    left_join(definiciones |> rename(preguntas = cues17) |> select(pregunta, preguntas)) |>
    select(preguntas, pregunta, ECO_PC_1:last_col())
  contribution_to_variance_aca <- as_tibble(get_pca_var(comp_aca)$contrib, rownames = "preguntas") |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_aca)) |>
    rename_with( ~ gsub("PC", "ACA_PC_", .x, fixed = TRUE)) |>
    left_join(definiciones |> rename(preguntas = cues17) |> select(pregunta, preguntas)) |>
    select(preguntas, pregunta, ACA_PC_1:last_col())
  contribution_to_variance_soc <- as_tibble(get_pca_var(comp_soc)$contrib, rownames = "preguntas") |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_soc)) |>
    rename_with( ~ gsub("PC", "SOC_PC_", .x, fixed = TRUE)) |>
    left_join(definiciones |> rename(preguntas = cues17) |> select(pregunta, preguntas)) |>
    select(preguntas, pregunta, SOC_PC_1:last_col())
  coordinates_eco <- as_tibble(get_pca_ind(comp_eco)$coord) |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_eco)) |>
    rename_with( ~ gsub("PC", "ECO_PC", .x, fixed = TRUE))
  coordinates_aca <- as_tibble(get_pca_ind(comp_aca)$coord) |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_aca)) |>
    rename_with( ~ gsub("PC", "ACA_PC", .x, fixed = TRUE))
  coordinates_soc <- as_tibble(get_pca_ind(comp_soc)$coord) |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_soc)) |>
    rename_with( ~ gsub("PC", "SOC_PC", .x, fixed = TRUE))
  pca_results <- list(variance_explained = list(eco = variance_explained_eco,
                                                aca = variance_explained_aca,
                                                soc = variance_explained_soc),
                      contribution_to_variance = list(eco = contribution_to_variance_eco,
                                                      aca = contribution_to_variance_aca,
                                                      soc = contribution_to_variance_soc),
                      coordinates = list(eco = coordinates_eco,
                                         aca = coordinates_aca,
                                         soc = coordinates_soc))
  pca_results
}

my_feols <-  function(y_dep, tipo_delito, vcov_spec, datos){
  f <- formula(paste(y_dep, "~", tipo_delito))
  reg <- feols(f, vcov = vcov_spec, data = datos)
  treg <- cbind(tidy(reg), tibble(r2 = fitstat(reg, ~ r2)$r2))
  treg <- treg |> mutate(across(where(is.numeric), ~ round(.,4)))
  treg <- treg |> mutate(especificacion = paste(y_dep, "~", tipo_delito),
                         delito = case_when(grepl("TIPO1", especificacion) ~ "PUBLICO",
                                            grepl("TIPO2", especificacion) ~ "PRIVADO",
                                            grepl("TIPO3", especificacion) ~ "HOMICIDIO",
                                            TRUE ~ "TODOS"),
                         delito = as.factor(delito),
                         distancia = case_when(grepl("D250", especificacion) ~ "250",
                                               grepl("D500", especificacion) ~ "500",
                                               TRUE ~ "1000"),
                         dias = case_when(grepl("T10", especificacion) ~ "10",
                                          grepl("T29", especificacion) ~ "29",
                                          TRUE ~ "90"))
  treg <- treg |> select(especificacion, term, delito:last_col(), estimate:r2) |> arrange(especificacion)
  treg <- treg |> filter(grepl("INC", term))
  treg <- treg |> mutate(across(where(is.numeric), ~ round(., digits = 4)),
                         across(p.value, ~ round(., digits = 3)),
                         astericos = case_when(p.value <= 0.01 ~ "***",
                                               (p.value > 0.01 & p.value <= 0.05) ~ "**",
                                               (p.value > 0.05 & p.value <= 0.1) ~ "*",
                                               TRUE ~ ""), .after = estimate)
  treg <- treg |> select(-std.error,-statistic)
}

aca_combinaciones <- function(base_con_pc){
  pcs <- paste0(names(base_con_pc |> select(all_of(matches(c("ECO", "SOC", "ACA"))))), collapse = " + ")
  lapply(combinaciones_delitos, function(x){paste0(x, " + ", pcs)})
}

# Models ------------------------------------------------------------------
# Getting right hand coefficient terms of the regression specifications -------------------------------------------------
# getting the name of variables that will enter as independent variables
combinaciones_delitos <- names(panel_secundarias_alumnos)[82:117]

# I apply PCA for each category available in the dictionary (economics, social and academic) related to the questionnaire
# to the following function is necessary to select a cut-off percentage of variance explained.  
# The function selects the number of principal components that cover that percentage of variance and returns a list with: a) proportion of variance explained, 
# b) contribution of each question in the questionnaire to the pc, and c) the coordinates of every student (only complete cases i.e. without NA)
# Unfortunately, PCA does not allow any NA, so I'll have to drop some entries
# I will leave as a future assignment to verify if NA's are related to crime
# For now, I'll need to assume that missing data is random (MAR, Missing at Random) and drop every row with at least one NA
panel_sin_na <- panel_secundarias_alumnos |> drop_na()
# I end up with 95,645 students from junior high school level

pca_panel_sin_na <- my_pca(panel_sin_na, cues17, 0.605)

# I can access to the proportion of variance as (for eco as an example)
pca_panel_sin_na$variance_explained$eco
# I can access the contribution of each question to the pc (for social as example)
pca_panel_sin_na$contribution_to_variance$soc
# An the coordinates of every student (for academic as example)
pca_panel_sin_na$coordinates$aca

# Let reshape the dataframe and instead of the questions (as literal) take the PC for each section 
# (every column is marked with the inicial of the section, for example PC for eco are ECO_PC1, ECO_PC2 and so on)
base_con_pc<- cbind(panel_sin_na |> select(NOFOLIO:INDEX_ESFUERZO),
                    pca_panel_sin_na$coordinates$eco,
                    pca_panel_sin_na$coordinates$aca,
                    pca_panel_sin_na$coordinates$soc,
                    panel_sin_na |> select(INC_TIPO1_D250_T90H:INC_TIPO3_D1000_T10H))

# Let create the RHS terms of the regressions such that CALIF ~ CRIME + CONTROLS
incidentes_mas_controles <- aca_combinaciones(base_con_pc)

coef_pos <- unlist(incidentes_mas_controles)
combinacion_genero <- modify2(coef_pos, modify(combinaciones_delitos, function(x){paste0(x, " * GENERO")}), ~ paste0(.x, " + ", .y))
combinacion_tutor_degree <- modify2(coef_pos, modify(combinaciones_delitos, function(x){paste0(x, " * TUTOR_DEGREE")}), ~ paste0(.x, " + ", .y))
combinacion_poder_adquisitivo <- modify2(coef_pos, modify(combinaciones_delitos, function(x){paste0(x, " * ECO_PC1")}), ~ paste0(.x, " + ", .y))
combinacion_numero_gente_en_casa <- modify2(coef_pos, modify(combinaciones_delitos, function(x){paste0(x, " * ECO_PC3")}), ~ paste0(.x, " + ", .y))
combinacion_horas_no_escuela <- modify2(coef_pos, modify(combinaciones_delitos, function(x){paste0(x, " * ECO_PC4")}), ~ paste0(.x, " + ", .y))
combinacion_escolaridad_casa <- modify2(coef_pos, modify(combinaciones_delitos, function(x){paste0(x, " * SOC_PC1")}), ~ paste0(.x, " + ", .y))
combinacion_raices_indigenas <- modify2(coef_pos, modify(combinaciones_delitos, function(x){paste0(x, " * SOC_PC2")}), ~ paste0(.x, " + ", .y))

terminos_coef_posibles <- list(incidentes = coef_pos, 
                               inter_genero = combinacion_genero,
                               inter_tutor_degree = combinacion_tutor_degree,
                               inter_poder_adquisitivo = combinacion_poder_adquisitivo,
                               inter_numero_gente = combinacion_numero_gente_en_casa, 
                               inter_horas_no_escuela = combinacion_horas_no_escuela,
                               inter_escolaridad_casa = combinacion_escolaridad_casa,
                               inter_raices_indigenas = combinacion_raices_indigenas)

resumen_estimaciones <- function(seccion, datos_panel){
  lapply(terminos_coef_posibles, \(x){
    map_df(x, \(x) reg_felm(seccion, x, datos_panel))})}

## Result for model secundarias

# ejemplo <- felm(CALIF_ESP ~ INC_TIPO3_D250_T29H | ID_UNICO + YEAR | 0 | ID_UNICO, data=panel_secundarias_alumnos)
# ejemplo_feols <- feols(CALIF_ESP ~ INC_TIPO3_D250_T29H | ID_UNICO + YEAR, panel_secundarias_alumnos)

estimaciones_espanol_alumno <- resumen_estimaciones("CALIF_ESP", base_con_pc)
estimaciones_matematicas_alumno <- resumen_estimaciones("CALIF_MAT", base_con_pc)

# Changing the variable to measure student achievement --------------------

# in the following model instead of using the test score (calif) I will use an "effort index" constructed from the social context

# Let create the RHS terms of the regressions such that ESFUERZO ~ CRIME + CONTROL ECO + CONTROL SOC
# Different to the previous estimations, here I only control for eco + soc because esfuerzo is made from ACA variables.

combinaciones_esfuerzo <- function(base_con_pc){
  eco_soc_pc <- paste0(names(base_con_pc |> select(all_of(matches(c("ECO", "SOC"))))), collapse = " + ")
  lapply(combinaciones_delitos, function(x){paste0(x, " + ", eco_soc_pc)})
}

incidentes_esfuerzo <- combinaciones_esfuerzo(base_con_pc)

# Same combinations as above for comparability

coef_esfuerzo <- unlist(incidentes_esfuerzo)
esfuerzo_genero <- modify2(coef_esfuerzo, modify(combinaciones_delitos, function(x){paste0(x, " * GENERO")}), ~ paste0(.x, " + ", .y))
esfuerzo_tutor_degree <- modify2(coef_esfuerzo, modify(combinaciones_delitos, function(x){paste0(x, " * TUTOR_DEGREE")}), ~ paste0(.x, " + ", .y))
esfuerzo_poder_adquisitivo <- modify2(coef_esfuerzo, modify(combinaciones_delitos, function(x){paste0(x, " * ECO_PC1")}), ~ paste0(.x, " + ", .y))
esfuerzo_numero_gente_en_casa <- modify2(coef_esfuerzo, modify(combinaciones_delitos, function(x){paste0(x, " * ECO_PC3")}), ~ paste0(.x, " + ", .y))
esfuerzo_horas_no_escuela <- modify2(coef_esfuerzo, modify(combinaciones_delitos, function(x){paste0(x, " * ECO_PC4")}), ~ paste0(.x, " + ", .y))
esfuerzo_escolaridad_casa <- modify2(coef_esfuerzo, modify(combinaciones_delitos, function(x){paste0(x, " * SOC_PC1")}), ~ paste0(.x, " + ", .y))
esfuerzo_raices_indigenas <- modify2(coef_esfuerzo, modify(combinaciones_delitos, function(x){paste0(x, " * SOC_PC2")}), ~ paste0(.x, " + ", .y))

terminos_esfuerzo <- list(incidentes = coef_esfuerzo, 
                               inter_genero = esfuerzo_genero,
                               inter_tutor_degree = esfuerzo_tutor_degree,
                               inter_poder_adquisitivo = esfuerzo_poder_adquisitivo,
                               inter_numero_gente = esfuerzo_numero_gente_en_casa, 
                               inter_horas_no_escuela = esfuerzo_horas_no_escuela,
                               inter_escolaridad_casa = esfuerzo_escolaridad_casa,
                               inter_raices_indigenas = esfuerzo_raices_indigenas)

resumen_esfuerzo <- function(seccion, datos_panel){
  lapply(terminos_esfuerzo, \(x){
    map_df(x, \(x) reg_felm(seccion, x, datos_panel))})}

## Results for index esfuerzo
write_clip(estimaciones_index_esfuerzo$inter_raices_indigenas |> filter(delito == "PUBLICO") |> arrange(distancia, -dias))

estimaciones_index_esfuerzo <- resumen_esfuerzo("INDEX_ESFUERZO", base_con_pc)

# estimacion panel FE para primarias  -------------------------------------

resumen_primarias <- function(seccion, datos_panel){
  map_df(combinaciones_delitos, \(x) reg_felm(seccion, x, datos_panel))
}

## Result for model primarias
# ejemplo_primarias <- feols(CALIF_ESP ~ INC_TIPO3_D250_T29H | ID_UNICO + YEAR, panel_primarias_alumnos)

write_clip(estimaciones_matematicas_alumno_primarias |> filter(delito == "PUBLICO") |> arrange(distancia, -dias))

estimaciones_espanol_alumno_primarias <- resumen_primarias("CALIF_ESP", panel_primarias_alumnos)
estimaciones_matematicas_alumno_primarias <- resumen_primarias("CALIF_MAT", panel_primarias_alumnos)

# Last models - Mechanisms from literature ------------------------------
# According to the literature some mechanisms that explain the role of violent crimes in academic development are
# b) Disminución de la actividad económica de la comunidad, 
# d) Deterioro en la calidad de la oferta en los centros educativos.

# I will only run some OLS regressions to see for each year in the context data if these relations hold
# This are more like sanity checks instead of a more complex estimations.
# Also, the estimates are more of the correlation type instead of a causal effect.

## Disminucion de la actividad económica, 
## Does students work more in the labor market in substitution of school?
## Let estimate a simple LPM

# For Junior high school panel
combinaciones_aca_soc <- function(panel_secundarias_alumnos){
  eco_soc_pc <- paste0(preguntas_todos |> filter(categoria != "económica") |> pull(cues17), collapse = " + ")
  lapply(combinaciones_delitos, function(x){paste0(x, " + ", eco_soc_pc)})
}

incidentes_aca_soc <- unlist(combinaciones_aca_soc(panel_secundarias_alumnos))

## Function for estimations

estimaciones_simples <- function(y_dep, datos_car, vector_combinaciones){
  map_df(vector_combinaciones, function(x){my_feols(y_dep, x, ~ID_UNICO, datos_car)})
}

panel_MLP_secundarias <- panel_secundarias_alumnos |> mutate(MERCADO_LABORAL = if_else(P_20 == 0, 0, 1))

## The coefficients indicate the change in the probability that the student is in the labor market after controlling for each social and academic characteristic

participacion_mercado_laboral <- estimaciones_simples("P_20", panel_MLP_secundarias, incidentes_aca_soc)

preguntas_frecuencia_maestros <- function(cuestionario_year){
  preguntas_todos |> filter(str_detect(pregunta, "Frecuencia maestros")) |> pull({{cuestionario_year}})
}

# Does teacher have worse practices?
## The coefficients indicate the change in the probability that the professor... controlling for eco and soc

combinaciones_soc_eco <- function(panel_secundarias_alumnos){
  eco_soc_pc <- paste0(preguntas_todos |> filter(categoria != "académica") |> pull(cues17), collapse = " + ")
  lapply(combinaciones_delitos, function(x){paste0(x, " + ", eco_soc_pc)})
}

incidentes_soc_eco <- unlist(combinaciones_soc_eco(panel_secundarias_alumnos))

preguntas_frecuencia_maestros(cues17)

# Maestro toma en cuenta opinión

maestros_opinion <- estimaciones_simples("P_56", panel_MLP_secundarias, incidentes_soc_eco)

# Maestro da confianza para preguntar
maestros_confianza <- estimaciones_simples("P_60", panel_MLP_secundarias, incidentes_soc_eco)

# Maestro organiza actividades con opinión
maestros_actividades <- estimaciones_simples("P_61", panel_MLP_secundarias, incidentes_soc_eco)

# Maestro pide escucharse cuando hay desacuerdo
maestros_escucharse_desacuerdo <- estimaciones_simples("P_65", panel_MLP_secundarias, incidentes_soc_eco)
