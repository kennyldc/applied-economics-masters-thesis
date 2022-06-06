# Script for running some regresion models for my master's thesis at the student level

pacman::p_load(tidyverse, janitor, broom, fixest, factoextra)

# setwd("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/analisis_por_alumno")
# 
# # Information about results and crime for each student
# caracteristicas_2016 <- readRDS("caracteristicas_2016.rds")
# caracteristicas_2017 <- readRDS("caracteristicas_2017.rds")
# caracteristicas_2018 <- readRDS("caracteristicas_2018.rds")
# caracteristicas_2019 <- readRDS("caracteristicas_2019.rds")

# Dictionary of the Social Context questionnaire database  --------------------------------------------------
# The dictionary is not intended to  make data analysis, however I will bring into R the question and the code (variable for each year)
diccionario <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-f3QSQ8YRVnQfU0xf522z3KrRQX_POR_RS65WnoBE0g/edit?usp=sharing")
# Getting all the questions that were made at least one year
diccionario <- diccionario |> select(pregunta, categoria, cues17, cues18, cues19) |> remove_empty()
# For the sake of comparability in each year of the analysis I will only use questions that were made every year.
preguntas_todos <- diccionario |> drop_na()

# Functions ---------------------------------------------------------------

my_feols <-  function(y_dep, tipo_delito, vcov_spec, datos){
  #seccion_examen <- deparse(substitute(seccion_examen))
  #tipo_delito <- deparse(substitute(tipo_delito))
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
}

estimaciones_simples <- function(seccion, datos_car){
  map_df(combinaciones_delitos, function(x){my_feols(seccion, x, "HC1", datos_car)})
}

multiple_t_test <- function(caracteristicas){
  alumnos_cuestionario <- caracteristicas |> filter(ASISTENCIA_CONTEXTO == "PRES")
  alumnos_NA <- alumnos_cuestionario[rowSums(is.na(alumnos_cuestionario)) > 0,]
  preguntas <- names(alumnos_NA[,26:188])
  bt_multi <- lapply(preguntas, \(x){cr <- t.test(alumnos_NA |> pull(x), 
                                                  alumnos_cuestionario |> drop_na()  |> pull(x))
  cr$p.value})
  t_test <- tibble(pregunta = preguntas, valor_p = unlist(bt_multi)) |> 
    mutate(relacion = if_else(valor_p < 0.1, "DIFERENTES", "IGUALES"))
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
  variance_explained_eco <- importance_eco |> select(1:all_of(pc_number_eco))
  variance_explained_aca <- importance_aca |> select(1:all_of(pc_number_aca))
  variance_explained_soc <- importance_soc |> select(1:all_of(pc_number_soc))
  contribution_to_variance_eco <- as_tibble(get_pca_var(comp_eco)$contrib, rownames = "preguntas") |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_eco))
  contribution_to_variance_aca <- as_tibble(get_pca_var(comp_aca)$contrib, rownames = "preguntas") |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_aca))
  contribution_to_variance_soc <- as_tibble(get_pca_var(comp_soc)$contrib, rownames = "preguntas") |>
    rename_with( ~ gsub("Dim.", "PC", .x, fixed = TRUE)) |> select(1:all_of(pc_number_soc))
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
}

aca_combinaciones <- function(base_con_pc){
  eco_soc_pc <- paste0(names(base_con_pc |> select(all_of(matches(c("ECO", "SOC"))))), collapse = " + ")
  lapply(combinaciones_delitos, function(x){paste0(x, " + ", eco_soc_pc, " + CALIF_MAT + CALIF_ESP + TURNO_BASE + FINANCIAMIENTO")})
}

regresiones_con_aca_pc <- function(y, datos, aca_combinaciones_year, kilometros = NULL, dias_escolares = NULL){
  if(is.null(kilometros) & is.null(dias_escolares)){
    map_df(aca_combinaciones_year, \(x){my_feols(y, x, "iid", datos)}) |> 
      filter(str_detect(term, "(Intercept)") | str_detect(term, "INC")) |> arrange(especificacion)
  }
  else{
    map_df(aca_combinaciones_year, \(x){my_feols(y, x, "iid", datos)}) |> 
      filter(str_detect(term, "(Intercept)") | str_detect(term, "INC")) |> arrange(especificacion) |>
      filter(distancia == kilometros & dias == dias_escolares)
  }
}

resumen_regresiones <- function(base_con_pc, aca_combinaciones, kilometros = NULL, dias_escolares = NULL){
  lapply(names(base_con_pc |> select(all_of(matches("ACA")))),
         \(x){regresiones_con_aca_pc(x, base_con_pc, aca_combinaciones, kilometros, dias_escolares)})
}

# Models ------------------------------------------------------------------
# Getting combinations of crimes
combinaciones_delitos <- names(caracteristicas_2016[,25:60])

# Getting the more simple regression of grade ~ crime
efecto_simple_primarias_2016 <- list(esp = estimaciones_simples("CALIF_ESP", caracteristicas_2016 |> filter(NIVEL == "primaria")),
                                     mate = estimaciones_simples("CALIF_MAT", caracteristicas_2016 |> filter(NIVEL == "primaria")))

efecto_simple_secundarias_2016 <- list(esp = estimaciones_simples("CALIF_ESP", caracteristicas_2016 |> filter(NIVEL == "secundaria")),
                                     mate = estimaciones_simples("CALIF_MAT", caracteristicas_2016 |> filter(NIVEL == "secundaria")))

efecto_simple_2017 <- list(esp = estimaciones_simples("CALIF_ESP", caracteristicas_2017),
                           mate = estimaciones_simples("CALIF_MAT", caracteristicas_2017))

efecto_simple_2018 <- list(esp = estimaciones_simples("CALIF_ESP", caracteristicas_2018),
                           mate = estimaciones_simples("CALIF_MAT", caracteristicas_2018))

efecto_simple_2019 <- list(esp = estimaciones_simples("CALIF_ESP", caracteristicas_2019),
                           mate = estimaciones_simples("CALIF_MAT", caracteristicas_2019))

# Analysis of the context questionnaire 

# The questions were made in 2017 to 2019. Each question is categorized either as: 1) economics, 2) social, 3) academic
# In this section I examine if crime has an effect in this questions. 

# I will start with simple examples 
# With the question of the number of rooms to sleep
map_df(combinaciones_delitos, \(x){my_feols("P_10", x, "iid", caracteristicas_2017)}) |> arrange(especificacion)
# for almost all combinations an increase in one crime relates to a *decrease* in the number of rooms to sleep.

# With the question wether the student repeated a school year
map_df(combinaciones_delitos, \(x){my_feols("P_17", x, "iid", caracteristicas_2017)}) |> arrange(especificacion)
# For almost all combinations an increase in one crime realates to an *increase* in the probability that the student has repeated a school year.

# When asked what year they believe they will study
map_df(combinaciones_delitos, \(x){my_feols("P_54_D", x, "iid", caracteristicas_2017)}) |> arrange(especificacion)
# For almost all combinations an increase in one crime relates to a *decrease* in the probability that the student believe he/she will study graduate school

# It is a constant that the combinations that have few kilometers and few days are more noisy than the rest. 
# This could be a sign of lack of statistical power for those combinations.


# PCA Analysis 2017 - With Academics PC's --------------------------------------

# I will combine the questions from the same category with a dimensionality reduction technique due to the enormous amount of combinations.
# Starting with 2017, students that did not answer any of the context questions (i.e. they were absent that day) are not useful for the analysis
alumnos_cuestionario_2017 <- caracteristicas_2017 |> filter(ASISTENCIA_CONTEXTO == "PRES")

# Let apply pca to 2017
# Unfortunately, PCA does not allow any NA, so I'll have to drop some entries
# I will leave as a future assignment to verify if NA's are related to crime
#df <- multiple_t_test(caracteristicas_2017)

# For now, I'll need to assume that missing data is random (MAR, Missing at Random) and drop every row with at least one NA
alumnos_cuestionario_2017 <- alumnos_cuestionario_2017 |> drop_na()
# I end up with 47,322 students from junior high school level

# I apply PCA for each category available in the dictionary (economics, social and academic) related to the questionnaire
# to the following function is necessary to select a cut-off percentage of variance explained.  
# The function selects the number of principal components that cover that percentage of variance and returns a list with: a) proportion of variance explained, 
# b) contribution of each question in the questionnaire to the pc, and c) the coordinates of every student (only complete cases i.e. without NA)

pca_resultados_2017 <- my_pca(alumnos_cuestionario_2017, cues17, 0.605)

# I can access to the proportion of variance as (for eco as an example)
pca_resultados_2017$variance_explained$eco
# I can access the contribution of each question to the pc (for social as example)
pca_resultados_2017$contribution_to_variance$soc
# An the coordinates of every student (for academic as example)
pca_resultados_2017$coordinates$aca

# Let reshape the dataframe and instead of the questions (as literal) take the PC for each section 
# (every column is marked with the inicial of the section, for example PC for eco are ECO_PC1, ECO_PC2 and so on)
base_con_pc_2017 <- cbind(alumnos_cuestionario_2017 |> select(NOFOLIO:MEAN_CALIF_MAT_ESCUELA),
      pca_resultados_2017$coordinates$eco,
      pca_resultados_2017$coordinates$aca,
      pca_resultados_2017$coordinates$soc,
      alumnos_cuestionario_2017 |> select(INC_TIPO1_D250_T90H:INC_TIPO3_D1000_T10H))

# Let create the RHS terms of the regressions such that ACA_PC ~ CRIME + CONTROLS
# Controls are all of the PCs for Eco and Social + calif mat + calif_esp + turno + financiamiento
aca_combinaciones_2017 <- aca_combinaciones(base_con_pc_2017)

# Apply the regressions for each LHS term of the regressions that correspond to each ACA_PC
# Warning: The following line of code is slow.
# Returns a list of dataframes with the results of each regression of ACA_PC ~ crime + controls
# There is a regression for each combination of crime after controlling for everything possible in the context questionnaire!
regresiones_aca_2017 <- resumen_regresiones(base_con_pc_2017, aca_combinaciones_2017)

# The big amount of combinations for crimes make the dataframes big and almost non-interpretable.
# We also saw that combinations with short distance and few days are noisy.
# Therefore we can repeat the exercise of getting all the regressions of ACA_PC ~ crime + controls but for only one combination of crime and distance
# Warning: The following line of code is slow.
regresiones_aca_2017_km_dias <- resumen_regresiones(base_con_pc_2017, aca_combinaciones_2017, 500, 29)

# Results for ACA_PC1 ~ crime + controls
regresiones_aca_2017[1] 
# or
df <- regresiones_aca_2017_km_dias[1]

# and so on for each ACA_PC...
lapply(1:ncol(base_con_pc_2017 |> select(all_of(matches("ACA")))),
       \(x){regresiones_aca_2017_km_dias[x]})

##############################################################
# Here I have two main problems:
# 1) ACA_PC_{i}  is completely difficult to interpret even though I have the importance of each question, as
pca_resultados_2017$contribution_to_variance$aca
# Questions are disaggregated by choices in the questionnaire. I will attempt to fix it by transforming the ACA questions to yes or no answers.
# However, there is a second problem; 2) R2 of each regression is too low. 
##############################################################

# PCA Analysis 2018 - With Academics PC's --------------------------------------
# For the sake of comparability, I'll repeat the analysis for 2018
# Removing students that did not answer any of the context questions (i.e. they were absent that day)
alumnos_cuestionario_2018 <- caracteristicas_2018 |> filter(ASISTENCIA_CONTEXTO == "PRES")

# Assuming that missing data is random (MAR, Missing at Random) and dropping every row with at least one NA
alumnos_cuestionario_2018 <- alumnos_cuestionario_2018 |> drop_na()
# I end up with 71,639 students from elementary school level

# Apply PCA for each category available in the dictionary (economics, social and academic) related to the questionnaire
# With cut-off percentage of variance explained of 60.5%. 
# Return a list with proportion of variance explained, contribution for each question and coordinates
pca_resultados_2018 <- my_pca(alumnos_cuestionario_2018, cues18, 0.605)

# Let reshape the dataframe and instead of the questions (as literal) take the PC for each section 
base_con_pc_2018 <- cbind(alumnos_cuestionario_2018 |> select(NOFOLIO:MEAN_CALIF_MAT_ESCUELA),
                          pca_resultados_2018$coordinates$eco,
                          pca_resultados_2018$coordinates$aca,
                          pca_resultados_2018$coordinates$soc,
                          alumnos_cuestionario_2018 |> select(INC_TIPO1_D250_T90H:INC_TIPO3_D1000_T10H))

# RHS terms of the regressions such that ACA_PC ~ CRIME + CONTROLS
# Controls are all of the PCs for Eco and Social + calif mat + calif_esp + turno + financiamiento
aca_combinaciones_2018 <- aca_combinaciones(base_con_pc_2018)

# Apply the regressions for each LHS term of the regressions that correspond to each ACA_PC
# Returns a list of dataframes with the results of each regression of ACA_PC ~ crime + controls
# There is a regression for each combination of crime after controlling for everything possible in the context questionnaire!
regresiones_aca_2018 <- resumen_regresiones(base_con_pc_2018, aca_combinaciones_2018)

# Repeat the exercise of getting all the regressions of ACA_PC ~ crime + controls but for only one combination of crime and distance
regresiones_aca_2018_km_dias <- resumen_regresiones(base_con_pc_2018, aca_combinaciones_2018, kilometros = 500, dias_escolares = 29)

# Results for ACA_PC1 ~ crime + controls
lapply(1:ncol(base_con_pc_2018 |> select(all_of(matches("ACA")))),
       \(x){regresiones_aca_2018_km_dias[x]})

# Mostly same problems as before
# 1) ACA_PC_{i}  is completely difficult to interpret
# 2) R2 of each regression is too low. 

# PCA Analysis 2019 - With Academics PC's --------------------------------------
# For the sake of comparability, I'll repeat the analysis for 2019
# Removing students that did not answer context and assuming MAR
alumnos_cuestionario_2019 <- caracteristicas_2019 |> filter(ASISTENCIA_CONTEXTO == "PRES") |> drop_na()
# 48,252 students from junior high school in 2019 (almost the same as 2017)

# Apply PCA for each category available in the dictionary and percentage of variance explained of 60.5%. 
pca_resultados_2019 <- my_pca(alumnos_cuestionario_2019, cues19, 0.605)

# Let reshape the dataframe and instead of the questions (as literal) take the PC for each section 
base_con_pc_2019 <- cbind(alumnos_cuestionario_2019 |> select(NOFOLIO:MEAN_CALIF_MAT_ESCUELA),
                          pca_resultados_2019$coordinates$eco,
                          pca_resultados_2019$coordinates$aca,
                          pca_resultados_2019$coordinates$soc,
                          alumnos_cuestionario_2019 |> select(INC_TIPO1_D250_T90H:INC_TIPO3_D1000_T10H))

# Apply the regressions for each LHS term of the regressions that correspond to each ACA_PC
regresiones_aca_2019 <- resumen_regresiones(base_con_pc_2019, aca_combinaciones(base_con_pc_2019))

# Repeat the exercise of getting all the regressions of ACA_PC ~ crime + controls but for only one combination of crime and distance
regresiones_aca_2019_km_dias <- resumen_regresiones(base_con_pc_2019, aca_combinaciones(base_con_pc_2019), kilometros = 500, dias_escolares = 29)

# Results for ACA_PC1 ~ crime + controls
lapply(1:ncol(base_con_pc_2019 |> select(all_of(matches("ACA")))),
       \(x){regresiones_aca_2019_km_dias[x]})

# Mostly same problems as before