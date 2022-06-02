# Script for running some regresion models for my master's thesis

# check if libraries are installed, then load
pacman::p_load(tidyverse, foreign, janitor, lfe, broom)
options(digits = 4,  scipen=999)

# Reading the panel with information of mean Planea school results  and crimes committed around each school in CDMX from 2016 to 2019 
# Reading the file from Google Drive

id_file <- "1Ax2Ucq91VcGqvaeF43u2KGF0hC9OFPkS"
nuevo_panel <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_file))

# changing some variables from class character to class factor
nuevo_panel <- nuevo_panel |>
  mutate(across(c(TURNO_BASE, N_TURNO_BASE, GRADO, NIVEL, FINANCIAMIENTO), as.factor))

# In this panel I have information of 4252 schools: 5920 elementary, 3826 junior high
# each school has at least observations for 2 years.

# These are examples of the fixed effects regressions that I'm running.
# The fixed effects variables are the school ID and the year. Errors are clustered by schools.
# felyc_tipo1_d250_t90 <- felm(MEAN_CALIF_ESP ~ INC_TIPO1_D500_T90H | ID_UNICO + AÑO | 0 | ID_UNICO, data=nuevo_panel |> filter(NIVEL == "PRIMARIA"), weights = NULL)
# felyc_tipo1_d250_t90_w1 <- felm(MEAN_CALIF_ESP ~ INC_TIPO1_D500_T90H | ID_UNICO + AÑO | 0 | ID_UNICO, data=nuevo_panel |> filter(NIVEL == "PRIMARIA"), weights = nuevo_panel |> filter(NIVEL == "PRIMARIA") |> pull(PORCENTAJE_DEL_TOTAL_ALUMNOS))
# felyc_tipo1_d250_t90_inter <- felm(MEAN_CALIF_ESP ~ INC_TIPO1_D500_T90H * FINANCIAMIENTO | ID_UNICO + AÑO | 0 | ID_UNICO, data=nuevo_panel |> filter(NIVEL == "PRIMARIA"), weights = NULL)
# felyc_tipo1_d250_t90_w1_inter <- felm(MEAN_CALIF_ESP ~ INC_TIPO1_D500_T90H * FINANCIAMIENTO | ID_UNICO + AÑO | 0 | ID_UNICO, data=nuevo_panel |> filter(NIVEL == "PRIMARIA"), weights = nuevo_panel |> filter(NIVEL == "PRIMARIA") |> pull(PORCENTAJE_DEL_TOTAL_ALUMNOS))

# Function to run multiple regressions with fixed effects, and if wanted: weights. The function returns a summary table
reg_felm <-  function(seccion_examen, tipo_delito, datos, nivel, pesos){
  # seccion_examen <- deparse(substitute(seccion_examen))
  # tipo_delito <- deparse(substitute(tipo_delito))
  # nivel <- deparse(substitute(nivel))
  f <- formula(paste(seccion_examen, "~", tipo_delito, "| ID_UNICO + AÑO | 0 | ID_UNICO")) # specification with fixed effects and class formula
  if (is.null(pesos)){
    regresion <- felm(f, data = datos |> filter(NIVEL == nivel)) # the function to run fixed effects regressions without weights
  }
  else{
    regresion <- felm(f, data = datos |> filter(NIVEL == nivel), weights = nuevo_panel |> filter(NIVEL == nivel) |> pull(PORCENTAJE_DEL_TOTAL_ALUMNOS)) # the function to run fixed effects regressions with weights
  }
  tabla <- summary(regresion)$coefficients # gets the coefficients info (coef, sd, p value)
  tabla <- tibble::rownames_to_column(as.data.frame(tabla), "termino") # format the results to dataframe
  adjr <- tibble(adj_r_squared = summary(regresion)$r.squared) # gets the adjusted R2 of the regression
  tabla <- cbind(tabla, adjr) # adds the adjusted R to the dataframe in order to get a summary
  # adding format to get the specification, the type of crime, distance and the number of days into the summary table
  tabla <- tabla |> mutate(especificacion = paste(seccion_examen, "~", tipo_delito),
                            grado = nivel,
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
  tabla <- tabla |> select(especificacion, grado, termino, delito:last_col(), Estimate:adj_r_squared)
  return(tabla)
}

# Getting right hand coefficient terms of the regression specifications -------------------------------------------------
# getting the name of variables that will enter as independent variables
combinaciones_incidentes <- names(nuevo_panel)[19:54]
# getting the right hand coefficient terms for interaction term with underdevelopment index (indice rezago)
interacciones_carencias <- unlist(lapply(combinaciones_incidentes, \(x) paste0(x, " + " , x,"*","INDICE_REZ")))
# getting the right hand coefficient terms for interaction term with school funding
interacciones_financiamiento <- unlist(lapply(combinaciones_incidentes, \(x) paste0(x, " + " , x,"*","FINANCIAMIENTO")))
# getting the right hand coefficient terms for interaction term with school shift (morning or afternoon)
interacciones_turno <- unlist(lapply(combinaciones_incidentes, \(x) paste0(x, " + " , x,"*","N_TURNO_BASE")))

# merging all combinations of right hand coef terms into a single list
terminos_coef_posibles <- list(incidentes = combinaciones_incidentes,
                               incidentes_interaccion_carencias = interacciones_carencias,
                               incidentes_interaccion_financiamiento = interacciones_financiamiento,
                               incidentes_interaccion_turno = interacciones_turno)

# LOG-LOG transformations -------------------------------------------------
# for the variables that reflect the number of crimes around the school there are several with a value equal to 0.
# Even for the mean grades, the minimum value in each case is 0
# summary(nuevo_panel[17:54])

# I follow the common practice to add a value of 1 in order to make the log-log transformation.
# I decide to make the transformation OUTSIDE the regression specification in order to use the function made above
nuevo_panel_log_log <- nuevo_panel
nuevo_panel_log_log[17:54] <- log(nuevo_panel_log_log[17:54]+1)

# Wrapping estimations inside a function.
# The following function allows to select the section, student level (elementary or junior high school), and either to include weights 
# (weights are defined by each year and according to the number of students)
# The results are stored in a list (of data frames) with the summaries of the estimate for each coefficient (all combinations - including interactions), 
# the type of crime, the type of level the number of days and the distance, the standard deviation, p value and adj R2
# To make the code less obscure, the first lapply iterates the making of the dataframe for each right hand coefficient term combination
# specified in the terminos_coef_posibles list (that is why the output has 4 daframes) with the use of an anonymous function,
# (the syntax \(x) is a new feature from R 4.0)
# The map_df produces the dataframe by iterating each coefficient from the combinations made above (see line 52 to 58) into
# the reg_felm function that produces a summary specifying the data and if the weights (pesos) are required.
# Arguments require the use of quotes ("). They were not deparsed on purpose.

resumen_estimaciones <- function(seccion, datos_panel, nivel_planea, elige_pesos){
  lapply(terminos_coef_posibles, \(x){
    map_df(x, \(x) reg_felm(seccion, x, datos_panel, nivel_planea, elige_pesos))})}

# Elementary Schools (Primarias) ------------------------------------------
# MEAN_CALIF_ESP - The effect of crime in the Spanish Test for elementary schools
# I create a list with the results with and without weights.
# The first estimation measure dependent and independent variables as levels

estimaciones_primarias_esp <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel, "PRIMARIA", NULL),
                                   estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel, "PRIMARIA", "YES"))

# MEAN_CALIF_ESP LOG-LOG Transformation -----------------------------------------
# Because the interpretation of the coefficient estimates is more or less difficult AND because the number of crimes are close to 0,
# I decide to run the same regressions but making a log transformation for both y and x 

estimaciones_primarias_esp_log_log <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel_log_log, "PRIMARIA", NULL),
                                           estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel_log_log, "PRIMARIA", "YES"))

# MEAN_CALIF_MAT The effect of crime in the Math Test for elementary schools
# levels
estimaciones_primarias_mat <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel, "PRIMARIA", NULL),
                                   estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel, "PRIMARIA", "YES"))

# MEAN_CALIF_MAT LOG-LOG Transformation -----------------------------------------
# I decide to run the same regressions but making a log transformation for both y and x 

estimaciones_primarias_mat_log_log <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel_log_log, "PRIMARIA", NULL),
                                   estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel_log_log, "PRIMARIA", "YES"))

# Junio High School (Secundarias) ------------------------------------------
# MEAN_CALIF_ESP - The effect of crime in the Spanish Test for junior high schools
# Levels
estimaciones_secundarias_esp <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel, "SECUNDARIA", NULL),
                                     estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel, "SECUNDARIA", "YES"))

# log-log
estimaciones_secundarias_esp_log_log <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel_log_log, "SECUNDARIA", NULL),
                                    estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_ESP", nuevo_panel_log_log, "SECUNDARIA", "YES"))

# MEAN_CALIF_MAT - The effect of crime in the Math Test for junior high schools
# Levels
estimaciones_secundarias_mat <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel, "SECUNDARIA", NULL),
                                     estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel, "SECUNDARIA", "YES"))

# log-log
estimaciones_secundarias_mat_log_log <- list(estimaciones_sin_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel_log_log, "SECUNDARIA", NULL),
                                             estimaciones_con_pesos = resumen_estimaciones("MEAN_CALIF_MAT", nuevo_panel_log_log, "SECUNDARIA", "YES"))