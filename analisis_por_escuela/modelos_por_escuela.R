# Script for running some regresion models for my master's thesis

# check if libraries are installed, then load
pacman::p_load(magrittr, tidyverse, foreign, janitor, lfe)

# Reading the panel with information of mean Planea school results  and crimes commited around each school in CDMX from 2016 to 2019 
# Reading the file from Google Drive

id_file <- "1Ax2Ucq91VcGqvaeF43u2KGF0hC9OFPkS"
nuevo_panel <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_file))

# changing some variables from class character to class factor
nuevo_panel <- nuevo_panel |>
  mutate(across(c(TURNO_BASE, N_TURNO_BASE, GRADO, NIVEL, FINANCIAMIENTO), as.factor))

# These are examples of the fixed effects regressions that I'm running.
# felyc_tipo1_d250_t90 <- felm(MEAN_CALIF_ESP ~ INC_TIPO1_D250_T90H | ID_UNICO + AÑO | 0 | ID_UNICO, data=nuevo_panel, weights = nuevo_panel$PORCENTAJE_DEL_TOTAL_ALUMNOS)
# felyc_tipo1_d250_t90_INTER <- felm(MEAN_CALIF_ESP ~ INC_TIPO1_D250_T90H + INC_TIPO1_D250_T90H * FINANCIAMIENTO  | ID_UNICO + AÑO | 0 | ID_UNICO, data=nuevo_panel, weights = NULL)

# Function to run multiple regressions with fixed effects, and if wanted: weights. The function returns a summary table
reg_felm <-  function(seccion_examen, tipo_delito, datos, pesos){
  f <- formula(paste(seccion_examen, "~", tipo_delito, "| ID_UNICO + AÑO | 0 | ID_UNICO")) # specification with fixed effects and class formula
  if (is.null(pesos)){
    regresion <- felm(f, data = datos) # the function to run fixed effects regressions withouth weights
  }
  else{
    regresion <- felm(f, data = datos, weights = datos$PORCENTAJE_DEL_TOTAL_ALUMNOS) # the function to run fixed effects regressions with weights
  }
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
  tabla <- tabla |> select(especificacion, termino, delito:last_col(), Estimate:adj_r_squared)
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

# LOG-LOG transformations -------------------------------------------------
# for the variables that reflect the number of crimes around the school there are several with a value equal to 0.
# Even for the mean grades, the minimun value in each case is 0
# summary(nuevo_panel[17:54])

# I follow the common practice to add a value of 1 in order to make the log-log transformation.
# I decide to make the transformation OUTSIDE the regression specification in order to use the function made above
nuevo_panel_log_log <- nuevo_panel
nuevo_panel_log_log[17:54] <- log(nuevo_panel_log_log[17:54]+1)


# Estimates
# MEAN_CALIF_ESP -----------------------------------------

# Running the regressions for the data without any transformation and NULL weights for all combination of crimes
resumen_coeficientes_esp <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos=NULL)))
# Running the regressions for the data without any transformation and WITH weights. Weights are pondered by the number of alumni in that school in terms of others from the same level.
resumen_coeficientes_esp_weights <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos="YES")))

# adding interaction term with social underdevelopment index, null weights
resumen_coeficientes_esp_carencias <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos=NULL)))
# adding interaction term with social underdevelopment index, with weights
resumen_coeficientes_esp_carencias_weights <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos="YES")))

# adding interaction term with school funding, null weights. The omitted variable is private funding
resumen_coeficientes_esp_financiamiento <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos=NULL)))
# adding interaction term with school funding, with weights. The omitted variable is private funding
resumen_coeficientes_esp_financiamiento_weights <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos="YES")))

# adding interaction term with school shift, null weights. The omitted variable is morning shift
resumen_coeficientes_esp_turno <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos=NULL)))
# adding interaction term with school shift, with weights. The omitted variable is morning shift
resumen_coeficientes_esp_turno_weights <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos="YES")))

# MEAN_CALIF_ESP LOG-LOG -----------------------------------------
#Same estimations as above but with the log-log specification

# Running the regressions for the data with log log  transformation and NULL weights for all combination of crimes
resumen_coeficientes_esp_log_log <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos=NULL)))
# Running the regressions for the data with log log transformation and WITH weights. Weights are pondered by the number of alumni in that school in terms of others from the same level.
resumen_coeficientes_esp_log_log_weights <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos="YES")))

# adding interaction term with social underdevelopment index, null weights
resumen_coeficientes_esp_log_log_carencias <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos=NULL)))
# adding interaction term with social underdevelopment index, with weights
resumen_coeficientes_esp_carencias_log_log_weights <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos="YES")))

# adding interaction term with school funding, null weights. The omitted variable is private funding
resumen_coeficientes_esp_log_log_financiamiento <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos=NULL)))
# adding interaction term with school funding, with weights. The omitted variable is private funding
resumen_coeficientes_esp_financiamiento_log_log_weights <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos="YES")))

# adding interaction term with school shift, null weights. The omitted variable is morning shift
resumen_coeficientes_esp_log_log_turno <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos=NULL)))
# adding interaction term with school shift, with weights. The omitted variable is morning shift
resumen_coeficientes_esp_turno_log_log_weights <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos="YES")))

# MEAN_CALIF_MAT -----------------------------------------
# same procedure as spanish WITHOUT transformation and without weights
resumen_coeficientes_mat <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos=NULL)))
# same procedure as spanish  WITHOUT transformation but WITH weights
resumen_coeficientes_mat_weights <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos="YES")))

# adding interaction term with social underdevelopment index, null weights
resumen_coeficientes_mat_carencias <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos=NULL)))
# adding interaction term with social underdevelopment index, with weights
resumen_coeficientes_mat_carencias_weights <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos="YES")))

# adding interaction term with school funding, null weights. The omitted variable is private funding
resumen_coeficientes_mat_financiamiento <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos=NULL)))
# adding interaction term with school funding, with weights. The omitted variable is private funding
resumen_coeficientes_mat_financiamiento_weights <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos="YES")))

# adding interaction term with school shift, null weights. The omitted variable is morning shift
resumen_coeficientes_mat_turno <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos=NULL)))
# adding interaction term with school shift, with weights. The omitted variable is morning shift
resumen_coeficientes_mat_turno_weights <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos="YES")))

# MEAN_CALIF_MAT LOG-LOG -----------------------------------------

# Running the regressions for the data with log log  transformation and NULL weights for all combination of crimes
resumen_coeficientes_mat_log_log <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos=NULL)))
# Running the regressions for the data with log log transformation and WITH weights. Weights are pondered by the number of alumni in that school in terms of others from the same level.
resumen_coeficientes_mat_log_log_weights <- bind_rows(lapply(combinaciones_incidentes, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos="YES")))

# adding interaction term with social underdevelopment index, null weights
resumen_coeficientes_mat_log_log_carencias <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos=NULL)))
# adding interaction term with social underdevelopment index, with weights
resumen_coeficientes_mat_carencias_log_log_weights <- bind_rows(lapply(interacciones_carencias, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos="YES")))

# adding interaction term with school funding, null weights. The omitted variable is private funding
resumen_coeficientes_mat_log_log_financiamiento <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos=NULL)))
# adding interaction term with school funding, with weights. The omitted variable is private funding
resumen_coeficientes_mat_financiamiento_log_log_weights <- bind_rows(lapply(interacciones_financiamiento, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos="YES")))

# adding interaction term with school shift, null weights. The omitted variable is morning shift
resumen_coeficientes_mat_log_log_turno <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos=NULL)))
# adding interaction term with school shift, with weights. The omitted variable is morning shift
resumen_coeficientes_mat_turno_log_log_weights <- bind_rows(lapply(interacciones_turno, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos="YES")))
