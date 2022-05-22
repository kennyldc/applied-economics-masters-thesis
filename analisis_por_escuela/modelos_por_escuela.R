# Script for running some regresion models for my master's thesis

# check if libraries are installed, then load
pacman::p_load(tidyverse, foreign, janitor, lfe)

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

# Estimates
# MEAN_CALIF_ESP -----------------------------------------

# Making each regression (without weights) for all combinations of righ hand coefficient terms (including interactions)
# The results are stored in a list (of dataframes) with the summaries of the estimate for each coefficient, the type of crime, 
# the number of days and the distance, the standard deviation, p value and adj R2
# To make the code less obscure, the first lapply iterates the making of the dataframe for each right hand coefficient term combination
# specified in the terminos_coef_posibles list (that is why the output has 4 daframes) with the use of an anonymous function,
# (the syntax \(x) is a new feature from R 4.0)
# The map_df produces the dataframe by iterating each coefficient from the combinations made above (see line 52 to 58) into
# the reg_felm function that produces a summary specyfing the data and if the weights (pesos) are required.
estimaciones_esp_sin_pesos <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos=NULL))
})

# Now repeating the process of running all regressions but for regressions that INCLUDE weights.
# Weights ponder the number of alumni in that school in terms of other schools from the same level.
estimaciones_esp_pesos <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel, pesos="YES"))
})

# MEAN_CALIF_ESP LOG-LOG Transformation -----------------------------------------
# Because the interpretation of the coefficient estimates is more or less difficult AND because the number of crimes are close to 0,
# I decide to run the same regressions but making a log transformation for both y and x 

estimaciones_esp_sin_pesos_log_log <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos=NULL))
})

estimaciones_esp_pesos_log_log <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_ESP", x, nuevo_panel_log_log, pesos="YES"))
})

# MEAN_CALIF_MAT -----------------------------------------
# Same procedures as in the spanish section
# regression without tranforming the data and null weights
estimaciones_mat_sin_pesos <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos=NULL))
})

# MATH regressions with weights
estimaciones_mat_pesos <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel, pesos="YES"))
})

# MEAN_CALIF_MAT LOG-LOG Transformation -----------------------------------------
# I decide to run the same regressions but making a log transformation for both y and x 

estimaciones_mat_sin_pesos_log_log <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos=NULL))
})

estimaciones_mat_pesos_log_log <- lapply(terminos_coef_posibles, \(x){
  map_df(x, \(x) reg_felm("MEAN_CALIF_MAT", x, nuevo_panel_log_log, pesos="YES"))
})
