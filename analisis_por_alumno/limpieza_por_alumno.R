# Script for cleaning the data for my master's thesis
# Cleaning the data for a student level anaylisis.

# check if libraries are installed, then load
pacman::p_load(tidyverse, foreign, googlesheets4, skimr, janitor, fastDummies, tidyselect)

# Util functions for cleaning the planea database ---------------------------

`%notin%` <- Negate(`%in%`)

# The grades databases for each year have (almost) the same format which makes the work easier and suitable for functions

variables_a_caracteres <-  function(resultados_año){
  # change some columns to class character
  resultados_año <- resultados_año |>
    mutate(across(c(NOFOLIO, ENT, ENTIDAD, CCT, NOMBRE_CT, NOMBRE_MUN, LOCALIDAD,
                    NOMBRE_LOC), as.character))
  return(resultados_año)
}

normaliza_calificaciones <- function(resultados_año){
  # converts all grades from "niveles de logro" (SEP's classification) to a variable which ranges from 0 to 1 
  resultados_año <- resultados_año |> mutate(
    # converts the column NVL to character to make tranformations
    NVL_ESP = as.character(NVL_ESP),
    NVL_MAT = as.character(NVL_MAT),
    # NVL variable has roman numbers, changing to ordinal numbers
    CALIF_ESP = case_when(NVL_ESP == "I" ~ 1,
                          NVL_ESP == "II" ~ 2,
                          NVL_ESP == "III" ~ 3,
                          TRUE ~ 4),
    # when the student does not answer the test the grades variable (calif) should be NA (not I, as coded in the original database)
    CALIF_ESP = as.integer(as.character(if_else(PRESEN_ESP == 0, "NA", as.character(CALIF_ESP)))),
    # scale the variable to take a range from 0 to 1
    CALIF_ESP = (CALIF_ESP-1)/3,
    # create a flag "NP" which later will be a level into the factor variable nvl in cases where the student did not took the test
    NVL_ESP = (if_else(PRESEN_ESP == 0, "NP", NVL_ESP)),
    # change NVL to factor (as it should be)
    NVL_ESP = as.factor(NVL_ESP),
    # Making same changes to MATH GRADES (MAT)
    CALIF_MAT = case_when(NVL_MAT == "I" ~ 1,
                          NVL_MAT == "II" ~ 2,
                          NVL_MAT == "III"~ 3,
                          TRUE ~ 4),
    CALIF_MAT = as.integer(as.character(if_else(PRESEN_MAT == 0, "NA", as.character(CALIF_MAT)))),
    CALIF_MAT = (CALIF_MAT-1)/3,
    NVL_MAT = (if_else(PRESEN_MAT == 0, "NP", NVL_MAT)),
    NVL_MAT = as.factor(NVL_MAT))
  return(resultados_año)
}

homologa_turnos <- function(resultados_año){
  # homologate the school shifts to only codify "morning shift" (matutino) and "afternoon shift" (vespertino)
  resultados_año <- resultados_año |> mutate(TURNO = as.character(TURNO),
                                             TURNO_BASE = case_when(TURNO == 2 ~ 2,
                                                                    TURNO == 3 ~ 2,
                                                                    TRUE ~ 1),
                                             N_TURNO_BASE = ifelse(TURNO_BASE == 1, "MATUTINO", "VESPERTINO"),
                                             TURNO = as.factor(TURNO)) |>
    # change order of columns
    select(NOFOLIO, CCT, NOMBRE_CT, TURNO_BASE, N_TURNO_BASE, TURNO:last_col()) |>
    # rename variables to avoid confusion
    rename(TURNO_VIEJO = TURNO) %>% rename(N_TURNO_VIEJO = N_TURNO)
  return(resultados_año)
}

crea_id_unico <- function(resultados_año){
  # create an id (id_unico) using the school code (CCT) and the "new" shift
  resultados_año <- resultados_año |> mutate(ID_UNICO = paste0(CCT,"-",TURNO_BASE)) |>
    # change order of columns
  select(1:2, ID_UNICO, 3:last_col())
  return(resultados_año)
}

limpia <- function(data){
  # a function that arranges all the cleaning functions
  data |> variables_a_caracteres() |>
    homologa_turnos() |>
    crea_id_unico() |>
    normaliza_calificaciones()
}

escuela_con_crimen <- function(year){
  nuevo_panel |> filter(AÑO == year) |> pull(ID_UNICO)
}

# Util functions for cleaning the social context database ---------------------------

# we can get really quick summaries of the data by using the skimr package
# let first create a new skimmer (i.e get a new stastic) with all the counts for each level in a factor variable
my_skim <- skim_with(factor = sfl(all_counts = \(x, max_char = 3) {
  counts <- sorted_count(x)
  count_names <- substr(names(counts), 1, max_char)
  paste0(count_names, ": ", counts, collapse = ", ")
}))

# the social context questionnaire is labeled with letters, in some cases the letter represents a number that could be changed to a continuous variable
factor_a_numerica <- function(x){
  as.numeric(case_when(x == "A" ~ 1,
                       x == "B" ~ 2,
                       x == "C" ~ 3,
                       x == "D" ~ 4,
                       x == "E" ~ 5,
                       x == "F" ~ 6,
                       x == "G" ~ 7,
                       x == "H" ~ 8,
                       x == "I" ~ 9,
                       TRUE ~ NA_real_))
}

# It will be useful in some cases to change the options that represents the number of products that the student have
# to only a dichotonomous variable (has or not). In this function I collapse the letters to a dichotonomous factor (as a dummy)
colapsa_multilevel_a_dummy <- function(x){
  x <- fct_collapse(x ,"1" = c("B", "C", "D", "E"), "0" = "A")
  x <- as.numeric(levels(x))[x]
}

# This function pulls from the data the name of the variables that only have to answers (yes or no) in order to create a dummy
variables_a_transformar_dummy <- function(data){
  map_dfc(data, \(x) {
    un <- unique(x[!is.na(x)])
    length(un)}) |> 
  pivot_longer(cols = everything(), names_to = "variables") |>
  filter(value == 2) |>
  pull(variables)
}

# This functions transforms the two class level factor into a dummy
convierte_a_dummy <- function(x){
  x <- fct_recode(x, "1" = "A", "0" = "B")
  x <- as.numeric(levels(x))[x]
}
# This functions collapses multilevel factor variables that should be counted as two class level (if has certain feature or not) and then transforms it into a dummy
colapsa_multilevel_a_dummy <- function(x){
  x <- fct_collapse(x ,"1" = c("B", "C", "D", "E"), "0" = "A")
  x <- as.numeric(levels(x))[x]
}

numeros_a_letras <- function(x){
  fct_collapse(x , A = "1", B = "2", C = "3", D = "4", E = "5", "F" = "6", G = "7", H = "8", I = "9")
}

#### Functions for alternative analysis with academic section ---------
pregunta_preescolar <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "Años de preescolar") |> pull({{cuestionario_year}})
}

pregunta_nivel_a_estudiar <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "¿Hasta qué nivel cree estudiar?") |> pull({{cuestionario_year}})
}

escolaridad_a_estudiar <- function(x){
  as.numeric(case_when(x == "A" ~ 9,
                       x == "B" ~ 12,
                       x == "C" ~ 16,
                       x == "D" ~ 18,
                       TRUE ~ NA_real_))
}

pregunta_variable_continua <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "Número de personas que viven en el hogar" | pregunta == "Número de cuartos que se usan para dormir") |> 
    pull({{cuestionario_year}})
}

pregunta_nivel_estudios_padres <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "Estudios del papá" | pregunta == "Estudios de la mamá") |> 
    pull({{cuestionario_year}})
}

dummy_estudios_universitarios <- function(x){
  x <- fct_collapse(x ,"1" = c("G"), "0" = c("A", "B", "C", "D", "E", "F", "H", "I"))
  x <- as.numeric(levels(x))[x]
}

pregunta_tiempo_actividades_no_escolares <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "Tiempo a los quehaceres familiares" |
                              pregunta == "Tiempo de ayuda a familiares en trabajo o negocio" |
                              pregunta == "Tiempo a trabajo (no familia)") |> 
    pull({{cuestionario_year}})
}

actividades_no_escolares_continua <- function(x){
  as.numeric(case_when(x == "A" ~ 0,
                       x == "B" ~ 1,
                       x == "C" ~ 3,
                       x == "D" ~ 5,
                       x == "E" ~ 7,
                       TRUE ~ NA_real_))
}

pregunta_focos <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "Número de focos") |> pull({{cuestionario_year}})
}

focos_continua <- function(x){
  as.numeric(case_when(x == "A" ~ 0,
                       x == "B" ~ 5,
                       x == "C" ~ 10,
                       x == "D" ~ 15,
                       x == "E" ~ 20,
                       TRUE ~ NA_real_))
}

pregunta_fuente_cocinar <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "¿Qué utilizan para cocinar?") |> pull({{cuestionario_year}})
}

dummy_fuente_cocina <- function(x){
  x <- fct_collapse(x ,"1" = c("A", "B"), "0" = c("C", "D"))
  x <- as.numeric(levels(x))[x]
}

pregunta_libros <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "¿Cuántos libros además de los escolares?") |> pull({{cuestionario_year}})
}

libros_continua <- function(x){
  as.numeric(case_when(x == "A" ~ 0,
                       x == "B" ~ 25,
                       x == "C" ~ 50,
                       x == "D" ~ 100,
                       x == "E" ~ 150,
                       TRUE ~ NA_real_))
}

pregunta_libros_leidos <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "¿Cuántos libros a leído en los últimos 10 meses?") |> pull({{cuestionario_year}})
}

libros_leidos_continua <- function(x){
  as.numeric(case_when(x == "A" ~ 0,
                       x == "B" ~ 2,
                       x == "C" ~ 4,
                       x == "D" ~ 6,
                       TRUE ~ NA_real_))
}

pregunta_uso_internet <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "¿Qué tan seguido usa Internet para tareas?") |> pull({{cuestionario_year}})
}

dummy_uso_internet <- function(x){
  x <- fct_collapse(x ,"1" = c("A", "B", "C"), "0" = c("D"))
  x <- as.numeric(levels(x))[x]
}

preguntas_cambio_sentido <- function(cuestionario_year){
  preguntas_todos |> filter(pregunta == "Frecuencia con que olvida hacer la tarea" | 
                              pregunta == "Frecuencia con que le desagrada hacer tarea" |
                              pregunta == "Frecuencia con que se distrae haciendo tarea") |> pull({{cuestionario_year}})
}

frecuencia_cambio_sentido <- function(x){
  x <- fct_collapse(x ,"1" = c("A", "B"), "0" = c("C", "D"))
  x <- as.numeric(levels(x))[x]
}

preguntas_frecuencia <- function(cuestionario_year){
  vector <- preguntas_todos |> filter(str_detect(pregunta, "Frecuencia")) |> pull({{cuestionario_year}})
  vector_cambio_sentido <- preguntas_todos |> filter(pregunta == "Frecuencia con que olvida hacer la tarea" | 
                                                       pregunta == "Frecuencia con que le desagrada hacer tarea" |
                                                       pregunta == "Frecuencia con que se distrae haciendo tarea") |> pull({{cuestionario_year}})
  vector <- vector [! vector %in% vector_cambio_sentido]
  return(vector)
}

frecuencia_si_no <- function(x){
  x <- fct_collapse(x ,"1" = c("D", "C"), "0" = c("A", "B"))
  x <- as.numeric(levels(x))[x]
}

preguntas_frecuencia_maestros <- function(cuestionario_year){
  preguntas_todos |> filter(str_detect(pregunta, "Frecuencia maestros")) |> pull({{cuestionario_year}})
}

# Function to merge the social context data with the test results and the number of crimes around the schoool.
# Because the format is basically the same for each year, I can use only one function.

junta_resultados_crimen<- function(year){
  if(year==2017){
    cuestionario <- cues17
    resultados <- res17
    internal_year <- 2017
    first_question <- deparse(substitute(P_03)) # the first column that corresponds to a question in the database
    last_question <- deparse(substitute(P_77)) # the last column that corresponds to a question
  }
  else{
    if(year==2018){
      cuestionario <- cues18
      resultados <- res18
      internal_year <- 2018
      first_question <- deparse(substitute(BP03))
      last_question <- deparse(substitute(BP77))
    }
    else{
      if(year==2019){
        cuestionario <- cues19
        resultados <- res19
        internal_year <- 2019
        first_question <- deparse(substitute(R01))
        last_question <- deparse(substitute(R58))
      }
      else{stop("Este año no está incluido en las base")}
    }
  }
  cuestionario |> left_join(resultados |> select(NOFOLIO:N_TURNO_BASE, NIVEL, MUNICIPIO:NOMBRE_LOC, NVL_ESP:last_col())) |>
    left_join(nuevo_panel |> filter(AÑO==internal_year) |> select(ID_UNICO, FINANCIAMIENTO, INDICE_REZ, N_PRESENTARON:last_col())) |>
    select(NOFOLIO, ASISTENCIA, CCT, ID_UNICO, NOMBRE_CT, TURNO_BASE, N_TURNO_BASE, NIVEL, FINANCIAMIENTO, INDICE_REZ, MUNICIPIO, 
           NOMBRE_MUN, LOCALIDAD, NOMBRE_LOC, NVL_ESP, NVL_MAT, PRESEN_ESP:CALIF_MAT, N_PRESENTARON, PORCENTAJE_DEL_TOTAL_ALUMNOS, 
           MEAN_CALIF_ESP, MEAN_CALIF_MAT, all_of(first_question):all_of(last_question), INC_TIPO1_D250_T90H:INC_TIPO3_D1000_T10H) |>
    rename(MEAN_CALIF_ESP_ESCUELA = MEAN_CALIF_ESP, MEAN_CALIF_MAT_ESCUELA = MEAN_CALIF_MAT, ASISTENCIA_CONTEXTO = ASISTENCIA) |>
    filter(!is.na(INC_D250_T10H))
}

# Dictionary of the Social Context questionnaire database  --------------------------------------------------
# Unfortunately, the social context information came to my without a dictionary of the questions and the name of the variables.
# However I made one stored here: https://docs.google.com/spreadsheets/d/1-f3QSQ8YRVnQfU0xf522z3KrRQX_POR_RS65WnoBE0g/edit?usp=sharing
# In that dictionary I identified which questions were in each of the years of the available data.
# The dictionary is not intended to  make data analysis, however I will bring into R the question and the code (variable for each year)
diccionario <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1-f3QSQ8YRVnQfU0xf522z3KrRQX_POR_RS65WnoBE0g/edit?usp=sharing")
# Getting all the questions that were made at least one year
diccionario <- diccionario |> select(pregunta, categoria, cues17, cues18, cues19) |> remove_empty()
# For the sake of comparability in each year of the analysis I will only use questions that were made every year.
preguntas_todos <- diccionario |> drop_na()
# The following questions are cancelled from the analysis because are poorly designed
preguntas_todos <- preguntas_todos |> filter((pregunta != "¿En la escuela le dan clases en la lengua indígena?") & 
                                               (pregunta != "Frecuencia falta tiempo para terminar tarea"))

# Reading the panel with information of crimes committed around each school in CDMX from 2016 to 2019  --------------------------------------------------------
# Reading the file from Google Drive
id_file <- "1Ax2Ucq91VcGqvaeF43u2KGF0hC9OFPkS"
nuevo_panel <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id_file))
# This panel is the one already processed and used for the school analysis.
# For the sake of comparability I will use the same set of schools
# Recalling from the analysis at the school level:
# In this panel I have information of 4252 schools: 5920 elementary, 3826 junior high
# each school has at least observations for 2 years.

# changing some variables from class character to class factor
nuevo_panel <- nuevo_panel |>
  mutate(across(c(TURNO_BASE, N_TURNO_BASE, GRADO, NIVEL, FINANCIAMIENTO), as.factor))

# Results of the Planea test for CDMX's schools in 2016 ---------------------------------------------
# This year contains test results for school level 6 and school level 9
res16 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2016/pb16_alumnos.dbf", as.is = TRUE) |>
  filter(ENT == "09")

res16 <- limpia(res16) |>
  # adding "nivel" column that exists in years 2017-2019 data
  mutate(NIVEL = ifelse(GRADO == 3, "secundaria", "primaria")) |>
  select(1:GRADO, NIVEL, GRUPO:last_col())

# Filter results to only include schools with crime info
res16 <- res16 |> filter(ID_UNICO %in% escuela_con_crimen(2016))

# In year 2016, SEP did not make the Social Context questionnaire 

# Merging only the results of the test with the number of crime around the school
caracteristicas_2016 <- res16 |> left_join(nuevo_panel |> filter(AÑO == 2016) |> select(ID_UNICO, FINANCIAMIENTO, INDICE_REZ, N_PRESENTARON:last_col())) |>
  select(NOFOLIO:N_TURNO_BASE, NIVEL, FINANCIAMIENTO, INDICE_REZ, MUNICIPIO:NOMBRE_LOC, NVL_ESP:CALIF_MAT, 
       N_PRESENTARON:MEAN_CALIF_MAT, INC_TIPO1_D250_T90H:INC_TIPO3_D1000_T10H) |> 
  rename(MEAN_CALIF_ESP_ESCUELA = MEAN_CALIF_ESP, MEAN_CALIF_MAT_ESCUELA = MEAN_CALIF_MAT)

# Junior High Schools 2017 SECUNDARIAS 2017 --------------------------------------------------------
# Grades Database (Planea Results)
res17 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2017/alumnosPB17.DBF") |>
  filter(ENT == "09")

# cleaning the grades database (see functions above)
res17 <- limpia(res17)

# Social context database
cues17 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2017/CUEST_ALUM/peb_2017_cuest_alu_nal.DBF")

# Filter to only get the information of CDMX
cues17 <- cues17 |>
  mutate(ENTIDAD = substr(NOFOLIO, 1,2)) |>
  filter(ENTIDAD == "09") |> select(-ENTIDAD)

# 5065 of 65635 students (7.7%) did not took the social context questionnaire, so I do not have information about them
cues17 |> filter(if_all(2:last_col(), is.na)) |> count()

# Create a new variable as an indicator if the student took the social context questionnaire or not
cues17 <- cues17 |> mutate(ASISTENCIA = if_else(if_all(2:last_col(), is.na), "NP", "PRES")) |>
  select(NOFOLIO, ASISTENCIA, 2:last_col())

# Filter to only have questions that were made in every year.
cues17 <- cues17 |> select(NOFOLIO, ASISTENCIA, all_of(preguntas_todos |> pull(cues17)))

# Skim (with my personalized skim) to get a summary of the database without the id and the assistance variable
resumen_cues17 <- my_skim(cues17 |> select(-NOFOLIO, -ASISTENCIA))
# The mean of complete rate of each column is above 91.8 %

# Without the people that did not take the test, the mean completion rate of each variable is 99.51%
resumen_cues17_pres <- my_skim(cues17 |> filter(ASISTENCIA == "PRES") |> select(-NOFOLIO, -ASISTENCIA))

# After making the dictionary (see line 126) I realized that the following variables need some adjustments:
cues17 <- cues17 |> mutate(across(pregunta_variable_continua(cues17), ~ factor_a_numerica(.)),
                          across(P_38:P_40, ~ colapsa_multilevel_a_dummy(.)),
                          across(variables_a_transformar_dummy(cues17 |> select(-1, -2)), ~ convierte_a_dummy(.)),
                          across(pregunta_preescolar(cues17), ~ colapsa_multilevel_a_dummy(.)),
                          across(pregunta_nivel_a_estudiar(cues17), ~ escolaridad_a_estudiar(.)),
                          across(pregunta_nivel_estudios_padres(cues17), ~ dummy_estudios_universitarios(.)),
                          across(pregunta_tiempo_actividades_no_escolares(cues17), ~ actividades_no_escolares_continua(.)),
                          across(pregunta_focos(cues17), ~ focos_continua(.)),
                          across(pregunta_fuente_cocinar(cues17), ~ dummy_fuente_cocina(.)),
                          across(pregunta_libros(cues17), ~ libros_continua(.)),
                          across(pregunta_libros_leidos(cues17), ~ libros_leidos_continua(.)),
                          across(pregunta_uso_internet(cues17), ~ dummy_uso_internet(.)),
                          across(preguntas_cambio_sentido(cues17), ~ frecuencia_cambio_sentido(.)),
                          across(preguntas_frecuencia(cues17), ~ frecuencia_si_no(.)))

# Merging the social context information with the test results and the number of crimes around the school for that year
# I only consider schools that have information of crime
caracteristicas_2017 <- junta_resultados_crimen(2017)

# Elementary Schools Primarias 2018 --------------------------------------------------------------
# Grades Database (Planea Results)
res18 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2018/pb2018_alumnos.DBF") |>
  filter(ENT == "09") |> select(1:21)

# cleaning the database (see functions above)
res18 <- limpia(res18)

# Social context database
cues18 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2018/Contex_AlumAyBNac.DBF")

# Filter to only get the information of CDMX
cues18 <- cues18 |>
  mutate(ENTIDAD = substr(NOFOLIO, 1,2)) |>
  filter(ENTIDAD == "09") |> select(-ENTIDAD)

# Taking a closer look of the questions, for this project the more important questions were made in the second day (see dictionary in line 120)
# The questions of the second day are comparable with the ones from other years. I will discard questions from the first day because are more psychology oriented.
cues18 <- cues18 |> select(NOFOLIO, CCT, TURNO, EV2, BP01:last_col())

# In fact, variable EV2 refers to assistance. Changing the name and the values of present or not to match with cues17
cues18 <- cues18 |> rename(ASISTENCIA = EV2) |>
  mutate(ASISTENCIA = as.factor(if_else(ASISTENCIA=="S", "PRES","NP")))

# The missing number of students for the second day is 7847 of 100356 (7.8%)
cues18 |> filter(ASISTENCIA =="NP") |> count()

# Filter to only have questions that were made in every year.
cues18 <- cues18 |> select(1:4, all_of(preguntas_todos |> pull(cues18)))

# There is a problem in the labeling of some answers. 
# Particularly, there are some ">" symbols which appears to be in the case where the student did not answer.
# Collapsing ">" into the same level as NA
cues18 <- cues18 |> mutate(across(5:last_col(), ~ fct_collapse(.,NULL = c(">", NA))))

# Getting a summary of the complete data to get a sense of the database
resumen_cues18 <- my_skim(cues18 |> select(-NOFOLIO, -CCT, -TURNO, -ASISTENCIA))

# In 2018 SEP instead of coding with the options in the book (letters), coded with numbers
# I change it back to letters to apply the functions made above.
cues18 <- cues18 |> mutate(across(c(BP03:BP77), ~ numeros_a_letras(.)))
# Transformation of variables
cues18 <- cues18 |> mutate(across(pregunta_variable_continua(cues18), ~ factor_a_numerica(.)),
                           across(c(BP35, BP36, BP37, BP39, BP40), ~ colapsa_multilevel_a_dummy(.)),
                           across(variables_a_transformar_dummy(cues18 |> select(-1, -2, -3, -4)), ~ convierte_a_dummy(.)),
                           across(pregunta_preescolar(cues18), ~ colapsa_multilevel_a_dummy(.)),
                           across(pregunta_nivel_a_estudiar(cues18), ~ escolaridad_a_estudiar(.)),
                           across(pregunta_nivel_estudios_padres(cues18), ~ dummy_estudios_universitarios(.)),
                           across(pregunta_tiempo_actividades_no_escolares(cues18), ~ actividades_no_escolares_continua(.)),
                           across(pregunta_focos(cues18), ~ focos_continua(.)),
                           across(pregunta_fuente_cocinar(cues18), ~ dummy_fuente_cocina(.)),
                           across(pregunta_libros(cues18), ~ libros_continua(.)),
                           across(pregunta_libros_leidos(cues18), ~ libros_leidos_continua(.)),
                           across(pregunta_uso_internet(cues18), ~ dummy_uso_internet(.)),
                           across(preguntas_cambio_sentido(cues18), ~ frecuencia_cambio_sentido(.)),
                           across(preguntas_frecuencia(cues18), ~ frecuencia_si_no(.)))

# Merging the social context information with the test results and the number of crimes around the school for that year
# I only consider schools that have information of crime
caracteristicas_2018 <- junta_resultados_crimen(2018)

caracteristicas_2018 <- caracteristicas_2018 |>
  mutate(TUTOR_DEGREE = if_else(BP11 == 1 | BP12 == 1, 1, 0), .after = MEAN_CALIF_MAT_ESCUELA) |>
  rowwise() |>
  mutate(INDEX_ESFUERZO = (sum(c_across(BP66:BP77), na.rm = T))/10, .after = TUTOR_DEGREE) |>
  mutate(INDEX_ESFUERZO = if_else(is.na(TUTOR_DEGREE), NA_real_, INDEX_ESFUERZO))

# Junior High Schools 2019 SECUNDARIAS 2019 --------------------------------------------------------
# Grades Database (Planea Results)
res19 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2019/pb2019_alumnos.DBF") |>
  filter(ENT == "09")

# This year the indicator variable used to signal that a student did not took the test (spanish and math) was coded differently (S and N)
# Fixing that variable to keep format from previous years
res19 <- res19 |>
  mutate(PRESEN_ESP = as.factor(if_else(PRESEN_ESP == "S", 1,0)),
         PRESEN_MAT = as.factor(if_else(PRESEN_MAT == "S", 1,0)))

# cleaning the database (see functions above)
res19 <- limpia(res19)

# Social context database
cues19 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2019/contexto_alum_peb2019.DBF")

# Filter to only get the information of CDMX
cues19 <- cues19 |>
  mutate(ENTIDAD = substr(NOFOLIO, 1,2)) |>
  filter(ENTIDAD == "09") |> select(-ENTIDAD)

# Changing order of columns 
cues19 <- cues19 |> select(NOFOLIO, CCT, TURNO, 2:last_col())

# Filter to only have questions that were made in every year.
cues19 <- cues19 |> select(1:3, all_of(preguntas_todos |> pull(cues19)))

# Collapsing ">" into the same level as NA
cues19 <- cues19 |> mutate(across(4:last_col(), ~ fct_collapse(.,NULL = c(">", NA))))

# All Students that are in the database have at least one answer in the social questionnaire.
cues19 <- cues19 |> mutate(ASISTENCIA="PRES", .after = TURNO)

resumen_cues19 <- my_skim(cues19 |> select(-NOFOLIO, -CCT, -TURNO, -ASISTENCIA))

# Changing it back answers coded as numbers to letters in order to apply the functions made above.
cues19 <- cues19 |> mutate(across(c(R01:last_col()), ~ numeros_a_letras(.)))

# Transformation of variables
cues19 <- cues19 |> mutate(across(pregunta_variable_continua(cues19), ~ factor_a_numerica(.)),
                           across(c(R28, R29, R30), ~ colapsa_multilevel_a_dummy(.)),
                           across(variables_a_transformar_dummy(cues19 |> select(-1, -2, -3, -4)), ~ convierte_a_dummy(.)),
                           across(pregunta_preescolar(cues19), ~ colapsa_multilevel_a_dummy(.)),
                           across(pregunta_nivel_a_estudiar(cues19), ~ escolaridad_a_estudiar(.)),
                           across(pregunta_nivel_estudios_padres(cues19), ~ dummy_estudios_universitarios(.)),
                           across(pregunta_tiempo_actividades_no_escolares(cues19), ~ actividades_no_escolares_continua(.)),
                           across(pregunta_focos(cues19), ~ focos_continua(.)),
                           across(pregunta_fuente_cocinar(cues19), ~ dummy_fuente_cocina(.)),
                           across(pregunta_libros(cues19), ~ libros_continua(.)),
                           across(pregunta_libros_leidos(cues19), ~ libros_leidos_continua(.)),
                           across(pregunta_uso_internet(cues19), ~ dummy_uso_internet(.)),
                           across(preguntas_cambio_sentido(cues19), ~ frecuencia_cambio_sentido(.)),
                           across(preguntas_frecuencia(cues19), ~ frecuencia_si_no(.)))

# Merging the social context information with the test results and the number of crimes around the school for that year
# I only consider schools that have information of crime
caracteristicas_2019 <- junta_resultados_crimen(2019)

# Creating a panel with students of each year -----------------------------

# Now that I have the results from the exam and the social, economic and academic characteristics of every student I will transform the data
# into a new panel in order to check whether crime affects students at the individual level.
# However, because in 2016 there is no social context questionnaire the main focus will be students from Junior High School level (with info in 2017 and 2019)

# I will start by merging 2017 and 2019 as a single panel.
caracteristicas_2017  <- caracteristicas_2017 |> mutate(YEAR = 2017, .after = NOMBRE_CT)
caracteristicas_2019 <- caracteristicas_2019 |> mutate(YEAR = 2019, .after = NOMBRE_CT)

# The only difference between the data is that in 2019 gender is labeled as sex and in 2017 level is in lowercase
caracteristicas_2017 <- caracteristicas_2017 |>
  mutate(NIVEL = fct_recode(NIVEL, "SECUNDARIA" = "secundaria"))

caracteristicas_2019 <- caracteristicas_2019 |>
  rename(GENERO = SEXO)

# The name of the variables (that indicate the social questions) will be the ones from 2017, see dictionary for more information
panel_secundarias_alumnos <- map2_df(caracteristicas_2017, caracteristicas_2019, c)

# Changing some columns to factors
panel_secundarias_alumnos <- panel_secundarias_alumnos |> 
  mutate(across(c(ASISTENCIA_CONTEXTO, YEAR, TURNO_BASE, N_TURNO_BASE), ~ as.factor(.)))

# I can only use for the panel schools that have at least two observations
# 1257 of 1292 share this characteristic 

# The 35 schools that do not have observations are removed
escuelas_sin_dos_obs <- panel_secundarias_alumnos |> 
  group_by(YEAR) |>
  summarise(escuelas = unique(ID_UNICO)) |>
  arrange(escuelas) |>
  group_by(escuelas) |>
  count() |>
  filter(n==1) |> pull(escuelas)

# Final panel is set to
panel_secundarias_alumnos <- panel_secundarias_alumnos |> 
  filter(ID_UNICO %notin% escuelas_sin_dos_obs)

# In order to have the panel complete for the model, let just add two more variables.
# The first indicates if one of the two parents have at least a college degree.
# This might be helpful in order to account for single parents
paste(panel_secundarias_alumnos |> select(P_66:P_77) |> names(), collapse = ",")

panel_secundarias_alumnos <- panel_secundarias_alumnos |>
  mutate(TUTOR_DEGREE = if_else(P_11 == 1 | P_12 == 1, 1, 0), .after = MEAN_CALIF_MAT_ESCUELA) |>
  rowwise() |>
  mutate(INDEX_ESFUERZO = (sum(c_across(P_66:P_77), na.rm = T))/10, .after = TUTOR_DEGREE) |>
  mutate(INDEX_ESFUERZO = if_else(is.na(TUTOR_DEGREE), NA_real_, INDEX_ESFUERZO))

# For completeness, in the case of elementary schools a panel is only possible with the results
# and no other context variable

# Recall that caracts2016 has elementary and junior high students. 
# Junior high 2016 is dropped

caracteristicas_2016 <- caracteristicas_2016 |> filter(NIVEL == "primaria") |>
  mutate(NIVEL = toupper(NIVEL))

# Caracts2016 has variables not matched in caracteristicas18
caracteristicas_2016 <- caracteristicas_2016 |> select(-COPIA)

# Year column 
caracteristicas_2016 <- caracteristicas_2016 |> mutate(YEAR = 2016, .after = NOMBRE_CT)

# Setting 2018
match_2018 <- caracteristicas_2018 |>
  mutate(NIVEL = toupper(NIVEL)) |>
  mutate(YEAR = 2018, .after = NOMBRE_CT) |>
  select(all_of(names(caracteristicas_2016)))

# Panel elementary schools (only results and crime)
panel_primarias_alumnos <- map2_df(caracteristicas_2016, match_2018, c)

# Changing some columns to factors
panel_primarias_alumnos <- panel_primarias_alumnos |> 
  mutate(across(c(YEAR, TURNO_BASE, N_TURNO_BASE), ~ as.factor(.)))

# I can only use for the panel schools that have at least two observations
# 2856 of 2960 share this characteristic 

# The 104 schools that do not have observations are removed
escuelas_sin_dos_obs_prim <- panel_primarias_alumnos |> 
  group_by(YEAR) |>
  summarise(escuelas = unique(ID_UNICO)) |>
  group_by(escuelas) |>
  count() |>
  filter(n==1) |> pull(escuelas)

# Final panel is set to
panel_primarias_alumnos <- panel_primarias_alumnos |> 
  filter(ID_UNICO %notin% escuelas_sin_dos_obs_prim)

# Saving the sources for the model
# Saving panel junior high and elementary

# saveRDS(panel_secundarias_alumnos, "panel_secundarias_alumnos.rds")
# saveRDS(panel_primarias_alumnos, "panel_primarias_alumnos.rds")

# Also save caracteristicas2018 that have the info from the social questionnarie for primarias
# saveRDS(caracteristicas_2018, "caracteristicas_2018.rds")
