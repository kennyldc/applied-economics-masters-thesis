# Script for cleaning the data for my master's thesis
# Cleaning the data for a school level anaylisis.

# check if libraries are installed, then load
pacman::p_load(tidyverse, foreign, skimr, janitor)

# Util functions for cleaning the planea database ---------------------------

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
                       TRUE ~ 9))
}

# It will be useful in some cases to change the options that represents the number of products that the student have to only a dicotonomos variable (has or not)
# In this function I collapse the letters to a dichotonomous factor (as a dummy)
colapsa_multilevel_a_dummy <- function(x){
  fct_collapse(x ,"1" = c("B", "C", "D", "E"), "0" = "A")
}

variables_a_transformar_dummy <- function(data){
  map_dfc(data, \(x) {
    un <- unique(x[!is.na(x)])
    length(un)}) |> 
  pivot_longer(cols = everything(), names_to = "variables") |>
  filter(value == 2) |>
  pull(variables)
}

convierte_a_dummy <- function(x){
  fct_recode(x, "1" = "A", "0" = "B")
}

# Dictionary of the Social Context questionnaire database  --------------------------------------------------
# Unfortunately, the social context information came to my without a dictionary of the questions and the name of the variables.
# However I made one stored here: https://docs.google.com/spreadsheets/d/1Rt9O_AwCZwJbrXTVV_TpBauMwPNQtkap/edit?usp=sharing&ouid=112127317397935540277&rtpof=true&sd=true
# In that dictionary I identified which questions were in each of the years of the available data.

# Reading the panel with information of crimes commited around each school in CDMX from 2016 to 2019  --------------------------------------------------------
# Reading the file from Google Drive
id <- "1dH27RW6-Ly05_UAnmfDTmmfhuAeD5iJ2"
panel_log <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
panel_log %<>% arrange(id_unico)
# changing variable names to uppercase
names(panel_log) <- toupper(names(panel_log))
# changing some variables from class character to class factor
panel_log <- panel_log |>
  mutate(across(c(FINANCIAMIENTO, NIVEL, CASO, CLAVE_TURNO), as.factor))

# Results of the Planea test for CDMX's schools in 2016 ---------------------------------------------
# This year contains test results for school level 6 and school level 9
res16 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2016/pb16_alumnos.dbf", as.is = TRUE) |>
  filter(ENT == "09")

res16 <- limpia(res16) |>
  # adding "nivel" column that exists in years 2017-2019 data
  mutate(NIVEL = ifelse(GRADO == 3, "secundaria", "primaria")) |>
  select(1:GRADO, NIVEL, GRUPO:last_col())

# In year 2016, SEP did not make the Social Context questionnaire 

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

# Skim (with my personalized skim) to get a summary of the database without the id and the assistance variable
resumen_cues17 <- my_skim(cues17 |> select(-NOFOLIO, -ASISTENCIA))
# The mean of complete rate of each column is above 91.8 %

# Without the people that did not take the test, the mean completion rate of each variable is 99.51%
resumen_cues17_pres <- my_skim(cues17 |> filter(ASISTENCIA == "PRES") |> select(-NOFOLIO, -ASISTENCIA))

# After making the dictionary (see line 90) I realized that the following variables need some adjustments.

wola <- cues17 |> mutate(across(c(P_03, P_10), ~ factor_a_numerica(.)),
                          across(P_38:P_40, ~ colapsa_multilevel_a_dummy(.)),
                         across(variables_a_transformar_dummy(cues17 |> select(-1, -2)), ~ convierte_a_dummy(.)))

nom <- resumen_cues17 |> filter(factor.n_unique == 2)


wola <- cues17 |> mutate(across(variables_a_transformar_dummy(cues17 |> select(-1, -2)), ~ convierte_a_dummy(.)))

#ME FALTA CREAR LA QUE PASA DE BINARIA A B a DUMMY

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

# There is a problem in the labeling of some answers. 
# Particularly, there are some ">" symbols which appears to be in the case where the student did not answer.
# Collapsing ">" into the same level as NA
cues18 <- cues18 |> mutate(across(6:last_col(), ~ fct_collapse(.,NULL = c(">", NA))))

# The questionnaire of 2018 were 2 set of questions taken in 2 days.
# The first set of questions was applied the day of the spanish test and the other the day of the math test
tabyl(cues18, EV1) # completion rate for the first day (s equals take the test, n did not took take the test)
tabyl(cues18, EV2) # the second day
tabyl(cues18, EV1, EV2) # the students in the database took the test at least one day

# Getting a summary of the complete data
resumen_cues18 <- my_skim(cues18 |> select(-NOFOLIO, -CCT, -TURNO, -EV1, -EV2))

# Taking a closer look to the questions, for this project the more important questions were made in the second day.
# The questions of the second day are comparable with the ones from other years.
# In that case, it is important to acknowledge that the missing number of students for the second day is 7847 of 100356 (7.8%)
cues18 |> filter(EV2 =="N") |> count()

# Leaving out from the analysis the people that not came the most important day 
resumen_cues18_pres <- my_skim(cues18 |> filter(EV2 =="S") |> select(-NOFOLIO, -CCT, -TURNO, -EV1, -EV2))
# The mean of completion rate of each column is 97.7 %

# Junior High Schools 2019 SECUNDARIAS 2019 --------------------------------------------------------
# Grades Database (Planea Results)
res19 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2019/pb2019_alumnos.DBF") |>
  filter(ENT == "09")

# This year the indicator variable used to signal that a student did not took the test (spanish and math) was coded differently (S and N)
# Fixing that variable to keep format from previous years
res19 <- res19 |>
  mutate(PRESEN_ESP = if_else(PRESEN_ESP == "S", 1,0),
         PRESEN_MAT = if_else(PRESEN_MAT == "S", 1,0))

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

# Collapsing ">" into the same level as NA
cues19 <- cues19 |> mutate(across(4:last_col(), ~ fct_collapse(.,NULL = c(">", NA))))

# All Students that are in the database have at least one answer in the social questionnaire. 
resumen_cues19 <- my_skim(cues19 |> select(-NOFOLIO, -CCT, -TURNO))


