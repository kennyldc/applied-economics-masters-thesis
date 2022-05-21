# Script for cleaning the data for my master's thesis
# Cleaning the data for a school level anaylisis.

# check if libraries are installed, then load
pacman::p_load(magrittr, tidyverse, foreign, janitor)

# Reading the panel with information of crimes commited around each school in CDMX from 2016 to 2019 (more info in my repo) --------------------------------------------------------
# Reading the file from Google Drive
id <- "1dH27RW6-Ly05_UAnmfDTmmfhuAeD5iJ2"
panel_log <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
panel_log %<>% arrange(id_unico)
# changing variable names to uppercase
names(panel_log) <- toupper(names(panel_log))
# changing some variables from class character to class factor
panel_log <- panel_log |>
  mutate(across(c(FINANCIAMIENTO, NIVEL, CASO, CLAVE_TURNO), as.factor))

# Util functions for cleaning the data ---------------------------

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
    select(CCT, NOMBRE_CT, TURNO_BASE, N_TURNO_BASE, TURNO:last_col()) |>
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

alumnos_por_escuela_prop <- function(data){
  # calculate the number of students per school that took the test and the proportion they represent in terms of the CDMX total 
  data |> group_by(ID_UNICO) |> tabyl(ID_UNICO) |>
    rename(N_PRESENTARON = n) |> rename(PORCENTAJE_DEL_TOTAL_ALUMNOS = percent)
}

califs_año_por_escuela <- function(data){
  # calculate the mean grade per school for a determined database (i.e. year)
  data |> group_by(ID_UNICO) |>
    summarise(MEAN_CALIF_ESP = mean(CALIF_ESP, na.rm = TRUE),
              MEAN_CALIF_MAT = mean(CALIF_MAT, na.rm = TRUE))
}

no_estan <- function(resultados_año, año){
  # shows the schools that are in the grades database (planea) and not in the crime database
  anti_join(resultados_año |> group_by(ID_UNICO) |> count(), 
                      panel_log |> filter(YEAR == año) |> 
                        rename(CCT = CLAVE_ESC))
}

promedios_año_con_pesos <- function(resultados_año){
  califs_año <- left_join(alumnos_por_escuela_prop(resultados_año), califs_año_por_escuela(resultados_año))
  resumen <- tibble(PROMEDIO_ESP = mean(califs_año$MEAN_CALIF_ESP, na.rm = T),
                    PROMEDIO_MAT = mean(califs_año$MEAN_CALIF_MAT, na.rm = T),
                    # weighted.mean calculates the weighted mean using the proportion of students previously defined
                    PROMEDIO_W_ESP = weighted.mean(califs_año$MEAN_CALIF_ESP, califs_año$PORCENTAJE_DEL_TOTAL_ALUMNOS, na.rm = TRUE),
                    PROMEDIO_W_MAT = weighted.mean(califs_año$MEAN_CALIF_MAT, califs_año$PORCENTAJE_DEL_TOTAL_ALUMNOS, na.rm = TRUE))
  return(resumen)
}

base_info_x_escuela <- function(resultados_año, año){
  # I took only one row per school and it becomes my observation unit
  base <- distinct(resultados_año, ID_UNICO,.keep_all = TRUE) |>
    # Adding a column with the year to keep control from the databases
    # change the column "modalidad" to keep only two clasess "private schools" (privada) and "public schools" (publico)
    mutate(AÑO = año,
           MODALIDAD = ifelse(MODALIDAD == "Privada" | MODALIDAD == "PARTICULAR", "PRIVADO", "PUBLICO")) |>
    # merge the grades per school and the proportion info (weights) calculated with the total of students
    left_join(left_join(alumnos_por_escuela_prop(resultados_año), califs_año_por_escuela(resultados_año))) |>
    # join with the crime data for each year
    left_join(panel_log |> filter(YEAR == año) |> select(-NIVEL)) |>
    # keeping the name of every school with the nomenclature of the crime base (when available)
    # keeping the funding variable with the nomenclature of the crime base (when available)
    mutate(NOMBRE_ESCUELA = coalesce(NOMBRE_ESCUELA, NOMBRE_CT),
           FINANCIAMIENTO = coalesce(FINANCIAMIENTO, MODALIDAD)) |>
    # select relevant features
    select(CCT, NOMBRE_ESCUELA, ID_UNICO, TURNO_BASE, N_TURNO_BASE, GRADO, NIVEL, FINANCIAMIENTO, CASO, LOCALIDAD, NOMBRE_LOC, INDICE_REZ, ALCALDIA, AÑO, N_PRESENTARON:MEAN_CALIF_MAT, INC_TIPO1_D250_T90H:INC_TIPO3_D1000_T10H)
}

# Results of the Planea test for CDMX's schools in 2016 ---------------------------------------------
# This year contains test results for school level 6 and school level 9
res16 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2016/pb16_alumnos.dbf") |>
  filter(ENT == "09")

# cleaning the database and adding relevant columns with the functions

res16 <- res16 |> 
  # converts some columns to class character
  variables_a_caracteres() |>
  # homologate school shifts to keep only 2
  homologa_turnos() |> 
  # creates id with CCT and school shift
  crea_id_unico() |>
  # scale the grades (test results) variable to take a range from 0 to 1
  normaliza_calificaciones() |>
  # adding "nivel" column that exists in years 2017-2019 data
  mutate(NIVEL = ifelse(GRADO == 3, "secundaria", "primaria")) |>
  select(CCT:GRADO, NIVEL, GRUPO:last_col())

# an alternative is to use
# res16 <- limpia(res16)

# Elementary schools 2016 PRIMARIAS 2016 ----------------------------------------------------------
# Filtering the grades (test results) database to keep level 6
prim16 <- res16 |> filter(GRADO == 6)

# filtering the crime database to keep level 6
delitos_esc_16_prim  <- panel_log |> filter(NIVEL == "PRIMARIA" & YEAR == 2016)

# ANALISIS POR ESCUELAS - ANALYSIS AT THE SCHOOL LEVEL

# Looking which schools are in the grades (test results) database but not in the crime database
no_estan_prim_16 <- no_estan(prim16, 2016)

# To calculate school means
# Obtaining the mean grades per school in both sections of the exam
califs_prim_16 <- califs_año_por_escuela(prim16)
# For a weighted average I calculate the number of students that took the test in each school and the proportion they represent in terms of the total 
alumnos_x_prim_16 <- alumnos_por_escuela_prop(prim16)
# merging grades and weights (proportion of students)
califs_prim_16 <- left_join(alumnos_x_prim_16, califs_prim_16) 
# comparing the simple mean with the weighted mean using weighted.mean in a single tibble
promedios_prim_16 <- promedios_año_con_pesos(prim16)

# this calculation implies that the weighted average (adding more importance to schools with more students) decreases the mean grade (test result)

# MERGING GRADES TO CRIMES AROUND THE SCHOOL INTO A SINGLE STANDARD FORMAT
planea_escuelas_prim_16 <- base_info_x_escuela(prim16, 2016)

# Junior High Schools 2016 SECUNDARIAS 2016 --------------------------------------------------------

# Filtering the grades (test results) database to keep level 9 (3 in Mexico's system)
sec16 <- res16 |> filter(GRADO == 3)

# ANALISIS POR ESCUELAS - ANALYSIS AT THE SCHOOL LEVEL

# Looking which schools are in the grades database but not in the crime database
no_estan_sec_16 <- no_estan(sec16, 2016)
nrow(no_estan_sec_16)

# comparing the simple mean with the weighted mean using weighted.mean in a single tibble
promedios_sec_16 <- promedios_año_con_pesos(sec16)
# this calculation implies that the weighted average (adding more importance to schools with more students) decreases the mean grade (test result)

# MERGING GRADES TO CRIMES AROUND THE SCHOOL INTO A SINGLE STANDARD FORMAT
planea_escuelas_sec_16 <- base_info_x_escuela(sec16, 2016)

# Junior High Schools 2017 SECUNDARIAS 2017 --------------------------------------------------------
# Grades Database (Planea Results)
res17 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2017/alumnosPB17.DBF") |>
  filter(ENT == "09")

# cleaning the database (see functions above)
res17 <- limpia(res17)

# ANALISIS POR ESCUELAS - ANALYSIS AT THE SCHOOL LEVEL
# Looking which schools are in the grades database but not in the crime database
no_estan_17 <- no_estan(res17, 2017)
nrow(no_estan_17)

# comparing the simple mean with the weighted mean using weighted.mean in a single tibble
promedios_17 <- promedios_año_con_pesos(res17)
# this calculation implies that the weighted average (adding more importance to schools with more students) decreases the mean grade. It is a less pronounced effect than that of 2016

# MERGING GRADES TO CRIMES AROUND THE SCHOOL INTO A SINGLE STANDARD FORMAT
planea_escuelas_17 <- base_info_x_escuela(res17, 2017)

# Elementary Schools Primarias 2018 --------------------------------------------------------------
# Grades Database (Planea Results)
res18 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2018/pb2018_alumnos.DBF") |>
  filter(ENT == "09") |> select(1:21)

# cleaning the database (see functions above)
res18 <- limpia(res18)

# Looking which schools are in the grades database but not in the crime database
no_estan_18 <- no_estan(res18, 2018)
nrow(no_estan_18)

# comparing the simple mean with the weighted mean using weighted.mean in a single tibble
promedios_18 <- promedios_año_con_pesos(res18)

# MERGING GRADES TO CRIMES AROUND THE SCHOOL INTO A SINGLE STANDARD FORMAT
planea_escuelas_18 <- base_info_x_escuela(res18, 2018)

# Junior High Schools 2019 SECUNDARIAS 2019 --------------------------------------------------------
# Grades Database (Planea Results)
res19 <- read.dbf("/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/solic_itam_2019/pb2019_alumnos.DBF") |>
  filter(ENT == "09")

# This year the indicator variable used to signal that a student did not took the test was coded differently (S and N)
# Fixing that variable to keep format from previous years

res19 %<>%
  mutate(PRESEN_ESP = if_else(PRESEN_ESP == "S", 1,0),
         PRESEN_MAT = if_else(PRESEN_MAT == "S", 1,0))

# cleaning the database (see functions above)
res19 <- limpia(res19)

# Looking which schools are in the grades database but not in the crime database
no_estan_19 <- no_estan(res19, 2019)
no_estan_19 |> nrow()

# comparing the simple mean with the weighted mean using weighted.mean in a single tibble
promedios_19 <- promedios_año_con_pesos(res19)

# MERGING GRADES TO CRIMES AROUND THE SCHOOL INTO A SINGLE STANDARD FORMAT
planea_escuelas_19 <- base_info_x_escuela(res19, 2019)

# Getting data together ---------------------------------------

# With the databases constructed above for each year, I create a new panel

nuevo_panel <- rbind(planea_escuelas_prim_16, planea_escuelas_sec_16, planea_escuelas_17, planea_escuelas_18, planea_escuelas_19) |> arrange(ID_UNICO)

# As noted above, some schools were in the grades databases but not in the crime database (i.e. no estaban en la base de la SEP que usé para extraer a mano los valores de las coordenadas)
# Looking closer to the number of NA's, it seems to be a minor problem
nuevo_panel |>  filter(is.na(CASO)) |> count()

# However, I replace the values with NA (particularly the number of crimes) with the mean of the neighborhood (localidad) for each year

nuevo_panel <- nuevo_panel |> 
  group_by(LOCALIDAD, AÑO) |> 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) |>
  ungroup()

# with this solution, only 10 out of 10233 schools do not have the number of crimes commited around the school
nuevo_panel |> filter(is.na(INC_D250_T90H)) |> count()

# Removing those rows from the panel
nuevo_panel <- nuevo_panel |> filter(!is.na(INC_D250_T90H))

# Writing the new panel into a csv
readr::write_excel_csv(nuevo_panel,"/Users/carloslopezdelacerda/Documents/tesis_eco_aplicada/nuevo_panel.csv")
