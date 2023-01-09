library(data.table)
library(tidyverse)
library(tidyselect)
library(reshape2)
library(plotKML)
library(gtools)
library(dplyr)
library(sf)

setwd("C:/Users/rmartinez/Desktop/Elecciones BC/Resultados Electorales")

# Our data source is Electoral Institute of Baja California: ieebc.mx/resultados-electorales
# Data was published in .xlsx format with multiple sheets and was restructured into singles csv.

# Files were stored in a single folder. We retrieve all file names in .csv format, read files and
# named them accordingly 
my_files <- list.files(pattern = "\\.csv$")
CASILLAS <- lapply(my_files, read.csv)
names(CASILLAS) <- gsub("\\.csv$", "", my_files)

# Use rev as we want to go from the most recent results to the oldest
CASILLAS <- rev(CASILLAS)
remove(my_files)


# ====================================================================================================
# ====================================================================================================


# Since are from different years there are several difference in how the data was published and so
# our first step is to standardized data.

# We star at the first column make all Distritos have the same format in arabic numbers
unique(unlist(lapply(CASILLAS, `[[`, "DISTRITO")))

CASILLAS <- lapply(CASILLAS, transform, DISTRITO = (gsub("\\*", "",DISTRITO)))
CASILLAS <- lapply(CASILLAS, transform, DISTRITO = (gsub(" ", "",DISTRITO)))

arabigos_romanos <- function(x) {
  x %>% mutate(DISTRITO = case_when(DISTRITO == "I" ~ "1",
                                    DISTRITO == "II" ~ "2",
                                    DISTRITO == "III" ~ "3",
                                    DISTRITO == "IV" ~ "4",
                                    DISTRITO == "V" ~ "5",
                                    DISTRITO == "VI" ~ "6",
                                    DISTRITO == "VII" ~ "7",
                                    DISTRITO == "VIII" ~ "8",
                                    DISTRITO == "IX" ~ "9",
                                    DISTRITO == "X" ~ "10",
                                    DISTRITO == "XI" ~ "11",
                                    DISTRITO == "XII" ~ "12",
                                    DISTRITO == "XIII" ~ "13",
                                    DISTRITO == "XIV" ~ "14",
                                    DISTRITO == "XV" ~ "15",
                                    DISTRITO == "XVI" ~ "16",
                                    TRUE ~ as.character(DISTRITO)
                                    ))
}
CASILLAS <- lapply(CASILLAS, arabigos_romanos)
remove(arabigos_romanos)

# Name all municipalities the same way, there's an use of an ID Number and discrepancies in Rosarito
unique(unlist(lapply(CASILLAS, `[[`, "MUNICIPIO")))

municipalities_name <- function(x) {
  x %>% mutate(MUNICIPIO = case_when(MUNICIPIO == 1 ~ "ENSENADA",
                                     MUNICIPIO == 2 ~ "MEXICALI",
                                     MUNICIPIO == 3 ~ "TECATE",
                                     MUNICIPIO == 4 ~ "TIJUANA",
                                     MUNICIPIO == 5 ~ "ROSARITO",
                                     MUNICIPIO == "P DE ROSARITO" ~ "ROSARITO",
                                     MUNICIPIO == "P. ROSARITO" ~ "ROSARITO",
                                     MUNICIPIO == "P. DE ROSARITO" ~ "ROSARITO",
                                TRUE ~ as.character(MUNICIPIO)
    ))
}
CASILLAS <- lapply(CASILLAS, municipalities_name)
remove(municipalities_name)

# To compare past with previous results, the next step is to assigned same name to all common variables.
# Parties that lead an alliance will have a "Pre_" added to them, later on will be explained why
colnames(CASILLAS[["Ayuntamiento_2010"]])
colnames(CASILLAS[["Diputados_2010"]])
names(CASILLAS[["Ayuntamiento_2010"]]) <- c("Distrito","Municipio","Seccion","Tipo","PAN","PRI","PRD","PT",
                                               "PEBC","Voto_Nulo","Total_Votos","Lista_Nominal","Participacion",
                                               "Abstencion")
names(CASILLAS[["Diputados_2010"]]) <- c("Distrito","Municipio","Seccion","Tipo","PAN","PRI","PRD","PT",
                                               "PEBC","Voto_Nulo","Total_Votos","Lista_Nominal","Participacion",
                                               "Abstencion")

colnames(CASILLAS[["Ayuntamiento_2013"]])
colnames(CASILLAS[["Diputados_2013"]])
colnames(CASILLAS[["Gubernatura_2013"]])
names(CASILLAS[["Ayuntamiento_2013"]]) <- c("Municipio", "Distrito","Seccion","Tipo",
                                            "PAN","PRI","MC","NO_REGISTRADOS","Voto_Nulo"
                                            ,"Total_Votos","Lista_Nominal","Participacion","Abstencion")
names(CASILLAS[["Diputados_2013"]]) <- c("Municipio", "Distrito","Seccion","Tipo",
                                            "PAN","PRI","MC","NO_REGISTRADOS","Voto_Nulo"
                                            ,"Total_Votos","Lista_Nominal","Participacion","Abstencion")
names(CASILLAS[["Gubernatura_2013"]]) <- c("Municipio", "Distrito","Seccion","Tipo",
                                              "PAN","PRI","MC","NO_REGISTRADOS","Voto_Nulo"
                                              ,"Total_Votos","Lista_Nominal","Participacion","Abstencion")

colnames(CASILLAS[["Ayuntamiento_2016"]])
colnames(CASILLAS[["Diputados_2016"]])
names(CASILLAS[["Ayuntamiento_2016"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","Pre_PRI","PRD","PT",
                                            "PVEM","PBC","PANAL","PES","MC","Morena","PPC","MUNICIPALISTA",
                                            "HUMANISTA","C1","C2","C3","C4","C5","C6","C7","C8","C9" ,"C10",
                                            "C11","CESAR_IVAN","GASTON_LUKEN","CAROLINA_AUBANEL",
                                            "JESUS_ALFREDO","OMAR_GARCIA","JUAN_CARLOS","FRANCISCO_DEL_CASTILLO",
                                            "JOSE_LUIS_MAR", "NO_REGISTRADOS","Voto_Nulo",
                                            "Total_Votos","Lista_Nominal","Participacion","Abstencion")

names(CASILLAS[["Diputados_2016"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","Pre_PRI","PRD","PT",
                                               "PVEM","PBC","PANAL","PES","MC","Morena","PPC","MUNICIPALISTA",
                                               "HUMANISTA","C1","C2","C3","C4","C5","C6","C7","C8","C9" ,"C10",
                                               "C11","DAYLIN_GARCIA","CORDELIA_CASAS","BLANCA_ESPERANZA",
                                               "RUBEN_FERNANDEZ","ERWIN_JORGE","ADOLFO_CALETTE","LUIS_HILARIO",
                                               "NO_REGISTRADOS","Voto_Nulo",
                                               "Total_Votos","Lista_Nominal","Participacion","Abstencion")

colnames(CASILLAS[["Ayuntamiento_2019"]])
colnames(CASILLAS[["Diputados_2019"]])
colnames(CASILLAS[["Gubernatura_2019"]])
names(CASILLAS[["Ayuntamiento_2019"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                               "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                               "C4","C5","C6","C7","C8","C9","C10","C11","Alfredo_Moreno",
                                               "Kevin_Fernando", "Gustavo_Flores","Rogelio_Castro",
                                               "No_Registrados","Voto_Nulo",
                                               "Total_Votos","Lista_Nominal","Participacion","Abstencion")

names(CASILLAS[["Diputados_2019"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                               "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                               "C4","C5","C6","C7","C8","C9","C10","C11","Fernanda_Angelica",
                                               "Tadeo_Javier", "No_Registrados","Voto_Nulo",
                                               "Total_Votos","Lista_Nominal","Participacion","Abstencion")

names(CASILLAS[["Gubernatura_2019"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                            "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                            "C4","C5","C6","C7","C8","C9","C10","C11",
                                            "No_Registrados","Voto_Nulo",
                                            "Total_Votos","Lista_Nominal","Participacion","Abstencion")

colnames(CASILLAS[["Ayuntamiento_2021"]])
colnames(CASILLAS[["Diputados_2021"]])
colnames(CASILLAS[["Gubernatura_2021"]])
names(CASILLAS[["Ayuntamiento_2021"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                               "PT","PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM",
                                               "Rogelio_Castro","Marco_Antonio","Cesar_Ivan","Celso_Arturo",
                                               "Luis_Fernando","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                               "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                               "NO_REGISTRADOS","Voto_Nulo","Total_Votos","Lista_Nominal",
                                               "Participacion","Abstencion")

names(CASILLAS[["Diputados_2021"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD","PT",
                                            "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","Jose_Antonio",
                                            "Ramiro_Orea","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                            "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                            "NO_REGISTRADOS","Voto_Nulo","Total_Votos","Lista_Nominal",
                                            "Participacion","Abstencion" )
names(CASILLAS[["Gubernatura_2021"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD","PT",
                                            "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","PAN_PRI_PRD","PAN_PRI",
                                            "PAN_PRD","PRI_PRD","PT_PVEM_MORENA","PT_PVEM","PT_MORENA",
                                            "PVEM_MORENA","NO_REGISTRADOS","Voto_Nulo","Total_Votos",
                                            "Lista_Nominal","Participacion","Abstencion")


# ====================================================================================================
# ====================================================================================================


# Seccions are identify by a number however they are meaningless for numeric operations,
# for standardization sake we add 0's to Seccion and make them all a four digit number 

CASILLAS <- lapply(CASILLAS, transform, Seccion = 
                     gsub("\\s", "0", format(Seccion,  justify = c("right"), width = 4)))

# Transform Participacion and Abstencion from char to numeric and take % out
CASILLAS <- lapply(CASILLAS, transform, Participacion = as.numeric(gsub("%", "",Participacion)))
CASILLAS <- lapply(CASILLAS, transform, Abstencion = as.numeric(gsub("%", "",Abstencion)))

# Convert everything to numeric except first four columns
CASILLAS <- lapply(CASILLAS, function(x) {
  x[,5:ncol(x)] <- lapply(x[,5:ncol(x)], as.numeric)
  x})

# Replace returned NAs with 0
CASILLAS <- lapply(CASILLAS, function(x) {x[is.na(x)] <- 0;x})


# ====================================================================================================
# ====================================================================================================


# In 2010 and 2013 ballots only include stand alone parties or alliance, from 2016 onward ballots include 
# all parties and all alliance combination. Starting in 2016, we will create two variables:
# "Leading_party" = leading party + all alliance combinations
# "Pre_leading_party" = leading party as stand alone

# First create a few helper variables, we also create 2010 and 2013 since they will be handy later
CASILLASnames <- names(CASILLAS)

y2010 = CASILLASnames[endsWith(CASILLASnames, "2010")]
y2013 = CASILLASnames[endsWith(CASILLASnames, "2013")]
y2016 = CASILLASnames[endsWith(CASILLASnames, "2016")]
y2019 = CASILLASnames[endsWith(CASILLASnames, "2019")]
y2021 = CASILLASnames[endsWith(CASILLASnames, "2021")]

# We create sum_party function that creates a new variable summing specified columns
sum_party <- function(x, leading, alliance, position) {
  x %>%
    mutate(!!leading := rowSums(select(., alliance)), .before = position)
}

# In 2016 PRI led the only alliance
CASILLAS[y2016] <- lapply(CASILLAS[y2016], sum_party, 
                          leading = "PRI",
                          alliance = c("Pre_PRI", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", 
                                       "C10", "C11"),
                          position = "Pre_PRI")

# In 2019 MORENA led the only alliance 
CASILLAS[y2019] <- lapply(CASILLAS[y2019], sum_party, 
                         leading = "Morena",
                         alliance = c("Pre_MORENA", "C1", "C2", "C3", "C4", "C5","C6", "C7", "C8", "C9",
                                      "C10", "C11"),
                         position = "Pre_MORENA")

# In 2021 there were 2 alliance, one led by MORENA and a Coalision by PAN + PRI + PRD
colnames(CASILLAS[["Ayuntamiento_2021"]])
CASILLAS[y2021] <- lapply(CASILLAS[y2021], sum_party, 
                          leading = "Morena",
                          alliance = c("Pre_MORENA","PT","PVEM","PT_PVEM_MORENA", "PT_PVEM","PT_MORENA",
                                       "PVEM_MORENA"),
                          position = "Pre_MORENA")

remove(CASILLASnames, y2010, y2013, y2016, y2019, y2021)


# ====================================================================================================
# ====================================================================================================


# The first elections results were published by polling station therefore subsequent elections were
# downloaded in the same way. However we are  interested in analyzing polling place = Seccion so
# we create a function to aggregate polling station results into Seccion results

groupby_summariseif <- function(x) {
  x %>%
    group_by(Municipio, Distrito, Seccion) %>% 
                 summarise_if(is.double, sum)
}
SECCIONES <- lapply(CASILLAS, groupby_summariseif)

# Aggregated results for Participacion and Abstencion aren't the actual values, we recalculate them
part_abst <- function(x) {
  x %>%
    mutate(Participacion = (Total_Votos / Lista_Nominal) * 100) %>%
    mutate(Abstencion = (100 - Participacion))
}
SECCIONES <- lapply(SECCIONES, part_abst)

remove(groupby_summariseif, part_abst)


# ====================================================================================================
# ====================================================================================================


# Since we are interested in alliances and leading parties, we delete party alliance combination 
# Create list of unique names in data frame list, select unwanted columns and set them NULL
all_names <- unique(unlist(sapply(SECCIONES, colnames)))
all_names
SECCIONES <- lapply(SECCIONES, function(x) {
  x[c("PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PT_PVEM_MORENA", "PT_PVEM", 
      "PT_MORENA", "PVEM_MORENA", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10",
      "C11")] <- NULL; x })
remove(all_names)

# ====================================================================================================
# ====================================================================================================


# We are interested in a Historical data frame that shows election results for each government level
# For that we create historic function

historic <- function(base, level) {
  # Temporarily helper table Secciones_year
  Secciones_year <- base

  # Add year to every column given election year
  for( i in names(Secciones_year)){
    year <- (substr(i, nchar(i)-4, nchar(i)))
    Secciones_year[[i]] <- Secciones_year[[i]] %>%
      rename_with(~paste0(., year), -c("Seccion"))
  }
  
  # Join data frames at specified government level
  datos <- Secciones_year[grepl(level, names(Secciones_year))]
  datos <- datos %>%
    reduce(full_join, by = "Seccion") %>%
    select(-ends_with(".y"),-ends_with(".x.x"), -ends_with(".yy"))
  
  # Returns one data frame at time, Historical data frame needs to be create outside function
  return(datos)
}

# HISTORICAL data frame
HISTORICOS <- list(Ayuntamiento = list(),
                   Diputados = list(),
                   Gubernatura = list())

HISTORICOS[["Ayuntamiento"]] <- historic(base = SECCIONES,
                                         level = "Ayuntamiento")
HISTORICOS[["Diputados"]] <- historic(base = SECCIONES,
                                      level = "Diputados")
HISTORICOS[["Gubernatura"]] <- historic(base = SECCIONES,
                                        level = "Gubernatura")

# ====================================================================================================
# ====================================================================================================


# winners create a historic table of winner party and the number of votes it got by Seccion
winners <- function(base, level) {
  # Temporarily helper table Secciones_year
  Secciones_year <- base
  df_list <- list()

  # Add year to every column given election year
  for( i in names(Secciones_year)){
    # Substract year from each data frame
    election_year <- substr(i, nchar(i)-4, nchar(i))

    # Subset data frame to obtain geo and electoral information
    geo <- Secciones_year[[i]][,1:3]
    votes <- Secciones_year[[i]][,4:(ncol(Secciones_year[[i]])-6)] 
    
    # Select winner party and number of votes (max)
    winner_party <- colnames(votes)[max.col(votes,ties.method = ("first"))]
    party_votes <- apply(votes, 1, max)
    
    df_list[[i]] <- data.frame(geo, winner_party, party_votes)
    # colnames(df_list[[i]]) 
    df_list[[i]] <- df_list[[i]] %>% 
      `colnames<-`(c("Municipio", "Distrito", "Seccion", "Ganador", "Votos")) %>%
      rename_with(~paste0(., election_year), -c("Seccion"))
  }
  
  # Join data frames at specified government level
  datos <- df_list[grepl(level, names(df_list))]
  datos <- datos %>%
    reduce(full_join, by = "Seccion") #%>%
  
  return(datos)
}

HISTORICOS_GANADORES <- list(Ayuntamiento = list(),
                                     Diputados = list(),
                                     Gubernatura = list())

HISTORICOS_GANADORES[["Ayuntamiento"]] <- winners(base = SECCIONES,
                                                  level = "Ayuntamiento")
HISTORICOS_GANADORES[["Diputados"]] <- winners(base = SECCIONES,
                                               level = "Diputados")
HISTORICOS_GANADORES[["Gubernatura"]] <- winners(base = SECCIONES,
                                                 level = "Gubernatura")


# ====================================================================================================
# ====================================================================================================


# Create COMPLETE data frame list. Data frames keep previous row number, set them NULL to reset
HISTORICOS_GANADORES_COMPLETE <- lapply(HISTORICOS_GANADORES, na.omit)
for (i in seq_along(HISTORICOS_GANADORES_COMPLETE)){
  i <- "Ayuntamiento"
  rownames(HISTORICOS_GANADORES_COMPLETE[[i]]) <- NULL
}


# ====================================================================================================
# ====================================================================================================


reparto <- function(base) {
  # Take winners historical table
  df <- base
  df <- df[grepl("Ganador", names(df))] 
  # Obtain number of occurrence (victories) each party has
  occurrence <- apply(df,MARGIN=1,table)
  # Transform table list into a data frame and delete index column
  occurrence <- dcast(melt(occurrence), L1~Var1, fill=0)
  occurrence$L1 <- NULL

  # Find party with most victories and number of victories
  party <- colnames(occurrence)[max.col(occurrence,ties.method = ("first"))]
  victories <- apply(occurrence, 1, max)
  party_victories <- data.frame(party, occurrence) 
  
  # Classify Secciones based on x party victories
  party_victories <- party_victories %>% 
    mutate(Tendencia = case_when(victories == 5 ~ paste("Dominio", party, sep=" ") ,
                               victories == 3 | victories == 4 ~ paste("Tendencia", party, sep=" "),
                               victories <= 3 ~ "Alternancia",
  ))
  
  df <- data.frame(base[,1:3], party_victories$Tendencia)
  colnames(df) <- c("Municipio", "Distrito", "Seccion", "Tendencia")
  return(df)
}

GANADORES_tendencia <- (lapply(HISTORICOS_GANADORES_COMPLETE, reparto))

remove(i)

# ====================================================================================================
# ====================================================================================================


estadisticos <- function(base, group, variables) {
  #
  base <- base %>%
    group_by(...) %>%
    ungroup() %>%
    select(matches(variables))
  
  base = melt(base)
  
  # La funcion de moda no existe la defiimos
  Modes <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
  }
  
  # Calculamos estadisticos de participación
  estadisticos <- base %>% 
    group_by_at(group) %>%   
    summarise(across(everything(), 
                     mean,
                     Modes
                     ))
  
  estadisticos <- Filter(function(x)!all(is.na(x)), estadisticos)
  return(estadisticos)
}

HISTORICOS_COMPLETE <- lapply(HISTORICOS, na.omit)
estadisticos_grles <- lapply(HISTORICOS_COMPLETE, estadisticos,
                             group = "Distrito_2021")

view(estadisticos_grles$Diputados)


# ====================================================================================================
# ====================================================================================================


# In www.plataformadetransparencia.org.mx we found a KML map from the 2013 State Election,
# we will convert it to SHP and add our working columns to plot maps as we prefer 

setwd("C:/Users/rmartinez/Desktop/Elecciones BC/Secciones")
Secciones <- st_read("Secciones.shp")
