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
names(CASILLAS[["Ayuntamiento_2010"]]) <- c("DISTRITO","MUNICIPIO","SECCION","TIPO","PAN","PRI","PRD","PT",
                                               "PEBC","VOTO_NULO","TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION",
                                               "ABSTENCION")
names(CASILLAS[["Diputados_2010"]]) <- c("DISTRITO","MUNICIPIO","SECCION","TIPO","PAN","PRI","PRD","PT",
                                               "PEBC","VOTO_NULO","TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION",
                                               "ABSTENCION")

colnames(CASILLAS[["Ayuntamiento_2013"]])
colnames(CASILLAS[["Diputados_2013"]])
colnames(CASILLAS[["Gubernatura_2013"]])
names(CASILLAS[["Ayuntamiento_2013"]]) <- c("MUNICIPIO", "DISTRITO","SECCION","TIPO",
                                            "PAN","PRI","MC","NO_REGISTRADOS","VOTO_NULO"
                                            ,"TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")
names(CASILLAS[["Diputados_2013"]]) <- c("MUNICIPIO", "DISTRITO","SECCION","TIPO",
                                            "PAN","PRI","MC","NO_REGISTRADOS","VOTO_NULO"
                                            ,"TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")
names(CASILLAS[["Gubernatura_2013"]]) <- c("MUNICIPIO", "DISTRITO","SECCION","TIPO",
                                              "PAN","PRI","MC","NO_REGISTRADOS","VOTO_NULO"
                                              ,"TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

colnames(CASILLAS[["Ayuntamiento_2016"]])
colnames(CASILLAS[["Diputados_2016"]])
names(CASILLAS[["Ayuntamiento_2016"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","Pre_PRI","PRD","PT",
                                            "PVEM","PBC","PANAL","PES","MC","MORENA","PPC","MUNICIPALISTA",
                                            "HUMANISTA","C1","C2","C3","C4","C5","C6","C7","C8","C9" ,"C10",
                                            "C11","CESAR_IVAN","GASTON_LUKEN","CAROLINA_AUBANEL",
                                            "JESUS_ALFREDO","OMAR_GARCIA","JUAN_CARLOS","FRANCISCO_DEL_CASTILLO",
                                            "JOSE_LUIS_MAR", "NO_REGISTRADOS","VOTO_NULO",
                                            "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

names(CASILLAS[["Diputados_2016"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","Pre_PRI","PRD","PT",
                                               "PVEM","PBC","PANAL","PES","MC","MORENA","PPC","MUNICIPALISTA",
                                               "HUMANISTA","C1","C2","C3","C4","C5","C6","C7","C8","C9" ,"C10",
                                               "C11","DAYLIN_GARCIA","CORDELIA_CASAS","BLANCA_ESPERANZA",
                                               "RUBEN.FERNANDEZ","ERWIN_JORGE","ADOLFO_CALLETTE","LUIS_HILARIO",
                                               "NO_REGISTRADOS","VOTO_NULO",
                                               "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

colnames(CASILLAS[["Ayuntamiento_2019"]])
colnames(CASILLAS[["Diputados_2019"]])
colnames(CASILLAS[["Gubernatura_2019"]])
names(CASILLAS[["Ayuntamiento_2019"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","PRI","PRD",
                                               "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                               "C4","C5","C6","C7","C8","C9","C10","C11","Alfredo_Moreno",
                                               "Kevin_Fernando", "Gustavo_Flores","Rogelio_Castro",
                                               "NO_REGISTRADOS","VOTO_NULO",
                                               "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

names(CASILLAS[["Diputados_2019"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","PRI","PRD",
                                               "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                               "C4","C5","C6","C7","C8","C9","C10","C11","Fernanda_Angelica",
                                               "Tadeo_Javier.", "NO_REGISTRADOS","VOTO_NULO",
                                               "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

names(CASILLAS[["Gubernatura_2019"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","PRI","PRD",
                                            "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                            "C4","C5","C6","C7","C8","C9","C10","C11",
                                            "NO_REGISTRADOS","VOTO_NULO",
                                            "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

colnames(CASILLAS[["Ayuntamiento_2021"]])
colnames(CASILLAS[["Diputados_2021"]])
colnames(CASILLAS[["Gubernatura_2021"]])
names(CASILLAS[["Ayuntamiento_2021"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","PRI","PRD",
                                               "PT","PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM",
                                               "Rogelio_Castro","Marco_Antonio.","Cesar_Ivan","Celso_Arturo",
                                               "Luis_Fernando","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                               "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                               "NO_REGISTRADOS","VOTO_NULO","TOTAL_VOTOS","LISTA_NOMINAL",
                                               "PARTICIPACION","ABSTENCION")

names(CASILLAS[["Diputados_2021"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","PRI","PRD","PT",
                                            "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","Jose_Antonio",
                                            "Ramiro_Orea","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                            "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                            "NO_REGISTRADOS","VOTO_NULO","TOTAL_VOTOS","LISTA_NOMINAL",
                                            "PARTICIPACION","ABSTENCION" )
names(CASILLAS[["Gubernatura_2021"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","PAN","PRI","PRD","PT",
                                            "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","PAN_PRI_PRD","PAN_PRI",
                                            "PAN_PRD","PRI_PRD","PT_PVEM_MORENA","PT_PVEM","PT_MORENA",
                                            "PVEM_MORENA","NO_REGISTRADOS","VOTO_NULO","TOTAL_VOTOS",
                                            "LISTA_NOMINAL","PARTICIPACION","ABSTENCION")


# ====================================================================================================
# ====================================================================================================


# SECCIONs are identify by a number however they are meaningless for numeric operations,
# for standardization sake we add 0's to SECCION and make them all a four digit number 

CASILLAS <- lapply(CASILLAS, transform, SECCION = 
                     gsub("\\s", "0", format(SECCION,  justify = c("right"), width = 4)))

# Transform PARTICIPACION and ABSTENCION from char to numeric and take % out
CASILLAS <- lapply(CASILLAS, transform, PARTICIPACION = as.numeric(gsub("%", "",PARTICIPACION)))
CASILLAS <- lapply(CASILLAS, transform, ABSTENCION = as.numeric(gsub("%", "",ABSTENCION)))

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
# downloaded in the same way. However we are  interested in analyzing polling place = SECCION so
# we create a function to aggregate polling station results into SECCION results

groupby_summariseif <- function(x) {
  x %>%
    group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
                 summarise_if(is.double, sum)
}
SECCIONES <- lapply(CASILLAS, groupby_summariseif)

# Aggregated results for PARTICIPACION and ABSTENCION aren't the actual values, we recalculate them
part_abst <- function(x) {
  x %>%
    mutate(PARTICIPACION = (TOTAL_VOTOS / LISTA_NOMINAL) * 100) %>%
    mutate(ABSTENCION = (100 - PARTICIPACION))
}
SECCIONES <- lapply(SECCIONES, part_abst)

remove(groupby_summariseif, part_abst)


# ====================================================================================================
# ====================================================================================================


# Since we are interested in alliances and leading parties, we delete party alliance combination 

colnames(HISTORICOS[["Ayuntamiento"]])
review <- colnames(HISTORICOS[["Ayuntamiento"]][c(21:28, 35:36, 47:57, 68:69, 84:94, 109:110)])
HISTORICOS[["Ayuntamiento"]][(review)] <- list(NULL)
colnames(HISTORICOS[["Ayuntamiento"]])

colnames(HISTORICOS[["Diputados"]])
review <- colnames(HISTORICOS[["Diputados"]][c(18:25, 32:33, 44:54, 63:64, 79:89, 103:104, 114:115)])
HISTORICOS[["Diputados"]][(review)] <- list(NULL)
colnames(HISTORICOS[["Diputados"]])

colnames(HISTORICOS[["Gubernatura"]])
review <- colnames(HISTORICOS[["Gubernatura"]][c(16:23, 30:31, 42:52, 59:60)])
HISTORICOS[["Gubernatura"]][(review)] <- list(NULL)
colnames(HISTORICOS[["Gubernatura"]])


# ====================================================================================================
# ====================================================================================================


# We are interested in a Historical data frame that shows election results for each goverment level
# For that we create historic funcion

historic <- function(base, level) {
  # Temporarily helper table SECCIONES_year
  SECCIONES_year <- base
  
  # Add year to every column given election year
  for( i in names(SECCIONES_year)){
    past_names <- colnames(SECCIONES_year[[i]][4:ncol(SECCIONES_year[[i]])])
    year <- (substr(i, nchar(i)-4, nchar(i)))
    
    new_names <- paste(past_names, year, sep="")
    
    colnames(SECCIONES_year[[i]])[4:ncol(SECCIONES_year[[i]])] <- c(new_names)
  }
  
  # Join data frames at specified government level
  datos <- SECCIONES_year[grepl(level, names(SECCIONES_year))]
  datos <- datos %>%
    reduce(full_join, by = "SECCION")
  
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

# AQUI
# AQUI
# AQUI
# AQUI
# AQUI
# AQUI

# remove "review" helper variable and ".x" from MUNICIPIO, DISTRITO and SECCION columns
remove(review)
HISTORICOS <- lapply(HISTORICOS, function(x) {
  colnames(x) <- gsub("[.x]", "", colnames(x))
  return(x)
})


# ====================================================================================================
# ====================================================================================================

# winners create a historic table of winner party and the number of votes it got by seccion
winners <- function(base, level) {
  # Temporarily helper table SECCIONES_year
  SECCIONES_year <- base
  df_list <- list()
  
  # Add year to every column given election year
  for( i in names(SECCIONES_year)){
    year <- (substr(i, nchar(i)-4, nchar(i)))
    election_year <- paste("Ganador", year, sep="")
    votes_year <- paste("Votos", year, sep="")
    
    # Take geo information and votes
    geo <- SECCIONES_year[[i]][,1:3]
    votes <- SECCIONES_year[[i]][,4:(ncol(SECCIONES_year[[i]])-6)] 
    
    # Select winner party and number of votes (max)
    winner_party <- colnames(votes)[max.col(votes,ties.method = ("first"))]
    party_votes <- apply(votes, 1, max)
    
    nombres <- c("MUNICIPIO", "DISTRITO", "SECCION", election_year, votes_year)
    df_list[[i]] <- data.frame(geo, winner_party, party_votes)
    colnames(df_list[[i]]) <- nombres
  }
  
  # Join data frames at specified government level
  datos <- df_list[grepl(level, names(df_list))]
  datos <- datos %>%
    reduce(full_join, by = "SECCION") %>%
    select(-ends_with(".y"),-ends_with(".x.x"), -ends_with(".yy"))
  
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

HISTORICOS_GANADORES <- lapply(HISTORICOS_GANADORES, function(x) {
  colnames(x) <- gsub("[.x]", "", colnames(x))
  return(x)
})

# Previously we used a full_join to unite election by section, however, every election the of section 

# Create COMPLETE data frame list, data frames keep previous row number,set them NULL to reset
HISTORICOS_GANADORES_COMPLETE <- lapply(HISTORICOS_GANADORES, na.omit)
for (i in seq_along(HISTORICOS_GANADORES_COMPLETE)){
  rownames(HISTORICOS_GANADORES_COMPLETE[[i]]) <- NULL
  HISTORICOS_GANADORES_COMPLETE[[i]][,c(ncol(HISTORICOS_GANADORES_COMPLETE[[i]])-3,
                                        (ncol(HISTORICOS_GANADORES_COMPLETE[[i]])-2))] <- NULL
}

# ====================================================================================================
# ====================================================================================================

reparto <- function(base) {
  # Take winners historical table
  df <- base[,c(seq(from=4, to=ncol(base), by = 2))]
  # Obtain number of occurrence (victories) each party has
  occurrence <- apply(df,MARGIN=1,table)
  # Transform table list into a data frame and delete index column
  occurrence <- dcast(melt(occurrence), L1~Var1, fill=0)
  occurrence$L1 <- NULL

  # Find party with most victories and number of victories
  party <- colnames(occurrence)[max.col(occurrence,ties.method = ("first"))]
  victories <- apply(occurrence, 1, max)
  party_victories <- data.frame(party, occurrence) 
  
  # Classify secciones based on x party victories
  party_victories <- party_victories %>% 
    mutate(Tendencia = case_when(victories == 5 ~ paste("Dominio", party, sep=" ") ,
                               victories == 3 | victories == 4 ~ paste("Tendencia", party, sep=" "),
                               victories <= 3 ~ "Alternancia",
  ))
  
  df <- data.frame(base[,1:3], party_victories$Tendencia)
  colnames(df) <- c("MUNICIPIO", "DISTRITO", "SECCION", "TENDENCIA")
  return(df)
}

GANADORES_tendencia <- (lapply(HISTORICOS_GANADORES_COMPLETE, reparto))
view(GANADORES_tendencia$Ayuntamiento)
table(GANADORES_tendencia$Ayuntamiento$TENDENCIA)
