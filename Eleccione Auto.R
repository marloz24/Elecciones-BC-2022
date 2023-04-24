library(data.table)
library(tidyverse)
library(tidyselect)
library(reshape2)
library(plotKML)
library(gtools)
library(dplyr)
library(sf)

setwd("C:/Users/marti/OneDrive/Escritorio/Elecciones BC")

# Data source is the Electoral Institute of Baja California: ieebc.mx/resultados-electorales
# Data was published in .xlsx format with multiple sheets and was restructured into singles csv.

# We retrieve all file names in .csv format, read and named them accordingly
my_files <- list.files(pattern = "\\.csv$")
CASILLAS <- lapply(my_files, read.csv, fileEncoding="latin1")
names(CASILLAS) <- gsub("\\.csv$", "", my_files)

# Use rev() to go from most recent results to the oldest
CASILLAS <- rev(CASILLAS)
remove(my_files)


# ====================================================================================================
# ====================================================================================================


# Since results are from different years there are several difference in how the data was published
# and so, the first step is to standardized

# Starting at the first column, making all Distritos have the same format in arabic numbers
unique(unlist(lapply(CASILLAS, `[[`, "DISTRITO")))

# Remove * and " " blank spaces
CASILLAS <- lapply(CASILLAS, transform, DISTRITO = (gsub("\\*", "",DISTRITO)))
CASILLAS <- lapply(CASILLAS, transform, DISTRITO = (gsub(" ", "",DISTRITO)))

# Rename roman to arabic numbers
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
                                    DISTRITO == "XVII" ~ "17",
                                    TRUE ~ as.character(DISTRITO)
  ))
}
CASILLAS <- lapply(CASILLAS, arabigos_romanos)
remove(arabigos_romanos)
unique(unlist(lapply(CASILLAS, `[[`, "DISTRITO")))

# Name all municipalities the same way, there's use of ID Number and discrepancies in Rosarito naming
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

# To compare results across time, the next step is to assigned same name to all common variables.
# Parties that lead an alliance will have a "Pre_" added to them, later on will be explained why

colnames(CASILLAS[["Ayuntamiento_2010"]]) == colnames(CASILLAS[["Diputados_2010"]])
colnames(CASILLAS[["Ayuntamiento_2010"]])
colnames(CASILLAS[["Diputados_2010"]])

names(CASILLAS[["Ayuntamiento_2010"]]) <- c("Distrito","Municipio","Seccion","Tipo","PAN","PRI","PRD","PT",
                                            "PEBC","Voto_Nulo","Total_Votos","Lista_Nominal","Participacion",
                                            "Abstencion")
names(CASILLAS[["Diputados_2010"]]) <- c("Distrito","Municipio","Seccion","Tipo","PAN","PRI","PRD","PT",
                                         "PEBC","Voto_Nulo","Total_Votos","Lista_Nominal","Participacion",
                                         "Abstencion")

colnames(CASILLAS[["Ayuntamiento_2013"]]) == colnames(CASILLAS[["Diputados_2013"]])
colnames(CASILLAS[["Diputados_2013"]]) == colnames(CASILLAS[["Gubernatura_2013"]])

names(CASILLAS[["Ayuntamiento_2013"]]) <- c("Municipio", "Distrito","Seccion","Tipo",
                                            "PAN","PRI","MC","No_Registrados","Voto_Nulo"
                                            ,"Total_Votos","Lista_Nominal","Participacion","Abstencion")
names(CASILLAS[["Diputados_2013"]]) <- c("Municipio", "Distrito","Seccion","Tipo",
                                         "PAN","PRI","MC","No_Registrados","Voto_Nulo"
                                         ,"Total_Votos","Lista_Nominal","Participacion","Abstencion")
names(CASILLAS[["Gubernatura_2013"]]) <- c("Municipio", "Distrito","Seccion","Tipo",
                                           "PAN","PRI","MC","No_Registrados","Voto_Nulo"
                                           ,"Total_Votos","Lista_Nominal","Participacion","Abstencion")

colnames(CASILLAS[["Ayuntamiento_2016"]]) == colnames(CASILLAS[["Diputados_2016"]])
colnames(CASILLAS[["Ayuntamiento_2016"]])
colnames(CASILLAS[["Diputados_2016"]])

names(CASILLAS[["Ayuntamiento_2016"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","Pre_PRI","PRD","PT",
                                            "PVEM","PBC","PANAL","PES","MC","Morena","PPC","MUNICIPALISTA",
                                            "HUMANISTA","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11",
                                            "CESAR_IVAN","Gaston_Luken","Carolina_Aubanel","Jesus_Alfredo",
                                            "OMAR_GARCIA","Juan_Carlos","Francisco_Del_Castillo","JOSE_LUIS_MAR",
                                            "No_Registrados","Voto_Nulo",
                                            "Total_Votos","Lista_Nominal","Participacion","Abstencion")

names(CASILLAS[["Diputados_2016"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","Pre_PRI","PRD","PT",
                                         "PVEM","PBC","PANAL","PES","MC","Morena","PPC","MUNICIPALISTA",
                                         "HUMANISTA","C1","C2","C3","C4","C5","C6","C7","C8","C9" ,"C10","C11",
                                         "DAYLIN_GARCIA","CORDELIA_CASAS","BLANCA_ESPERANZA",
                                         "RUBEN_FERNANDEZ","ERWIN_JORGE","ADOLFO_CALETTE","LUIS_HILARIO",
                                         "No_Registrados","Voto_Nulo",
                                         "Total_Votos","Lista_Nominal","Participacion","Abstencion")

colnames(CASILLAS[["Ayuntamiento_2019"]])
names(CASILLAS[["Ayuntamiento_2019"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                            "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                            "C4","C5","C6","C7","C8","C9","C10","C11","Alfredo_Moreno",
                                            "Kevin_Fernando", "Gustavo_Flores","Rogelio_Castro",
                                            "No_Registrados","Voto_Nulo",
                                            "Total_Votos","Lista_Nominal","Participacion","Abstencion")
colnames(CASILLAS[["Diputados_2019"]])
names(CASILLAS[["Diputados_2019"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                         "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                         "C4","C5","C6","C7","C8","C9","C10","C11","Fernanda_Angelica",
                                         "Tadeo_Javier", "No_Registrados","Voto_Nulo",
                                         "Total_Votos","Lista_Nominal","Participacion","Abstencion")
colnames(CASILLAS[["Gubernatura_2019"]])
names(CASILLAS[["Gubernatura_2019"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                           "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                           "C4","C5","C6","C7","C8","C9","C10","C11",
                                           "No_Registrados","Voto_Nulo",
                                           "Total_Votos","Lista_Nominal","Participacion","Abstencion")

colnames(CASILLAS[["Ayuntamiento_2021"]])
names(CASILLAS[["Ayuntamiento_2021"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD",
                                            "PT","PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM",
                                            "Rogelio_Castro","Marco_Antonio","Cesar_Ivan","Celso_Arturo",
                                            "Luis_Fernando","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                            "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                            "No_Registrados","Voto_Nulo","Total_Votos","Lista_Nominal",
                                            "Participacion","Abstencion")
colnames(CASILLAS[["Diputados_2021"]])
names(CASILLAS[["Diputados_2021"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD","PT",
                                         "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","Jose_Antonio",
                                         "Ramiro_Orea","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                         "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                         "No_Registrados","Voto_Nulo","Total_Votos","Lista_Nominal",
                                         "Participacion","Abstencion" )
colnames(CASILLAS[["Gubernatura_2021"]])
names(CASILLAS[["Gubernatura_2021"]]) <- c("Municipio","Distrito","Seccion","Tipo","PAN","PRI","PRD","PT",
                                           "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","PAN_PRI_PRD","PAN_PRI",
                                           "PAN_PRD","PRI_PRD","PT_PVEM_MORENA","PT_PVEM","PT_MORENA",
                                           "PVEM_MORENA","No_Registrados","Voto_Nulo","Total_Votos",
                                           "Lista_Nominal","Participacion","Abstencion")


# ====================================================================================================
# ====================================================================================================


# Seccions are identify by a number however they are meaningless for numeric operations,
# for standardization sake we add 0's to Seccion and make them all a four digit number 
CASILLAS <- lapply(CASILLAS, transform, Seccion = 
                     gsub("\\s", "0", format(Seccion,  justify = c("right"), width = 4)))

# Transform Participacion and Abstencion from char to numeric type and take "%" out
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


# In 2010 and 2013 ballots only included stand alone parties or alliance, from 2016 onward ballots include 
# all parties and alliance combination. Starting from 2016, we will create two variables:
# "Leading_party" = leading party + all alliance combinations
# "Pre_leading_party" = leading party as stand alone

# First we create a few helper variables, also create 2010 and 2013 since they'll be handy later
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
    # name is not leading "!!", name is supply ":=", select(all rows, columns specified)
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

CASILLAS[y2021] <- lapply(CASILLAS[y2021], sum_party, 
                          leading = "Va_por_Mexico",
                          alliance = c("PAN","PRI","PRD","PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD"),
                          position = "Morena")

remove(CASILLASnames, y2010, y2013, y2016, y2019, y2021)


# ====================================================================================================
# ====================================================================================================


# We remove "casillas especiales" for voters outside their cities, to reflect actual results.
CASILLAS <- lapply(CASILLAS, function(x){
  x[!grepl("S", x[,4]),]
})


# ====================================================================================================
# ====================================================================================================


# The first elections results were published by polling station therefore subsequent elections results
# were downloaded in the same way. However we are  interested in analyzing polling place = Seccion,
# we create a function to aggregate polling station results into Seccions results

Casillas_Secciones <- function(x) {
  x %>%
    group_by(Municipio, Distrito, Seccion) %>% 
    summarise_if(is.double, sum)
}
SECCIONES <- lapply(CASILLAS, Casillas_Secciones)

# Aggregated results for Participacion and Abstencion aren't the actual values, we recalculate them
parti_abst <- function(x) {
  x %>%
    mutate(Participacion = (Total_Votos / Lista_Nominal) * 100) %>%
    mutate(Abstencion = (100 - Participacion))
}
SECCIONES <- lapply(SECCIONES, parti_abst)

remove(Casillas_Secciones, parti_abst)


# ====================================================================================================
# ====================================================================================================


# Since we are interested in alliances and leading parties, we create a list of unique party/candidates,
# select unwanted party/candidates and set them NULL

parties_candidates <- unique(unlist(sapply(SECCIONES, colnames)))
parties_candidates
SECCIONES <- lapply(SECCIONES, function(x) {
  x[c("PAN_PRI_PRD", "PAN_PRI", "PAN_PRD", "PRI_PRD", "PT_PVEM_MORENA", "PT_PVEM", 
      "PT_MORENA", "PVEM_MORENA", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10",
      "C11")] <- NULL; x })
remove(parties_candidates)


# ====================================================================================================
# ====================================================================================================


# We define historic() function to create data frame showing election results over time
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


# Define winners_votes() function to create a historic table of winner parties and votes by Seccion

winners_votes <- function(base, level) {
  df_list <- list()
  base <- SECCIONES
  level <- "Diputados"
  i <- "Diputados_2010"
  # Add year to every column given election year
  for( i in names(base)){
    # Substract year from each data frame
    election_year <- substr(i, nchar(i)-4, nchar(i))
    
    eleccion <- base[[i]][base[[i]]["Total_Votos"]>0,]
    
    # Subset data frame to obtain SECCION and election results
    geo <- eleccion[,1:3]
    votes <- eleccion[,4:(ncol(eleccion)-6)]

    # Select winner party and number of votes (max)
    winner_party <- colnames(votes)[ max.col(votes, ties.method = ("first")) ]
    party_votes <- apply(votes, 1, max)
    
    df_list[[i]] <- data.frame(geo, winner_party, party_votes)
     
    df_list[[i]] <- df_list[[i]] %>% 
      `colnames<-`(c("Municipio", "Distrito", "Seccion", "Ganador", "Votos")) %>%
      rename_with(~paste0(., election_year), -c("Seccion"))
  }
  
  # Join data frames at specified government level
  datos <- df_list[grepl(level, names(df_list))]
  datos <- datos %>%
    reduce(full_join, by = "Seccion") #%>%
  
  datos <- datos %>% select(-contains("Municipio"))
  return(datos)
}

HISTORICOS_GANADORES <- list(Ayuntamiento = list(),
                             Diputados = list(),
                             Gubernatura = list())

HISTORICOS_GANADORES[["Ayuntamiento"]] <- winners_votes(base = SECCIONES,
                                                  level = "Ayuntamiento")
HISTORICOS_GANADORES[["Diputados"]] <- winners_votes(base = SECCIONES,
                                               level = "Diputados")
HISTORICOS_GANADORES[["Gubernatura"]] <- winners_votes(base = SECCIONES,
                                                 level = "Gubernatura")


# ====================================================================================================
# ====================================================================================================


# Reset data frame row numbers to go from 1 to n-row
HISTORICOS_GANADORES_COMPLETE <- lapply(HISTORICOS_GANADORES, na.omit)
for (i in seq_along(HISTORICOS_GANADORES_COMPLETE)){
  #i <- "Ayuntamiento"
  rownames(HISTORICOS_GANADORES_COMPLETE[[i]]) <- NULL
  remove(i)
}


# ====================================================================================================
# ====================================================================================================


# Through a small classification algorithm, tendencia() classifies secciones based on election results
tendencia <- function(base) {
  # Take winners_votes historical table
  base <- base[which(base$Distrito_2021 == "10"), ]
  
  df <- base
  df <- df[grepl("Ganador", names(df))] 

  # Obtain number of occurrence (victories) each party has
  occurrence <- apply(df, MARGIN=1, table)
  
  # Transform table list into a data frame and delete index column
  occurrence <- dcast(melt(occurrence), L1~Var1, fill=0)
  occurrence$L1 <- NULL
  
  #
  occurrence <- occurrence %>% mutate(PAN = case_when(Va_por_Mexico >= 1 ~ (PAN + Va_por_Mexico),
                                                        TRUE ~ PAN),
                                      PRI = case_when(Va_por_Mexico >= 1 ~ (PRI + Va_por_Mexico),
                                                      TRUE ~ PRI),
                                      PRD = case_when(Va_por_Mexico >= 1 ~ (PRD + Va_por_Mexico),
                                                      TRUE ~ PRD))
  
  # Find party with most victories and number of victories
  party <- colnames(occurrence)[max.col(occurrence,ties.method = ("first"))]
  victories <- apply(occurrence, 1, max)
  party_victories <- data.frame(party, occurrence) 
  
  # Classify Secciones based on x party victories
  party_victories <- party_victories %>% 
    mutate(Tendencia = case_when(victories == 5 ~ paste("Domina", party, sep=" ") ,
                                 victories == 3 | victories == 4 ~ paste("Tendencia", party, sep=" "),
                                 victories <= 3 ~ "Alternancia",
    ))
  
  df <- data.frame(base[,1:2], party_victories$Tendencia)
  colnames(df) <- c("Distrito", "Seccion", "Tendencia")
  return(df)
}

Tendencia_Voto <- (lapply(HISTORICOS_GANADORES_COMPLETE, tendencia))


# ====================================================================================================
# ====================================================================================================


# We create basic_stats() function before our analysis since will be using it quite frequently
basic_stats <- function(base, columnas) {
  #
  by = c(as.character(groups(base)))
  
  base <- base %>%
    group_by_at(by) %>%
    ungroup() %>%
    select(matches(columnas))
  
  base <- melt(base, id.vars = NULL)
  base$value <- round(base$value, 1)
  
  # Mode function does not exist, we define it
  Modes <- function(x) {
    x <- na.omit(x)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calculate basic statistics
  estadisticos <- base %>% 
    group_by_at("variable") %>%   
    summarise(Media = mean(value, na.rm = TRUE),
              Mediana = median(value, na.rm = TRUE),
              Moda = Modes(value),
              Total = sum(value, na.rm = TRUE)
    )
  
  descriptivos <- Filter(function(x)!all(is.na(x)), estadisticos)
  return(descriptivos)
}


# ====================================================================================================
# ====================================================================================================


# In www.plataformadetransparencia.org.mx we located the KML map for the 2013 State Election,
# we converted it to SHP and we'll use it to georeference data

setwd("C:/Users/marti/OneDrive/Escritorio/Elecciones BC/Mapas/Secciones")
Mapa_Secciones <- st_read("Secciones.shp")
colnames(Mapa_Secciones)[1] <- "Seccion"


# ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS
# ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS
# ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS
# ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS
# ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS ========== ANALISIS


# Return to primary folder
setwd("C:/Users/marti/OneDrive/Escritorio/Elecciones BC")

# We are interested in analyzing District 10, the first question to answer is "Who's" the District
# How many Sections it has, whats the electoral population, has this change over time?
# We are also interested in the demographics, Male, Females, Ages in other basic stats

# This is our key Dataframe, here we susbset all information for a given district
Distrito <- HISTORICOS[["Diputados"]][which(HISTORICOS$Diputados$Distrito_2021 == "10"), ]
Mapa_Secciones <- Mapa_Secciones[Mapa_Secciones$Seccion %in% Distrito$Seccion,]

# We also subset for entire city of Tijuana
Tijuana <- HISTORICOS[["Diputados"]][which(HISTORICOS$Diputados$Municipio_2021 == "TIJUANA"), ]

# City and district population, and whats the portion from city
Listado_Distrito <- sum(Distrito$Lista_Nominal_2021)
Listado_Ciudad <- sum(Tijuana$Lista_Nominal_2021, na.rm = TRUE)
Dis_Listado_Municipio <- (Listado_Distrito / Listado_Ciudad) * 100
Listado_Ciudad
Listado_Distrito
Dis_Listado_Municipio

# Seccion in city, district population and portion of city
Secciones_Distrito <- sum(!is.na(Distrito$Lista_Nominal_2021))
Secciones_Ciudad <- sum(!is.na(Tijuana$Lista_Nominal_2021), na.rm = TRUE)
Dis_Secciones_Municipio <- (Secciones_Distrito / Secciones_Ciudad) * 100
Secciones_Distrito
Dis_Secciones_Municipio

# See if Sections numbers has change over time
Num_secciones <- colSums(!is.na(Distrito))
Num_secciones <- as.data.frame(Num_secciones[grepl("^Mun",names(Num_secciones))])
Num_secciones

# See how voter registration has change over time
Listado <- select(Distrito, matches("Lista_Nominal"))
Listado_descriptivo <- as.data.frame( basic_stats(base = Listado, "Lista_Nominal") )
Listado_descriptivo

# In 2019 district delimitation changed, we check composition based on previous districts 
Composicion_seccion <- table(Distrito$Distrito_2021, Distrito$Distrito_2016, useNA = c("no"))
Composicion_seccion <- as.data.frame(prop.table(Composicion_seccion)*100)
colnames(Composicion_seccion) <- c("Distrito", "Antiguo", "Proporcion")
Composicion_seccion

# And composition in terms of electoral register population and percentage
Composicion_listado <- Distrito %>%
  group_by(Distrito$Distrito_2016) %>%
  summarise(Total = sum(Lista_Nominal_2021))
Composicion_listado[,3] <- (Composicion_listado[,2] / sum(Composicion_listado$Total, na.rm = TRUE) )*100
Composicion_listado


# ====================================================================================================
# ====================================================================================================


# Now we'll see the District's demographics
setwd("C:/Users/marti/OneDrive/Escritorio/Elecciones BC/INEGI")
INEGI <- subset(read.csv("INE_SECCION_2020.csv"), ENTIDAD == 2) # Subset to BC

# Format Secciones adding needed 0's
INEGI$SECCION <- gsub("\\s", "0", format(INEGI$SECCION ,  justify = c("right"), width = 4))

# Change to Municipilaties name from ID and rename to lowercase SECCION
unique(unlist(INEGI$MUNICIPIO))
INEGI <- municipalities_name(INEGI)

# Subset to District of interest
INEGI <- INEGI[INEGI$SECCION %in% unique(Distrito$Seccion), ]

# Create District and Seccions demographic data frame
INEGI_Pre_Secciones <- data.frame(Seccion <- INEGI$SECCION,
                                  Hombres <- INEGI$POBMAS,
                                  Mujeres <- INEGI$POBFEM,
                                  Poblacion <- Hombres + Mujeres,
                                  
                                  P0a4 <- INEGI$POBTOT - INEGI$P_5YMAS, #Goes
                                  P18a24 <- INEGI$P_18A24, #Goes
                                  P65MAS <- INEGI$POB65_MAS, #Goes
                                  
                                  P25a64 <- INEGI$POBTOT - (INEGI$P_0A17 + INEGI$P_18A24 + INEGI$POB65_MAS),
                                  P5a17 <- INEGI$POBTOT - (P0a4) - (P18a24 + P25a64 + P65MAS), #Goes
                                  
                                  Salud <- INEGI$PDER_SS,
                                  Edad_ocupada <- INEGI$P_12YMAS,
                                  Ocupada <- INEGI$POCUPADA,
                                  
                                  Edad_estudiar <- (INEGI$P_3A5 + INEGI$P_6A11 + INEGI$P_12A14 + INEGI$P_15A17),
                                  Estudian <- Edad_estudiar - (INEGI$P3A5_NOA + INEGI$P6A11_NOA + INEGI$P12A14NOA + INEGI$P15A17A),
                                  Años_Educacion <- INEGI$GRAPROES,
                                  
                                  ViviendasF <- INEGI$HOGJEF_F,
                                  Viviendas <- INEGI$TOTHOG
                                  )

# Create dataframe for overall district results
INEGI_Secciones <- INEGI_Pre_Secciones

# Rename columns
colnames(INEGI_Secciones) <- c("Seccion", "Hombres", "Mujeres", "Poblacion", "P0a4", "P18a24", "P65MAS", "P25a64", "P5a17",
                               "Salud", "Edad_ocupada", "Ocupada", "Edad_estudiar", "Estudian", "Años_Educacion",
                               "ViviendasF", "Viviendas")

# Calculate needed columns for Secciones
INEGI_Secciones$ViviendasF <- round( (INEGI_Secciones$ViviendasF / INEGI_Secciones$Viviendas) * 100,2 )
INEGI_Secciones$Estudian <- round( (INEGI_Secciones$Estudian / INEGI_Secciones$Edad_estudiar) * 100, 2)
INEGI_Secciones$Salud <- round( (INEGI_Secciones$Salud / INEGI_Secciones$Poblacion) * 100, 2)
INEGI_Secciones$Ocupada <- round( (INEGI_Secciones$Ocupada / INEGI_Secciones$Edad_ocupada) * 100, 2)

# Re-order columns
INEGI_Secciones <- INEGI_Secciones[, c("Seccion", "Hombres", "Mujeres", "Poblacion",
                                     "P0a4", "P5a17", "P18a24", "P25a64", "P65MAS",
                                     "Salud", "Edad_ocupada", "Ocupada", "Edad_estudiar", "Estudian", 
                                     "Años_Educacion", "Viviendas", "ViviendasF")]

# Calculate summarise columns for Distrito and rename columns
INEGI_Distrito <- summarise_all(INEGI_Pre_Secciones[,2:ncol(INEGI_Pre_Secciones)], sum)
colnames(INEGI_Distrito) <- c("Hombres", "Mujeres", "Poblacion", "P0a4", "P18a24", "P65MAS", "P25a64", "P5a17",
                              "Salud", "Edad_ocupada", "Ocupada", "Edad_estudiar", "Estudian", "Años_Educacion",
                              "ViviendasF", "Viviendas")

# Re-order columns
INEGI_Distrito <- INEGI_Distrito[, c("Hombres", "Mujeres", "Poblacion",
                                     "P0a4", "P5a17", "P18a24", "P25a64", "P65MAS",
                                     "Salud", "Edad_ocupada", "Ocupada", "Edad_estudiar", "Estudian", 
                                     "Años_Educacion", "Viviendas", "ViviendasF")]

# Calculate needed columns for Distrito
INEGI_Distrito[2,c(1:10,12)] <- round( (INEGI_Distrito[1,c(1:10,12)]/INEGI_Distrito$Poblacion) *100, 2)
INEGI_Distrito[2,15] <- INEGI_Distrito[1,"Viviendas"]

# Ocupada, Estudian, Vivienda y Años educación
INEGI_Distrito[2,11] <- paste(as.character( round((INEGI_Distrito[1,11]/INEGI_Distrito[1,10])*100, 2) ), "*")
INEGI_Distrito[2,13] <- paste(as.character( round((INEGI_Distrito[1,13]/INEGI_Distrito[1,12])*100, 2) ), "*")
INEGI_Distrito[2,16] <- paste(as.character( round((INEGI_Distrito[1,16]/INEGI_Distrito[1,15])*100, 2) ), "*")
INEGI_Distrito[,14] <-  paste(as.character( round(median(INEGI_Secciones$Años_Educacion),2) ), "*")
INEGI_Distrito

mean(INEGI_Secciones$Salud)


remove(Seccion, Hombres, Mujeres, Poblacion, P0a4, P18a24, P65MAS, P25a64, P5a17, Salud, Edad_ocupada, Ocupada,
       Edad_estudiar, Estudian, Años_Educacion, ViviendasF, Viviendas, INEGI_Pre_Secciones)

# We would like to map some indices, e.g women as heads of household, for that we create Mapa_INEGI
Mapa_INEGI <- merge(x = Mapa_Secciones, y = INEGI_Secciones, 
                          by = "Seccion", all.y = TRUE)

# Prepare data to graph indices
INEGI_grafica <- INEGI_Secciones[,c("Salud", "Ocupada", "ViviendasF")]
INEGI_grafica = na.omit(melt(INEGI_grafica, id.vars = NULL))

# Graph indices
ggplot(data=INEGI_grafica, aes(x=value, group=variable, fill=variable)) +
  geom_density(kernel = "gaussian", adjust=1.5, alpha=0.4) +
  labs(x = "% de población", y = "Probabilidad") + theme(plot.title = element_text(size=22)) +
  facet_wrap(~fct_rev(variable), ncol = 1) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank()) +
  scale_x_continuous(breaks = seq(0, 100, by = 10))

# Create a 3-way categorical variable for women as head of household
Mapa_INEGI$ViviendasF_Clas <- cut(x = as.numeric(Mapa_INEGI$ViviendasF),
                                  # Remember we can set 3 to use more homogeneous groups
                                  breaks = quantile(Mapa_INEGI$ViviendasF, 
                                            probs = c(seq(from = 0, to = 1, by = 1/3))),
                                  include.lowest = TRUE,
                                  labels = c("Poca", "Media", "Alta"))

# And map it
for (Categoria in unique(Mapa_INEGI$ViviendasF_Clas)) {
  mapa <- subset(Mapa_INEGI, Mapa_INEGI$ViviendasF_Clas == Categoria)
  
  plotKML(obj = mapa,
          file.name = paste(Categoria, ".kml", sep=""),
          folder.name = Categoria,
          plot.labpt = FALSE)
  remove(mapa)
}

# Create a 3-way categorical variable for occupational rate
Mapa_INEGI$Ocupada_Clas <- cut(x = as.numeric(Mapa_INEGI$Ocupada),
                                  # Remember we can set 3 to use more homogeneous groups
                                  breaks = quantile(Mapa_INEGI$Ocupada, 
                                                    probs = c(seq(from = 0, to = 1, by = 1/3))),
                                  include.lowest = TRUE,
                                  labels = c("Poca", "Media", "Alta"))

# Map it
for (Categoria in unique(Mapa_INEGI$Ocupada_Clas)) {
  mapa <- subset(Mapa_INEGI, Mapa_INEGI$Ocupada_Clas == Categoria)
  
  plotKML(obj = mapa,
          file.name = paste(Categoria, ".kml", sep=""),
          folder.name = Categoria,
          plot.labpt = FALSE)
  remove(mapa)
}

# Create a 3-way categorical variable for salud
Mapa_INEGI$Salud_Clas <- cut(x = as.numeric(Mapa_INEGI$Salud),
                               # Remember we can set 3 to use more homogeneous groups
                               breaks = quantile(Mapa_INEGI$Salud, 
                                                 probs = c(seq(from = 0, to = 1, by = 1/3))),
                               include.lowest = TRUE,
                               labels = c("Poca", "Media", "Alta"))

# And map it
for (Categoria in unique(Mapa_INEGI$Salud_Clas)) {
  mapa <- subset(Mapa_INEGI, Mapa_INEGI$Salud_Clas == Categoria)
  
  plotKML(obj = mapa,
          file.name = paste(Categoria, ".kml", sep=""),
          folder.name = Categoria,
          plot.labpt = FALSE)
  remove(mapa)
}

# ====================================================================================================
# ====================================================================================================


# Now, we will analyze Participation rates for that we go back to main folder
setwd("C:/Users/marti/OneDrive/Escritorio/Elecciones BC")

# See Participation stats over time
Participacion_descriptivo <- basic_stats(base = Distrito, "Participacion")
Participacion_descriptivo

# Prepare data to graph Participation in the last six elections
Participacion <- Distrito %>%
  group_by_at(c(as.character(groups(Distrito)))) %>%
  ungroup() %>%
  select(matches("Participacion"))
Parti_every = na.omit(melt(Participacion, id.vars = NULL))

# Graph last six elections
ggplot(data=Parti_every, aes(x=value, group=variable, fill=variable)) +
  geom_density(kernel = "gaussian", adjust=1.5, alpha=0.4) +
  labs(x = "% de participacion", y = "Probabilidad") + theme(plot.title = element_text(size=22)) +
  facet_wrap(~fct_rev(variable), ncol = 2) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank()) +
  scale_x_continuous(breaks = round(seq(min(Parti_every$value), max(Parti_every$value), by = 10),1))

# Create Participation table adding Municipality, District and Section
Participacion <- Distrito %>%
  select(matches("Seccion") | matches("Participacion"))

# Calculate historic participation rate averaging each section
Participacion$Media <- round(as.numeric( rowMeans(Participacion[,4:8], na.rm = TRUE) ), 2)

# Plot overall participation rate distribution
ggplot(data=Participacion, aes(x=Media, fill="red")) +
  geom_density(kernel = "gaussian", adjust=1.5, alpha=0.4) +
  labs(x = "% de participacion", y = "Probabilidad",
       title ="Participacion promedio 2010-21") + theme(plot.title = element_text(size=22)) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank()) +
  scale_x_continuous(breaks = round(seq(min(Participacion$Media), max(Participacion$Media), by = 5),1))

remove(Parti_every)

# 
Participacion[,10] <- cut(x = as.numeric(unlist(Participacion[,9])),
                          # Remember we can set 3 to use more homogeneous groups
                          breaks = quantile(Participacion$Media, 
                                            probs = c(seq(from = 0, to = 1, by = 1/3))),
                          include.lowest = TRUE,
                          labels = c("Poca", "Media", "Alta"))
colnames(Participacion)[10] <- "Tendencia"

# Create Participation map
Mapa_Participacion <- merge(x = Mapa_Secciones, y = Participacion[,c(3:10)],
                             by = "Seccion", all.y = TRUE)

# Plot map
for (Categoria in unique(Mapa_Participacion$Tendencia)) {
  mapa <- subset(Mapa_Participacion, Mapa_Participacion$Tendencia == Categoria)
  
  plotKML(obj = mapa,
          file.name = paste(Categoria, ".kml", sep=""),
          folder.name = Categoria,
          plot.labpt = FALSE)
  remove(mapa)
}

# plotKML(obj = Mapa_Participacion[,7],
#         colour_scale = c("firebrick1","yellow","orange","green2"),
#         alpha = 1,
#         folder.name = "Mapa Participacion",
#         file.name = paste("Mapa Participacion", ".kml", sep=""),
#         plot.labpt = FALSE)


# ====================================================================================================
# ====================================================================================================


# Now, we are in mapping how different sections vote based on historical trend
Tendencia_Voto <- Tendencia_Voto$Diputados[Tendencia_Voto$Diputados$Seccion %in% Distrito$Seccion,]
Tendencia_Voto <- merge(x = Mapa_Secciones, y = Tendencia_Voto, 
                        by = "Seccion", all.y = TRUE)

Tendencia_Tabla <- table(Tendencia_Voto$Tendencia)

# Plot map
for (Tend in unique(Tendencia_Voto$Tendencia)) {
  mapa <- subset(Tendencia_Voto, Tendencia_Voto$Tendencia == Tend)

  plotKML(obj = mapa,
          file.name = paste(Tend, ".kml", sep=""),
          folder.name = Tend,
          plot.labpt = FALSE)
  remove(mapa, Tend)
}


# ====================================================================================================
# ====================================================================================================


# Now we are interested in mapping voting trends based on the last 5 elections
Mapa_Tendencia <- HISTORICOS_GANADORES$Diputados[HISTORICOS_GANADORES$Diputados$Seccion %in% 
                                                     Distrito$Seccion,]
Mapa_Tendencia <- merge(x = Mapa_Secciones, y = Mapa_Tendencia, 
                        by = "Seccion", all.y = TRUE)

# After 
for (Ganador in unique(Mapa_Tendencia$Ganador_2021)) { 
  mapa <- subset(Mapa_Tendencia, Mapa_Tendencia$Ganador_2021 == Ganador)
  
  plotKML(obj = mapa,
          file.name = paste(Ganador, ".kml", sep=""),
          folder.name = Ganador,
          plot.labpt = FALSE)
  remove(mapa, Ganador, Categoria)
}
remove(Ganador, )

# ====================================================================================================
# ====================================================================================================

#
Resultados_distrito <- lapply(SECCIONES,function(x) {
  #x <- SECCIONES$Diputados_2010
  votos <- x[x$Seccion %in% Distrito$Seccion,][,4:(ncol(x[x$Seccion %in% Distrito$Seccion,])-4)]
  total_votos <- x[x$Seccion %in% Distrito$Seccion,][,"Total_Votos"]
  lista_nominal <- x[x$Seccion %in% Distrito$Seccion,][,"Lista_Nominal"]

  votos <- votos %>%
    select(-starts_with("Pre"))

  votos <- summarise_all(votos, sum)
  total_votos <- summarise_all(total_votos, sum)
  lista_nominal <- summarise_all(lista_nominal, sum)

  votos[,"Total_Votos"] <- total_votos
  votos[,"Lista_Nominal"] <- lista_nominal
  
  votos[2,] <-  (votos/ t(votos[,"Lista_Nominal"])) * 100 #Proporciones / Listado Nominal
  
  votos[1,] <- round(votos[1,],0)
  votos[2,] <- round(votos[2,],2)
  
  return(votos)
})


# ====================================================================================================
# ====================================================================================================


Ayuntamiento <- HISTORICOS_GANADORES_COMPLETE[[("Ayuntamiento")]][HISTORICOS_GANADORES_COMPLETE$Ayuntamiento$Seccion %in% 
                                                                    (Distrito$Seccion), ]
Diputados <- HISTORICOS_GANADORES_COMPLETE[[("Diputados")]][HISTORICOS_GANADORES_COMPLETE$Diputados$Seccion %in% 
                                                                    (Distrito$Seccion), ]

Secciones <- Ayuntamiento$Seccion

Ayuntamiento <- Ayuntamiento[grepl("Ganador", names(Ayuntamiento))]
Diputados <- Diputados[grepl("Ganador", names(Diputados))]

Parejo_Cruzado <- as.data.frame(Ayuntamiento == Diputados)
Parejo_Cruzado$Count <- rowSums(Parejo_Cruzado == TRUE)

Parejo_Cruzado <- Parejo_Cruzado %>% 
  mutate(Parejo_Cruzado = case_when(Count >= 3 ~ "Parejo",
                                    Count <= 2 ~ "Cruzado",
                                    TRUE ~ as.character(Count)))

Parejo_Cruzado <- data.frame(Secciones, Parejo_Cruzado)
Parejo_Cruzado_tabla <- as.data.frame(table(Parejo_Cruzado$Parejo_Cruzado))

remove(Ayuntamiento, Diputados, Secciones)

# ====================================================================================================
# ====================================================================================================
