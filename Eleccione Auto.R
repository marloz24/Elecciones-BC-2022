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
  # Temporarily helper table Secciones_year
  Secciones_year <- base
  df_list <- list()
  
  # Add year to every column given election year
  for( i in names(Secciones_year)){
    # Substract year from each data frame
    election_year <- substr(i, nchar(i)-4, nchar(i))
    
    # Subset data frame to obtain SECCION and election results
    geo <- Secciones_year[[i]][,3]
    votes <- Secciones_year[[i]][,4:(ncol(Secciones_year[[i]])-6)] 
    
    # Select winner party and number of votes (max)
    winner_party <- colnames(votes)[max.col(votes,ties.method = ("first"))]
    party_votes <- apply(votes, 1, max)
    
    df_list[[i]] <- data.frame(geo, winner_party, party_votes)
    # colnames(df_list[[i]]) 
    df_list[[i]] <- df_list[[i]] %>% 
      `colnames<-`(c("Seccion", "Ganador", "Votos")) %>%
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
    mutate(Tendencia = case_when(victories == 5 ~ paste("Domina", party, sep=" ") ,
                                 victories == 3 | victories == 4 ~ paste("Tendencia", party, sep=" "),
                                 victories <= 3 ~ "Alternancia",
    ))
  
  df <- data.frame(base[,1:3], party_victories$Tendencia)
  colnames(df) <- c("Municipio", "Distrito", "Seccion", "Tendencia")
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
  
  base = melt(base, id.vars = NULL)
  base$value <- round(base$value, 1)
  
  # Mode function does not exist, we define it
  Modes <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calculate basic statistics
  estadisticos <- base %>% 
    group_by_at("variable") %>%   
    summarise(Media = mean(value),
              Mediana = median(value),
              Moda = Modes(value),
              Total = sum(value)
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


# ====================================================================================================
# ====================================================================================================


# 
setwd("C:/Users/marti/OneDrive/Escritorio/Elecciones BC")

# We are interested in analyzing District 10, the first question to answer is "Who's" the District
# How many Sections it has, whats the electoral population, has this change over time?
# We are also interested in the demographics, Male, Females, Ages in other basic stats

# Subset to Distrit 10 and see if Sections numbers has change over time
Distrito <- subset(HISTORICOS$Diputados, HISTORICOS$Diputados$Distrito_2021 == "10")
Num_secciones <- colSums(!is.na(Distrito))
Num_secciones <- as.data.frame(Num_secciones[grepl("^Mun",names(Num_secciones))])

# See how voter registration has change over time
Listado <- select(Distrito, matches("Lista_Nominal"))
Listado_descriptivo <- as.data.frame(basic_stats(base = Listado, "Lista_Nominal"))
Listado_descriptivo

# In 2019 district delimitation changed, we check composition based on previous districts 
Composicion_seccion <- table(Distrito$Distrito_2021, Distrito$Distrito_2016, useNA = c("ifany"))
Composicion_seccion <- as.data.frame(prop.table(Composicion_seccion)*100) #Distrito 10 2021vs2016 (pre-re-distritacion)
Composicion_seccion

# And composition in terms of electoral register population and percentage
Composicion_listado <- Distrito %>%
  group_by(Distrito$Distrito_2016) %>%
  summarise(Total = sum(Lista_Nominal_2016))

Composicion_listado[,3] <- (Composicion_listado[,2] / as.numeric(Listado_descriptivo[1,5]))*100
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

# Create District and Seccions demographic data frame
INEGI_Distrito <- data.frame(SECCION <- INEGI$SECCION,
                             
                             Hombres <- INEGI$POBMAS,
                             Mujeres <- INEGI$POBFEM,
                             
                             P0a4 <- INEGI$POBTOT - INEGI$P_5YMAS,
                             P18a24 <- INEGI$P_18A24,
                             P25a64 <- INEGI$POBTOT - (INEGI$P_0A17 + INEGI$P_18A24 + INEGI$POB65_MAS),
                             P65MAS <- INEGI$POB65_MAS,
                             P5a17 <- INEGI$POBTOT - P0a4 - P18a24 - P25a64 - P65MAS,
                             
                             Salud <- INEGI$PDER_SS,
                             Educacion <- INEGI$GRAPROES)

INEGI_Secciones <- data.frame(SECCION <- INEGI$SECCION,
                               (INEGI_Distrito[,2:9] / INEGI$POBTOT)*100,
                               Educacion <- INEGI$GRAPROES)

# Rename District and Secciones columns
INEGI_names <- c("SECCION", "Hombres", "Mujeres", "P0a4", "P18a24","P25a64", "P65MAS", "P5a17",
                 "Salud", "Educacion")
colnames(INEGI_Distrito) <- INEGI_names
colnames(INEGI_Secciones) <- INEGI_names

# Round percentages
INEGI_Secciones[,2:10] <- round(INEGI_Secciones[,2:10], 2)

# Rearrange District and Secciones columns, P5a17 
INEGI_Distrito <- INEGI_Distrito[, c("SECCION",
                                     "Hombres", "Mujeres",
                                     "P0a4", "P5a17", "P18a24","P25a64", "P65MAS",
                                     "Salud", "Educacion")]
INEGI_Secciones <- INEGI_Secciones[, c("SECCION",
                                       "Hombres", "Mujeres",
                                       "P0a4", "P5a17", "P18a24","P25a64", "P65MAS",
                                       "Salud", "Educacion")]

# Subset to District of interest
INEGI_Distrito <- INEGI_Distrito[INEGI_Distrito$SECCION %in% unique(Distrito$Seccion), ]

# Calculate District population to calculate percentages, Educacion is calculated  as an average
INEGI_Distrito_Poblacion <- sum(INEGI_Distrito$Hombres) + sum(INEGI_Distrito$Mujeres)
INEGI_Distrito_Educacion <- mean(INEGI_Distrito$Educacion)

# For District we are interested in overall stats, so we discard SECCION column from data frame
INEGI_Distrito <- INEGI_Distrito[,2:10]

# Sum all columns to obtain District total and calculate percentages
INEGI_Distrito <- summarise_all(INEGI_Distrito[,1:8], sum)
INEGI_Distrito <- (INEGI_Distrito / INEGI_Distrito_Poblacion) * 100

# Educacion average is added to data frame apart
INEGI_Distrito[,9] <- INEGI_Distrito_Educacion
names(INEGI_Distrito)[names(INEGI_Distrito) == "V9"] <- "Educacion"

# Round percentages
INEGI_Distrito <- round(INEGI_Distrito, 2)
INEGI_Distrito

# We are interest in every Seccion that belong to the District, so we only subset
INEGI_Secciones <- INEGI_Secciones[INEGI_Secciones$SECCION %in% unique(Distrito$Seccion), ]

remove(Hombres, Mujeres, Salud, Educacion, 
       P0a4, P18a24,P25a64, P65MAS, P5a17,
       INEGI_Distrito_Educacion, INEGI_Distrito_Poblacion, INEGI_names, SECCION)


# ====================================================================================================
# ====================================================================================================


# Set back main folder
setwd("C:/Users/marti/OneDrive/Escritorio/Elecciones BC")

# See Participation stats over time
Participacion_descriptivo <- basic_stats(base = Distrito, "Participacion")
Participacion_descriptivo[,1:4]

# Prepare data to graph Participation in the last six elections
Participacion <- Distrito %>%
  group_by_at(c(as.character(groups(Distrito)))) %>%
  ungroup() %>%
  select(matches("Participacion"))
Parti_every = melt(Participacion, id.vars = NULL)

# Graph last six elections
ggplot(data=Parti_every, aes(x=value, group=variable, fill=variable)) +
  geom_density(kernel = "gaussian", adjust=1.5, alpha=0.4) +
  labs(x = "% de participacion", y = "Probabilidad",
       title ="Participación 2010-21") + theme(plot.title = element_text(size=22)) +
  facet_wrap(~variable) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank()) +
  scale_x_continuous(breaks = round(seq(min(Parti_every$value), max(Parti_every$value), by = 10),1))

# Create Participation table adding Municipality, District and Section
Participacion <- Distrito %>%
  select(matches("Seccion") | matches("Participacion"))

# Calculate historic participation rate averaging each section
Participacion$Media <- round(as.numeric(rowMeans(Participacion[,4:8])),2)

# Plot overall participation rate distribution
ggplot(data=Participacion, aes(x=Media, fill="red")) +
  geom_density(kernel = "gaussian", adjust=1.5, alpha=0.4) +
  labs(x = "% de participacion", y = "Probabilidad",
       title ="Participacion 2010-21") + theme(plot.title = element_text(size=22)) +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), axis.ticks.x=element_blank()) +
  scale_x_continuous(breaks = round(seq(min(Participacion$Media), max(Participacion$Media), by = 5),1))

remove(Parti_every)


# ====================================================================================================
# ====================================================================================================


# 
Participacion[,10] <- cut(x = as.numeric(unlist(Participacion[,9])),
                          # Remember we can set 3 to use more homogeneous groups
                          breaks = quantile(Participacion$Media, 
                                            probs = c(seq(from = 0, to = 1, by = 1/3))),
                          include.lowest = TRUE,
                          labels = c("Poca", "Media", "Alta"))
colnames(Participacion)[10] <- "Tendencia"

# Create Participation map
Mapa_participacion_promedio <- merge(x = Mapa_Secciones, y = Participacion[,c(3,10)],
                             by = "Seccion", all.x = TRUE)
Mapa_participacion_promedio <- Mapa_participacion_promedio[!is.na(Mapa_participacion_promedio$Tendencia),]

# Plot map
# for (Categoria in unique(Mapa_participacion_promedio$Tendencia)) {
#   mapa <- subset(Mapa_participacion_promedio, Mapa_participacion_promedio$Tendencia == Categoria)
# 
#   plotKML(obj = mapa,
#           file.name = paste(Categoria, ".kml", sep=""),
#           folder.name = Categoria,
#           plot.labpt = FALSE)
#   remove(mapa)
# }


# ====================================================================================================
# ====================================================================================================

#
Distrito_resultados <- lapply(SECCIONES,function(x) {
  votos <- x[x$Seccion %in% Distrito$Seccion,][,4:(ncol(x[x$Seccion %in% Distrito$Seccion,])-6)]
  lista_nominal <- x[x$Seccion %in% Distrito$Seccion,][,"Lista_Nominal"]
  
  votos <- summarise_all(votos, sum)
  lista_nominal <- summarise_all(lista_nominal, sum)
  
  votos[,"Lista_Nominal"] <- lista_nominal
  votos[2,] <-  (votos/ t(votos[,"Lista_Nominal"])) * 100
  
  votos[1,] <- round(votos[1,],0)
  votos[2,] <- round(votos[2,],2)
  
  return(votos)
})


# ====================================================================================================
# ====================================================================================================


Ayuntamiento <- HISTORICOS_GANADORES_COMPLETE[[("Ayuntamiento")]]
Diputados <- HISTORICOS_GANADORES_COMPLETE[[("Diputados")]]

Ayuntamiento <- Ayuntamiento[Ayuntamiento$Seccion %in% (Distrito$Seccion), ]
Diputados <- Diputados[Diputados$Seccion %in% (Distrito$Seccion), ]

Secciones <- Ayuntamiento$Seccion

Ayuntamiento <- Ayuntamiento[grepl("Ganador", names(Ayuntamiento))]
Diputados <- Diputados[grepl("Ganador", names(Diputados))]

Parejo_Cruzado <- as.data.frame(Ayuntamiento == Diputados)

Parejo_Cruzado$Count <- rowSums(Parejo_Cruzado == TRUE)

Parejo_Cruzado <- Parejo_Cruzado %>% mutate(Parejo_Cruzado = case_when(Count >= 3 ~ "Parejo",
                                                                       Count <= 2 ~ "Cruzado",
                                                                       TRUE ~ as.character(Count)))

Parejo_Cruzado <- data.frame(Secciones, Parejo_Cruzado)
Parejo_Cruzado_tabla <- as.data.frame(table(Parejo_Cruzado$Parejo_Cruzado))

remove(Ayuntamiento, Diputados, Secciones)
