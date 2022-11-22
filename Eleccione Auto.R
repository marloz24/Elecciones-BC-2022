library(data.table)
library(tidyverse)
library(tidyselect)
library(plotKML)
library(gtools)
library(dplyr)
library(sf)

setwd("C:/Users/rmartinez/Desktop/Elecciones BC/Resultados Electorales")

# Our data source is Electoral Institute of Baja California: ieebc.mx/resultados-electorales
# Data CASILLAS format was .xlsx with a number of sheets, for personal convenience 
# it was restructured in various single csv.

# List of file names ending in .csv and read all file name. Results from 2010 and 2013 were
# published by polling station wich in spanish is CASILLAS and so the name of our variable
my_files <- list.files(pattern = "\\.csv$")
CASILLAS <- lapply(my_files, read.csv)
names(CASILLAS) <- gsub("\\.csv$", "", my_files)

CASILLAS <- rev(CASILLAS)

# To facilitate understanding we and because it will be useful later, we create CASILLASname
CASILLASnames <- names(CASILLAS)
remove(my_files)

# ====================================================================================================
# ====================================================================================================

# Before working with data we need to standardized it, for personal preference
# The first step is to make DISTRITO all use Arabic numbers
unique(unlist(lapply(CASILLAS, `[[`, "DISTRITO")))

CASILLAS <- lapply(CASILLAS, transform, DISTRITO = (gsub("\\*", "",DISTRITO)))
CASILLAS <- lapply(CASILLAS, transform, DISTRITO = (gsub(" ", "",DISTRITO)))

arabigos_romanos <- function(x) {
  x %>%
    mutate(DISTRITO = case_when(DISTRITO == "I" ~ "1",
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

# Now we will name all municipalities the same way, since in 2010 and 2013 they use ID Number.
table(CASILLAS[["Ayuntamiento_2010"]]$MUNICIPIO)
CASILLAS[["Ayuntamiento_2010"]]$MUNICIPIO <- case_when(
  CASILLAS[["Ayuntamiento_2010"]]$MUNICIPIO == 1 ~ "ENSENADA",
  CASILLAS[["Ayuntamiento_2010"]]$MUNICIPIO == 2 ~ "MEXICALI",
  CASILLAS[["Ayuntamiento_2010"]]$MUNICIPIO == 3 ~ "TECATE",
  CASILLAS[["Ayuntamiento_2010"]]$MUNICIPIO == 4 ~ "TIJUANA",
  CASILLAS[["Ayuntamiento_2010"]]$MUNICIPIO == 5 ~ "ROSARITO")

CASILLAS[["Diputados_2010"]]$MUNICIPIO <- case_when(
  CASILLAS[["Diputados_2010"]]$MUNICIPIO == 1 ~ "ENSENADA",
  CASILLAS[["Diputados_2010"]]$MUNICIPIO == 2 ~ "MEXICALI",
  CASILLAS[["Diputados_2010"]]$MUNICIPIO == 3 ~ "TECATE",
  CASILLAS[["Diputados_2010"]]$MUNICIPIO == 4 ~ "TIJUANA",
  CASILLAS[["Diputados_2010"]]$MUNICIPIO == 5 ~ "ROSARITO")


# The next step is to review names and for all common variables to have the same name.
# As to compare past with previous results, in 2010 and 2013 ballots only include stand alone parties
# or alliance, in recent HISTORICOS ballots include check boxes for all parties and alliance combination  

# Since we are interested in matching results and find trends, we will rename alliance by the name of the
# leading party. From '16 and onward main parties will have "Pre_" added to their name. We will create
# new variables taking the name of the main party and add ONLY the parties combination of the alliance,
# allies party will keep their votes, this way we can match previous and recent results.

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
names(CASILLAS[["Ayuntamiento_2019"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","Pre_PAN","Pre_PRI","PRD",
                                               "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                               "C4","C5","C6","C7","C8","C9","C10","C11","Alfredo_Moreno",
                                               "Kevin_Fernando", "Gustavo_Flores","Rogelio_Castro",
                                               "NO_REGISTRADOS","VOTO_NULO",
                                               "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

names(CASILLAS[["Diputados_2019"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","Pre_PAN","Pre_PRI","PRD",
                                               "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                               "C4","C5","C6","C7","C8","C9","C10","C11","Fernanda_Angelica",
                                               "Tadeo_Javier.", "NO_REGISTRADOS","VOTO_NULO",
                                               "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

names(CASILLAS[["Gubernatura_2019"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","Pre_PAN","Pre_PRI","PRD",
                                            "PVEM","PT","PBC","TRANS","MC","Pre_MORENA","C1","C2","C3",
                                            "C4","C5","C6","C7","C8","C9","C10","C11",
                                            "NO_REGISTRADOS","VOTO_NULO",
                                            "TOTAL_VOTOS","LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

colnames(CASILLAS[["Ayuntamiento_2021"]])
colnames(CASILLAS[["Diputados_2021"]])
colnames(CASILLAS[["Gubernatura_2021"]])
names(CASILLAS[["Ayuntamiento_2021"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","Pre_PAN","Pre_PRI","PRD",
                                               "PT","PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM",
                                               "Rogelio_Castro","Marco_Antonio.","Cesar_Ivan","Celso_Arturo",
                                               "Luis_Fernando","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                               "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                               "NO_REGISTRADOS","VOTO_NULO","TOTAL_VOTOS","LISTA_NOMINAL",
                                               "PARTICIPACION","ABSTENCION")

names(CASILLAS[["Diputados_2021"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","Pre_PAN","Pre_PRI","PRD","PT",
                                            "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","Jose_Antonio",
                                            "Ramiro_Orea","PAN_PRI_PRD","PAN_PRI","PAN_PRD","PRI_PRD",
                                            "PT_PVEM_MORENA","PT_PVEM","PT_MORENA","PVEM_MORENA",
                                            "NO_REGISTRADOS","VOTO_NULO","TOTAL_VOTOS","LISTA_NOMINAL",
                                            "PARTICIPACION","ABSTENCION" )
names(CASILLAS[["Gubernatura_2021"]]) <- c("MUNICIPIO","DISTRITO","SECCION","TIPO","Pre_PAN","Pre_PRI","PRD","PT",
                                            "PVEM","PBC","MC","Pre_MORENA","PES","RSP","FXM","PAN_PRI_PRD","PAN_PRI",
                                            "PAN_PRD","PRI_PRD","PT_PVEM_MORENA","PT_PVEM","PT_MORENA",
                                            "PVEM_MORENA","NO_REGISTRADOS","VOTO_NULO","TOTAL_VOTOS",
                                            "LISTA_NOMINAL","PARTICIPACION","ABSTENCION")

# While SECCION are identify by number, in reality they are meaningless for numeric operations
# for standardization sake, we will add 0 to SECCION to make them all a four digit number 

CASILLAS <- lapply(CASILLAS, transform, SECCION = 
                     gsub("\\s", "0", format(SECCION,  justify = c("right"), width = 4)))

# ====================================================================================================
# ====================================================================================================

# In 2010 and 2013 results were published by polling station, not polling place since we are interested 
# in the latter, we have to transform PARTICIPACION and ABSTENCION from char to numeric and take % out

CASILLAS <- lapply(CASILLAS, transform, PARTICIPACION = as.numeric(gsub("%", "",PARTICIPACION)))
CASILLAS <- lapply(CASILLAS, transform, ABSTENCION = as.numeric(gsub("%", "",ABSTENCION)))

# We convert to numeric PARTICIPACION and ABSTENCION since they where char values, 
CASILLAS <- lapply(CASILLAS, function(x) {
  x[,5:ncol(x)] <- lapply(x[,5:ncol(x)], as.numeric)
  x})

# Replace returned NAs with 0
CASILLAS <- lapply(CASILLAS, function(x) {x[is.na(x)] <- 0;x})


# As established, from '16 and onward We will create new variables taking the name of the main party and 
# add ONLY the parties combination of the alliance, allies party WILL KEEP their votes, this way we can 
# match previous and recent results.The reasoning is that the majority of  votes are attributed to this party

# Create a few helper variables
y2016 = CASILLASnames[endsWith(CASILLASnames, "2016")]
y2019 = CASILLASnames[endsWith(CASILLASnames, "2019")]
y2021 = CASILLASnames[endsWith(CASILLASnames, "2021")]

# We also create 2010 and 2013 since they will be handy later
y2010 = CASILLASnames[endsWith(CASILLASnames, "2010")]
y2013 = CASILLASnames[endsWith(CASILLASnames, "2013")]

remove(CASILLASnames)

# In 2016 PRI led the only alliance 
aggregate_party <- function(x) {
  x %>%
    mutate(PRI = rowSums(select(., "Pre_PRI", "C1", "C2", "C3", "C4", "C5", 
                                "C6", "C7", "C8", "C9", "C10", "C11")), .before = "Pre_PRI")
  }
CASILLAS[y2016] <- lapply(CASILLAS[y2016], aggregate_party)


# In 2019 MORENA led the only alliance 
aggregate_party <- function(x) {
  x %>%
    mutate(Morena = rowSums(select(., "Pre_MORENA", "C1", "C2", "C3", "C4", "C5", 
                                    "C6", "C7", "C8", "C9", "C10", "C11")), .before = "Pre_MORENA")
}
CASILLAS[y2019] <- lapply(CASILLAS[y2019], aggregate_party)


# In 2021 there were 2 alliance, one led by MORENA and the other by PAN + PRI + PRD
# w
colnames(CASILLAS[["Ayuntamiento_2021"]])
aggregate_party <- function(x) {
  x %>%
    mutate(Morena = rowSums(select(.,"Pre_MORENA","PT","PVEM","PT_PVEM_MORENA",
                                   "PT_PVEM","PT_MORENA","PVEM_MORENA")), .before = "TIPO")
}
CASILLAS[y2021] <- lapply(CASILLAS[y2021], aggregate_party)


aggregate_party <- function(x) {
  x %>%
    mutate(Coalicion = rowSums(select(.,"Pre_PAN","Pre_PRI","PRD","PAN_PRI_PRD", "PAN_PRI",
                                       "PAN_PRD","PRI_PRD")), .before = "TIPO")
}
CASILLAS[y2021] <- lapply(CASILLAS[y2021], aggregate_party)

remove(aggregate_party)

# Now that the new main party variables were created as the sum of the party and allies combinations
# we will group by MUNICIPIO, DISTRITO and SECCION, omitting TIPO since we aren't interested in polling
# station. Results will be store as SECCIONES and keep CASILLAS unchanged

groupby_summariseif <- function(x) {
  x %>%
    group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
                 summarise_if(is.double, sum)
}
SECCIONES <- lapply(CASILLAS, groupby_summariseif)
SECCIONES <- lapply(CASILLAS, groupby_summariseif)

# As result of summing  columns PARTICIPACION and ABSTENCION are not representative of the actual values
# we recalculate them above

part_abst <- function(x) {
  x %>%
    mutate(PARTICIPACION = (TOTAL_VOTOS / LISTA_NOMINAL)*100) %>%
    mutate(ABSTENCION = (100 - PARTICIPACION))
}
SECCIONES <- lapply(SECCIONES, part_abst)

remove(part_abst)


# ====================================================================================================
# ====================================================================================================


# We will create a temporarily helper table. We are interested in creating HISTORICOS data frame 
# in which we will see all results by MUNICIPIO, DISTRITO and SECCION through every election, 
# to identify 2010 PAN from 2021 PAN, we will add "_year" to every column given the election

# We create a temporarily helper table: SECCIONES_year. We are interested in creating HISTORICOS
# data frame to see results by MUNICIPIO, DISTRITO and SECCION through every election. To identify,
# for example, between 2010 PAN from 2021 PAN, we will add "_year" to every column given election year

# Temporarily helper table SECCIONES_year
SECCIONES_year <- SECCIONES

# Add year to every column given election year
for( i in names(SECCIONES_year)){
  past_names <- colnames(SECCIONES_year[[i]][4:ncol(SECCIONES_year[[i]])])
  year <- (substr(i, nchar(i)-4, nchar(i)))
  
  past_names <- paste(past_names, year, sep="")
  
  colnames(SECCIONES_year[[i]])[4:ncol(SECCIONES_year[[i]])] <- c(past_names)
}

# HISTORICAL data frame
HISTORICOS <- list(Ayuntamiento = list(),
                        Diputados = list(),
                        Gubernatura = list())

historic <- function(datos, level){
  datos <- datos[grepl(level, names(datos))]
  datos <- datos %>%
    reduce(full_join, by = "SECCION")
  return(datos)
}

HISTORICOS[["Ayuntamiento"]] <- historic(datos = SECCIONES_year, level = "Ayuntamiento")
HISTORICOS[["Diputados"]] <- historic(datos = SECCIONES_year, level = "Diputados")
HISTORICOS[["Gubernatura"]] <- historic(datos = SECCIONES_year, level = "Gubernatura")

remove(i, year, past_names, y2010, y2013, y2016, y2019, y2021)

                                   
# Since we are interested in alliance and leading party, party alliance combination 
# will be deleted, NULL columns were selected after reviewing data

colnames(HISTORICOS[["Ayuntamiento"]])
review <- HISTORICOS[["Ayuntamiento"]][c(22:29, 36:37, 47:58, 69:70, 85:95, 110:111)]
names(review)
HISTORICOS[["Ayuntamiento"]][c(22:29, 36:37, 48:58, 69:70, 85:95, 110:111)]  <- list(NULL)

colnames(HISTORICOS[["Diputados"]])
review <- HISTORICOS[["Diputados"]][c(19:26, 33:34, 45:55, 64:65, 80:90, 104:105, 115:116)]
names(review)
HISTORICOS[["Diputados"]][c(19:26, 33:34, 45:55, 64:65, 80:90, 104:105, 115:116)]  <- list(NULL)

colnames(HISTORICOS[["Gubernatura"]])
review <- HISTORICOS[["Gubernatura"]][c(17:24, 31:32, 43:53, 60:61)]
names(review)
HISTORICOS[["Gubernatura"]][c(17:24, 31:32, 43:53, 60:61)]  <- list(NULL)

remove(review)
# remove ".x" from MUNICIPIO, DISTRITO and SECCION columns
HISTORICOS <- lapply(HISTORICOS, function(x) {
  colnames(x) <- gsub("[.x]", "", colnames(x))
  return(x)
})


# ====================================================================================================
# ====================================================================================================


# WINNERS table will resume for each election, what party won and by how many votes for each section
# for that we create the winners function.

 winners <- function(base) {
   election <- base
   votes <- election[,4:(ncol(election)-6)] 
   
   mun <- as.vector(unlist(base[,1]))
   dis <- as.vector(unlist(base[,2]))
   sec <- as.vector(unlist(base[,3]))
   
   winner_party <- colnames(votes)[max.col(votes,ties.method = ("first"))]
   party_votes <- apply(votes, 1, max)
   
   year <- (substr(winner_party, nchar(winner_party)-3, nchar(winner_party)))
   
   ganador <- unique(paste("GANADOR", year, sep="_"))
   votos <- unique(paste("VOTOS", year, sep="_"))
   
   nombres <- c("MUNICIPIO", "DISTRITO", "SECCION", ganador, votos)
   df <- data.frame(mun, dis, sec, winner_party, party_votes)
   colnames(df) <- nombres
   return(df)
}

GANADORES_SECCION <- (lapply(SECCIONES_year, winners))

# Using historic function again, we create a historical data frame showing which winner party and
# vote for each election by section and election level

GANADORES_HISTORICO <- list(Ayuntamiento = list(),
                            Diputados = list(),
                            Gubernatura = list())

GANADORES_HISTORICO[["Ayuntamiento"]] <- historic(datos = GANADORES_SECCION, level = "Ayuntamiento")
GANADORES_HISTORICO[["Diputados"]] <- historic(datos = GANADORES_SECCION, level = "Diputados")
GANADORES_HISTORICO[["Gubernatura"]] <- historic(datos = GANADORES_SECCION, level = "Gubernatura")

# Previously we used a full_join to unite election by section, however, every election the of section 
# grows taking in consideration population growth and urban expansion, because of it we will create
# secondary sections for GANADORES_HISTORICAL that will delete all rows with NA

GANADORES_HISTORICO_COMPLETE <- lapply(GANADORES_HISTORICO, na.omit)
for (i in seq_along(GANADORES_HISTORICO_COMPLETE)){
  rownames(GANADORES_HISTORICO_COMPLETE[[i]]) <- NULL
}


# ====================================================================================================
# ====================================================================================================




datos <- GANADORES_HISTORICO_COMPLETE$Ayuntamiento
T1 <- 4:5
T2 <- 6:8
output_T1 <- c()
output_T2 <- c()
wildcard = "Coalicion"

for (row in 1:nrow(datos)) {  
  
  ifelse(
    # Check if i section has vote every election for same party taking wildcard in consideration
    ((datos[row,4] == datos[row,5]) | (datos[row,4] == wildcard)) & (datos[row,5] == datos[row,6]) & 
      (datos[row,6] == datos[row,7]) & (datos[row,7] == datos[row,8]),
    
    # if TRUE assign Dominio + Party name two outputs
    return(c ( paste("Dominio", datos[row,5]), paste("Dominio", datos[row,5]) ) )
  )
}

table(output_T1)
table(output_T2)
