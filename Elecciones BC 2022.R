library(data.table)
library(tidyverse)
library(tidyselect)
library(dplyr)

# Our data source is Electoral Institute of Baja California: https://ieebc.mx/resultados-electorales/
# Data original format was .xlsx with a number of sheets, for personal convenience it was restructured in various single csv
# After uploading to github, our fist step is to load or data into different tables

Casilla_Ayuntamiento_2010 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2010.csv")
Casilla_Ayuntamiento_2013 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2013.csv")
Casilla_Ayuntamiento_2016 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2016.csv")
Casilla_Ayuntamiento_2019 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2019.csv")
Casilla_Ayuntamiento_2021 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2021.csv")

Casilla_Diputados_2010 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2010.csv")
Casilla_Diputados_2013 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2013.csv")
Casilla_Diputados_2016 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2016.csv")
Casilla_Diputados_2019 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2019.csv")
Casilla_Diputados_2021 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2021.csv")

Casilla_Gubernatura_2013 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gobernatura_2013.csv")
Casilla_Gubernatura_2019 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gobernatura_2019.csv")
Casilla_Gubernatura_2021 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gobernatura_2021.csv")


# ====================================================================================================
# ====================================================================================================


#We notice abstention and participation are char variable because of % replace and convert to numeric

Casilla_Ayuntamiento_2010 <- data.frame(lapply(Casilla_Ayuntamiento_2010, function(x) gsub("%", "", x)))
Casilla_Ayuntamiento_2013 <- data.frame(lapply(Casilla_Ayuntamiento_2013, function(x) gsub("%", "", x)))
Casilla_Ayuntamiento_2016 <- data.frame(lapply(Casilla_Ayuntamiento_2016, function(x) gsub("%", "", x)))
Casilla_Ayuntamiento_2019 <- data.frame(lapply(Casilla_Ayuntamiento_2019, function(x) gsub("%", "", x)))
Casilla_Ayuntamiento_2021 <- data.frame(lapply(Casilla_Ayuntamiento_2021, function(x) gsub("%", "", x)))

Casilla_Diputados_2010 <- data.frame(lapply(Casilla_Diputados_2010, function(x) gsub("%", "", x)))
Casilla_Diputados_2013 <- data.frame(lapply(Casilla_Diputados_2013, function(x) gsub("%", "", x)))
Casilla_Diputados_2016 <- data.frame(lapply(Casilla_Diputados_2016, function(x) gsub("%", "", x)))
Casilla_Diputados_2019 <- data.frame(lapply(Casilla_Diputados_2019, function(x) gsub("%", "", x)))
Casilla_Diputados_2021 <- data.frame(lapply(Casilla_Diputados_2021, function(x) gsub("%", "", x)))

Casilla_Gubernatura_2013 <- data.frame(lapply(Casilla_Gubernatura_2013, function(x) gsub("%", "", x)))
Casilla_Gubernatura_2019 <- data.frame(lapply(Casilla_Gubernatura_2019, function(x) gsub("%", "", x)))
Casilla_Gubernatura_2021 <- data.frame(lapply(Casilla_Gubernatura_2021, function(x) gsub("%", "", x)))

Casilla_Ayuntamiento_2010[,5:14] <- sapply(Casilla_Ayuntamiento_2010[,5:14], as.numeric)
Casilla_Ayuntamiento_2013[,5:13] <- sapply(Casilla_Ayuntamiento_2013[,5:13], as.numeric)
Casilla_Ayuntamiento_2016[,5:42] <- sapply(Casilla_Ayuntamiento_2016[,5:42], as.numeric)
Casilla_Ayuntamiento_2019[,5:34] <- sapply(Casilla_Ayuntamiento_2019[,5:34], as.numeric)
Casilla_Ayuntamiento_2021[,5:34] <- sapply(Casilla_Ayuntamiento_2021[,5:34], as.numeric)

Casilla_Diputados_2010[,5:14] <- sapply(Casilla_Diputados_2010[,5:14], as.numeric)
Casilla_Diputados_2013[,5:13] <- sapply(Casilla_Diputados_2013[,5:13], as.numeric)
Casilla_Diputados_2016[,5:41] <- sapply(Casilla_Diputados_2016[,5:41], as.numeric)
Casilla_Diputados_2019[,5:32] <- sapply(Casilla_Diputados_2019[,5:32], as.numeric)
Casilla_Diputados_2021[,5:31] <- sapply(Casilla_Diputados_2021[,5:31], as.numeric)

Casilla_Gubernatura_2013[,5:13] <- sapply(Casilla_Gubernatura_2013[,5:13], as.numeric)
Casilla_Gubernatura_2019[,5:30] <- sapply(Casilla_Gubernatura_2019[,5:30], as.numeric)
Casilla_Gubernatura_2021[,5:29] <- sapply(Casilla_Gubernatura_2021[,5:29], as.numeric)


# ====================================================================================================
# ====================================================================================================


# We convert all NAs in all tables to 0

Casilla_Ayuntamiento_2010[is.na(Casilla_Ayuntamiento_2010)] <- 0
Casilla_Ayuntamiento_2013[is.na(Casilla_Ayuntamiento_2013)] <- 0
Casilla_Ayuntamiento_2016[is.na(Casilla_Ayuntamiento_2016)] <- 0
Casilla_Ayuntamiento_2019[is.na(Casilla_Ayuntamiento_2019)] <- 0
Casilla_Ayuntamiento_2021[is.na(Casilla_Ayuntamiento_2021)] <- 0

Casilla_Diputados_2010[is.na(Casilla_Diputados_2010)] <- 0
Casilla_Diputados_2013[is.na(Casilla_Diputados_2013)] <- 0
Casilla_Diputados_2016[is.na(Casilla_Diputados_2016)] <- 0
Casilla_Diputados_2019[is.na(Casilla_Diputados_2019)] <- 0
Casilla_Diputados_2021[is.na(Casilla_Diputados_2021)] <- 0

Casilla_Gubernatura_2013[is.na(Casilla_Gubernatura_2013)] <- 0
Casilla_Gubernatura_2019[is.na(Casilla_Gubernatura_2019)] <- 0
Casilla_Gubernatura_2021[is.na(Casilla_Gubernatura_2021)] <- 0


# ====================================================================================================
# ====================================================================================================


#Before formatting data we check that results match to those published by IEEBC

aggregate(Casilla_Ayuntamiento_2010$CABC, by = list(Category = Casilla_Ayuntamiento_2010$MUNICIPIO), FUN=sum)
aggregate(Casilla_Diputados_2010$CABC, by = list(Category = Casilla_Diputados_2010$MUNICIPIO), FUN=sum)

aggregate(Casilla_Ayuntamiento_2013$UNIDOS.POR.BAJA.CALIFORNIA, by = list(Category = Casilla_Ayuntamiento_2013$MUNICIPIO), FUN=sum)
aggregate(Casilla_Diputados_2013$UNIDOS.POR.BAJA.CALIFORNIA, by = list(Category = Casilla_Diputados_2013$MUNICIPIO), FUN=sum)
aggregate(Casilla_Gubernatura_2013$UNIDOS.POR.BAJA.CALIFORNIA, by = list(Category = Casilla_Gubernatura_2013$MUNICIPIO), FUN=sum)

aggregate(Casilla_Ayuntamiento_2016$PAN, by = list(Category = Casilla_Ayuntamiento_2016$MUNICIPIO), FUN=sum)
aggregate(Casilla_Diputados_2016$PAN, by = list(Category = Casilla_Diputados_2016$MUNICIPIO), FUN=sum)

aggregate(Casilla_Ayuntamiento_2019$PAN, by = list(Category = Casilla_Ayuntamiento_2019$MUNICIPIO), FUN=sum)
aggregate(Casilla_Diputados_2019$PAN, by = list(Category = Casilla_Diputados_2019$MUNICIPIO), FUN=sum)
aggregate(Casilla_Gubernatura_2019$PAN, by = list(Category = Casilla_Gubernatura_2019$MUNICIPIO), FUN=sum)

aggregate(Casilla_Ayuntamiento_2021$PAN, by = list(Category = Casilla_Ayuntamiento_2021$MUNICIPIO), FUN=sum)
aggregate(Casilla_Diputados_2021$PAN, by = list(Category = Casilla_Diputados_2021$MUNICIPIO), FUN=sum)
aggregate(Casilla_Gubernatura_2021$PAN, by = list(Category = Casilla_Gubernatura_2021$MUNICIPIO), FUN=sum)


# ====================================================================================================
# ====================================================================================================


# After validating csv match final published results, we format and standardized  our tables

colnames(Casilla_Ayuntamiento_2010)
colnames(Casilla_Diputados_2010)

setnames(Casilla_Ayuntamiento_2010, old = c("CASILLA","CABC", "CGR", "CRBC"), 
         new = c("SECCION","PAN", "PRI", "PT"))
setnames(Casilla_Diputados_2010, old = c("CASILLA", "CABC", "CGR", "CRBC"), 
         new = c("SECCION", "PAN", "PRI", "PT"))

Casilla_Ayuntamiento_2010$MUNICIPIO <- case_when(
  Casilla_Ayuntamiento_2010$MUNICIPIO == 1 ~ "ENSENADA",
  Casilla_Ayuntamiento_2010$MUNICIPIO == 2 ~ "MEXICALI",
  Casilla_Ayuntamiento_2010$MUNICIPIO == 3 ~ "TECATE",
  Casilla_Ayuntamiento_2010$MUNICIPIO == 4 ~ "TIJUANA",
  Casilla_Ayuntamiento_2010$MUNICIPIO == 5 ~ "ROSARITO")
Casilla_Diputados_2010$MUNICIPIO <- case_when(
  Casilla_Diputados_2010$MUNICIPIO == 1 ~ "ENSENADA",
  Casilla_Diputados_2010$MUNICIPIO == 2 ~ "MEXICALI",
  Casilla_Diputados_2010$MUNICIPIO == 3 ~ "TECATE",
  Casilla_Diputados_2010$MUNICIPIO == 4 ~ "TIJUANA",
  Casilla_Diputados_2010$MUNICIPIO == 5 ~ "ROSARITO")


colnames(Casilla_Ayuntamiento_2013)
colnames(Casilla_Diputados_2013)
colnames(Casilla_Gubernatura_2013)

setnames(Casilla_Ayuntamiento_2013, old = c("CASILLA", "UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("SECCION", "PAN", "PRI"))
setnames(Casilla_Diputados_2013, old = c("CASILLA", "UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("SECCION", "PAN", "PRI"))
setnames(Casilla_Gubernatura_2013, old = c("CASILLA", "UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("SECCION", "PAN", "PRI"))


colnames(Casilla_Ayuntamiento_2016)
colnames(Casilla_Diputados_2016)

setnames(Casilla_Ayuntamiento_2016, old = c("PRI"), new = c("PRI"))
setnames(Casilla_Diputados_2016, old = c("PRI"), new = c("Pre_PRI"))

Casilla_Ayuntamiento_2016 <- Casilla_Ayuntamiento_2016 %>%  
  mutate(PRI = rowSums(select_(., "Pre_PRI", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .after = "Pre_PRI")
Casilla_Diputados_2016 <- Casilla_Diputados_2016 %>%  
  mutate(PRI = rowSums(select_(., "Pre_PRI", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .after = "Pre_PRI")


colnames(Casilla_Ayuntamiento_2019)
colnames(Casilla_Diputados_2019)
colnames(Casilla_Gubernatura_2019)

setnames(Casilla_Ayuntamiento_2019, old = c("MORENA"), new = c("Pre_Morena"))
setnames(Casilla_Diputados_2019, old = c("MORENA"), new = c("Pre_Morena"))
setnames(Casilla_Gubernatura_2019, old = c("MORENA"), new = c("Pre_Morena"))

Casilla_Ayuntamiento_2019 <- Casilla_Ayuntamiento_2019 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .after = "Pre_Morena")
Casilla_Diputados_2019 <- Casilla_Diputados_2019 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .after = "Pre_Morena")
Casilla_Gubernatura_2019 <- Casilla_Gubernatura_2019 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .after = "Pre_Morena")


colnames(Casilla_Ayuntamiento_2021)
colnames(Casilla_Diputados_2021)
colnames(Casilla_Gubernatura_2021)

setnames(Casilla_Ayuntamiento_2021, old = c("MORENA"), new = c("Pre_Morena" ))
setnames(Casilla_Diputados_2021, old = c("MORENA"), new = c("Pre_Morena" ))
setnames(Casilla_Gubernatura_2021, old = c("MORENA"), new = c("Pre_Morena" ))

Casilla_Ayuntamiento_2021 <- Casilla_Ayuntamiento_2021 %>%  
  mutate(Coalicion = rowSums(select_(., "PAN", "PRI", "PRD", "PAN...PRI...PRD", "PAN...PRI",
                               "PAN...PRD", "PRI.PRD")), .after = "TIPO")
Casilla_Ayuntamiento_2021 <- Casilla_Ayuntamiento_2021 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "PT", "PVEM", "PT.PVEM.MORENA", "PT.PVEM", 
                               "PT...MORENA", "PVEM...MORENA")), .after = "TIPO")
Casilla_Diputados_2021 <- Casilla_Diputados_2021 %>%  
  mutate(Coalicion = rowSums(select_(., "PAN", "PRI", "PRD", "PAN...PRI...PRD", "PAN...PRI",
                               "PAN...PRD", "PRI.PRD")), .after = "TIPO")
Casilla_Diputados_2021 <- Casilla_Diputados_2021 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "PT", "PVEM", "PT.PVEM.MORENA", "PT.PVEM", 
                               "PT...MORENA", "PVEM...MORENA")), .after = "TIPO")
Casilla_Gubernatura_2021 <- Casilla_Gubernatura_2021 %>%  
  mutate(Coalicion = rowSums(select_(., "PAN", "PRI", "PRD", "PAN...PRI...PRD", "PAN...PRI",
                               "PAN...PRD", "PRI.PRD")), .after = "TIPO")
Casilla_Gubernatura_2021 <- Casilla_Gubernatura_2021 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "PT", "PVEM", "PT.PVEM.MORENA", "PT.PVEM", 
                               "PT...MORENA", "PVEM...MORENA")), .after = "TIPO")


# ====================================================================================================
# ====================================================================================================


# Original election results come by polling place, we are interested in results by section

Seccion_Ayuntamiento_2010 <- Casilla_Ayuntamiento_2010 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Ayuntamiento_2013 <- Casilla_Ayuntamiento_2013 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Ayuntamiento_2016 <- Casilla_Ayuntamiento_2016 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Ayuntamiento_2019 <- Casilla_Ayuntamiento_2019 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Ayuntamiento_2021 <- Casilla_Ayuntamiento_2021 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)

Seccion_Diputados_2010 <- Casilla_Diputados_2010 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Diputados_2013 <- Casilla_Diputados_2013 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Diputados_2016 <- Casilla_Diputados_2016 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Diputados_2019 <- Casilla_Diputados_2019 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Diputados_2021 <- Casilla_Diputados_2021 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)

Seccion_Gubernatura_2013 <- Casilla_Gubernatura_2013 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Gubernatura_2019 <- Casilla_Gubernatura_2019 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)
Seccion_Gubernatura_2021 <- Casilla_Gubernatura_2021 %>% group_by(MUNICIPIO, DISTRITO, SECCION) %>% 
  summarise_if(is.numeric, sum)


# ====================================================================================================
# ====================================================================================================


# Since participation and abstention rate were sum, we re-calculated them

Seccion_Ayuntamiento_2010 <- Seccion_Ayuntamiento_2010 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.....ABST. = (100 - X..DE.PARTICIP.))
Seccion_Ayuntamiento_2013 <- Seccion_Ayuntamiento_2013 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.....ABST. = (100 - X..DE.PARTICIP.))
Seccion_Ayuntamiento_2016 <- Seccion_Ayuntamiento_2016 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))
Seccion_Ayuntamiento_2019 <- Seccion_Ayuntamiento_2019 %>%  
  mutate(X..DE.ABST. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.ABST.))
Seccion_Ayuntamiento_2021 <- Seccion_Ayuntamiento_2021 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))


Seccion_Diputados_2010 <- Seccion_Diputados_2010 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE....ABST. = (100 - X..DE.PARTICIP.))
Seccion_Diputados_2013 <- Seccion_Diputados_2013 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.....ABST. = (100 - X..DE.PARTICIP.))
Seccion_Diputados_2016 <- Seccion_Diputados_2016 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))
Seccion_Diputados_2019 <- Seccion_Diputados_2019 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.ABST.))
Seccion_Diputados_2021 <- Seccion_Diputados_2021 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))


Seccion_Gubernatura_2013 <- Seccion_Gubernatura_2013 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.....ABST. = (100 - X..DE.PARTICIP.))
Seccion_Gubernatura_2019 <- Seccion_Gubernatura_2019 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.ABST.))
Seccion_Gubernatura_2021 <- Seccion_Gubernatura_2021 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))


# ====================================================================================================
# ====================================================================================================


# Finally, we build our historic databases

colnames(Seccion_Ayuntamiento_2010)[-3] <- paste(colnames(Seccion_Ayuntamiento_2010)[-3], "2010", sep = "_")
colnames(Seccion_Ayuntamiento_2013)[-3] <- paste(colnames(Seccion_Ayuntamiento_2013)[-3], "2013", sep = "_")
colnames(Seccion_Ayuntamiento_2016)[-3] <- paste(colnames(Seccion_Ayuntamiento_2016)[-3], "2016", sep = "_")
colnames(Seccion_Ayuntamiento_2019)[-3] <- paste(colnames(Seccion_Ayuntamiento_2019)[-3], "2019", sep = "_")
colnames(Seccion_Ayuntamiento_2021)[-3] <- paste(colnames(Seccion_Ayuntamiento_2021)[-3], "2021", sep = "_")

colnames(Seccion_Diputados_2010)[-3] <- paste(colnames(Seccion_Diputados_2010)[-3], "2010", sep = "_")
colnames(Seccion_Diputados_2013)[-3] <- paste(colnames(Seccion_Diputados_2013)[-3], "2013", sep = "_")
colnames(Seccion_Diputados_2016)[-3] <- paste(colnames(Seccion_Diputados_2016)[-3], "2016", sep = "_")
colnames(Seccion_Diputados_2019)[-3] <- paste(colnames(Seccion_Diputados_2019)[-3], "2019", sep = "_")
colnames(Seccion_Diputados_2021)[-3] <- paste(colnames(Seccion_Diputados_2021)[-3], "2021", sep = "_")

colnames(Seccion_Gubernatura_2013)[-3] <- paste(colnames(Seccion_Gubernatura_2013)[-3], "2013", sep = "_")
colnames(Seccion_Gubernatura_2019)[-3] <- paste(colnames(Seccion_Gubernatura_2019)[-3], "2019", sep = "_")
colnames(Seccion_Gubernatura_2021)[-3] <- paste(colnames(Seccion_Gubernatura_2021)[-3], "2021", sep = "_")


Ayuntamiento <- list(Seccion_Ayuntamiento_2021, Seccion_Ayuntamiento_2019, Seccion_Ayuntamiento_2016,
                     Seccion_Ayuntamiento_2013, Seccion_Ayuntamiento_2010) %>% 
  reduce(left_join, by = "SECCION")
colnames(Ayuntamiento)
Ayuntamiento[ ,c(22:29, 36:37, 48:58, 69:67, 84:94, 109:110)] <- list(NULL)
colnames(Ayuntamiento)

Diputados <- list(Seccion_Diputados_2021, Seccion_Diputados_2019, Seccion_Diputados_2016,
                  Seccion_Diputados_2013, Seccion_Diputados_2010) %>% 
  reduce(left_join, by = "SECCION")
colnames(Diputados)
Diputados[ ,c(19:26, 33:34, 45:55, 64:65, 80:90, 104:105, 115:116)] <- list(NULL)
colnames(Diputados)


Gubernatura <- list(Seccion_Gubernatura_2021, Seccion_Gubernatura_2019, Seccion_Gubernatura_2013) %>% 
  reduce(left_join, by = "SECCION")
colnames(Gubernatura)
Gubernatura[ ,c(17:24, 31:32, 43:53, 60:61)] <- list(NULL)
colnames(Gubernatura)


# ====================================================================================================
# ====================================================================================================

colnames(Gubernatura)
Gub_2021 <- (4:16)
Gub_2019 <- (23:33)
Gub_2013 <- (39:41)

colnames(Diputados)
Dip_2021 <- (4:19)
Dip_2019 <- (25:36)
Dip_2016 <- (42:63)
Dip_2013 <- (69:72)
Dip_2010 <- (78:82)

colnames(Ayuntamiento)
Ayu_2021 <- (4:22)
Ayu_2019 <- (28:42)
Ayu_2016 <- (47:68)
Ayu_2013 <- (74:77)
Ayu_2010 <- (85:90)


Gubernatura_2021 <- colnames(Gubernatura[,Gub_2021])[max.col((Gubernatura[,Gub_2021]))]
Gubernatura_2019 <- colnames(Gubernatura[,Gub_2019])[max.col((Gubernatura[,Gub_2019]))]
Gubernatura_2013 <- colnames(Gubernatura[,Gub_2013])[max.col((Gubernatura[,Gub_2013]))]

Gubernatura_2021_Votos <- apply(Gubernatura[Gub_2021], 1, max)
Gubernatura_2019_Votos <- apply(Gubernatura[Gub_2019], 1, max)
Gubernatura_2013_Votos <- apply(Gubernatura[Gub_2013], 1, max)

Diputados_2021 <- colnames(Diputados[,Dip_2021])[max.col((Diputados[,Dip_2021]))]
Diputados_2019 <- colnames(Diputados[,Dip_2019])[max.col((Diputados[,Dip_2019]))]
Diputados_2016 <- colnames(Diputados[,Dip_2016])[max.col((Diputados[,Dip_2016]))]
Diputados_2013 <- colnames(Diputados[,Dip_2013])[max.col((Diputados[,Dip_2013]))]
Diputados_2010 <- colnames(Diputados[,Dip_2010])[max.col((Diputados[,Dip_2010]))]

Diputados_2021_votos <- apply(Diputados[Dip_2021], 1, max)
Diputados_2019_votos <- apply(Diputados[Dip_2019], 1, max)
Diputados_2016_votos <- apply(Diputados[Dip_2016], 1, max)
Diputados_2013_votos <- apply(Diputados[Dip_2013], 1, max)
Diputados_2010_votos <- apply(Diputados[Dip_2010], 1, max)

Ayuntamiento_2021 <- colnames(Ayuntamiento[,Ayu_2021])[max.col((Ayuntamiento[,Ayu_2021]))]
Ayuntamiento_2019 <- colnames(Ayuntamiento[,Ayu_2019])[max.col((Ayuntamiento[,Ayu_2019]))]
Ayuntamiento_2016 <- colnames(Ayuntamiento[,Ayu_2016])[max.col((Ayuntamiento[,Ayu_2016]))]
Ayuntamiento_2013 <- colnames(Ayuntamiento[,Ayu_2013])[max.col((Ayuntamiento[,Ayu_2013]))]
Ayuntamiento_2010 <- colnames(Ayuntamiento[,Ayu_2010])[max.col((Ayuntamiento[,Ayu_2010]))]

Ayuntamiento_2021_votos <- apply(Ayuntamiento[Ayu_2021], 1, max)
Ayuntamiento_2019_votos <- apply(Ayuntamiento[Ayu_2019], 1, max)
Ayuntamiento_2016_votos <- apply(Ayuntamiento[Ayu_2016], 1, max)
Ayuntamiento_2013_votos <- apply(Ayuntamiento[Ayu_2013], 1, max)
Ayuntamiento_2010_votos <- apply(Ayuntamiento[Ayu_2010], 1, max)


Ganador_Gubernatura <- data.frame(Gubernatura[,1:3], Gubernatura_2021, Gubernatura_2021_Votos, 
                                  Gubernatura_2019, Gubernatura_2019_Votos,
                                  Gubernatura_2013, Gubernatura_2013_Votos)

Ganador_Diputados <- data.frame(Diputados[,1:3], Diputados_2021_votos, 
                                Diputados_2019, Diputados_2019_votos, 
                                Diputados_2016, Diputados_2016_votos,
                                Diputados_2013, Diputados_2013_votos,
                                Diputados_2010, Diputados_2010_votos)

Ganador_Ayuntamiento <- data.frame(Ayuntamiento[,1:3], Ayuntamiento_2021, Ayuntamiento_2021_votos,
                                   Ayuntamiento_2019, Ayuntamiento_2019_votos,
                                   Ayuntamiento_2016, Ayuntamiento_2016_votos,
                                   Ayuntamiento_2013, Ayuntamiento_2013_votos,
                                   Ayuntamiento_2010, Ayuntamiento_2010_votos)