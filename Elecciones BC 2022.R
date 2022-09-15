library(data.table)
library(tidyverse)
library(tidyselect)
library(plotKML)
library(gtools)
library(dplyr)
library(sf)

setwd("C:/Users/rmartinez/Desktop/Elecciones BC")

# Our data source is Electoral Institute of Baja California: ieebc.mx/resultados-electorales
# Data original format was .xlsx with a number of sheets, for personal convenience 
# it was restructured in various single csv. After uploading to github, our fist step 
# is to load or data into different tables

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


# We notice that a few columns are char tye, especiallt abstention and participation
# because of % symbol, we will subtract it and convert columns to numeric type

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


# Just a standar practice, we will convert all NAs to 0

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


# We will format and standardized  our tables such that we have common row and column names

# First we notice that in 2010 municipalities use an ID number, we will change it to their names

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

# In 2010 and 2013 results were not published desegregated manner, we will change the
# name of the alliance to that of the leading party
colnames(Casilla_Ayuntamiento_2010)
colnames(Casilla_Diputados_2010)
setnames(Casilla_Ayuntamiento_2010, old = c("CASILLA","CABC", "CGR", "CRBC"), 
         new = c("SECCION","PAN", "PRI", "PT"))
setnames(Casilla_Diputados_2010, old = c("CASILLA", "CABC", "CGR", "CRBC"), 
         new = c("SECCION", "PAN", "PRI", "PT"))


colnames(Casilla_Ayuntamiento_2013)
colnames(Casilla_Diputados_2013)
colnames(Casilla_Gubernatura_2013)

setnames(Casilla_Ayuntamiento_2013, old = c("CASILLA", "UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("SECCION", "PAN", "PRI"))
setnames(Casilla_Diputados_2013, old = c("CASILLA", "UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("SECCION", "PAN", "PRI"))
setnames(Casilla_Gubernatura_2013, old = c("CASILLA", "UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("SECCION", "PAN", "PRI"))


# In 2016 and the following years the results were disaggregated, however, we aren't
# interested in this type of detail, we will group the results by the leading party

# In this election there was only one alliance lead by PRI, we will created a column name
# PRI with the cumulative of votes and Pre_PRI will be votes that gather as standalone

colnames(Casilla_Ayuntamiento_2016)
colnames(Casilla_Diputados_2016)

setnames(Casilla_Ayuntamiento_2016, old = c("PRI"), new = c("Pre_PRI"))
setnames(Casilla_Diputados_2016, old = c("PRI"), new = c("Pre_PRI"))

Casilla_Ayuntamiento_2016 <- Casilla_Ayuntamiento_2016 %>%  
  mutate(PRI = rowSums(select(., "Pre_PRI", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .before = "Pre_PRI")
Casilla_Diputados_2016 <- Casilla_Diputados_2016 %>%  
  mutate(PRI = rowSums(select(., "Pre_PRI", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .before = "Pre_PRI")

# 2019 was the first election with Morena as major force and it did as the only 
# alliance,similarly we create Pre_Morena with standalone votes and Morena with
# how many vote got in alliance

colnames(Casilla_Ayuntamiento_2019)
colnames(Casilla_Diputados_2019)
colnames(Casilla_Gubernatura_2019)

setnames(Casilla_Ayuntamiento_2019, old = c("MORENA"), new = c("Pre_Morena"))
setnames(Casilla_Diputados_2019, old = c("MORENA"), new = c("Pre_Morena"))
setnames(Casilla_Gubernatura_2019, old = c("MORENA"), new = c("Pre_Morena"))

Casilla_Ayuntamiento_2019 <- Casilla_Ayuntamiento_2019 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .before = "Pre_Morena")
Casilla_Diputados_2019 <- Casilla_Diputados_2019 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .before = "Pre_Morena")
Casilla_Gubernatura_2019 <- Casilla_Gubernatura_2019 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "C1", "C2", "C3", "C4", "C5", 
                               "C6", "C7", "C8", "C9", "C10", "C11")), .before = "Pre_Morena")

# In 2021 two major alliance were form, one with Morena and allies and the other with the
# PRI, PAN and PRD as "Coalision", We will continue to create "Pre_" and standalone variables
colnames(Casilla_Ayuntamiento_2021)
colnames(Casilla_Diputados_2021)
colnames(Casilla_Gubernatura_2021)

setnames(Casilla_Ayuntamiento_2021, old = c("MORENA"), new = c("Pre_Morena" ))
setnames(Casilla_Diputados_2021, old = c("MORENA"), new = c("Pre_Morena" ))
setnames(Casilla_Gubernatura_2021, old = c("MORENA"), new = c("Pre_Morena" ))

Casilla_Ayuntamiento_2021 <- Casilla_Ayuntamiento_2021 %>%  
  mutate(Coalicion = rowSums(select_(., "PAN", "PRI", "PRD", "PAN...PRI...PRD", "PAN...PRI",
                               "PAN...PRD", "PRI.PRD")), .before = "TIPO")
Casilla_Ayuntamiento_2021 <- Casilla_Ayuntamiento_2021 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "PT", "PVEM", "PT.PVEM.MORENA", "PT.PVEM", 
                               "PT...MORENA", "PVEM...MORENA")), .before = "TIPO")
Casilla_Diputados_2021 <- Casilla_Diputados_2021 %>%  
  mutate(Coalicion = rowSums(select_(., "PAN", "PRI", "PRD", "PAN...PRI...PRD", "PAN...PRI",
                               "PAN...PRD", "PRI.PRD")), .before = "TIPO")

Casilla_Diputados_2021 <- Casilla_Diputados_2021 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "PT", "PVEM", "PT.PVEM.MORENA", "PT.PVEM", 
                               "PT...MORENA", "PVEM...MORENA")), .before = "TIPO")
Casilla_Gubernatura_2021 <- Casilla_Gubernatura_2021 %>%  
  mutate(Coalicion = rowSums(select_(., "PAN", "PRI", "PRD", "PAN...PRI...PRD", "PAN...PRI",
                               "PAN...PRD", "PRI.PRD")), .before = "TIPO")
Casilla_Gubernatura_2021 <- Casilla_Gubernatura_2021 %>%  
  mutate(Morena = rowSums(select_(., "Pre_Morena", "PT", "PVEM", "PT.PVEM.MORENA", "PT.PVEM", 
                               "PT...MORENA", "PVEM...MORENA")), .before = "TIPO")


# ====================================================================================================
# ====================================================================================================

# We notice that Seccion is a chr variable and that not all value are 4 digits length, we will fix this
# again, to have standardized tables

Casilla_Ayuntamiento_2010$SECCION <- gsub("\\s", "0", format(Casilla_Ayuntamiento_2010$SECCION,  justify = c("right"), width = 4))
Casilla_Ayuntamiento_2013$SECCION <- gsub("\\s", "0", format(Casilla_Ayuntamiento_2013$SECCION,  justify = c("right"), width = 4))
Casilla_Ayuntamiento_2016$SECCION <- gsub("\\s", "0", format(Casilla_Ayuntamiento_2016$SECCION,  justify = c("right"), width = 4))
Casilla_Ayuntamiento_2019$SECCION <- gsub("\\s", "0", format(Casilla_Ayuntamiento_2019$SECCION,  justify = c("right"), width = 4))
Casilla_Ayuntamiento_2021$SECCION <- gsub("\\s", "0", format(Casilla_Ayuntamiento_2021$SECCION,  justify = c("right"), width = 4))

Casilla_Diputados_2010$SECCION <- gsub("\\s", "0", format(Casilla_Diputados_2010$SECCION,  justify = c("right"), width = 4))
Casilla_Diputados_2013$SECCION <- gsub("\\s", "0", format(Casilla_Diputados_2013$SECCION,  justify = c("right"), width = 4))
Casilla_Diputados_2016$SECCION <- gsub("\\s", "0", format(Casilla_Diputados_2016$SECCION,  justify = c("right"), width = 4))
Casilla_Diputados_2019$SECCION <- gsub("\\s", "0", format(Casilla_Diputados_2019$SECCION,  justify = c("right"), width = 4))
Casilla_Diputados_2021$SECCION <- gsub("\\s", "0", format(Casilla_Diputados_2021$SECCION,  justify = c("right"), width = 4))

Casilla_Gubernatura_2013$SECCION <- gsub("\\s", "0", format(Casilla_Gubernatura_2013$SECCION,  justify = c("right"), width = 4))
Casilla_Gubernatura_2019$SECCION <- gsub("\\s", "0", format(Casilla_Gubernatura_2019$SECCION,  justify = c("right"), width = 4))
Casilla_Gubernatura_2021$SECCION <- gsub("\\s", "0", format(Casilla_Gubernatura_2021$SECCION,  justify = c("right"), width = 4))


# ====================================================================================================
# ====================================================================================================


# Original election results come by polling place, we are interested in results by section; we summarize

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


# We re-calculated Participation and Abstention rate since they were sum

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
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))
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
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))
Seccion_Diputados_2021 <- Seccion_Diputados_2021 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))


Seccion_Gubernatura_2013 <- Seccion_Gubernatura_2013 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.....ABST. = (100 - X..DE.PARTICIP.))
Seccion_Gubernatura_2019 <- Seccion_Gubernatura_2019 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))
Seccion_Gubernatura_2021 <- Seccion_Gubernatura_2021 %>%  
  mutate(X..DE.PARTICIP. = (TOTAL.VOTOS / LISTA.NOMINAL)*100) %>%
  mutate(X..DE.ABST. = (100 - X..DE.PARTICIP.))


# ====================================================================================================
# ====================================================================================================


# We are interested in a historical data table, to differentiate between elections
# we will add the year in the column name

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


# Every election the electoral grows in number of section taking in consideration population growth
# and urban expansion, because of it we will join our table using 2021 election as reference. Also
# since we are interested in alliance and leading party, party alliance combination will be deleted 

Ayuntamiento <- list(Seccion_Ayuntamiento_2021, Seccion_Ayuntamiento_2019, Seccion_Ayuntamiento_2016,
                     Seccion_Ayuntamiento_2013, Seccion_Ayuntamiento_2010) %>% 
  reduce(left_join, by = "SECCION")
colnames(Ayuntamiento)
Ayuntamiento[ ,c(22:29, 36:37, 48:58, 69:67, 84:95, 109:110)] <- list(NULL)
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


# Based on our historical data table, we build a table with the winning party by election
# and how many votes it got

colnames(Gubernatura)
Gub_2021 <- (4:16)
Gub_2019 <- (23:32)
Gub_2013 <- (39:41)

colnames(Diputados)
Dip_2021 <- (4:18)
Dip_2019 <- (25:36)
Dip_2016 <- (43:63)
Dip_2013 <- (70:72)
Dip_2010 <- (79:83)

colnames(Ayuntamiento)
Ayu_2021 <- (4:21)
Ayu_2019 <- (28:41)
Ayu_2016 <- (47:67)
Ayu_2013 <- (74:76)
Ayu_2010 <- (85:89)

Gubernatura_2021 <- colnames(Gubernatura[,Gub_2021])[max.col((Gubernatura[,Gub_2021]), ties.method = ("first"))]
Gubernatura_2019 <- colnames(Gubernatura[,Gub_2019])[max.col((Gubernatura[,Gub_2019]), ties.method = ("first"))]
Gubernatura_2013 <- colnames(Gubernatura[,Gub_2013])[max.col((Gubernatura[,Gub_2013]), ties.method = ("first"))]

Gubernatura_2021_votos <- apply(Gubernatura[Gub_2021], 1, max)
Gubernatura_2019_votos <- apply(Gubernatura[Gub_2019], 1, max)
Gubernatura_2013_votos <- apply(Gubernatura[Gub_2013], 1, max)

Diputados_2021 <- colnames(Diputados[,Dip_2021])[max.col((Diputados[,Dip_2021]), ties.method = ("first"))]
Diputados_2019 <- colnames(Diputados[,Dip_2019])[max.col((Diputados[,Dip_2019]), ties.method = ("first"))]
Diputados_2016 <- colnames(Diputados[,Dip_2016])[max.col((Diputados[,Dip_2016]), ties.method = ("first"))]
Diputados_2013 <- colnames(Diputados[,Dip_2013])[max.col((Diputados[,Dip_2013]), ties.method = ("first"))]
Diputados_2010 <- colnames(Diputados[,Dip_2010])[max.col((Diputados[,Dip_2010]), ties.method = ("first"))]

Diputados_2021_votos <- apply(Diputados[Dip_2021], 1, max)
Diputados_2019_votos <- apply(Diputados[Dip_2019], 1, max)
Diputados_2016_votos <- apply(Diputados[Dip_2016], 1, max)
Diputados_2013_votos <- apply(Diputados[Dip_2013], 1, max)
Diputados_2010_votos <- apply(Diputados[Dip_2010], 1, max)

Ayuntamiento_2021 <- colnames(Ayuntamiento[,Ayu_2021])[max.col((Ayuntamiento[,Ayu_2021]), ties.method = ("first"))]
Ayuntamiento_2019 <- colnames(Ayuntamiento[,Ayu_2019])[max.col((Ayuntamiento[,Ayu_2019]), ties.method = ("first"))]
Ayuntamiento_2016 <- colnames(Ayuntamiento[,Ayu_2016])[max.col((Ayuntamiento[,Ayu_2016]), ties.method = ("first"))]
Ayuntamiento_2013 <- colnames(Ayuntamiento[,Ayu_2013])[max.col((Ayuntamiento[,Ayu_2013]), ties.method = ("first"))]
Ayuntamiento_2010 <- colnames(Ayuntamiento[,Ayu_2010])[max.col((Ayuntamiento[,Ayu_2010]), ties.method = ("first"))]

Ayuntamiento_2021_votos <- apply(Ayuntamiento[Ayu_2021], 1, max)
Ayuntamiento_2019_votos <- apply(Ayuntamiento[Ayu_2019], 1, max)
Ayuntamiento_2016_votos <- apply(Ayuntamiento[Ayu_2016], 1, max)
Ayuntamiento_2013_votos <- apply(Ayuntamiento[Ayu_2013], 1, max)
Ayuntamiento_2010_votos <- apply(Ayuntamiento[Ayu_2010], 1, max)

Ganador_Gubernatura <- data.frame(Gubernatura[,1:3], 
                                  Gubernatura_2021, Gubernatura_2019, Gubernatura_2013,
                                  Gubernatura_2021_votos,Gubernatura_2019_votos, Gubernatura_2013_votos)

Ganador_Diputados <- data.frame(Diputados[,1:3], 
                                Diputados_2021, Diputados_2019, 
                                Diputados_2016, Diputados_2013, Diputados_2010, 
                                Diputados_2021_votos, Diputados_2019_votos,
                                Diputados_2016_votos, Diputados_2013_votos,
                                Diputados_2010_votos)

Ganador_Ayuntamiento <- data.frame(Ayuntamiento[,1:3], 
                                   Ayuntamiento_2021, Ayuntamiento_2019,
                                   Ayuntamiento_2016, Ayuntamiento_2013, Ayuntamiento_2010, 
                                   Ayuntamiento_2021_votos, Ayuntamiento_2019_votos, 
                                   Ayuntamiento_2016_votos,Ayuntamiento_2013_votos, 
                                   Ayuntamiento_2010_votos)

table(Ganador_Ayuntamiento$Ayuntamiento_2016, Ganador_Ayuntamiento$MUNICIPIO_2021)

# ====================================================================================================
# ====================================================================================================


# Now we will find how many vote would every party and alliance would have gotten
# using the NEW redistricting  

Resultados_Gubernatura_Distrito <- Gubernatura %>%
  group_by(DISTRITO_2021) %>%
  summarise(across(where(is.numeric), .f = (sum = sum), na.rm = TRUE))

Resultados_Diputados_Distrito <- Diputados %>%
  group_by(DISTRITO_2021) %>%
  summarise(across(where(is.numeric), .f = (sum = sum), na.rm = TRUE))

Resultados_Ayuntamiento_Distrito <- Ayuntamiento %>%
  group_by(DISTRITO_2021) %>%
  summarise(across(where(is.numeric), .f = (sum = sum), na.rm = TRUE))


# ====================================================================================================
# ====================================================================================================


# We proceed to create a summary table pointing out winning party by election and how many votes 
# would have gotten, again, with the new redistricting. To find Party with most wins (occurrence) 
# we must standardize deleting the election year that follows

Ganador_Gubernatura[,4:6] <- lapply(Ganador_Gubernatura[,4:6], function(x) sub("_\\d+$", "", x))
Ganador_Diputados[,4:8] <- lapply(Ganador_Diputados[,4:8], function(x) sub("_\\d+$", "", x))
Ganador_Ayuntamiento[,4:8] <- lapply(Ganador_Ayuntamiento[,4:8], function(x) sub("_\\d+$", "", x))


# ====================================================================================================
# ====================================================================================================


# We already stated that electoral map is expanding every election meaning that newer
# sections will have NAs in previous elections, for that we create "complete" tables

Ganador_Gubernatura_completo <- na.omit(Ganador_Gubernatura)
Ganador_Diputados_completo <- na.omit(Ganador_Diputados)
Ganador_Ayuntamiento_completo <- na.omit(Ganador_Ayuntamiento)

rownames(Ganador_Gubernatura_completo) <- NULL 
rownames(Ganador_Diputados_completo) <- NULL 
rownames(Ganador_Ayuntamiento_completo) <- NULL 

table(is.na(Ganador_Gubernatura_completo))


# ====================================================================================================
# ====================================================================================================

# Our main interest is mapping what section traditionally vote for certain party
# with Morena disruption, this can't be done by just obtain the most frequent winner
# by seccion, so we have to write an algorithm to define section historical vote preference

# In following table we we see how regardless of public position, Morena has
# an overwhelming victory in terms of won sections

table(Ganador_Ayuntamiento_completo$Ayuntamiento_2019)
table(Ganador_Diputados_completo$Diputados_2019)
table(Ganador_Gubernatura_completo$Gubernatura_2019)

colnames(Ganador_Ayuntamiento_completo)


#rows, columns
datos <- Ganador_Ayuntamiento_completo
T1 <- 4:5
T2 <- 6:8
output_T1 <- c()
output_T2 <- c()
wildcard = "Coalicion"

for (row in 1:nrow(datos)) {  

  ifelse(
    # Here we evaluate equality among values and use a wildcard
    ((datos[row,4] == datos[row,5]) | (datos[row,4] == wildcard)) & (datos[row,5] == datos[row,6]) & 
            (datos[row,6] == datos[row,7]) & (datos[row,7] == datos[row,8]),

          # if TRUE we assign Dominio + Party name to both outputs
          output_T1[row] <- output_T2[row] <- paste("Dominio", datos[row,5]),
          # if FALSE, we create two tables for our two periods of analysis
          {Tiempo1 = datos[,T1]; Tiempo2 = datos[,T2];
          
          # Knowing there isn't a dominating party, we check if there's a tendency or alternation
          # First period test
          ifelse( (Tiempo1[row,1] == Tiempo1[row,2]),
                  output_T1[row] <- (paste("Tendencia", Tiempo1[row,1])),
                  output_T1[row] <-("Alternancia")
                )
          # Second period test
          ifelse( (Tiempo2[row,1] == Tiempo2[row,2]) & (Tiempo2[row,2] == Tiempo2[row,3]),
                  output_T2[row] <- (paste("Tendencia", Tiempo2[row,1])),
                  output_T2[row] <- ("Alternancia")
                )
          }
  )
  }

table(output_T1)
table(output_T2)


# ====================================================================================================
# ====================================================================================================


# Before working with our district map, we will add our new data to our "Ganador" tables

Ganador_Ayuntamiento_completo$Perido_2019_21 <- output_T1
Ganador_Ayuntamiento_completo$Perido_2010_16 <- output_T2

# ====================================================================================================
# ====================================================================================================


# In www.plataformadetransparencia.org.mx we found a KML map from the 2013 State Election,
# we will convert it to SHP and add our working columns to plot maps as we prefer 

all_layers <- st_layers("Distritacion 2013.kml")
all_layers <- as.vector(all_layers$name)

Casillas <- data.frame()
Secciones <- data.frame()

elemento <- st_read("Distritacion 2013.kml", layer = "SECCION_0150.KML")
Casillas <- elemento[1,]
Secciones <- elemento[2,]

#for (i in all_layers) {

   elemento <- st_read("Distritacion 2013.kml", layer = i)

   Casillas <- rbind(Casillas, elemento[1,])
   Secciones <- rbind(Secciones, elemento[2,])
   Secciones$Name <- Casillas$Name

 }

# We make sure our SHP file is plotted as it has to and save them

st_drivers()

plot(Casillas)
str(Casillas)
Casillas$Description <- NULL
# st_write(Casillas, dsn = "Casillas", driver= "kml", "Casillas.shp")

plot(Secciones)
str(Secciones)
Secciones$Description <- NULL
# st_write(Secciones, dsn = "Secciones", driver= "ESRI Shapefile", "Secciones.shp")

# There are some inconsistencies in some section names, we proceed to fix it
# First, wee need to subtract "Casilla ... B,C" 

setwd("C:/Users/rmartinez/Desktop/Elecciones BC/Secciones")
Secciones <- st_read("Secciones.shp")
plot(Secciones)

# Polygons name comes as "Casilla #### B,C" we will subtract the section out of it 
correccion <- str_match(Secciones$Name, "Casilla \\s*(.*?)\\s*B,C")[,2]

# As standard practice, we check for NA and fix them if they appear
table(is.na(correccion))
which(is.na(correccion))
Secciones$Name[c(84, 85, 1724)]

# correccion variable will store correct seccion names
correccion[c(84, 85)] <- c(0217, 0218)
correccion[1724] <- "0150" # We found "0150" by checking adjacent rows

# Even when we have pulled the seccion number, we must ensure all value are 4 digit length

table(nchar(correccion))

mistakes_location  <- which(nchar(correccion) == 12)
correccion[c(mistakes_location)]
correccion[c(mistakes_location)] <- c("1943", "1945", "1946")
correccion[c(mistakes_location)]

which(nchar(correccion) != 4)

correccion <- gsub("\\s", "0", format(correccion,  justify = c("right"), width = 4))

# With correccion finally being correct, we replace
Secciones$Name <- correccion
colnames(Secciones)[1] <- "SECCION"

# ====================================================================================================
# ====================================================================================================

str(Secciones)
Secciones <- list(Secciones, Ganador_Ayuntamiento_completo) %>% 
  reduce(left_join, by = "SECCION")

Secciones <- na.omit(Secciones)
rownames(Secciones) <- NULL
table(Secciones$Perido_2019_21)

# Remember to change BOTH values
for (categorias in unique(Secciones$Perido_2019_21)) {
  print(categorias)
  pre_mapa <- subset(Secciones, Secciones$Perido_2019_21 == categorias)
  plotKML(pre_mapa, categorias, plot.labpt = FALSE)
}


