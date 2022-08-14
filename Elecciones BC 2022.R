library(data.table)

# Our data source is Electoral Institute of Baja California: https://ieebc.mx/resultados-electorales/
# Data original format was .xlsx with a number of sheets, for personal convenience it was restructured in various single csv
# After uploading to github, our fist step is to load or data into different tables

BC_Ayuntamiento_2010 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2010.csv")
BC_Ayuntamiento_2013 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2013.csv")
BC_Ayuntamiento_2016 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2016.csv")
BC_Ayuntamiento_2019 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2019.csv")
BC_Ayuntamiento_2021 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntamiento_2021.csv")

BC_Diputados_2010 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2010.csv")
BC_Diputados_2013 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2013.csv")
BC_Diputados_2016 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2016.csv")
BC_Diputados_2019 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2019.csv")
BC_Diputados_2021 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2021.csv")

BC_Gubernatura_2013 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gobernatura_2013.csv")
BC_Gubernatura_2019 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gobernatura_2019.csv")
BC_Gubernatura_2021 <- fread("https://raw.githubusercontent.com/marloz24/Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gobernatura_2021.csv")


#We notice abstention and participation are char variable because of % replace and convert to numeric
BC_Ayuntamiento_2010 <- data.frame(lapply(BC_Ayuntamiento_2010, function(x) gsub("%", "", x)))
BC_Ayuntamiento_2013 <- data.frame(lapply(BC_Ayuntamiento_2013, function(x) gsub("%", "", x)))
BC_Ayuntamiento_2016 <- data.frame(lapply(BC_Ayuntamiento_2016, function(x) gsub("%", "", x)))
BC_Ayuntamiento_2019 <- data.frame(lapply(BC_Ayuntamiento_2019, function(x) gsub("%", "", x)))
BC_Ayuntamiento_2021 <- data.frame(lapply(BC_Ayuntamiento_2021, function(x) gsub("%", "", x)))

BC_Diputados_2010 <- data.frame(lapply(BC_Diputados_2010, function(x) gsub("%", "", x)))
BC_Diputados_2013 <- data.frame(lapply(BC_Diputados_2013, function(x) gsub("%", "", x)))
BC_Diputados_2016 <- data.frame(lapply(BC_Diputados_2016, function(x) gsub("%", "", x)))
BC_Diputados_2019 <- data.frame(lapply(BC_Diputados_2019, function(x) gsub("%", "", x)))
BC_Diputados_2021 <- data.frame(lapply(BC_Diputados_2021, function(x) gsub("%", "", x)))

BC_Gubernatura_2013 <- data.frame(lapply(BC_Gubernatura_2013, function(x) gsub("%", "", x)))
BC_Gubernatura_2019 <- data.frame(lapply(BC_Gubernatura_2019, function(x) gsub("%", "", x)))
BC_Gubernatura_2021 <- data.frame(lapply(BC_Gubernatura_2021, function(x) gsub("%", "", x)))

BC_Ayuntamiento_2010[,5:14] <- sapply(BC_Ayuntamiento_2010[,5:14], as.numeric)
BC_Ayuntamiento_2013[,5:13] <- sapply(BC_Ayuntamiento_2013[,5:13], as.numeric)
BC_Ayuntamiento_2016[,5:42] <- sapply(BC_Ayuntamiento_2016[,5:42], as.numeric)
BC_Ayuntamiento_2019[,5:34] <- sapply(BC_Ayuntamiento_2019[,5:34], as.numeric)
BC_Ayuntamiento_2021[,5:34] <- sapply(BC_Ayuntamiento_2021[,5:34], as.numeric)

BC_Diputados_2010[,5:16] <- sapply(BC_Diputados_2010[,5:16], as.numeric)
BC_Diputados_2013[,5:13] <- sapply(BC_Diputados_2013[,5:13], as.numeric)
BC_Diputados_2016[,5:41] <- sapply(BC_Diputados_2016[,5:41], as.numeric)
BC_Diputados_2019[,5:32] <- sapply(BC_Diputados_2019[,5:32], as.numeric)
BC_Diputados_2019[,5:31] <- sapply(BC_Diputados_2019[,5:31], as.numeric)

BC_Gubernatura_2013[,5:13] <- sapply(BC_Gubernatura_2013[,5:13], as.numeric)
BC_Gubernatura_2019[,5:30] <- sapply(BC_Gubernatura_2019[,5:30], as.numeric)
BC_Gubernatura_2021[,5:29] <- sapply(BC_Gubernatura_2021[,5:29], as.numeric)

# A few NAs have arised, we will convert them to zeros
BC_Ayuntamiento_2013[is.na(BC_Ayuntamiento_2013)] <- 0
BC_Ayuntamiento_2016[is.na(BC_Ayuntamiento_2013)] <- 0
BC_Diputados_2013[is.na(BC_Diputados_2016)] <- 0

#
colnames(BC_Ayuntamiento_2010)
colnames(BC_Diputados_2010)
BC_Diputados_2010 <- subset(BC_Diputados_2010, select=-c(V15, V16))
setnames(BC_Ayuntamiento_2010, old = c("CABC", "CGR", "CRBC"), new = c("PAN", "PRI", "PT"))
setnames(BC_Diputados_2010, old = c("CABC", "CGR", "CRBC"), new = c("PAN", "PRI", "PT"))

colnames(BC_Ayuntamiento_2013)
colnames(BC_Diputados_2013)
colnames(BC_Gubernatura_2013)
setnames(BC_Ayuntamiento_2013, old = c("UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("Pre - PAN", "Pre - PRI"))
setnames(BC_Diputados_2013, old = c("UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("Pre - PAN", "Pre - PRI"))
setnames(BC_Gubernatura_2013, old = c("UNIDOS.POR.BAJA.CALIFORNIA", "COMPROMISO.POR.BAJA.CALIFORNIA"), 
         new = c("Pre - PAN", "Pre - PRI"))

colnames(BC_Ayuntamiento_2016)
colnames(BC_Diputados_2016)
setnames(BC_Ayuntamiento_2016, old = c("PAN", "PRI", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8","C9", "C10", "C11"), 
         new = c("Pre - PAN", "Pre - PRI", rep(c("Pre - PRI"),each=11)))
setnames(BC_Diputados_2016, old = c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8","C9", "C10", "C11"), 
         new = c("Pre - PAN", "Pre - PRI", rep(c("Pre - PRI"),each=11)))


colnames(BC_Ayuntamiento_2019)
colnames(BC_Diputados_2019)
colnames(BC_Gubernatura_2019)
setnames(BC_Ayuntamiento_2019, old = c("PAN", "PRI", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8","C9", "C10", "C11"), 
         new = c("Pre - PAN", "Pre - PRI", rep(c("Pre - Morena"),each=11)))
setnames(BC_Diputados_2019, old = c("PAN", "PRI","C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8","C9", "C10", "C11"), 
         new = c("Pre - PAN", "Pre - PRI", rep(c("Pre - Morena"),each=11)))
setnames(BC_Gubernatura_2019, old = c("PAN", "PRI","C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8","C9", "C10", "C11"), 
         new = c("Pre - PAN", "Pre - PRI", rep(c("Pre - Morena"),each=11)))

colnames(BC_Ayuntamiento_2021)
colnames(BC_Diputados_2021)
colnames(BC_Gubernatura_2021)
setnames(BC_Ayuntamiento_2021, old = c("PAN", "PRI", "PRD", "PT", "PVEM", "PBC", "MC", "MORENA", "PES", "RSP", 
                                       "FXM", "PAN...PRI...PRD", "PAN...PRI", "PAN...PRD", "PRI.PRD", 
                                       "PT.PVEM.MORENA", "PT.PVEM", "PT...MORENA", "PVEM...MORENA"), 
         new = c("Pre - PAN", "Pre - PRI", "Pre - PRD", "Pre - PT", "Pre - PVEM", 
                 "Pre - PBC", "Pre - MC", "Pre - Morena", "Pre - PES", "Pre - RSP", "Pre - FXM",
                 rep("Pre - Coalicion", each = 8)))
setnames(BC_Diputados_2021, old = c("PAN", "PRI", "PRD", "PT", "PVEM", "PBC", "MC", "MORENA", "PES", "RSP", 
                                       "FXM", "PAN...PRI...PRD", "PAN...PRI", "PAN...PRD", "PRI.PRD", 
                                       "PT.PVEM.MORENA", "PT.PVEM", "PT...MORENA", "PVEM...MORENA"), 
         new = c("Pre - PAN", "Pre - PRI", "Pre - PRD", "Pre - PT", "Pre - PVEM", 
                 "Pre - PBC", "Pre - MC", "Pre - Morena", "Pre - PES", "Pre - RSP", "Pre - FXM",
                 rep("Pre - Coalicion", each = 8)))
setnames(BC_Gubernatura_2021, old = c("PAN", "PRI", "PRD", "PT", "PVEM", "PBC", "MC", "MORENA", "PES", "RSP", 
                                       "FXM", "PAN...PRI...PRD", "PAN...PRI", "PAN...PRD", "PRI.PRD", 
                                       "PT.PVEM.MORENA", "PT.PVEM", "PT...MORENA", "PVEM...MORENA"), 
         new = c("Pre - PAN", "Pre - PRI", "Pre - PRD", "Pre - PT", "Pre - PVEM", 
                 "Pre - PBC", "Pre - MC", "Pre - Morena", "Pre - PES", "Pre - RSP", "Pre - FXM",
                 rep("Pre - Coalicion", each = 8)))


#Before formatting data we check that results match to those published by IEEBC 
aggregate(BC_Ayuntamiento_2010$CABC, by=list(Category = BC_Ayuntamiento_2010$MUNICIPIO), FUN=sum)
aggregate(BC_Diputados_2010$CABC, by=list(Category=BC_Diputados_2010$MUNICIPIO), FUN=sum)