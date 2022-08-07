library(data.table)

# Our data source is Electoral Institute of Baja California: https://ieebc.mx/resultados-electorales/
# Data original format was .xlsx with a number of sheets, for personal convenience it was restructured in various single csv
# After uploading to github, our fist step is to load or data into different tables

BC_Ayuntamiento_2010 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntaminto_2010.csv")
BC_Ayuntamiento_2013 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntaminto_2013.csv")
BC_Ayuntamiento_2016 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntaminto_2016.csv")
BC_Ayuntamiento_2019 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntaminto_2019.csv")
BC_Ayuntamiento_2021 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Ayuntaminto_2021.csv")

BC_Diputados_2010 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2010.csv")
BC_Diputados_2013 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2013.csv")
BC_Diputados_2016 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2016.csv")
BC_Diputados_2019<- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2019.csv")
BC_Diputados_2021 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Diputados_2021.csv")

BC_Gubernatura_2013 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gubernatura_2013.csv")
BC_Gubernatura_2019 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gubernatura_2019.csv")
BC_Gubernatura_2021 <- fread("https://raw.githubusercontent.com/marloz24/marloz24-Elecciones-BC-2022/main/Resultados%20Campanas/BC_Gubernatura_2021.csv")

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

test <- BC_Gubernatura_2021
test[] <- sapply(BC_Gubernatura_2021[], as.numeric)

#Before formatting data we check that results match to those published by IEEBC 
aggregate(BC_Ayuntamiento_2010$CABC, by=list(Category=BC_Ayuntamiento_2010$MUNICIPIO), FUN=sum)
aggregate(BC_Ayuntamiento_2013$`UNIDOS POR BAJA CALIFORNIA`, by=list(Category=BC_Ayuntamiento_2013$MUNICIPIO), FUN=sum)