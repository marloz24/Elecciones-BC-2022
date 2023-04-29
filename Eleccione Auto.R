
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
  helper <- as.vector(votos["Total_Votos"][1,])
  votos[3,] <-  ( votos[1,]/ t(helper) ) * 100 
  
  votos[1,] <- round(votos[1,],0)
  votos[2:3,] <- round(votos[2:3,],2)
  
  return(votos)
})

sum(HISTORICOS$Ayuntamiento$PRI_2010, na.rm = TRUE)

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


