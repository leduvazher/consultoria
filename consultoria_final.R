

#Paqueterias
if(!require(dplyr)) {install.packages("dplyr")}
library(dplyr)
if(!require(tidyr)) {install.packages("tidyr")}
library(tidyr)
if(!require(plyr)) {install.packages("plyr")}
library(plyr)


#Seleccionar carpeta
setwd("C:/Users/Eduardo/Documents/consultoria")

#Leer data de delitos federales
federal <- read.csv("federal.csv")
str(federal)
federal[is.na(federal)] <- 0


#Seleccionamos el año 2020 y hacemos un gather de la data

library(dplyr)
federal1 <- federal %>% filter(AÑO == 2020) %>%
         gather(key = "MES", value = "VALOR", 7:18)
summary(federal1)
str(federal1)
#Generamos 4 tablas para pegarlas

#Tabla 1
install.packages("dplyr")
library(dplyr)
contra_salud <- federal1 %>% group_by(ENTIDAD) %>% filter(CONCEPTO == "CONTRA LA SALUD") %>%
                             summarise(contra_salud = sum(VALOR))

#Tabla 2

servidores <- federal1 %>% filter(TIPO == "COMETIDOS POR SERVIDORES PUBLICOS") %>%
                          group_by(ENTIDAD) %>%
                          summarise(servidores_publicos = sum(VALOR))

#Tabla 3

electorales <- federal1 %>% filter(TIPO == "ELECTORALES") %>%
              group_by(ENTIDAD) %>%
              summarise(electorales = sum(VALOR))

#Tabla 4


contra_integridad <- federal1 %>% filter(TIPO == "CONTRA LA INTEGRIDAD CORPORAL") %>%
                group_by(ENTIDAD) %>%
                 summarise(contra_integridad = sum(VALOR))


#Table 5

patrimoniales <-    federal1 %>% filter(TIPO == "PATRIMONIALES") %>%
                     group_by(ENTIDAD) %>%
                      summarise(patrimoniales = sum(VALOR))


#Tabla 6

ambientales <-    federal1 %>% filter(TIPO == "CONTRA EL AMBIENTE Y LA GESTION AMBIENTAL") %>%
                    group_by(ENTIDAD) %>%
                    summarise(ambientales = sum(VALOR))



#Join tables para crear tabla Federal

library(plyr)
tabla_federales <- join_all(list(contra_salud, servidores, electorales,
                                 contra_integridad, patrimoniales, ambientales), by="ENTIDAD",
                                 type = "left")


###Estatal

setwd("C:/Users/Eduardo/Documents/consultoria")
estatal <- read.csv("estatal.csv")
str(estatal)
estatal[is.na(estatal)] <- 0

#Agrupamiento de datos
install.packages("dplyr")
library(dplyr)
estatal1 <- estatal %>% filter(Año == 2020) %>%
            tidyr::gather(key = "MES", value = "VALOR", 8:19)
estatal1$VALOR <- as.numeric(estatal1$VALOR)
summary(estatal1)

#write.csv(estatal1, "testestatal.csv")


#Tabla 1 Estatal - Homocidio doloso

install.packages("dplyr")
library(dplyr)

estatal_homicidio_doloso <- estatal1 %>% group_by(Entidad) %>% 
                            filter(Subtipo.de.delito == "Homicidio doloso") %>%
                            summarise(estatal_homicidio_doloso = sum(VALOR))


#Tabla 2 Estatal - Lesiones doloso

estatal_lesiones_dolosas <- estatal1 %>% group_by(Entidad) %>% 
                            filter(Subtipo.de.delito == "Lesiones dolosas") %>%
                            summarise(estatal_lesiones_dolosas = sum(VALOR))

#tabla 3 Estatal - Libertad personal

estatal_libertad_personal <- estatal1 %>% group_by(Entidad) %>% 
                            filter(Subtipo.de.delito == "Otros delitos que atentan contra la libertad personal") %>%
                             summarise(estatal_libertad_personal = sum(VALOR))

#tabla 4 Estatal - Medio ambiente

estatal_medio_ambiente <- estatal1 %>% group_by(Entidad) %>% 
                             filter(Subtipo.de.delito == "Contra el medio ambiente") %>%
                               summarise(estatal_medio_ambiente = sum(VALOR))

#tabla 5 Estatal - Servidores publicos

estatal_servidores_publicos <- estatal1 %>% group_by(Entidad) %>% 
                           filter(Subtipo.de.delito == "Delitos cometidos por servidores públicos") %>%
                            summarise(estatal_servidores_publicos = sum(VALOR))


#tabla 5 Estatal - Estatañ

estatal_electorales <- estatal1 %>% group_by(Entidad) %>% 
                                    filter(Subtipo.de.delito == "Electorales") %>%
                                    summarise(estatal_electorales = sum(VALOR))


#Tabla 6 Casa habiatacion


estatal_casahab <- estatal1 %>% group_by(Entidad) %>% 
                                    filter(Subtipo.de.delito == "Robo a casa habitación") %>%
                                    summarise(estatal_casahab = sum(VALOR))

#Tabla 7 Casa habiatacion

estatal_vehiculoauto <- estatal1 %>% group_by(Entidad) %>% 
                                     filter(Subtipo.de.delito == "Robo de vehículo automotor") %>%
                                     summarise(estatal_vehiculoauto = sum(VALOR))

#Tabla 8 autopartes

estatal_autopartes <- estatal1 %>% group_by(Entidad) %>% 
                                    filter(Subtipo.de.delito == "Robo de autopartes") %>%
                                    summarise(estatal_autopartes = sum(VALOR))                                  
  
#Tabla 9 Robo a transportista



estatal_robotransportista <- estatal1 %>% group_by(Entidad) %>% 
                                  filter(Subtipo.de.delito == "Robo a transportista") %>%
                                  summarise(estatal_robotransportista = sum(VALOR))                                  

#Tabla 10. Transeunte via publica


estatal_transeunte_viapub <- estatal1 %>% group_by(Entidad) %>% 
                                  filter(Subtipo.de.delito == "Robo a transeúnte en vía pública") %>%
                                  summarise(estatal_transeunte_viapub = sum(VALOR))                                  

#tABLA 11. Transeunte espacio abierto


estatal_transeunte_abiertopub <- estatal1 %>% group_by(Entidad) %>% 
                                  filter(Subtipo.de.delito == "Robo a transeúnte en espacio abierto al público") %>%
                                  summarise(estatal_transeunte_abiertopub = sum(VALOR))                        


#tABLA 11. transporte publico indiv


estatal_transpub_indiv <- estatal1 %>% group_by(Entidad) %>% 
                                   filter(Subtipo.de.delito == "Robo en transporte público individual") %>%
                                   summarise(estatal_transpub_indiv = sum(VALOR))   


#tABLA 11. transporte publico colec


estatal_transpub_colec <- estatal1 %>% group_by(Entidad) %>% 
                                   filter(Subtipo.de.delito == "Robo en transporte público colectivo") %>%
                                   summarise(estatal_transpub_colec = sum(VALOR))   



#tABLA 11. transporte indiv


estatal_trans_indv <- estatal1 %>% group_by(Entidad) %>% 
                                    filter(Subtipo.de.delito == "Robo en transporte individual") %>%
                                    summarise(estatal_trans_indv = sum(VALOR))   


#tABLA 11. robo inst bancaria


estatal_inst_banc <- estatal1 %>% group_by(Entidad) %>% 
                                     filter(Subtipo.de.delito == "Robo a institución bancaria") %>%
                                     summarise(estatal_inst_banc = sum(VALOR))  


#tABLA 11. robo negocio

estatal_robo_neg <- estatal1 %>% group_by(Entidad) %>% 
                                      filter(Subtipo.de.delito == "Robo a negocio") %>%
                                      summarise(estatal_robo_neg = sum(VALOR))  



#tABLA 11. robo ganado

estatal_robo_ganado <- estatal1 %>% group_by(Entidad) %>% 
                                       filter(Subtipo.de.delito == "Robo de ganado") %>%
                                       summarise(estatal_robo_ganado = sum(VALOR))  



#tABLA 11. robo marquinaria

estatal_robo_maquinaria <- estatal1 %>% group_by(Entidad) %>% 
                                      filter(Subtipo.de.delito == "Robo de maquinaria") %>%
                                      summarise(estatal_robo_maquinaria = sum(VALOR))  



#creamos la tabla estatal

library(plyr)


tabla_estatal <- join_all(list(estatal_homicidio_doloso, estatal_lesiones_dolosas, estatal_libertad_personal,
                               estatal_medio_ambiente, estatal_servidores_publicos, estatal_electorales,
                               estatal_casahab, estatal_vehiculoauto, estatal_autopartes, estatal_robotransportista,
                               estatal_transeunte_viapub, estatal_transeunte_abiertopub, estatal_transpub_indiv,
                               estatal_transpub_colec, estatal_trans_indv, estatal_inst_banc, estatal_robo_neg,
                               estatal_robo_ganado, estatal_robo_maquinaria), by="Entidad", type = "left")


###Tercera parte


setwd("C:/Users/Eduardo/Documents/consultoria")
comun <- read.csv("comun.csv")
str(comun)
comun[is.na(comun)] <- 0

#Agrupamiento de datos
install.packages("dplyr")
library(dplyr)
comun1 <- comun %>% filter(Año == 2020 & Sexo == c("Mujer", "No identificado")) %>%
                  tidyr::gather(key = "MES", value = "VALOR", 10:21)
comun1$VALOR <- as.numeric(comun1$VALOR)
summary(comun1)
str(comun1)

###Feminicidio

comun_feminicidio_muj <- comun1 %>% group_by(Entidad) %>% 
                            filter(Subtipo.de.delito == "Feminicidio") %>%
                              summarise(comun_feminicidio_muj = sum(VALOR))  


###aborto

comun_aborto_muj <- comun1 %>% group_by(Entidad) %>% 
                      filter(Subtipo.de.delito == "Aborto") %>%
                         summarise(comun_aborto_muj = sum(VALOR))  

###corrupcion de menores

comun_corrup_menores_muj <- comun1 %>% group_by(Entidad) %>% 
                         filter(Subtipo.de.delito == "Corrupción de menores") %>%
                         summarise(comun_corrup_menores = sum(VALOR))  

###extorsion

comun_extorsion_muj <- comun1 %>% group_by(Entidad) %>% 
                              filter(Subtipo.de.delito == "Extorsión") %>%
                               summarise(comun_extorsion = sum(VALOR))  

###Homicidio_culpso

comun_homculp_muj <- comun1 %>% group_by(Entidad) %>% 
                              filter(Subtipo.de.delito == "Homicidio culposo") %>%
                              summarise(comun_homculp_muj = sum(VALOR))  


###Homicidio Dolososo

comun_homdol_muj <- comun1 %>% group_by(Entidad) %>% 
                filter(Subtipo.de.delito == "Homicidio doloso") %>%
                  summarise(comun_homdol_muj = sum(VALOR))  


###Lesiones culposas

comun_lesculp_muj <- comun1 %>% group_by(Entidad) %>% 
                    filter(Subtipo.de.delito == "Lesiones culposas") %>%
                    summarise(comun_lesculp_muj = sum(VALOR))  


###comun_lesdol_muj

comun_lesdol_muj <- comun1 %>% group_by(Entidad) %>% 
                       filter(Subtipo.de.delito == "Lesiones dolosas") %>%
                       summarise(comun_lesdol_muj = sum(VALOR))  



###Vcomun_rapoto_muj

comun_rapoto_muj <- comun1 %>% group_by(Entidad) %>% 
                            filter(Subtipo.de.delito == "Rapto") %>%
                            summarise(comun_rapoto_muj = sum(VALOR))  


###comun_secuesto_muj

comun_secuesto_muj <- comun1 %>% group_by(Entidad) %>% 
                             filter(Subtipo.de.delito == "Secuestro") %>%
                             summarise(comun_secuesto_muj = sum(VALOR))  

###traf menores

comun_trafmen_muj <- comun1 %>% group_by(Entidad) %>% 
                               filter(Subtipo.de.delito == "Tráfico de menores") %>%
                               summarise(comun_trafmen_muj = sum(VALOR))  


###comun_trata_muj

comun_trata_muj <- comun1 %>% group_by(Entidad) %>% 
                                filter(Subtipo.de.delito == "Trata de personas") %>%
                                summarise(comun_trata_muj = sum(VALOR))  


##join

library(plyr)

tabla_comun <- join_all(list(comun_feminicidio_muj, comun_corrup_menores_muj, comun_extorsion_muj,
                               comun_homculp_muj, comun_homdol_muj, comun_lesculp_muj, comun_lesdol_muj,
                               comun_rapoto_muj, comun_secuesto_muj, comun_trafmen_muj, comun_trata_muj
                               ), by="Entidad", type = "left")


#### Join all

colnames(tabla_federales)[1] <- "Entidad"
tabla_federales$Entidad <- tolower(tabla_federales$Entidad)
tabla_estatal$Entidad <- tolower(tabla_estatal$Entidad)
tabla_comun$Entidad <- tolower(tabla_comun$Entidad)

final_dataset_1 <- left_join(tabla_estatal, tabla_comun, by="Entidad")
final_dataset_2 <- left_join(final_dataset_1, tabla_federales, by="Entidad")
final <- final_dataset_2

write.csv(final_dataset_1, "final_1.csv")
write.csv(tabla_federales, "federales.csv")
