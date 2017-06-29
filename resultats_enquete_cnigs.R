#fonction caracteristique formulaires
caracteristique <- function(base){
  #chargement des librairies
  library("sp")
  library("stringr")
  #chargement du fichier d’enquête
  enquete <- read.csv(base, header = TRUE, colClasses = c("adresse.localite"="character", "adresse.numero"="character", "adresse.rue"="character", "sous_type"="factor", "remarque"="character"))
  #enquete <- read.csv("~/Documents/jftardieu/enquete_grandsud/260617_1508_enquete.csv")
  enquete<- enquete[-c(1:65),]
  #sd005
  enquete <- enquete[-c(1180),]
  #sd004
  enquete$zone_travail[2863] <- "np004" 
  enquete$zone_travail[3437] <- "np004" 
  #sd003
  enquete <- enquete[-c(1434),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm
  enquete <- enquete[-c(1466),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm
  enquete <- enquete[-c(2332),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm
  #np002
  enquete$zone_travail[1194] <- "gd002"
  #sd001
  enquete <- enquete[-c(2467),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm OK
  enquete <- enquete[-c(615),]#suppression car données de enquete du vendredi 12 mai
  #sd002
  enquete$zone_travail[1627] <- "sd001" #OK
  #sd003
  enquete$zone_travail[3231] <- "sd009" #OK
  #sd002
  enquete$zone_travail[1733:1736] <- "np002" #OK
  enquete <- enquete[-c(1104),]#suppression pour cause coordonnées incoherentes par rapport formulaires suivants et précédents ok
  enquete <- enquete[-c(546),]#suppression pour cause coordonnées incoherentes par rapport formulaires suivants et précédents
  enquete <- enquete[-c(585),]#suppression pour cause coordonnées incoherentes tombe dans la mer
  enquete <- enquete[-c(1003:1008,1336),]#suppression pour cause coordonnées incoherentes tombe à Miami
  enquete <- enquete[-c(1088),]#suppression pour cause coordonnées incoherentes
  enquete <- enquete[-c(1075),]#suppression pour cause coordonnées incoherentes
  enquete <- enquete[-c(4594),]#suppression pour cause coordonnées incoherentes
  enquete$zone_travail[4543] <- "sd007"
  enquete <- enquete[-c(4538),]#suppression pour cause coordonnées incoherentes
  enquete$zone_travail[115] <- "sd005"
  enquete <- enquete[-c(2758),]#suppression pour cause coordonnées incoherentes
  enquete <- enquete[-c(345),]#suppression pour cause coordonnées incoherentes a recontroler ou mettre coordonnes sections comm
  enquete <- enquete[-c(611),]#suppression donnees jour de texte
  enquete$zone_travail[1906] <- "np003"
  enquete$zone_travail[351] <- "sd004"
  enquete$zone_travail[1786] <- "gd005"
  #a controler et verifier zone travail correspondante pour l’instant gd004
  #enquete$zone_travail[1224:1227] <- 
  enquete <- enquete[-c(3493),]#suppression pour cause coordonnées incoherentes zone de PAP
  enquete <- enquete[-c(989),]#suppression pour cause coordonnées incoherentes zone de PAP
  enquete$zone_travail[5165] <- "sd003"
  enquete <- enquete[-c(7938:7970,7973:7974,7976:7984,7986:7993),] #suppression pas de end, pas de longitude et latitude, sd011
  enquete <- enquete[c(7887:7889,7891:7893,7895:7932,7934:7937),] #suppression pas de end, pas de longitude et latitude, sd011
  
  
  #gd007 <- read.csv("~/Documents/jftardieu/enquete_grandsud/gd007/formulaires_briefcase/gd007_250517_0921.csv", header = TRUE, colClasses = c("adresse.localite"="character", "adresse.numero"="character", "adresse.rue"="character", "sous_type"="factor", "remarque"="character"))
  #correction date 
  
  #fusion enquete et gd007
  #enquete <- rbind(enquete, gd007)
  
  enquete$start <- sub("*May*","05", enquete$start, ignore.case = FALSE, fixed = FALSE) #remplacer nom mois mai par son numéro
  enquete$start <- sub("*Jun*","06", enquete$start, ignore.case = FALSE, fixed = FALSE) #remplacer nom mois juin par son numéro
  enquete$debut <- strptime((paste(str_sub(enquete$start, -23, -10),str_sub(enquete$start, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT")
  enquete$fin <- strptime((paste(str_sub(enquete$end, -23, -10),str_sub(enquete$end, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT")
  #variables dates
  enquete$date_debut <- strptime((paste(str_sub(enquete$start, -23, -19),str_sub(enquete$start, -4, -1))),format = "%m %e %Y", tz = "GMT")
  enquete$date_fin <- strptime((paste(str_sub(enquete$end, -23, -19),str_sub(enquete$end, -4, -1))),format = "%m %e %Y", tz = "GMT")
  
  #calcul intervalle remplissage formulaire
  enquete$duree_reponse <- as.numeric(difftime(enquete$fin, enquete$debut, units = c("mins")))
  #calcul intervalle jours travail
  
  #données qualitatives
  ##batiment
  ###Genre
  enquete$batiment.genre <- factor(enquete$batiment.genre, levels = c(1,2,3), labels = c("Homme","Femme", "Inconnu"))
  ###Mur
  enquete$batiment.mur <- factor(enquete$batiment.mur, levels = c(1,2,3,4,5,6), labels = c("Bloc / Béton","Bois", "Terre", "Tôle", "Carton / Plastique", "Autre"))
  
  ## ajout nom type et sous-type
  enquete$type <- factor(enquete$type, labels = c("1-Industrie / artisanat","2- Transport & énergie",	"3- Commerce (sauf marché public)",	"4- Service professionnel / Bureau privé",	"5- Santé",	"6- Éducation",	"7- Finance",	"8- Restauration (Hôtel /Restaurant / club…)",	"9- Religion / Loisirs / Culture",	"10- Services technologiques / télé-communication",	"11- Services publics (sauf éducation et santé)",	"12- Marché public",	"13- Établissements divers",	"14- Autre Établissement"))
  enquete$sous_type_nom <- factor(enquete$sous_type, 
                                  levels = c("1.1",	"1.2",	"1.3",	"1.4",	"1.5",	"1.6",	"1.7",	"1.8",	"1.9",	"1.1088",
                                             "2.1",	"2.2",	"2.3",	"2.4",	"2.5",	"2.6",	"2.7",	"2.8",	"2.9",
                                             "3.1",	"3.2",	"3.3",	"3.4",	"3.5",	"3.6",
                                             "4.1",	"4.2",	"4.3",	"4.4",	"4.5",
                                             "5.1",	"5.2",	"5.3",	"5.4",	"5.5",	"5.6",	"5.7",
                                             "6.0",
                                             "7.1",	"7.2",	"7.3",	"7.4",	"7.5",
                                             "8.1",	"8.2",	"8.3",	"8.4",
                                             "9.1",	"9.2",	"9.3",	"9.4",	"9.5",	"9.6",	"9.7",
                                             "10.1",	"10.2", "10.3",	"10.4",	"10.5",	"10.6",
                                             "11.0",	"12.0",	"13.1",	"13.2",	"13.3",	"13.4",	"13.5",	"13.6",	"13.7",	"14.0"),
                                  labels = c("1.1- Boulangerie",	"1.2- Guildive",	"1.3- Autre transformation agricole & alimentaire",	"1.4- Atelier d’ébénisterie",	"1.5- Ferronnerie",	"1.6- Artisanat utilitaire",	"1.7- Atelier d’art",	"1.8- Fabrication de matériel de construction",	"1.9- Traitement d’eau potable",	"1.10- Autre",
                                             "2.1  Aéroport",	"2.2  Piste d’atterrissage",	"2.3  Port / quai (wharf)",	"2.4  Gare routière",	"2.5  Station de bus",	"2.6  Station de tap-tap",	"2.7  Station de moto",	"2.8  Centrale électrique",	"2.9  Autre établissement transport / énergie",
                                             "3.1  Super-marché (market)"	,"3.2  Dépôt alimentaire",	"3.3  Quincaillerie",	"3.4  Boutique (détail seulement)",	"3.5  Station d’essence",	"3.6  Autre commerce",
                                             "4.1  Cabinet d’avocat","4.2 Notaire",	"4.3  Bureau d’association professionnelle",	"4.4  Coopérative",	"4.5  Autre bureau",
                                             "5.1  Dispensaire",	"5.2  Centre de santé",	"5.3  Hôpital","5.4  Clinique",	"5.5  Pharmacie",	"5.6  Laboratoire médical",	"5.7  Autre à préciser",
                                             "6.0- Établissement éducatif",
                                             "7.1- Succursale de banque",	"7.2- Caisse populaire",	"7.3- Autre institution de micro-crédit",	"7.4- Maison de transfert de fonds / Comptoir bancaire",	"7.5- Autre institution",
                                             "8.1- Hôtel",	"8.2- Club de danse / night club",	"8.3- Restaurant / bar",	"8.4- Autre",
                                             "9.1- Église",	"9.2- Temple vodou",	"9.3- Place publique",	"9.4- Musée","9.5- Salle de théatre / cinéma",	"9.6- Gaguerre",	"9.7- Autre établissement de loisir / culture",
                                             "10.1- Station de radio",	"10.2- Station de télévision",	"10.3- Station de radio & télévision",	"10.4- Téléphonie",	"10.5- Services informatiques / électronique",	"10.6- Autre services technologiques",
                                             "11.0- Services publics", 
                                             "12.0- Marché public",
                                             "13.1- Parloir funèbre",	"13.2- Morgue",	"13.3- Cimetière",	"13.4- Blanchisserie (dry cleaning)",	"13.5- Salon de beauté",	"13.6- Coiffeur",
                                             "13.7- Autre service",
                                             "14.0-Établissement non répertorié"))
  #ajout nom propriété
  enquete$batiment.propriete <- factor(enquete$batiment.propriete, labels = c("Publique",	"Privée",	"Communautaire /ONG / non lucratif",	"Coopérative",	"Religieux / Congréganiste"), levels = c(1,2,3,4,5))
  #Jours de marché
  enquete$marche.lundi <- factor(enquete$marche.lundi, labels = c("Principal", "Secondaire", "Fermé"), levels = c(1,2,3))
  enquete$marche.mardi <- factor(enquete$marche.mardi, labels = c("Principal", "Secondaire", "Fermé"), levels = c(1,2,3))
  enquete$marche.mercredi <- factor(enquete$marche.mercredi, labels = c("Principal", "Secondaire", "Fermé"), levels = c(1,2,3))
  enquete$marche.jeudi <- factor(enquete$marche.jeudi, labels = c("Principal", "Secondaire", "Fermé"), levels = c(1,2,3))
  enquete$marche.vendredi <- factor(enquete$marche.vendredi, labels = c("Principal", "Secondaire", "Fermé"), levels = c(1,2,3))
  enquete$marche.samedi <- factor(enquete$marche.samedi, labels = c("Principal", "Secondaire", "Fermé"), levels = c(1,2,3))
  enquete$marche.dimanche <- factor(enquete$marche.dimanche, labels = c("Principal", "Secondaire", "Fermé"), levels = c(1,2,3))
  #donnees geographiques
  enquete$coords <- as.character(paste(enquete$code_gps.Latitude, enquete$code_gps.Longitude, sep = ","))
  standard <- enquete[,c(3:11,14,15,63, 16:32,36:45,48:55, 58:61)]
  enquete <- enquete[,c("debut","date_debut","fin", "date_fin", "zone_travail", "donn_admin.departement", "donn_admin.commune", "donn_admin.section", "adresse.localite", "adresse.rue", "adresse.numero", "type", "sous_type_nom","etablissement", "batiment.genre", "batiment.mur", "batiment.toiture", "batiment.niveau", "batiment.bati", "batiment.propriete", "remarque",  "duree_reponse", "code_gps.Latitude", "code_gps.Longitude", "coords")]
  write.csv(enquete, "~/Documents/jftardieu/enquete_grandsud/retravaille_260617_1508.csv")
  
  write.csv(standard, "~/Documents/jftardieu/enquete_grandsud/standard_260617_1508.csv")
  
  return(base)
}

caracteristique("~/Documents/jftardieu/enquete_grandsud/260617_1508_enquete.csv")

enquete <- read.csv("~/Documents/jftardieu/enquete_grandsud/retravaille_260617_1508.csv")



essai <- function(formulaires){
#chargement des librairies
library("leaflet")
library("rgdal")
library("maptools")
library("sp")
library("stringr")
library("rgeos")
library("ggmap")
library("dplyr")
#configuration du dossier par défaut
setwd("~/Documents/jftardieu/enquete_grandsud/")

#fonction nettoyage données 
#test <- read.csv(formulaires)
test <- read.csv("~/Documents/jftardieu/enquete_grandsud/070617_1030_enquetecnigs.csv")
test<- test[-c(1:65),]
#sd005
test <- test[-c(1180),]
#sd004
test$zone_travail[2863] <- "np004" 
test$zone_travail[3437] <- "np004" 
#sd003
test <- test[-c(1434),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm
test <- test[-c(1466),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm
test <- test[-c(2332),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm
#np002
test$zone_travail[1194] <- "gd002"
#sd001
test <- test[-c(2467),]#suppression pour cause coordonnées incorrectes essager possibilite mettre coordonnes centre section comm OK
test <- test[-c(615),]#suppression car données de test du vendredi 12 mai
#sd002
test$zone_travail[1627] <- "sd001" #OK
#sd003
test$zone_travail[3231] <- "sd009" #OK
#sd002
test$zone_travail[1733:1736] <- "np002" #OK
test <- test[-c(1104),]#suppression pour cause coordonnées incoherentes par rapport formulaires suivants et précédents ok
test <- test[-c(546),]#suppression pour cause coordonnées incoherentes par rapport formulaires suivants et précédents
test <- test[-c(585),]#suppression pour cause coordonnées incoherentes tombe dans la mer
test <- test[-c(1003:1008,1336),]#suppression pour cause coordonnées incoherentes tombe à Miami
test <- test[-c(1088),]#suppression pour cause coordonnées incoherentes
test <- test[-c(1075),]#suppression pour cause coordonnées incoherentes
test <- test[-c(4594),]#suppression pour cause coordonnées incoherentes
test$zone_travail[4543] <- "sd007"
test <- test[-c(4538),]#suppression pour cause coordonnées incoherentes
test$zone_travail[115] <- "sd005"
test <- test[-c(2758),]#suppression pour cause coordonnées incoherentes
test <- test[-c(345),]#suppression pour cause coordonnées incoherentes a recontroler ou mettre coordonnes sections comm
test <- test[-c(611),]#suppression donnees jour de texte
test$zone_travail[1906] <- "np003"
test$zone_travail[351] <- "sd004"
test$zone_travail[1786] <- "gd005"
#a controler et verifier zone travail correspondante pour l’instant gd004
#test$zone_travail[1224:1227] <- 
test <- test[-c(3493),]#suppression pour cause coordonnées incoherentes zone de PAP
test <- test[-c(989),]#suppression pour cause coordonnées incoherentes zone de PAP
test$zone_travail[5165] <- "sd003"
gd007 <- read.csv("~/Documents/jftardieu/enquete_grandsud/gd007/formulaires_briefcase/gd007_250517_0921.csv")
test <- rbind(test, gd007)

test$start <- sub("*May*","05", test$start, ignore.case = FALSE, fixed = FALSE) #remplacer nom mois mai par son numéro
test$start <- sub("*Jun*","06", test$start, ignore.case = FALSE, fixed = FALSE) #remplacer nom mois juin par son numéro
test$debut <- strptime((paste(str_sub(test$start, -23, -10),str_sub(test$start, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT")
test$fin <- strptime((paste(str_sub(test$end, -23, -10),str_sub(test$end, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT")
#variables dates
test$date_debut <- strptime((paste(str_sub(test$start, -23, -19),str_sub(test$start, -4, -1))),format = "%m %e %Y", tz = "GMT")
test$date_fin <- strptime((paste(str_sub(test$end, -23, -19),str_sub(test$end, -4, -1))),format = "%m %e %Y", tz = "GMT")
#écriture nouveau data frame
write.csv(test, "~/Documents/jftardieu/enquete_grandsud/ts.csv")
return(test)

}
essai("~/Documents/jftardieu/enquete_grandsud/310517_1040_enquete.csv")

ts <- read.csv("~/Documents/jftardieu/enquete_grandsud/ts.csv")

#conversion caractère en date
test[grepl("May", test$start),] %>%
test$debut <- strptime((paste("05",str_sub(test$start, -20, -10),str_sub(test$start, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT") 
test$fin <- strptime((paste("05",str_sub(test$end, -20, -10),str_sub(test$end, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT")
#recodage fuseau horaire Haïti
#date
test$date_debut <- strptime((paste("05",str_sub(test$start, -20, -19),str_sub(test$start, -4, -1))),format = "%m %e %Y", tz = "GMT") 
test$date_fin <- strptime((paste("05",str_sub(test$end, -20, -19),str_sub(test$end, -4, -1))),format = "%m %e %Y", tz = "GMT")



test <- test[-c(),]#suppression pour cause coordonnées incoherentes
sd011 <- enquete[enquete$zone_travail == "sd011" & enquete$date_debut == "2017-05-15" ,c("etablissement", "debut", "date_debut", "coords", "fin")]
test[test$etablissement == "Église adventiste des coteaux",c("etablissement", "zone_travail", "code_gps.Latitude", "code_gps.Longitude", "donn_admin.section")]

test <- test[]

test[5895:5915, c("end", "zone_travail", "etablissement", "code_gps.Latitude", "code_gps.Longitude", "donn_admin.section")]
test[274, c("start", "end", "zone_travail", "etablissement", "code_gps.Latitude", "code_gps.Longitude", "donn_admin.section")]
#65 a retirer
match(test$etablissement )
plot(sd009$code_gps.Latitude ~ s$code_gps.Longitude, col = "red", cex = 1)
#test<- test[-c(495),]
sd009 <- test[which(start == Jun),c("etablissement", "zone_travail", "code_gps.Latitude", "code_gps.Longitude", "end")]

View(sd009)
library("dplyr")

test[test$etablissement == "99",]

filtre <- filter(test, code_gps.Lati)
filter(test, grepl("741", donn_admin.commune))
filtre[, c("etablissement", "code_gps.Latitude", "code_gps.Longitude")]


test[test$donn_admin.section == "1013-04","zone_travail"]

#calcul nombre jours travail
library("tidyverse")
group_by(test$end) 
  summarise(test$end)

test[2871,]


#controle qualité
#nombre de formulaires sans nom (99)
enquete$etablissement[enquete$etablissement == 99]
enquete$type[enquete$type = 14.0]

controle_enque <- sd009$zone_travail[which(sd009$zone_travail == "sd009")]
#focnction analyse donnees enqueteur
travail<-function(enqueteur){
  
# choix zones de travail
  enqueteur <- enquete[enquete$zone_travail == enqueteur,c("debut", "fin", "duree_reponse","zone_travail", "donn_admin.section", "adresse.localite", "adresse.numero", "adresse.rue", "type", "sous_type_nom", "etablissement", "batiment.propriete", "remarque", "code_gps.Longitude", "code_gps.Latitude")]
  #zones_trav <- zones[zones$travail == enqueteur,] #filtre par zone d’enquete
  attach(enqueteur)
  #enqueteur <- enqueteur[,c("debut", "fin", "duree_reponse","zone_travail", "donn_admin.section", "adresse.localite", "adresse.numero", "adresse.rue", "type", "sous_type_nom", "etablissement", "batiment.propriete", "remarque", "code_gps.Longitude", "code_gps.Latitude")]
  
  
  plot(zones_trav)
  points(enqueteur$code_gps.Latitude ~ enqueteur$code_gps.Longitude, col = "red", cex = 1)
  
  #over(enqueteur, zones_trav)
  View(enqueteur)
 
  
  return(enqueteur)

}


#examples showing how the function is used  
travail('sd001')

#controle qualité
#cartographique



#controles des données récoltéees
##nom établissement et type et sous-type
nom <- enquete[enquete$duree_reponse >= 15, c("zone_travail", "duree_reponse", "fin","debut", "etablissement", "type", "sous_type_nom" )]
hist(nom$duree_reponse)
#chargement des zones d’enquête
#routes_osm <- readShapeLines("~/Documents/jftardieu/grand_sud_shp/planet_osm_line.shp")
#r <- readOGR("~/Documents/jftardieu/grand_sud_shp/planet_osm_line.shp")
zones <- readShapePoly("~/Documents/jftardieu/zones_enquete/enquete_zone84/zones_enquete84.shp")
zones$travail <- tolower(zones$Code_enque) #code enqueteur en minuscules
zones_trav <- zones[zones$travail == "sd005",] #filtre par zone d’enquete
enqueteur <- enquete[enquete$zone_travail == "sd007",] 

CRSojb <- CRS("+proj=longlat +datum=WGS84 +no_defs") #scr de référence
#enqueteur@proj4string <- CRSojb 
zones_trav@proj4string <- CRSojb
coordinates(enqueteur) <- ~ code_gps.Longitude + code_gps.Latitude #définition coordonnées « enqueteur »
proj4string(enqueteur) <- proj4string(zones_trav) #mise en place scr identique deux jeux données
#point dans polygone
#sd <- over(enqueteur, zones_trav)
over <-over(enqueteur, zones_trav)
# carte point dans polygone
plot(zones_trav)
points(enqueteur$code_gps.Latitude ~ enqueteur$code_gps.Longitude, col = "red", cex = 1)


e240517 <- read.csv("~/Documents/jftardieu/enquete_grandsud/240517_1046_retravaille.csv")
e <- e240517[e240517$zone_travail == "gd003", ]
# chargement données dans leaflet

t <- leaflet(s) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #addPolygons(zones_enqu, lng, lat) %>%
  addMarkers(
    clusterOptions = markerClusterOptions(),
    lng = ~code_gps.Longitude , lat =~code_gps.Latitude, popup = paste(s$etablissement, s$type, s$sous_type_nom))
  
t

sd001 <- enquete[enquete$zone_travail == "sd007",]
nrow(enquete$type)


#zones_e <- readOGR(dsn = "~/Documents/jftardieu/zones_enquete/zonesenquete.shp") #imposssible à charger



poly_osm_initial <- readShapePoly("~/Documents/jftardieu/grand_sud_shp/planet_osm_polygon.shp")
poly_osm_enquete <- readShapePoly("~/Documents/jftardieu/zones_enquete/enquete_zone84/polygon_osm_zonesenquete84.shp")
#zone <- readOGR(dsn = "~/Documents/jftardieu/zones_enquete/zonesenquete.shp" )
#enquete120517$batiment.genre
#enquete <- enquete150517[-c()]


# Écriture du CSV
write.csv(enquete, file = "~/Documents/jftardieu/enquete_grandsud/enquete210517_2313retravaille.csv")
nettoye <- read.csv("~/Documents/jftardieu/enquete_grandsud/retravaille_310517_1040.csv")

nettoye <- nettoye[nettoye$date_debut <= "2017-05-12"]
table(nettoye$date_debut)
#controler les coordonnées duplioquées
distinct(nettoye, code_gps.Latitude)
distinct(nettoye, code_gps.Longitude)
distinct(nettoye, code_gps.Longitude, code_gps.Latitude)
