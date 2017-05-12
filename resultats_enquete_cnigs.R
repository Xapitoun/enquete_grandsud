#chargemen des librairies
library("leaflet")
#configuration du dossier par défaut
setwd("~/Documents/jftardieu/enquete_grandsud/")

#chargement du fichier d’enquête
enquete120517 <- read.csv("120517_1313_enquete.csv")
enquete120517$batiment.genre


#données qualitatives
##batiment
###Genre
enquete120517$batiment.genre <- factor(enquete120517$batiment.genre, levels = c(1,2,3), labels = c("Homme","Femme", "Inconnu"))
###Mur
enquete120517$batiment.mur <- factor(enquete120517$batiment.mur, levels = c(1,2,3,4,5,6), labels = c("Bloc / Béton","Bois", "Terre", "Tôle", "Carton / Plastique", "Autre"))

##type
enquete120517$type <- factor(enquete120517$type, labels = c("1Industrie / artisanat	","2- Transport & énergie",	"3- Commerce (sauf marché public)",	"4- Service professionnel / Bureau privé",	"5- Santé",	"6- Éducation",	"7- Finance",	"8- Restauration (Hôtel /Restaurant / club…)",	"9- Religion / Loisirs / Culture",	"10- Services technologiques / télé-communication",	"11- Services publics (sauf éducation et santé)",	"12- Marché public",	"13- Établissements divers",	"14- Autre Établissement"
))
enquete120517$batiment.genre


# chargement données dans leaflet
m <- leaflet(enquete120517) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(
    clusterOptions = markerClusterOptions(),
    lng = ~code_gps.Longitude , lat =~code_gps.Latitude, popup = ~as.character(etablissement))
m