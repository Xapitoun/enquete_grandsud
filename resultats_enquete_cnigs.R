#chargement des librairies
library("leaflet")
library("rgdal")
library("maptools")
library("sp")
library("stringr")
library("rgeos")
#library("OpenStreetMap")
library("ggmap")
#configuration du dossier par défaut
setwd("~/Documents/jftardieu/enquete_grandsud/")

#fonction transformation enquete avec toutes caracteristiques approppriées
caracteristique <- function(base){
#chargement du fichier d’enquête
enquete_initial <- read.csv(base, header = TRUE, colClasses = c("adresse.localite"="character", "adresse.numero"="character", "adresse.rue"="character", "sous_type"="factor", "remarque"="character"))
enquete <- enquete_initial[-c(1:120),]

attach(enquete)
#conversion caractère en date
enquete$debut <- strptime((paste("05",str_sub(enquete$start, -20, -10),str_sub(enquete$start, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT") 
enquete$fin <- strptime((paste("05",str_sub(enquete$end, -20, -10),str_sub(enquete$end, -4, -1))),format = "%m %e %H:%M:%S %Y", tz = "GMT") 
#calcul intervalle remplissage formulaire
enquete$duree_reponse <- as.numeric(difftime(enquete$fin, enquete$debut, units = c("mins")))

#données qualitatives
##batiment
###Genreer
enquete$batiment.genre <- factor(enquete$batiment.genre, levels = c(1,2,3), labels = c("Homme","Femme", "Inconnu"))
###Mur
#enquete$batiment.mur <- factor(enquete$batiment.mur, levels = c(1,2,3,4,5,6), labels = c("Bloc / Béton","Bois", "Terre", "Tôle", "Carton / Plastique", "Autre"))

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

return(base)
}

caracteristique("~/Documents/jftardieu/enquete_grandsud/210517_0956_enquete.csv")
#controle qualité
#nombre de formulaires sans nom (99)
enquete$etablissement[enquete$etablissement == 99]
enquete$type[enquete$type = 14.0]

controle_enque <- enquete$zone_travail[which(enquete$zone_travail == "gd007")]
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
enqueteur <- enquete[enquete$zone_travail == "sd005",] 

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




# chargement données dans leaflet
enqueteur <- enquete[enquete$zone_travail == "np003",] 
m <- leaflet(sd005) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  #addPolygons(zones_enqu, lng, lat) %>%
  addMarkers(
    clusterOptions = markerClusterOptions(),
    lng = ~code_gps.Longitude , lat =~code_gps.Latitude, popup = paste(sd005$etablissement, sd005$type, sd005$sous_type_nom))
  
m

sd001 <- enquete[enquete$zone_travail == "sd001",]
nrow(enquete$type)


#zones_e <- readOGR(dsn = "~/Documents/jftardieu/zones_enquete/zonesenquete.shp") #imposssible à charger



poly_osm_initial <- readShapePoly("~/Documents/jftardieu/grand_sud_shp/planet_osm_polygon.shp")
poly_osm_enquete <- readShapePoly("~/Documents/jftardieu/zones_enquete/enquete_zone84/polygon_osm_zonesenquete84.shp")
#zone <- readOGR(dsn = "~/Documents/jftardieu/zones_enquete/zonesenquete.shp" )
#enquete120517$batiment.genre
#enquete <- enquete150517[-c()]


# Écriture du CSV
write.csv(enquete, file = "enquete200517_0957_retravaille.csv")

