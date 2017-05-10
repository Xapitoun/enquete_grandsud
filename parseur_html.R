library(XML)
library(plyr)
library(dplyr)




#get raw html for Lake Minnetonka
html.raw<-htmlTreeParse(
  'http://www.dnr.state.mn.us/lakefind/showreport.html?downum=27013300',
  useInternalNodes=T
)

#parse html using <p> tag    
html.parse<-xpathApply(html.raw, path="//p", fun=xmlValue)

#find elements that containt 'Depth'
robj.parse<-grep('*Depth*',unlist(html.parse),value=T)

#get correct element with maximum depth info
robj.parse<-robj.parse[[1]]

#use 'strsplit' to get actual value for maximum depth
depth.parse<-as.numeric(strsplit(strsplit(robj.parse,'ft): ')[[1]][2],'Water')[[1]][1])

#combine above code into a single function
depth.fun<-function(lake){
  
  url.in<-paste(
    'http://www.dnr.state.mn.us/lakefind/showreport.html?downum',
    lake,
    sep='='
  )
  
  html.raw<-htmlTreeParse(url.in,useInternalNodes=T)
  
  html.parse<-xpathApply(html.raw, path="//p", fun=xmlValue)
  
  robj.parse<-grep('*Depth*',unlist(html.parse),value=T)
  
  depth.parse<-as.numeric(strsplit(strsplit(robj.parse,'ft): ')[[1]][2],'Water')[[1]][1])
  
  return(depth.parse)
  
}

#examples showing how the function is used  
depth.fun('27013300')

lake.ids<-c('27013700','82004600','82010400')
sapply(lake.ids,depth.fun)


#francoisguillemfr
centre_sante2014 <- function(code) {
library(XML)
library(dplyr)

  #fonction hopital 2014
  #récupération données centre  
url.entree  <-paste(
      'http://sisnu.net:8081/cs/inst.php?c',
      code,
      sep='='
    )
    
    centre <- htmlParse(url.entree)  
  
#   centre <- 'http://sisnu.net:8081/cs/inst.php?c=2'
  
  


#récupération nom centre
nom <- xpathSApply(centre, "//div[contains(@class, 'alert alert-info')]/h1[@id='inst_title']", xmlValue, recursive = FALSE)
#infos de base
info <- xpathSApply(centre, "//table[contains(@class, 'table table-condensed')]/tr/td", xmlValue, recursive = FALSE)
#récupération code institution
code <- info[[10]]
#récupération coordonnées géographiques et administratives
coords <- info[[6]]
departement <- info[[2]]
commune <- info[[4]]
type <- info[[8]]

#fusion type admin
admin <- data.frame(nom, code, coords, departement, commune, type)
infodeb <- xpathSApply(centre, "//table[contains(@class, 'table table-condensed')]/tr/td", xmlValue)
#services
services <- xpathSApply(centre, "//div[contains(@id, 'tab-services')]/table/tr/td", xmlValue)
chirurgie_mineure <- as.factor(services[18])
laboratoire <- services[22]
medecine_generale <- services[26]
hospitalisation <- services[28]
planifi_familiale <- services[2]
soin_prenatal <- services[4]
soin_ptme <- services[6]
accouchement <- services[8]
malaria <- services[10]
tb <- services[12]
traitement_vih <- services[14]
noncomdiseases <- services[16]
cesarienne <- services[20]
transfusion_sanguine <- services[24]

#services_recup <- data.frame()
service_offert <- data.frame(chirurgie_mineure, laboratoire, medecine_generale, hospitalisation, planifi_familiale, soin_prenatal, soin_ptme, accouchement, malaria, tb, 
                             traitement_vih, noncomdiseases, cesarienne, transfusion_sanguine)
#infrastructures
infra <- xpathSApply(centre, "//div[contains(@id, 'tab-insfrast')]/table/tr/td", xmlValue)

#materiel et personnel à récupérer
service_24_24 <- as.factor(infra[6])
lit_dispo <- as.numeric(infra[2])
lit_maternite <- as.numeric(infra[8])
lit_accouchement <- as.numeric(infra[10])
ambulance <- as.numeric(infra[4])
medecin_generaliste <- as.numeric(infra[12])
medecin_specialiste <- as.numeric(infra[14])
medecin_anesthesiste <- as.numeric(infra[18])
sage_femme <- as.numeric(infra[24])
sage_femme_diplome <- as.numeric(infra[26])
infirmiere <- as.numeric(infra[20])
auxiliaire <- as.numeric(infra[22])
paramedical <- as.numeric(infra[16])
pharmacien <- as.numeric(infra[28])
technicien_pharmacie <- as.numeric(infra[30])
assistant_pharmacie <- as.numeric(infra[32])
laborantin <- as.numeric(infra[34])
laboratoire_technologiste <- as.numeric(infra[36])
laboratoire_technicien <- as.numeric(infra[38])
# fusion par type
personnel <- data.frame(medecin_generaliste, medecin_specialiste, medecin_anesthesiste, sage_femme, sage_femme_diplome, infirmiere, auxiliaire, paramedical)
pharmacie <- data.frame(pharmacien, technicien_pharmacie, assistant_pharmacie)
laboratoire <- data.frame(laborantin, laboratoire_technicien, laboratoire_technologiste)
lit <- data.frame(lit_dispo, lit_maternite, lit_accouchement)

#activités
activ <- xpathSApply(centre, "//div[contains(@id, 'tab-activites')]/table/tr/td", xmlValue)

##données à récupérer
vaccination_enfants <- activ[2]
pesee_enfants <- activ[4]
service_pediatrie <- activ[6]
maladie_sexuellement_transmissible <- activ[8]
groupe_sanguin <- activ[10]

activites <- data.frame(vaccination_enfants, pesee_enfants, service_pediatrie, maladie_sexuellement_transmissible, groupe_sanguin)
#colnames(coche) <- c("Laboratoire", "Soins_dentaires", "Pharmacie", "Soins_Intensifs", "Urgence", "Chirurgie", "ORL" , "Service_Assistance_psychologique", "ObGyn", "Pediatrie", "Salle_Operatoire_SOP", "Consultation_generale")

return(data.frame(admin,service_offert, service_24_24, ambulance, lit, personnel, pharmacie, laboratoire, activites))
}

#examples showing how the function is used  
#extracteur('1252', '1250')
#centre_sante2014('1')

code.ids<-c(1:875)
#code.ids <- c(5:10)
l <- lapply(code.ids,centre_sante2014)

sante2014 <- do.call(rbind.data.frame, l)
write.csv(sante2014, file = "~/Documents/haiti_administratif/sante/sante2014.csv")
#récupération url
#rose <- sprintf("http://sisnu.net:8081/cs/inst.php?c=%s", 1:3)

#assemblage données par hopital




#récupération url institution
#institution <- htmlParse('http://www.mspp.gouv.ht/cartographie/detail_institution.php?idInstitution=1250')

#fonction extraction données site carto mspp données cochées
extracteur <-function(sante) {
  library(XML)
  library(dplyr)  
url.in  <-paste(
    'http://www.mspp.gouv.ht/cartographie/detail_institution.php?idInstitution',
    sante,
    sep='='
  )

  institution <- htmlParse(url.in)
  #nom institution
  nom <- as.character(xpathSApply(institution, "//div[contains(@class, 'm')]/h1", xmlValue))
  # récupérations informations base institution
  info <- xpathSApply(institution, "//table[contains(@class, 'tablepopup')]/tr/td", xmlValue)
  ## infos
  code <- info[5]
  longitude <- info[19]
  latitude <- info[21]
  lit_adultes <- as.numeric(info[23])
  lit_enfants <- as.numeric(info[25])
  ## services
  cocher <-  xpathSApply(institution, "//table[contains(@class, 'tablepopup')]/tr/td/input", xmlGetAttr, name = "checked")
  coche <- t(cocher[1:12])
  colnames(coche) <- c("Laboratoire", "Soins_dentaires", "Pharmacie", "Soins_Intensifs", "Urgence", "Chirurgie", "ORL" , "Service_Assistance_psychologique", "ObGyn", "Pediatrie", "Salle_Operatoire_SOP", "Consultation_generale")
  return(data.frame(nom, code, longitude, latitude, lit_adultes, lit_enfants, coche))
}

#examples showing how the function is used  
extracteur('1252')

sante.ids<-c(785:1588)
l <- lapply(sante.ids,extracteur)

ls <- do.call(rbind.data.frame, l)

write.csv(ls, file = "~/Documents/haiti_administratif/sante/santecheck.csv")

names(l783)

data <- NULL
error <- NULL
for(l in extracteur) {
  tmp <- NULL
  try(tmp <- extracteur)
}
