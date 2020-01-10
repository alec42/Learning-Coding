#workflow 100% sf 

# trafic data here
#https://www.donneesquebec.ca/recherche/fr/dataset/debits-de-circulation-transports-quebec/resource/9de14998-2e3b-4936-a587-2da4f3ddd3af

library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(viridis)
library(htmlwidgets)
library(readr)
library(ggmap)
library(stringr)
library(forcats)
#download and import shapefle
download.file("http://ws.mapserver.transports.gouv.qc.ca/donnees/geomatique/cir_v_geo_sectn_trafc_locls.zip", destfile= "./data/cir_v_geo_sectn_trafc_locls.zip")
utils::unzip("./data/cir_v_geo_sectn_trafc_locls.zip", exdir="./data")

sfdf<- st_read(
  "./data/cir_v_geo_sectn_trafc_locls.shp", 
  stringsAsFactors = FALSE)  %>%
  mutate(djma = as.numeric(djma),
         djme = as.numeric(djme),
         djmh = as.numeric(djmh)) %>%
  st_transform(., "+proj=longlat +datum=WGS84") 

## plot trafic volume variables
#DJMA (débit journalier moyen annuel)  -- annual average daily  traffic volume
#DJME (débit journalier moyen estival --  summer average daily traffic volume (juin, juillet, août, septembre)) 
# DJMH (débit journalier moyen hivernal -- winter average daily traffic volume (décembre, janvier, février, mars)) 
sfdf %>%   plot_ly(x =~ djma)
sfdf %>%   plot_ly(x =~ djme)
sfdf %>%   plot_ly(x =~ djmh)


## tous les polygons sont distincts
#sfdf %>% summarise(count = n_distinct(geometry))
#define palette
#mypal <- colorNumeric(palette = viridis_pal(option="C"), 
                      #domain = sfdf$djma)


mypal <- colorNumeric(palette = viridis(5), 
                      domain = sfdf$djma)

map_leaflet <- sfdf %>%leaflet()%>%   
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolylines(color = ~ mypal(djma), 
             label = ~ paste0(djma)) %>%
  addLegend("bottomleft",
            pal = mypal,
            values = ~ djma,
            title = "djma")

map_leaflet
#workaround to save widget in differente folder
f<-"results\\mapleaflet.html"
saveWidget(map_leaflet,file.path(normalizePath(dirname(f)),basename(f)), selfcontained = F)


#########################
# download accident reports
# https://www.donneesquebec.ca/recherche/fr/dataset/rapports-d-accident
#########################

download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2016.csv", destfile= "./data/rapports-accident-2016.csv")
download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2015.csv", destfile= "./data/rapports-accident-2015.csv")
download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2014.csv", destfile= "./data/rapports-accident-2014.csv")
download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2013.csv", destfile= "./data/rapports-accident-2013.csv")
download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2012.csv", destfile= "./data/rapports-accident-2012.csv")
download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2011.csv", destfile= "./data/rapports-accident-2011.csv")




accidents16 <- read_csv("./data/rapports-accident-2016.csv") %>%
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN))
accidents15 <- read_csv("./data/rapports-accident-2015.csv")%>%
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN))
accidents14 <- read_csv("./data/rapports-accident-2014.csv")%>%
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN))
accidents13 <- read_csv("./data/rapports-accident-2013.csv")%>%
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN))
accidents12 <- read_csv("./data/rapports-accident-2012.csv")%>%
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN))
accidents11 <- read_csv("./data/rapports-accident-2011.csv")%>%
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN))



#(https://www.mamrot.gouv.qc.ca/recherche-avancee/fiche/municipalite/).
# convert municipalite code to municipalite name
code_to_mun <- read_tsv("./data/code_to_mun.tsv")
# batch geocoding
# https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/
# geocode tidyverse 
# https://andrewbtran.github.io/NICAR/2017/maps/mapping-census-data.html
testdata <- accidents %>% 
  left_join(code_to_mun %>% select(CD_MUNCP, NAME_MUNCP), by= "CD_MUNCP") %>% 
  filter(CD_MUNCP == "10043") %>% #rimouski
  slice(1:20) %>%
  mutate(location = str_c( 
                           ifelse(is.na(RUE_ACCDN), str_replace_na(BORNE_KM_ACCDN, ""), str_replace_na(as.numeric(NO_CIVIQ_ACCDN), "")) ,
                           ifelse(is.na(RUE_ACCDN), str_replace_na(NO_ROUTE, ""), str_replace_na(RUE_ACCDN, "")) ,
                           ifelse(is.na(ACCDN_PRES_DE), "", str_c(" & ", ACCDN_PRES_DE)), # & to signal intersection?
                           ", ",
                           str_replace_na(NAME_MUNCP,""),
                           ", QC, Canada",
                           sep = " ") %>%
                    str_replace("FACE AU", "") %>%  # "in front of"
                    str_replace("FACE", "") ) # "facing"
  
geo <-ggmap::geocode(location = testdata$location, output = "latlon", source= "google")
testdata$lon <- geo$lon
testdata$lat <- geo$lat

#convert lat long to sf
pouet <-  st_as_sf(testdata, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# map new sf
pouet %>% leaflet()%>%   
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addMarkers(label = ~ paste0(location))

### on va jouer avec les vélos
library(lubridate)
velo <- bind_rows(accidents11,accidents12, accidents13, 
                  accidents14, accidents15, accidents16)  %>% 
  filter(nb_bicyclette > 0) %>% 
  left_join(code_to_mun %>% select(CD_MUNCP, NAME_MUNCP), by= "CD_MUNCP") %>%
  filter(NAME_MUNCP == "Québec") %>%
  mutate(year = isoyear(DT_ACCDN),
         week = isoweek(DT_ACCDN),
         month = month(DT_ACCDN),
         monday = floor_date(DT_ACCDN, unit = "week") ) %>%
  arrange(DT_ACCDN)  %>%
  mutate(gravite = as.factor(gravite) %>% fct_relevel("Dommages matériels seulement", "Léger", "Grave", "Mortel")) 

velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bN-D\\b", "NOTRE-DAME") # attention rouler ceci avant NORD car le - coupe le mot
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bBD\\b", "BOULEVARD")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bCH\\b", "CHEMIN")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bMT\\b", "MONT")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bAV\\b", "AVENUE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bN\\b", "NORD")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bS\\b", "SUD")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bE\\b", "EST")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bO\\b", "OUEST")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bST\\b", "SAINT")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bSTE\\b", "SAINTE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bRTE\\b", "ROUTE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bTSSE\\b", "TERRASSE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bGDE\\b", "GRANDE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bAUT\\b", "AUTOROUTE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bPTE\\b", "POINTE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bPRDEE\\b", "PROMENADE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bAL\\b", "ALLÉE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bPL\\b", "PLACE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bCT\\b", "CÔTE")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bMGR\\b", "MONSEIGNEUR")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bQ BOURG\\b", "QUATRE-BOURGEOIS")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bQBOURG\\b", "QUATRE-BOURGEOIS")
velo$RUE_ACCDN <- str_replace(velo$RUE_ACCDN, "\\bW PELLETIER\\b", "WILFRID-PELLETIER")


velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bN-D\\b", "NOTRE-DAME") # attention rouler ceci avant NORD car le - coupe le mot
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bBD\\b", "BOULEVARD")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bCH\\b", "CHEMIN")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bMT\\b", "MONT")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bAV\\b", "AVENUE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bN\\b", "NORD")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bS\\b", "SUD")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bE\\b", "EST")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bO\\b", "OUEST")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bST\\b", "SAINT")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bSTE\\b", "SAINTE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bRTE\\b", "ROUTE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bTSSE\\b", "TERRASSE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bGDE\\b", "GRANDE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bAUT\\b", "AUTOROUTE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bPTE\\b", "POINTE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bPRDEE\\b", "PROMENADE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bAL\\b", "ALLÉE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bPL\\b", "PLACE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bCT\\b", "CÔTE")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bMGR\\b", "MONSEIGNEUR")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bQ BOURG\\b", "QUATRE-BOURGEOIS")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bQBOURG\\b", "QUATRE-BOURGEOIS")
velo$ACCDN_PRES_DE <- str_replace(velo$ACCDN_PRES_DE, "\\bW PELLETIER\\b", "WILFRID-PELLETIER")


velo <- velo %>%
  mutate(location =
           case_when(
             !is.na(NO_CIVIQ_ACCDN) & !is.na(RUE_ACCDN) ~ 
               str_c(str_replace_na(as.numeric(NO_CIVIQ_ACCDN), ""),
                     " ",
                     str_replace_na(RUE_ACCDN, "")," ",
                     str_replace_na(NAME_MUNCP,""), ", QC, Canada"),
             TP_REPRR_ACCDN==1 & !is.na(RUE_ACCDN) & !is.na(ACCDN_PRES_DE) ~ 
               str_c(str_replace_na(RUE_ACCDN, ""),
                     " and ",
                     str_replace_na(ACCDN_PRES_DE, "")," ",
                     str_replace_na(NAME_MUNCP,""), ", QC, Canada"),
             TP_REPRR_ACCDN==1 & (!is.na(RUE_ACCDN) | !is.na(ACCDN_PRES_DE)) ~ 
               str_c(str_replace_na(RUE_ACCDN, ""),
                     str_replace_na(ACCDN_PRES_DE, "")," ",
                     str_replace_na(NAME_MUNCP,""),", QC, Canada"),
             !is.na(RUE_ACCDN) & !is.na(ACCDN_PRES_DE) ~ 
               str_c(str_replace_na(RUE_ACCDN, ""),
                     " and ",
                     str_replace_na(ACCDN_PRES_DE, "")," ",
                     str_replace_na(NAME_MUNCP,""),", QC, Canada"),
             str_detect(toupper(RUE_ACCDN), " ET ") ~
               str_c(RUE_ACCDN,
                     " ",
                     str_replace_na(NAME_MUNCP,""),", QC, Canada"),
             str_detect(toupper(ACCDN_PRES_DE), " ET ") ~
               str_c(ACCDN_PRES_DE,
                     " ",
                     str_replace_na(NAME_MUNCP,""),", QC, Canada")             
             ))
z <- velo %>% filter(is.na(location))

velopropre  <-velo %>% filter(!is.na(location))
geo <-ggmap::geocode(location = velopropre$location, output = "latlon", source= "google")
velopropre$lon <- geo$lon
velopropre$lat <- geo$lat

#convert lat long to sf

pouet <-  st_as_sf(velopropre %>% filter(-71.5 < lon, lon < -71), coords = c("lon", "lat"), crs = 4326, agr = "constant")

library(leaflet)
library(viridis)
library(leaflet.extras)
ndistinct<- as.numeric(as.data.frame(pouet %>% summarise( count = n_distinct(gravite))) %>% select(count))
mypal <- leaflet::colorFactor(viridis_pal(option="C")(ndistinct), domain = pouet$gravite, reverse = TRUE)

# map new sf
pouet %>% leaflet(options = leafletOptions(maxZoom = 15))%>%   
  #addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addProviderTiles(providers$Stamen.TonerLines) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addCircles(color = ~ mypal(gravite),
             opacity = 0.9,
             fillOpacity = 0.9,
             label = ~ paste0(gravite," - ", DT_ACCDN, " - ", location)) %>%
  addLegend("bottomleft",
            pal = mypal,
            values = ~ gravite,
            title = "Accidents impliquant des vélos selon la gravité, Québec 2011-2016")


#https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
#doesnt work
# labs <- lapply(seq(nrow(pouet)), function(i) {
#   paste0( '<p>', pouet[i, "DT_ACCDN"], '<p></p>', 
#           pouet[i, "gravite"], ', ', 
#           pouet[i, "location"], '</p>' ) 
# })

#ths works
#https://github.com/rstudio/leaflet/blob/master/inst/examples/marker-clustering.R

#library( htmltools )
pouet %>% leaflet(options = leafletOptions(maxZoom = 17)) %>%   
  #addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  #addProviderTiles(providers$Stamen.TonerLines) %>%
  #addProviderTiles(providers$Stamen.TonerLabels) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addMarkers(clusterOptions =
               markerClusterOptions(spiderfyOnMaxZoom= TRUE),
             label = ~ paste0(gravite,"  ", DT_ACCDN, "  ", location)             )  

pouet %>% leaflet(options = leafletOptions(maxZoom = 14))%>%   
  addProviderTiles(providers$Stamen.TonerLines) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
addHeatmap(blur = 2, max = 3, radius = 5)



