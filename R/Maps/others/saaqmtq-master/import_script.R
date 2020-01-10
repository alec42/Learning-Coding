# setup ----
#devtools::install_github("ropensci/opencage")
library(opencage)
library(osmdata)
knitr::opts_chunk$set(message=FALSE, warning=FALSE, error= FALSE)
#rm(list = ls())
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)
library(htmlwidgets)
library(readr)
require(scales)
#devtools::install_github("dkahle/ggmap") # nécessite la version de développement 2.7 pour pouvoir utiliser une api key.
library(ggmap)
library(stringr)
library(forcats)
library(lubridate)
library(leaflet)
library(viridis)
library(leaflet.extras)
library(DT)

ville <- "province"
type <- "both"   # vélos , piétons, both

download_files <- FALSE
read_from_csv <- TRUE
geocode <- FALSE

#https://stackoverflow.com/questions/36175529/getting-over-query-limit-after-one-request-with-geocode
register_google(key = Sys.getenv("googlemap_api_key"),
                account_type="premium")


if(ville == "province"){
  villetexte <- "province de Québec"
} else {villetexte <- paste0("ville de ",ville)}


# functions ----
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



prep_csv_data <- function(.data, .ville, .type){
  code_to_mun <- read_tsv("./data/code_to_mun.tsv")
  accidents <- .data
  
  if (.ville != "province"){
    accidents <- accidents %>% filter(NAME_MUNCP == .ville)
  }
  
  if (.type == "piétons"){
    accidents <- accidents %>% filter(NB_VICTIMES_PIETON > 0)
  } else if (.type == "vélos"){
    accidents <- accidents %>% filter(nb_bicyclette > 0) 
  }  else if(.type == "both"){
    accidents <- accidents %>% filter(nb_bicyclette > 0 | NB_VICTIMES_PIETON > 0) 
  } else {stop(".type doit être piétons ou vélos ou both")}
  
  
  accidents <- accidents %>% 
    mutate(type = case_when(
      nb_bicyclette > 0 & NB_VICTIMES_PIETON >0 ~ "vélos_et_piétons",
      nb_bicyclette > 0 ~ "vélo",
      NB_VICTIMES_PIETON > 0 ~ "piéton",
      TRUE ~ "ni_piéton_ni_vélo")) %>%
    left_join(code_to_mun %>% select(CD_MUNCP, NAME_MUNCP), by= "CD_MUNCP") %>%
    mutate(year = year(DT_ACCDN),  #normalement j'utiliserais  isoyear, mais je ne veux pas voir de 2010..
           week = isoweek(DT_ACCDN),
           month = month(DT_ACCDN),
           month.abb = as.factor(base::month.abb[month]) %>%
             fct_relevel("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
           monday = floor_date(DT_ACCDN, unit = "week") ) %>%
    arrange(DT_ACCDN)  %>%
    mutate(gravite = as.factor(gravite) %>% fct_relevel("Dommages matériels seulement", "Léger", "Grave", "Mortel"))  %>% 
    mutate(heure = as.numeric(ifelse(HR_ACCDN == "Non précisé", NA, str_sub(HR_ACCDN,1,2) )),
           region_num = as.numeric(str_sub(REG_ADM, -3, -2))) %>%
    mutate(row_num = row_number())  %>%
    mutate(CD_ENVRN_ACCDN = fct_recode(as.factor(CD_ENVRN_ACCDN) ,
                                       "Scolaire"= "1", 
                                       "Résidentiel" = "2", 
                                       "Affaires / commercial"= "3",
                                       "Industriel / Manufacturier" = "4",
                                       "Rural" = "5",
                                       "Forestier" = "6", 
                                       "Récréatif / parc / camping" = "7",
                                       "Autre"    = "9",
                                       "Non précisé" = "0")) %>%
    mutate(CD_COND_METEO =
             fct_recode(as.factor(CD_COND_METEO),
                        "Clair" = "11",
                        "Couvert (nuageux/sombre)" = "12",
                        "Brouillard/brume "= "13",
                        "Pluie/bruine"= "14",
                        "Averse (pluie forte)" = "15",
                        "Vent fort (pas de poudrerie, pas de pluie)"= "16",
                        "Neige/grêle"= "17",
                        "Poudrerie/tempête de neige"= "18",
                        "Verglas"= "19",
                        "Autre" = "99")) %>%
    mutate(CD_POSI_ACCDN = 
             fct_recode(as.factor(CD_POSI_ACCDN),
                        "Voie réservée en service"= "01",
                        "Voie lente/voie de dépassement" = "02",
                        "Perte/gain de voie"= "03", 
                        "Voie de virage à gauche dans les deux sens"= "04",
                        "Voie cyclable/chaussée désignée"= "05",
                        "Voie de circulation "= "06",
                        "Accotement (ou bord de la chaussée)"= "07",
                        "Terre-plein central ou îlot"= "08",
                        "Trottoir"= "09",
                        "Autre" = "10")) %>%
    mutate(      clean_REG_ADM = str_replace(REG_ADM,"\\(\\d+\\)", ""),
                 ville = paste0(str_replace_na(NAME_MUNCP,""),  ", QC, Canada"),
                 ville_w_regadm = paste0(str_replace_na(NAME_MUNCP,""), ",", str_replace_na(clean_REG_ADM,""), ", QC, Canada"))
  
  
  
  accidents$RUE_ACCDNmod <- accidents$RUE_ACCDN
  accidents$ACCDN_PRES_DEmod <- accidents$ACCDN_PRES_DE
  # replace short street names by full name, fix a few typos.
  # \\b represents the boundary of a word in regexp
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bN-D\\b", "NOTRE-DAME") # attention rouler ceci avant NORD car le - coupe le mot
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bBD\\b", "BOULEVARD")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bCH\\b", "CHEMIN")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bMT\\b", "MONT")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bAV\\b", "AVENUE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bN\\b", "NORD")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bS\\b", "SUD")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bE\\b", "EST")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bO\\b", "OUEST")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bST\\b", "SAINT")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bSTE\\b", "SAINTE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bRTE\\b", "ROUTE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bTSSE\\b", "TERRASSE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bGD\\b", "GRAND")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bGDE\\b", "GRANDE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bAUT\\b", "AUTOROUTE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPTE\\b", "POINTE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPRDE\\b", "PROMENADE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bRG\\b", "RANG")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPR\\b", "PROMENADE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bAL\\b", "ALLÉE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPL\\b", "PLACE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bCT\\b", "CÔTE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bMGR\\b", "MONSEIGNEUR")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bSTIE\\b", "SORTIE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bENTR\\b", "ENTRÉE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bMTEE\\b", "MONTÉE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bMTE\\b", "MONTÉE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bVIAD\\b", "VIADUC")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bRIV\\b", "RIVIÈRE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bCROIS\\b", "AND")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bINTERSECTION\\b", "AND")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bSERV\\b", "SERVICE")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bFACE À\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bFACE AU\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bEN FACE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bFACE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPRÈS DE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPRÈS DU\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPRES DE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bPRES DU\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bARR START\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bARR DU\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bARR DE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bARR STAT\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bARR\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bOPP DU\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bOPP DE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bOPP\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bQ BOURG\\b", "QUATRE-BOURGEOIS")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bQBOURG\\b", "QUATRE-BOURGEOIS")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bW PELLETIER\\b", "WILFRID-PELLETIER")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bSORTIE HENRI IV NORD\\b", "HENRI IV")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bDES RIVS\\b", "DES RIVIÈRES")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bSORTIE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bENTREE\\b", "")
  accidents$RUE_ACCDNmod <- str_replace(accidents$RUE_ACCDNmod, "\\bENTRÉE\\b", "")
  
  
  
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bN-D\\b", "NOTRE-DAME") # attention rouler ceci avant NORD car le - coupe le mot
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bBD\\b", "BOULEVARD")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bCH\\b", "CHEMIN")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bMT\\b", "MONT")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bAV\\b", "AVENUE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bN\\b", "NORD")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bS\\b", "SUD")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bE\\b", "EST")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bO\\b", "OUEST")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bST\\b", "SAINT")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bSTE\\b", "SAINTE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bRTE\\b", "ROUTE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bTSSE\\b", "TERRASSE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bGD\\b", "GRAND")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bGDE\\b", "GRANDE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bAUT\\b", "AUTOROUTE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPTE\\b", "POINTE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPRDE\\b", "PROMENADE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bRG\\b", "RANG")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPR\\b", "PROMENADE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bAL\\b", "ALLÉE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPL\\b", "PLACE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bCT\\b", "CÔTE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bMGR\\b", "MONSEIGNEUR")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bSTIE\\b", "SORTIE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bENTR\\b", "ENTRÉE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bMTEE\\b", "MONTÉE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bMTE\\b", "MONTÉE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bVIAD\\b", "VIADUC")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bRIV\\b", "RIVIÈRE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bCROIS\\b", "AND")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bINTERSECTION\\b", "AND")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bSERV\\b", "SERVICE")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bFACE À\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bFACE AU\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bEN FACE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bFACE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPRÈS DE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPRÈS DU\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPRES DE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bPRES DU\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bARR START\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bARR DU\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bARR DE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bARR STAT\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bARR\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bOPP DU\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bOPP DE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bOPP\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bQ BOURG\\b", "QUATRE-BOURGEOIS")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bQBOURG\\b", "QUATRE-BOURGEOIS")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bW PELLETIER\\b", "WILFRID-PELLETIER")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bSORTIE HENRI IV NORD\\b", "HENRI IV")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bSORTIE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bENTREE\\b", "")
  accidents$ACCDN_PRES_DEmod <- str_replace(accidents$ACCDN_PRES_DEmod, "\\bENTRÉE\\b", "")
  
  
  # si lévis, alors remplacer COMMERCIALE par TANIATA. 
  #  pour coin taniata/20 est on va aller éditer directement dans location car api capricieux
  accidents <- accidents %>%  mutate(ACCDN_PRES_DEmod =
                                       ifelse(
                                         NAME_MUNCP == "Lévis",
                                         str_replace(ACCDN_PRES_DEmod, "COMMERCIALE", "TANIATA"),
                                         ACCDN_PRES_DEmod),
                                     
                                     RUE_ACCDNmod = 
                                       ifelse(
                                         NAME_MUNCP == "Lévis",
                                         str_replace(RUE_ACCDNmod, "COMMERCIALE", "TANIATA"),
                                         RUE_ACCDNmod))
  
  ## create a location variable that is understandable by google maps
}

mutate_location <- function(.data){
  
  accidents <- .data %>%
    mutate(
      location =
        case_when(
          !is.na(NO_CIVIQ_ACCDN) & !is.na(RUE_ACCDNmod) ~ 
            str_c(str_replace_na(as.numeric(NO_CIVIQ_ACCDN), ""),
                  " ",
                  str_replace_na(RUE_ACCDNmod, "")," ",
                  ville),
          TP_REPRR_ACCDN==1 & !is.na(RUE_ACCDNmod) & !is.na(ACCDN_PRES_DEmod) ~ 
            str_c(str_replace_na(RUE_ACCDNmod, ""),
                  " and ",
                  str_replace_na(ACCDN_PRES_DEmod, "")," ",
                  ville),
          TP_REPRR_ACCDN==1 & (!is.na(RUE_ACCDNmod) | !is.na(ACCDN_PRES_DEmod)) ~ 
            str_c(str_replace_na(RUE_ACCDNmod, ""),
                  str_replace_na(ACCDN_PRES_DEmod, "")," ",
                  ville),
          !is.na(RUE_ACCDNmod) & !is.na(ACCDN_PRES_DEmod) ~ 
            str_c(str_replace_na(RUE_ACCDNmod, ""),
                  " and ",
                  str_replace_na(ACCDN_PRES_DEmod, "")," ",
                  ville),
          str_detect(toupper(RUE_ACCDNmod), " ET ") ~
            str_c(RUE_ACCDNmod,
                  " ",
                  ville),
          str_detect(toupper(ACCDN_PRES_DEmod), " ET ") ~ # intersection de 2 routes
            str_c(ACCDN_PRES_DEmod,
                  " ",
                  ville),
          str_detect(str_sub(ACCDN_PRES_DEmod,1,2), "\\d") ~  # un chiffre dans les 2 premiers caractères de accdn_pres_de ressemble a une adresse..
            str_c(ACCDN_PRES_DEmod,
                  " ",
                  ville),
          
        ),
      
      location = ifelse( 
        NAME_MUNCP == "Lévis" & str_detect(location, "TANIATA") & str_detect(location, "20"), "TANIATA and  20  Lévis, QC, Canada", 
        location)
      
      
    )
  
  
  
  return(accidents)
}

get_villes_w_regadm_bounding_boxes <- function(.villes_w_regadm, old_rds = NULL){ # all_binding_boxes.rds
  
  if (!is.null(old_rds)){
    old_boxes <- read_rds(old_rds)
    .villes_w_regadm <- .villes_w_regadm %>% anti_join(old_boxes)
  }
  message("nrows unmatched = ",nrow(.villes_w_regadm))
  output <- NULL
  if (nrow(.villes_w_regadm)>0){
    output <- matrix(ncol=4, nrow=nrow(.villes_w_regadm))
    for(i in 1:nrow(.villes_w_regadm)){
      message("Getting ville ",.villes_w_regadm$ville_w_regadm[i])
      Sys.sleep(1)
      box <- osmdata::getbb(.villes_w_regadm$ville_w_regadm[i])
      box2 <- c(box[1,1], box[2,1], box[1,2], box[2,2])
      output[i,] <- box2
    }
    
    output <- data.frame(output)
    names(output) <- c("min_x", "min_y", "max_x", "max_y")
    output$ville_w_regadm <- .villes_w_regadm$ville_w_regadm
    output <- output %>% select(ville_w_regadm, everything())
    
    if (!is.null(old_rds)){
      output <- bind_rows(old_boxes, output)
      write_rds(output, old_rds)
    }  
  }
  return(output)
}


get_ville_google_centers <- function(.villes, old_rds = NULL){ #"all_villes_centers.rds"
  .villes <- .villes %>% filter(!is.na(ville))
  if (!is.null(old_rds)){
    old_centers <- read_rds(old_rds)
    .villes <- .villes %>% anti_join(old_centers)
  }
  message("nrows unmatched = ",nrow(.villes))
  output <- NULL
  if (nrow(.villes) > 0){
    centers <- ggmap::geocode(location = .villes  %>% pull(ville), output = "latlon", source= "google")
    output <- .villes %>% add_column(ville_lon = centers$lon) %>% add_column(ville_lat = centers$lat)
    
    if (!is.null(old_rds)){
      output <- bind_rows(old_centers, output)
      write_rds(output, old_rds)
    }  
  }
  return(output)  
}

add_opencage <- function(.data) {
  .data %>% 
    mutate(
      row_num = row_number(),
      row_count = n(),
      opencage_return = pmap(list(location, min_x, min_y, max_x, max_y, row_num, row_count),
                             
                             function(.location, .min_x, .min_y, .max_x, .max_y, row_num, row_count){
                               if(!is.na(location)){
                                 Sys.sleep(1)
                                 message(.location, ", ",row_num, " / ", row_count)
                                 
                                 if(!is.na(.min_x)){
                                   opencage_forward(.location, 
                                                    bounds = c(.min_x, .min_y, .max_x, .max_y),  # within the city
                                                    limit = 1 , # just the best result
                                                    #language= "fr",
                                                    min_confidence = 10, # max 250 m uncertainty
                                                    countrycode = "CA" # canada
                                   )} else{
                                     opencage_forward(.location, 
                                                      limit = 1 , # just the best result
                                                      #language= "fr",
                                                      min_confidence = 10, # max 250 m uncertainty
                                                      countrycode = "CA" # canada
                                                      
                                     )}
                               } else{list(results= NULL)}
                               
                             }
      ),
      
      opencage_lat = map_dbl(opencage_return, ~{ if (!is.null(.x$results)){.x$results$geometry.lat} else{NA}}),
      opencage_lon = map_dbl(opencage_return, ~{ if (!is.null(.x$results)){.x$results$geometry.lng} else{NA}})
      
      
    ) %>%
    select(-row_num, -row_count)
}

add_google <- function(.data,.check_opencage = TRUE) {
  
  if (.check_opencage){
    .data %>% 
      mutate(
        google_return = pmap(list(location, opencage_lat), function(.location, .opencage_lat){
          if(!is.na(.location) & (is.na(.opencage_lat))){ # has a location string but hasnt been geocoded by opencage 
            ggmap::geocode(location =  .location, output = "latlon", source= "google")
            
          }  else {
            NA
          }
        }
        ),
        google_lat = map_dbl(google_return, ~ if(!is.na(.x)){.x$lat} else { NA}),
        google_lon = map_dbl(google_return, ~ if(!is.na(.x)){.x$lon} else { NA}),
        
      )
    
  } else {
    .data %>% 
      mutate(
        google_return = pmap(list(location), function(.location){
          if(!is.na(.location) ){ # has a location string but hasnt been geocoded by opencage 
            ggmap::geocode(location =  .location, output = "latlon", source= "google")
            
          }  else {
            NA
          }
        }
        ),
        google_lat = map_dbl(google_return, ~ if(!is.na(.x)){.x$lat} else { NA}),
        google_lon = map_dbl(google_return, ~ if(!is.na(.x)){.x$lon} else { NA}),
        
      )
    
  }
}


check_valid <- function(.data){
  
  required_vars <- c("opencage_lat", "opencage_lon", "google_lat", "google_lon")
  Missing <- setdiff(required_vars, names(.data))
  .data[Missing] <- NA_real_
  .data %>% 
    mutate(
      inside_box_opencage = case_when(
        is.na(opencage_lat) ~ 0,
        is.na(min_x) ~ 0,
        opencage_lat >= min_y-0.2  & opencage_lat <= max_y+0.2 & opencage_lon >= min_x-0.2 & opencage_lon <= max_x+0.2 ~1,
        TRUE  ~0 ),
      inside_box_google = case_when(
        is.na(google_lat) ~ 0,
        is.na(min_x) ~ 0,
        google_lat >= min_y - 0.2 & google_lat <= max_y + 0.2 & google_lon >= min_x - 0.2 & google_lon <= max_x + 0.2 ~1,
        TRUE  ~0 ),
      not_equal_ville_opencage = case_when(
        is.na(opencage_lat) ~ 0,
        is.na(ville_lat) ~ 0,
        opencage_lat != ville_lat & opencage_lon != ville_lon ~ 1,
        TRUE ~ 0),
      not_equal_ville_google =case_when(
        is.na(google_lat) ~ 0,
        is.na(ville_lat) ~ 0,
        google_lat != ville_lat & google_lon != ville_lon ~ 1,
        TRUE ~ 0),
      valide_opencage = if_else(inside_box_opencage ==1 & not_equal_ville_opencage ==1 , 1, 0),
      valide_google = if_else(inside_box_google ==1 & not_equal_ville_google ==1 , 1, 0),
      count_valide = valide_opencage +valide_google,
      final_lat = case_when(
        valide_google == 1 ~ google_lat,
        valide_opencage==1 ~ opencage_lat,
        TRUE ~ NA_real_),
      final_lon = case_when(
        valide_google == 1 ~ google_lon,
        valide_opencage==1 ~ opencage_lon,
        TRUE ~ NA_real_)
    ) 
}


# download, read and append csv ----
if(download_files){
  ## crashes files
  download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2017.csv", 
                destfile= "./data/rapports-accident-2017.csv")
  
  download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2016.csv", 
                destfile= "./data/rapports-accident-2016.csv")
  download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2015.csv", 
                destfile= "./data/rapports-accident-2015.csv")
  download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2014.csv", 
                destfile= "./data/rapports-accident-2014.csv")
  download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2013.csv", 
                destfile= "./data/rapports-accident-2013.csv")
  download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2012.csv", 
                destfile= "./data/rapports-accident-2012.csv")
  download.file("https://saaq.gouv.qc.ca/donnees-ouvertes/rapports-accident/rapports-accident-2011.csv", 
                destfile= "./data/rapports-accident-2011.csv")
  
  # table to convert municipalite code to municipalite name
  # manually created a .tsv from the table found here
  #(https://www.mamrot.gouv.qc.ca/recherche-avancee/fiche/municipalite/).
  download.file("https://raw.githubusercontent.com/SimonCoulombe/saaqmtq/master/data/code_to_mun.tsv", destfile= "./data/code_to_mun.tsv")
  
  # comptage
  download.file("http://donnees.ville.montreal.qc.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/6caecdd0-e5ac-48c1-a0cc-5b537936d5f6/download/comptagevelo20162.csv",
                destfile="./data/comptagevelo20162.csv")
  
  #localisation comptage
  
  download.file("http://donnees.ville.montreal.qc.ca/dataset/f170fecc-18db-44bc-b4fe-5b0b6d2c7297/resource/c7d0546a-a218-479e-bc9f-ce8f13ca972c/download/localisationcompteursvelo2015.csv", 
                destfile="./data/localisationcompteursvelo2015.csv")
  
  #shapefile
  download.file("http://donnees.ville.montreal.qc.ca/dataset/5ea29f40-1b5b-4f34-85b3-7c67088ff536/resource/234c8ee4-d9d8-4bb1-b957-3e5cd495a5aa/download/reseaucyclable2017juin2017shp.zip",
                destfile = "./data/reseaucyclable2017juin2017shp.zip")
  utils::unzip("./data/reseaucyclable2017juin2017shp.zip", 
               exdir = "./data")
}

code_to_mun <- read_tsv("./data/code_to_mun.tsv")


accidents17 <- read_csv("./data/rapports-accident-2017.csv") %>%
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN),
         CD_MUNCP = as.numeric(CD_MUNCP))
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
  mutate(NO_ROUTE = as.numeric(NO_ROUTE), SFX_NO_CIVIQ_ACCDN= as.character(SFX_NO_CIVIQ_ACCDN)) %>%
  rename(HR_ACCDN = heure_accdn) %>% 
  rename(AN = an)

# wrangle data, keeping only crashes involving bikes in Québec city
# 910 obs
accidents <- bind_rows(accidents11,accidents12, accidents13, 
                       accidents14, accidents15, accidents16, accidents17
)  


# work ----

prepared <- prep_csv_data(.data= accidents, .ville = ville, .type = type) %>%
  mutate_location

# get bounding boxes for opencage and city centers from google 
get_villes_w_regadm_bounding_boxes(prepared %>% distinct(ville_w_regadm), old_rds = "new_bounding_boxes.rds")
get_ville_google_centers(prepared %>% distinct(ville), old_rds = "new_villes_centers.rds")


### ok ça ce mont mes nouvelles données d'accidents, non géocodées ----
prepared2 <- prepared %>% 
  left_join (read_rds("new_bounding_boxes.rds") )  %>%
  left_join(read_rds("new_villes_centers.rds"))

### ok là je vais regarder ec que j'ai déjà géocodé en 2011-2016 (google seulement) et 2017 (opencage + google) et voir ce qui est récupérable

all_location_gps <- bind_rows(
  read_rds("data_final_2011_2017piétons.rds"),
  read_rds("data_final_2011_2017vélos.rds"))%>%  
  select(-min_x, -min_y, -max_x, -max_y, -ville_lat, -ville_lon, ville)%>%
  mutate(
    clean_REG_ADM = str_replace(REG_ADM,"\\(\\d+\\)", ""),
    ville = paste0(str_replace_na(NAME_MUNCP,""),  ", QC, Canada"),
    ville_w_regadm = paste0(str_replace_na(NAME_MUNCP,""), ",", str_replace_na(clean_REG_ADM,""), ", QC, Canada")) %>%
  left_join (read_rds("new_bounding_boxes.rds") )  %>%
  left_join(read_rds("new_villes_centers.rds")) %>%
  check_valid() %>%
  mutate_location()

# ok pour chaque location je vais sauver une seule paire de final_lon /final_lat
#distincts_locs_bak <- distincts_locs
distincts_locs <- all_location_gps %>%
  distinct(location, ville , ville_w_regadm, opencage_return, opencage_lat,
           opencage_lon, google_return,google_lat, google_lon,min_x,min_y,max_x,max_y,
           ville_lon,ville_lat,inside_box_opencage,inside_box_google,
           not_equal_ville_opencage,not_equal_ville_google,valide_opencage,
           valide_google, count_valide , final_lat,final_lon                  ) %>%
  group_by(location) %>%
  arrange(-valide_google, -valide_opencage ) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(location))

write_rds(distincts_locs, "distincts_locs.rds")

# ok toute les locations qui ont jamais été geocodées par google is.na(google_lat) ont droit à une deuxième chance.  
need_google_locs <- distincts_locs %>% 
  filter(is.na(google_lat)) %>% 
  select(location, ville, ville_w_regadm, min_x,min_y,max_x,max_y, ville_lon,ville_lat) %>%
  add_google(., .check_opencage = FALSE)

# les deux left join sont nécessaire carj 'avais oublié de faireun keep sur min_x et ville_lat..
need_google_locs2 <- need_google_locs %>%
  #left_join (read_rds("new_bounding_boxes.rds") )  %>%
  #left_join(read_rds("new_villes_centers.rds")) %>%
  check_valid()

write_rds(need_google_locs2, "need_google_locs2.rds")

# on greffe les google géocodées aux anciennes distincts locs
distincts_locs_regoogled <- 
  bind_rows(distincts_locs %>% anti_join(need_google_locs2 %>% filter(!is.na(final_lat)) %>% select(location, ville, ville_w_regadm)) ,
            need_google_locs2 %>% filter(!is.na(final_lat))
  )

write_rds(distincts_locs_regoogled, "distincts_locs_regoogled.rds")

# si j'ai pas de google valide (donc le google a rien retourné ou bien que le google a retourné out of bound, alors on tente enfin avec opencage)
# opencage est un dernier recours car il a tendance a mettre les adresse ensemble sur une grande distance
last_ditch_attempt_opencage_locs <-
  distincts_locs_regoogled %>% 
  filter(valide_google == 0)%>%
  select(location, ville, ville_w_regadm, min_y, min_x, max_y, max_x,  google_lat, google_lon)  

last_ditch_attempt_opencage_locs2 <- last_ditch_attempt_opencage_locs %>%  
  add_opencage()
write_rds(last_ditch_attempt_opencage_locs2, "last_ditch_attempt_opencage_locs2.rds")

last_ditch_attempt_opencage_locs3 <- last_ditch_attempt_opencage_locs2 %>% 
  left_join(read_rds("new_villes_centers.rds")) %>%
  check_valid()

distincts_locs_final <-
  bind_rows(distincts_locs_regoogled %>% 
              anti_join(last_ditch_attempt_opencage_locs3 %>% 
                          filter(!is.na(opencage_lat)) %>% 
                          select(location, ville, ville_w_regadm )),
            last_ditch_attempt_opencage_locs3 %>% filter(!is.na(opencage_lat))
  )

write_rds(distincts_locs_final,"distincts_locs_final.rds")
write_csv(distincts_locs_final %>% select( location, ville, ville_w_regadm, final_lat, final_lon),"distincts_locs_final.csv")



prepared3 <- prepared2 %>%  select(-min_x, -min_y, -max_x, -max_y, -ville_lon, -ville_lat) %>%
  left_join(distincts_locs_final)

write_rds(prepared3, "prepared3_michemin.rds")

# liste des top intersections

prepared3 <- read_rds( "prepared3_michemin.rds")

###14  juin 2019, on va essayer de clusterer les points dans des ronds de moins de 150m.
library(rgdal)
library(geosphere)

temp <- prepared3 %>%
  filter(!is.na(final_lat)) %>%
  mutate(final_lat2 = if_else(near(final_lat,45.550883) & near(final_lon,-73.629461), as.double(45.5502703), as.double(final_lat)),
         final_lon2 = if_else(near(final_lat,45.550883) & near(final_lon, -73.629461), as.double(-73.6293019), as.double(final_lon))) %>%
  mutate(final_lon = final_lon2, final_lat = final_lat2)%>%
  st_as_sf(coords = c("final_lon2", "final_lat2"), crs = 4326, agr = "constant") 

prepared3 %>%  filter(near(final_lat,45.550883) & near(final_lon,-73.629461)) %>% dim # 10
prepared3 %>%  filter(near(final_lat,45.5502703) & near(final_lon,-73.6293019)) %>% dim # 19
temp %>%  filter(near(final_lat,45.550883) & near(final_lon,-73.629461)) %>% dim # 0 
temp  %>%  filter(near(final_lat,45.5502703) & near(final_lon,-73.6293019)) %>% dim # 29


library(tictoc)
tic()
distance_matrix <- st_distance( temp)
toc()
hc <- hclust(as.dist(distance_matrix), method="complete")

tic()
d=50
temp$clust <- cutree(hc, h=d)
toc()

# get mode de tout les clusters
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Mode <- function(x) {
#   ux <- unique(x)
#   if(!anyDuplicated(x)){
#     NA_character_ } else { 
#       tbl <-   tabulate(match(x, ux))
#       toString(ux[tbl==max(tbl)])
#     }
#}

cluster_coords <- temp %>% st_set_geometry(NULL) %>% group_by(clust) %>%
  summarise(mode_final_lat = Mode(final_lat),
            mode_final_lon = Mode(final_lon))

prepared3  <-  temp %>% 
  st_set_geometry(NULL) %>% 
  inner_join(cluster_coords) %>% 
  mutate(final_lat = mode_final_lat, final_lon = mode_final_lon ) %>%
  select(-mode_final_lat, -mode_final_lon) %>% 
  bind_rows(prepared3 %>% filter(is.na(final_lat)) ) # greffer les lignes non géocodées

write_rds(prepared3, "prepared3.rds")

write_csv(prepared3 %>%
            select( -region_num, -row_num , -not_equal_ville_opencage, -not_equal_ville_google,
                    -ACCDN_PRES_DEmod, -RUE_ACCDNmod, -ville, -ville_w_regadm, -HR_ACCDN,
                    -opencage_return, -opencage_lat, -opencage_lon,
                    -google_return, -google_lat, -google_lon,
                    -min_x, -min_y, -max_x, -max_y, 
                    -ville_lon, -ville_lat,
                    -inside_box_opencage, -inside_box_google, - valide_opencage, - valide_google, -count_valide), "prepared3.csv")


write_rds(prepared3 %>%
            select(DT_ACCDN, HR_ACCDN, gravite, type, NAME_MUNCP, clean_REG_ADM, 
                   NO_CIVIQ_ACCDN, SFX_NO_CIVIQ_ACCDN, RUE_ACCDN, ACCDN_PRES_DE, NO_ROUTE , location, final_lat, final_lon, clust), 
          "prepared3_for_shiny.rds")
#mapview::mapview(mtlhead %>% filter(ville == "Montréal, QC, Canada" ) %>% head(2000), zcol = "clust")

# get the centroid coords for each cluster
# circles <- matrix(ncol=2, nrow=max(mtlhead$clust))
# for (i in 1:max(mtlhead$clust)){
#   circles[i,1] <- mtlhead %>% filter(clust == i) %>% summarise(final_lat = mean(final_lat)) %>% pull(final_lat)
#   circles[i,2] <- mtlhead %>% filter(clust == i) %>% summarise(final_lon = mean(final_lon)) %>% pull(final_lon)
#     
# }
# circles <- circles %>% as_tibble()
# 
# mypalette <- leaflet::colorNumeric(palette = "plasma", domain = c(mtlhead$clust))
# 
# 
# leaflet(circles) %>% addTiles %>%
#   addCircles(lng = ~V2, lat = ~V1, radius = d)  %>%
#   addCircleMarkers(data=mtlhead, color = ~mypalette(clust), label = ~paste0(clust))
# top 10 clusters

top10 <- prepared3 %>% 
  filter(!is.na(clust))%>%
  group_by(clust) %>%
  mutate(rapports = n()) %>%
  select(location,NAME_MUNCP , clean_REG_ADM, rapports , final_lat, final_lon, clust) %>%
  group_by(clust, final_lat, final_lon,location)%>%
  mutate(location_count = n()) %>%
  group_by(clust) %>%
  arrange(-location_count) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(-rapports) %>%
  select(-location_count) %>%
  select(location, everything())%>%
  rename(location_name = location)

# ## fin préparer clustering
# top10 <- prepared3 %>%
#   filter(!is.na(final_lat)) %>%
#   group_by(final_lat, final_lon)  %>%
#   mutate( rapports = n(), ) %>%
#   select(location,NAME_MUNCP , clean_REG_ADM, rapports , final_lat, final_lon) %>%
#   group_by(final_lat, final_lon,location)%>%
#   mutate(location_count = n()) %>%
#   group_by(final_lat, final_lon) %>%
#   arrange(-location_count) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(-rapports) %>%
#   select(-location_count) %>%
#   select(location, everything())%>%
#   rename(location_name = location)

top10 %>% 
  filter(rapports >= 7) %>%  
  sf::st_as_sf(x = ., coords = c("final_lon", "final_lat"), crs = 4326, agr = "constant") %>%
  head(100) %>% mapview::mapview(zcol = "rapports")

# liste des accidents aux top intersections
prepared3 %>% select(clust, type ) %>%
  inner_join(top10 %>% 
               select(clust, final_lat, final_lon, rapports, NAME_MUNCP, clean_REG_ADM,  location_name) %>% 
               rename(region_administrative = clean_REG_ADM) ) %>% 
  group_by(location_name, NAME_MUNCP, region_administrative,  clust, rapports, type) %>%
  summarise(decompte = n()) %>%
  spread(key=type, value= decompte, fill = 0) %>%
  ungroup() %>%
  arrange(-rapports) %>%
  #filter(region_administrative == "Chaudière-Appalaches")%>% 
  mutate(rang = row_number())  %>%
  select(-clust)


prepared3 %>% select(clust, type, DT_ACCDN, heure,JR_SEMN_ACCDN, CD_COND_METEO, gravite,  NB_MORTS, NB_BLESSES_GRAVES , NB_BLESSES_LEGERS, location) %>%
  inner_join(top10 %>% 
               select(clust,  rapports))  %>%
  filter(rapports == max(rapports)) %>%
  select(-clust, -rapports) %>% View
  


# zombies stuff below --------

# output de mes anciens et nouveaux geocodes ;
#best_location_data <- read_rds( "best_location_data.rds")

# #all_location_gps2_bak <- all_location_gps2
# all_location_gps2 <- all_location_gps %>%  # on enleve les variables géocodées et on greffe la meilleure géocodée
#   select(-opencage_return, -opencage_lat,               
#          -opencage_lon, -google_return,-google_lat, -google_lon,-min_x,-min_y,-max_x,-max_y,
#          -ville_lon,-ville_lat,-inside_box_opencage,-inside_box_google,
#          -not_equal_ville_opencage,-not_equal_ville_google,-valide_opencage,
#          -valide_google, -count_valide , -final_lat,-final_lon ) %>%
#   left_join(distincts_locs)
# #left_join(best_location_data)
# # 
# all_location_gps2 %>%
#   #filter(!is.na(final_lat)) %>%
#   filter(!is.na(google_lat)) %>%
#   group_by(final_lat, final_lon)  %>%
#   mutate( n = n()) %>%
#   select(location,ville , clean_REG_ADM, n ) %>%
#   group_by(final_lat, final_lon,location)%>%
#   mutate(location_count = n()) %>%
#   group_by(final_lat, final_lon) %>%
#   arrange(-location_count) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(-n) %>%
#   select(-location_count) %>%
#   select(location, everything())%>%
#   View()
# # 
# # 
# # # 2e chance au geocodage google pour les villes qui ont pas marché (check valide est aussi rendu plus généreux)
# deuxiemechance_bak <- deuxiemechance
# deuxiemechance <- all_location_gps2 %>% 
#   filter(is.na(google_lat)) %>% 
#   distinct(location,ville) %>% 
#   filter(!is.na(location)) %>%
#   left_join (read_rds("new_bounding_boxes.rds") )  %>%
#   left_join(read_rds("new_villes_centers.rds")) %>%
#   #add_opencage()  %>%
#   add_google(.check_opencage= FALSE) #%>%
# #check_valid()
# 
# #write_rds(deuxiemechance , "deuxiemechance.rds")
# deuxiemechance <- read_rds("deuxiemechance.rds")
# 
# distincts_locs_deuxieme <- deuxiemechance %>% check_valid() %>% 
#   # distinct(location,  
#   #          #opencage_return, opencage_lat,  opencage_lon, 
#   #          google_return,google_lat, google_lon,min_x,min_y,max_x,max_y,
#   #          ville_lon,ville_lat,
#   #          #inside_box_opencage,
#   #          inside_box_google,
#   #          #not_equal_ville_opencage,
#   #          not_equal_ville_google,valide_opencage,
#   #          valide_google, count_valide , final_lat,final_lon                  ) %>%
#   group_by(location) %>%
#   arrange(#-valide_opencage, 
#     -valide_google ) %>%
#   slice(1) %>%
#   ungroup()
# distincts_locs_deuxieme %>% 
#   filter(!is.na(final_lat)) %>% 
#   group_by(final_lat, final_lon )%>% 
#   mutate(n = n()) %>% 
#   ungroup() %>%
#   arrange(-n, final_lat) %>% View
# best_location_data_bak <- best_location_data
# best_location_data2 <- bind_rows(best_location_data %>% anti_join(deuxiemechance %>% filter(!is.na(google_lat)) %>%select(location)),
#                                  distincts_locs_deuxieme %>% filter(!is.na(final_lat)))
# write_rds(best_location_data2, "best_location_data2.rds")
# 
# all_location_gps3 <- all_location_gps %>%  # on enleve les variables géocodées et on greffe la meilleure géocodée
#   select(-opencage_return, -opencage_lat,               
#          -opencage_lon, -google_return,-google_lat, -google_lon,-min_x,-min_y,-max_x,-max_y,
#          -ville_lon,-ville_lat,-inside_box_opencage,-inside_box_google,
#          -not_equal_ville_opencage,-not_equal_ville_google,-valide_opencage,
#          -valide_google, -count_valide , -final_lat,-final_lon ) %>%
#   left_join(best_location_data2)
# #left_join(best_location_data)
# 
# 
# 
# all_location_gps3 %>%
#   filter(!is.na(final_lat)) %>%
#   group_by(final_lat, final_lon)  %>%
#   mutate( n = n()) %>%
#   select(location,ville , clean_REG_ADM, n ) %>%
#   group_by(final_lat, final_lon,location)%>%
#   mutate(location_count = n()) %>%
#   group_by(final_lat, final_lon) %>%
#   arrange(-location_count) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(-n) %>%
#   select(-location_count) %>%
#   select(location, everything())%>%
#   View()
# 
# all_location_gps3 %>% count(count_valide)
# 
# 
# all_location_gps3 %>%
#   filter(!is.na(final_lat)) %>%
#   group_by(final_lat, final_lon)  %>%
#   mutate( n = n()) %>%
#   select(location,ville , clean_REG_ADM, n ) %>%
#   ungroup() %>%
#   arrange(-n)  %>% View
