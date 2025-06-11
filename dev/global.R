#### global.R
# My Shiny App
# Copyright (C) 2024 Reto Spielhofer; Norwegian Institute for Nature Research (NINA)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

library(bslib)
library(shiny)
library(bsicons)
library(shinyjs)
library(shinythemes)
library(bigrquery)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leafem)
library(leafpop)
library(dplyr)
library(sf)
library(SSDM)
library(stringi)
library(dbplyr)
library(DT)
library(shinyalert)
library(tibble)
library(shinyWidgets)
library(tidyverse)
library(bsplus)
library(shinybusy)
library(googleCloudStorageR)
library(raster)
library(shinyBS)
library(htmlwidgets)
library(mapedit)
library(glue)
library(pool)


rm(list = ls())




#### Global settings

project_id<-"pareus" #eu-wendy, pareus
env<-"dev" #c("dev","prod")
site_id<-"FRL04" #wendy: GRC, ESP, ITA, pareus: NO0601, SK021, FRL04
var_lang<-"fra" #c("grk","en","ita","esp")

if(project_id=="eu-wendy"){
  source("mod_questionnaire.R")
  source("mod_mapping1_V2.R")
  source("mod_instructions_V2.R")
  source("mod_ahp_group.R")
  source("mod_ahp_single.R")
  source("mod_dist_impact.R")
}else{
  source("mod_questionnaire.R")
  source("mod_mapping1_V2.R")
  source("mod_instructions_V2.R")
  source("mod_ahp_group.R")
  source("mod_ahp_single.R")

}


###########################
# read setting
setting<-read.csv("setup/setup.csv")
setting<-setting%>%filter(study_id == project_id)

###
draw_pol<-as.logical(setting$polygon)
if(draw_pol==F){
  target_geom = "rectangles"
}else{
  target_geom = "polygons"
}

#the wendy project is wrongly named for downloading data
bqprojID<-setting$bqproj_id
wind_lca_questions<-setting$include_wind_lca
num_tabs <- setting$num_tabs


## color schema
orange = "#ffa626"
blue = "#53adc9"
green = "#50b330"


### gcs bucket and bq settings
cred_path<-paste0("gcs_keys/",project_id,"_key.json")

#bucket
bucket_name<-paste0(bqprojID,"_geopros_",env)
gcs_auth(cred_path)
#gcs_auth("C:/Users/reto.spielhofer/git/wendy_geopros_R1/gcs_keys/eu-wendy_key.json")
gcs_global_bucket(bucket_name) #set it as global bucket

## BQ connection to store rectangles
dataset <- paste0(bqprojID,"_",env)
bq_auth(
  path = cred_path
)

con_admin<-data.frame(
  project = project_id,
  dataset = dataset,
  billing = project_id
)

con_admin <- dbPool(
  bigrquery::bigquery(),
  project = con_admin$project,
  dataset = con_admin$dataset,
  billing = con_admin$billing)
onStop(function() {
  poolClose(con_admin)
})


# load study and site params
site<-tbl(con_admin, "study_site")
# site<-site%>%collect()%>%filter(projID == bqprojID & siteID == site_id)
site<-site%>%collect()%>%filter(siteID == site_id)


if (!("siteTYPE" %in% names(site))) {
  site_type<-"onshore"
} else {
  site_type<-as.character(site%>%dplyr::select(siteTYPE))
}

area_name<-site$siteNAME
sf_stud_geom <- sf::st_as_sf(site, wkt = "geometry" )%>%st_set_crs(4326)


# load the ES list
es_study<-tbl(con_admin, "es_descr")
stud_all<-es_study%>%collect()


if(isTRUE(wind_lca_questions)){
  stud_es<-stud_all%>%filter(type == site$siteTYPE)
  
}else{
  stud_es<-stud_all
}


## a grid for the questionnaire
grd<-st_make_grid(sf_stud_geom, cellsize = 0.05,
                  offset = st_bbox(sf_stud_geom)[1:2],  what = "polygons")


#with res of 250m grid we can sample at least 10 pts with variaton within 0.6km2
A_min<-as.numeric(setting$a_min_ha) #ha
if(site_type == "onshore"){
  resolution = 250^2
  A_max<-A_min*setting$factor_a_max #ha
  A_max_km2<-A_max/100
}else{
  resolution = 1000^2
  A_max<-A_min*1000 #ha
  A_max_km2<-A_max/100
}
A_football<-round(A_min*2.47,0)

max_rectangles = setting$max_rectangles
