#### global file

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
library(mapview)
library(dplyr)
library(mapedit)
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

rm(list = ls())

source("mod_questionnaire.R")
source("mod_mapping1.R")
source("mod_instructions.R")
source("mod_ahp_group.R")
source("mod_ahp_single.R")
source("mod_dist_impact.R")



#### Global settings

project_id<-"eu-wendy"

#the wendy project is wrongly named for downloading data
if(project_id == "eu-wendy"){
  bqprojID<-"wendy"
}else{
  bqprojID<-project_id
}

#site_id<-"NO06_1"
site_id<-"TRD_TEST"
env<-"dev" #c("dev","prod")
var_lang<-"en"
#how many es should be mapped by each participant from all ES?
num_tabs <- 3

### gcs bucket and bq settings
cred_path<-paste0("gcs_keys/",project_id,"_key.json")

#bucket
bucket_name<-paste0(bqprojID,"_geopros_",env)
gcs_auth(cred_path)
gcs_global_bucket(bucket_name) #set it as global bucket

## BQ connection to store rectangles
dataset <- paste0(bqprojID,"_",env)
bq_auth(
  path = cred_path
)

con_admin<-data.frame(
  project = project_id,
  dataset = dataset,
  billing =project_id
)


con_admin <- dbConnect(
  bigrquery::bigquery(),
  project = con_admin$project,
  dataset = con_admin$dataset,
  billing = con_admin$billing
)


# load study and site params
site<-tbl(con_admin, "study_site")
# site<-site%>%collect()%>%filter(projID == bqprojID & siteID == site_id)
site<-site%>%collect()%>%filter(projID == project_id & siteID == site_id)


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
stud_es<-stud_all%>%filter(esID == "recr" | esID == "nat_haz" | esID == "farm")

## a grid for the questionnaire
grd<-st_make_grid(sf_stud_geom, cellsize = 0.05,
                  offset = st_bbox(sf_stud_geom)[1:2],  what = "polygons")
