## upload pareus helper
library(bigrquery)
library(sf)
library(dplyr)
library(giscoR)
library(ggplot2)
library(terra)

dev <- "dev" #prod
cntr_proj<-"FRA"


# upload db
bq_auth(
  path = "dev/gcs_keys/pareus_key.json"
)

dataset<-paste0("pareus_",dev)


### from an ee raster extent
# Step 1: Load the raster
r <- rast("C:/Users/reto.spielhofer/git/geoprospective_R1/dev/env_var_OVR/int.tif")

# Step 2: Create a binary mask where cells with data are 1, and NA cells are NA
mask <- !is.na(r)
mask <- classify(mask, cbind(0, NA))  # make sure 0 becomes NA

# Step 3: Convert the raster mask to polygons
polygons <- as.polygons(mask, dissolve = TRUE)  # dissolve = TRUE merges adjacent cells

# Step 4: Convert to sf object
in_geom <- st_as_sf(polygons)



## load provided geometry
# in_geom<-"P:/312204_pareus/WP2/PGIS_ES_mapping/TRD/TRD.shp"
# in_geom<-st_read(in_geom)
#make sure that crs is 4326
in_geom<-st_transform(in_geom,crs = st_crs("EPSG:4326"))
in_geom<-st_buffer(in_geom, 0)
in_geom<-st_make_valid(in_geom)
## or get it as admin bound from CISCO adjust accordingly
# in_geom<-gisco_get_nuts(year = "2021",
#                          epsg = "4326",
#                          cache = TRUE,
#                          update_cache = FALSE,
#                          cache_dir = NULL,
#                          verbose = FALSE,
#                          resolution = "01",
#                          spatialtype = "RG",
#                          country = cntr_proj,
#                          nuts_id = c("ITF65","ITF64","ITF63"),
#                          nuts_level = "3")
# #nuts<-dat_sf%>%filter(cntrID == cntr_proj)%>%st_buffer(0.6)
# st_crs(in_geom) <- 4326
#ita_nuts_diss<-st_union(nuts)%>%st_sf()

##### establish df
in_df <- data.frame(siteID="OVR",
                 cntrID=cntr_proj, 
                 projID = "pareus",
                 stringsAsFactors=FALSE)

in_df$siteAREAkm2<-as.integer(st_area(in_geom)/1000000)
in_df$siteNAME<-"Overhalla kommune"
in_df$siteN_es <- as.integer(5)
in_df$siteTYPE<-"onshore"
in_df$siteLANG<-"en"
in_df$siteSTATUS<-as.integer(1)
in_df$siteCREATETIME<-Sys.time()
in_df$siteCREATOR <-"r.spielhofer"
in_df$INSTITUTION <-"NINA"
# add geometry
in_df$geometry<-st_as_text(in_geom$geometry)

es_tab = bq_table(project = "pareus", dataset = dataset, table = 'study_site')
bq_table_upload(x = es_tab, values = in_df, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

## france has 2 id's FRL04 = Bouche du Rhone and
## Slovakia has SK021

############## from gisco
in_geom<-gisco_get_nuts(year = "2021",
                         epsg = "4326",
                         cache = TRUE,
                         update_cache = FALSE,
                         cache_dir = NULL,
                         verbose = FALSE,
                         resolution = "01",
                         spatialtype = "RG",
                         country = cntr_proj,
                         nuts_id = c("FRL04"),
                         nuts_level = "3")

##### establish df
in_df <- data.frame(siteID="FRL04",
                    cntrID=cntr_proj, 
                    projID = "pareus",
                    stringsAsFactors=FALSE)

in_df$siteAREAkm2<-as.integer(st_area(in_geom)/1000000)
in_df$siteNAME<-in_geom$NAME_LATN
in_df$siteN_es <- as.integer(5)
in_df$siteTYPE<-"onshore"
in_df$siteLANG<-"fra"
in_df$siteSTATUS<-as.integer(1)
in_df$siteCREATETIME<-Sys.time()
in_df$siteCREATOR <-"r.spielhofer"
in_df$INSTITUTION <-"INRAE"
# add geometry
in_df$geometry<-st_as_text(in_geom$geometry)

es_tab = bq_table(project = "pareus", dataset = dataset, table = 'study_site')
bq_table_upload(x = es_tab, values = in_df, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')

