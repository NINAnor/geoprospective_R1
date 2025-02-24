## upload pareus helper
library(bigrquery)
library(sf)
library(dplyr)
library(giscoR)
library(ggplot2)

dev <- "dev" #prod
cntr_proj<-"FRA"


# upload db
bq_auth(
  path = "dev/gcs_keys/pareus_key.json"
)

dataset<-paste0("pareus_",dev)

## load provided geometry
in_geom<-"P:/312204_pareus/WP2/test_case_FRA/rectangle_around_inrae/myExportTableTask.shp"
in_geom<-st_read(in_geom)
#make sure that crs is 4326
in_geom<-st_transform(in_geom,crs = st_crs("EPSG:4326"))
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
in_df <- data.frame(siteID="FRA_test",
                 cntrID=cntr_proj, 
                 projID = "pareus",
                 stringsAsFactors=FALSE)

in_df$siteAREAkm2<-as.integer(st_area(in_geom)/1000000)
in_df$siteNAME<-"Test site INRAE"
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


