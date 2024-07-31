library(bigrquery)
library(sf)

# upload db
bq_auth(
  path = "bq_wendy.json"
)


#study_area_data
dat<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/4_case_studies/overview_case_studies.csv")
dat<-dat%>%dplyr::select(siteTYPE,siteNAME,siteSTATUS,siteLANG,cntrID,siteID,projID,siteN_es,lng,lat)
dat_sf<-st_as_sf(dat,coords = c("lng","lat"))
# st_crs(dat_sf)<-4326
rect<-st_buffer(dat_sf, dist=0.15,endCapStyle = "SQUARE")
sf_stud_geom<-st_set_crs(rect,4326)

sf_stud_geom$siteAREAkm2<-as.integer(round(as.numeric(st_area(sf_stud_geom))/1000000,0))
sf_stud_geom$siteCREATETIME<-Sys.time()
sf_stud_geom$siteCREATOR <-"eu-wendy-proj"

# st_write(sf_stud_geom,"stud_area.shp")
polygons<-sf_stud_geom%>%st_drop_geometry()
polygons$geometry<-st_as_text(sf_stud_geom$geometry)
# #
# # #save it on bq
poly_table = bq_table(project = "eu-wendy", dataset = "wendy_dev", table = 'studSITE')
bq_table_upload(x = poly_table, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')


mapview(rect)


## upload to wendy

## ecosystem services
dat<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/setup_230710/es_descr.csv")
es_tab = bq_table(project = "eu-wendy", dataset = "wendy_dev", table = 'es_descr')
bq_table_upload(x = es_tab, values = dat, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
