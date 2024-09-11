library(bigrquery)
library(sf)
library(dplyr)
library(giscoR)
library(ggplot2)



# upload db
bq_auth(
  path = "dev/gcs_keys/eu-wendy_key.json"
)


#study_area_data
dat<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/4_case_studies/case_studies_geoprospective.csv")
dat<-dat%>%dplyr::select(siteTYPE,siteSTATUS,windfarmNAME,siteLANG,cntrID,siteID,projID,siteN_es,lng,lat)
dat_sf<-st_as_sf(dat,coords = c("lng","lat"))




### ITALY

# for the italian case we select the larger region of Regio Calabria (	ITF65) containing both study sites and clipped by the shore line
ita_nuts<-gisco_get_nuts(year = "2021",
                         epsg = "4326",
                         cache = TRUE,
                         update_cache = FALSE,
                         cache_dir = NULL,
                         verbose = FALSE,
                         resolution = "01",
                         spatialtype = "RG",
                         country = "ITA",
                         nuts_id = c("ITF65","ITF64","ITF63"),
                         nuts_level = "3")
wt_ita<-dat_sf%>%filter(cntrID == "ITA")
st_crs(wt_ita) <- 4326

ita_nuts_diss<-st_union(ita_nuts)%>%st_sf()


ggplot() +
  geom_sf(data = ita_nuts_diss, fill = "lightblue", color = "black") +  # Plot the polygon
  geom_sf(data = wt_ita, color = "red", size = 3) +  # Plot the point on top
  theme_minimal()

ita_nuts_diss$siteAREAkm2<-as.integer(st_area(ita_nuts_diss)/1000000)
ita_nuts_diss$siteNAME<-"South Calabria"
ita_nuts_diss$siteN_es <- as.integer(5)
ita_nuts_diss$projID<-"wendy"
ita_nuts_diss$siteID<-"ITA"
ita_nuts_diss$siteSTATUS<-as.integer(1)
ita_nuts_diss$siteCREATETIME<-Sys.time()
ita_nuts_diss$siteCREATOR <-"r.spielhofer"
ita_nuts_diss$respPARTNER <-"EGP"
ita_nuts_diss$siteTYPE<-"onshore"
ita_nuts_diss$siteLANG<-"en"
ita_nuts_diss$cntrID<-"ITA"
ita_nuts_diss$windfarmNAME<-"San Florio and Bagaladi Motta Montebello"
polygons<-ita_nuts_diss%>%st_drop_geometry()
polygons$geometry<-st_as_text(ita_nuts_diss$geometry)

es_tab = bq_table(project = "eu-wendy", dataset = "wendy_dev", table = 'study_site')
bq_table_upload(x = es_tab, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')





###### Spain, we just consider the three northern case studies, not the

wt_esp<-dat_sf%>%filter(cntrID == "ESP",windfarmNAME !="Motilla")%>%st_buffer(0.5)
st_crs(wt_esp) <- 4326
bbox_wf<-st_bbox(wt_esp)%>%st_as_sfc()%>%st_sf()

esp_nuts<-gisco_get_nuts(year = "2021",
                         epsg = "4326",
                         cache = TRUE,
                         update_cache = FALSE,
                         cache_dir = NULL,
                         verbose = FALSE,
                         resolution = "01",
                         spatialtype = "RG",
                         country = "ESP",
                         nuts_id = NULL,
                         nuts_level = "3")



# st_area(bbox_wf)/1000000
wt_esp<-st_join(wt_esp,esp_nuts,join = st_intersects)
nuts_uni<-wt_esp%>%distinct(wt_esp$NUTS_ID)

nuts_filter<-esp_nuts%>%filter(NUTS_ID %in% nuts_uni$`wt_esp$NUTS_ID`)

#st_write(bbox_wf,"bbox_esp.shp",append = F)


ggplot() +
  geom_sf(data = nuts_filter, fill = "lightblue", color = "black") +  # Plot the polygon
  geom_sf(data = wt_esp, color = "red", size = 3) +  # Plot the point on top
  geom_sf(data = bbox_wf, fill = NA, color = "red", linetype = "dashed") +
  theme_minimal()

## in spain we take the bbox of the three northern wf with a buffer of 0.5 degree
bbox_wf$siteAREAkm2<-as.integer(st_area(bbox_wf)/1000000)
bbox_wf$siteNAME<-"Greater Zaragoza area"
bbox_wf$siteN_es <- as.integer(5)
bbox_wf$projID<-"wendy"
bbox_wf$siteID<-"ESP"
bbox_wf$siteSTATUS<-as.integer(1)
bbox_wf$siteCREATETIME<-Sys.time()
bbox_wf$siteCREATOR <-"r.spielhofer"
bbox_wf$respPARTNER <-"CIRCE"
bbox_wf$siteTYPE<-"onshore"
bbox_wf$siteLANG<-"en"
bbox_wf$cntrID<-"ESP"
bbox_wf$windfarmNAME<-"El Campo, Campo Olivia and Primoral"
polygons<-bbox_wf%>%st_drop_geometry()
polygons$geometry<-st_as_text(bbox_wf$geometry)

es_tab = bq_table(project = "eu-wendy", dataset = "wendy_dev", table = 'study_site')
bq_table_upload(x = es_tab, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')



#### For greece we take the whole crete island (EL43)

grc_nuts<-gisco_get_nuts(year = "2021",
                         epsg = "4326",
                         cache = TRUE,
                         update_cache = FALSE,
                         cache_dir = NULL,
                         verbose = FALSE,
                         resolution = "01",
                         spatialtype = "RG",
                         country = NULL,
                         nuts_id = c("EL43"),
                         nuts_level = "2")
grc_nuts<-grc_nuts%>%select(geometry)
st_area(grc_nuts)/1000000
wt_grc<-dat_sf%>%filter(cntrID == "GRC")%>%st_buffer(0.5)
st_crs(wt_grc) <- 4326
# bbox_wf<-st_bbox(wt_esp)%>%st_as_sfc()
# st_area(bbox_wf)/1000000
# wt_esp<-st_join(wt_esp,esp_nuts,join = st_intersects)



ggplot() +
  geom_sf(data = grc_nuts, fill = "lightblue", color = "black") +  # Plot the polygon
  geom_sf(data = wt_grc, color = "red", size = 3) +  # Plot the point on top
  #geom_sf(data = bbox_wf, fill = NA, color = "red", linetype = "dashed") +
  theme_minimal()

grc_nuts$siteAREAkm2<-as.integer(st_area(grc_nuts)/1000000)
grc_nuts$siteNAME<-"Crete"
grc_nuts$siteN_es <- as.integer(5)
grc_nuts$projID<-"wendy"
grc_nuts$siteID<-"GRC"
grc_nuts$siteSTATUS<-as.integer(1)
grc_nuts$siteCREATETIME<-Sys.time()
grc_nuts$siteCREATOR <-"r.spielhofer"
grc_nuts$respPARTNER <-"MEC"
grc_nuts$siteTYPE<-"onshore"
grc_nuts$siteLANG<-"en"
grc_nuts$cntrID<-"GRC"
grc_nuts$windfarmNAME<-"Minoan"
polygons<-grc_nuts%>%st_drop_geometry()
polygons$geometry<-st_as_text(grc_nuts$geometry)

es_tab = bq_table(project = "eu-wendy", dataset = "wendy_dev", table = 'study_site')
bq_table_upload(x = es_tab, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')


all<-rbind(grc_nuts,bbox_wf,ita_nuts_diss)
st_write(all,"stud_sites.shp")









## to derive the study site name for geoprospective, get NUTS
# comm<-gisco_get_communes(
#   year = "2016",
#   epsg = "4326",
#   cache = TRUE,
#   update_cache = FALSE,
#   cache_dir = NULL,
#   verbose = FALSE,
#   spatialtype = "RG",
#   country = c("ITA","ESP","GRC")
# )
# 
# 
# dat_sf<-st_set_crs(dat_sf,4326)
# dat_sf<-st_join(dat_sf,comm,join = st_intersects)
dat_sf<-dat_sf%>%dplyr::select(siteTYPE,siteSTATUS,windfarmNAME,siteLANG,cntrID,siteID,projID,siteN_es,COMM_NAME)
# 
# # st_crs(dat_sf)<-4326
# rect<-st_buffer(dat_sf, dist=0.2,endCapStyle = "ROUND")
# sf_stud_geom<-st_set_crs(rect,4326)
# 
# ## clip all of them with coast
# onshore<-sf_stud_geom%>%filter(siteTYPE == "onshore")
# offshore<-sf_stud_geom%>%filter(siteTYPE == "offshore")
# 
# 
# 
# 
# cntr<-gisco_get_countries(year = "2020",
#                           epsg = "4326",
#                           cache = TRUE,
#                           update_cache = FALSE,
#                           cache_dir = NULL,
#                           verbose = FALSE,
#                           resolution = "01",
#                           spatialtype = "RG",
#                           country = NULL,
#                           region = "Europe")
# cntr<-cntr%>%filter(CNTR_ID != "RU")
# 
# on_int<-sf::st_intersection(cntr,onshore)%>%dplyr::select("siteTYPE","siteSTATUS","windfarmNAME","siteLANG","cntrID","siteID","projID","siteN_es","COMM_NAME")
# plot(on_int)
# 
# ## rbin offshore
# all<-rbind(on_int,offshore)


# all$siteAREAkm2<-as.integer(round(as.numeric(st_area(all))/1000000,0))
# all$siteCREATETIME<-Sys.time()
# all$siteCREATOR <-"eu-wendy-proj"


# colnames(all)[colnames(all) == "COMM_NAME"] <- "siteNAME"




st_write(all,"study_sites.shp")

# st_write(sf_stud_geom,"stud_area.shp")
polygons<-all%>%st_drop_geometry()
polygons$geometry<-st_as_text(all$geometry)
# #
# # #save it on bq
poly_table = bq_table(project = "eu-wendy", dataset = "wendy_dev", table = 'study_site')
bq_table_upload(x = poly_table, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')


mapview(rect)


## upload to wendy

## ecosystem services
dat<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/PGIS_ES/data_base/setup_230710/es_descr.csv")
es_tab = bq_table(project = "eu-wendy", dataset = "wendy_dev", table = 'es_descr')
bq_table_upload(x = es_tab, values = dat, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
