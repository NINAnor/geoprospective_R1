### stats and graphs for D5.4 and D5.3
library(bigrquery)
library(dplyr)
library(sf)
library(SSDM)
library(tidyverse)
library(googleCloudStorageR)
library(rnaturalearth)
library(rnaturalearthdata)

# upload db
bq_auth(
  path = "dev/gcs_keys/eu-wendy_key.json"
)

dataset<-"wendy_prod"
project_id<-"eu-wendy"

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
################# MAPS of study areas
#### overview map of areas:
# Get Europe map (excluding non-European countries)

europe <- gisco_get_countries(region = "Europe", resolution = "20")

# Get NUTS2 regions
nuts2 <- gisco_get_nuts(nuts_level = 2, resolution = "20")

# Filter for the three regions
highlight_regions <- nuts2 %>%
  filter(NUTS_ID %in% c("ES24", "ITF6", "EL43"))  # Zaragoza, Calabria, Crete

# Add numbering
highlight_regions$Label <- c(1, 2, 3)

# Get ocean data for Norwegian & North Sea
ocean <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")


# Plot the map
ggplot() +
  geom_sf(data = europe, fill = "gray90", color = "black") +
  geom_sf(data = NOR_off, fill = "lightblue", color = NA) + # Base map
  geom_sf(data = highlight_regions, fill = "red", color = "black") +  # Highlight regions
  geom_sf_text(data = highlight_regions, aes(label = Label), color = "black", size = 6) + 
  theme_minimal()+
  coord_sf(xlim=c(-20,45), ylim=c(30, 73) ) 


################### general stats of mapping
es_map_stats<-tbl(con_admin,"es_mappingR1")
es_map_stats<-es_map_stats%>%collect()

## calculate
summary_df <- es_map_stats %>%
  group_by(esID, siteID) %>%
  summarise(
    total_users = n_distinct(userID),  # Count unique userIDs
    total_poss_mapping_false = sum(!poss_mapping),
    total_poss_mapping_true = sum(poss_mapping),# Count FALSE values in poss_mapping
    .groups = "drop"
  )

stacked_df <- summary_df %>%
  pivot_longer(cols = c(total_poss_mapping_true, total_poss_mapping_false),
               names_to = "category",
               values_to = "count")



### loop
unique_esIDs <- unique(stacked_df$esID)
max_count <- max(stacked_df$count, na.rm = TRUE)
for (es in unique_esIDs) {
  p <- ggplot(stacked_df %>% filter(esID == es), aes(x = siteID, y = count, fill = category)) +
    geom_bar(stat = "identity") +  
    geom_hline(yintercept = 2, color = "black", linetype = "dashed", size = 1) +  
    scale_fill_manual(values = c("red", "green")) +
    ylim(0, max_count+1) +
    labs(x = "", y = "N individual maps") +
    theme(axis.text.x = element_text(size = rel(3), angle = 45, hjust = 1),
          axis.text.y = element_text(size = rel(3), angle = 45, hjust = 1),# 3x bigger text
          legend.position = "none")
  
  # Save plot
  ggsave(filename = paste0("plot_", es, ".png"), plot = p, width = 8, height = 6, dpi = 300)
}







#### ES mean maps


## PDF D5.4
es_descr<-tbl(con_admin, "es_descr")
es_descr<-es_descr%>%collect()%>%select(esID,esNAME_en)

### es z values
z_val<-tbl(con_admin, "z_val")
z_val<-z_val%>%collect()
z_val<-z_val%>%left_join(es_descr)


ggplot(z_val, aes(x = esNAME_en, y = z)) +
  geom_col(fill = "steelblue") +  # Bar plot with blue fill
  geom_hline(yintercept = 1, na.rm = TRUE, color = "black", linetype = "dashed") +
  facet_grid(siteID ~ ., scales = "fixed") +  # Facet by siteID, stacked vertically with fixed y-axis
  theme_minimal() +
  labs(title = "", x = "", y = "z-value") +
  theme(axis.text.x = element_text(size = rel(2), angle = 45, hjust = 1),
        axis.text.y = element_text(size = rel(2), angle = 0, hjust = 1),  # 3x bigger text
        strip.text = element_text(size = rel(2)),  # 3x bigger facet labels
        legend.position = "none",
        panel.spacing = unit(2, "lines"))

######################## critical distances [m]
crit_dist<-tbl(con_admin,"crit_dist_val")
crit_dist<-crit_dist%>%collect()

crit_dist <- crit_dist %>%
left_join(es_descr)
unique_esIDs <- unique(crit_dist$esID)

site_colors <- c("GRC" = "#1B9E77", "ESP" = "#D95F02", "ITA" = "#7570B3")
#scale_color_brewer(palette = "Set1") 


for (es in unique_esIDs) {
  es_dist<-crit_dist%>% filter(esID == es)
  p<-ggplot(es_dist) + 
    geom_segment(aes(x = siteID, xend = siteID, y = min_crit_d_m, yend = max_crit_d_m, color = siteID), size = 3) +
    geom_hline(yintercept = mean(c(es_dist$min_crit_d_m, es_dist$max_crit_d_m), na.rm = TRUE), color = "red", linetype = "dashed") +
    theme_bw() +
    labs(title = crit_dist$esNAME_en,
         x = "", y = "Impact distance range (m)")+
    theme(axis.text.x = element_text(size = rel(3), angle = 45, hjust = 1),
          axis.text.y = element_text(size = rel(3), angle = 45, hjust = 1),# 3x bigger text
          legend.position = "none")+
    scale_color_manual(values = site_colors)  # Assign fixed colors
  
  ggsave(filename = paste0("critdist_", es, ".png"), plot = p, width = 8, height = 6, dpi = 300)
}





################ ranking ES plot
es_rank<-tbl(con_admin,"es_ranking")
es_rank<-es_rank%>%collect()


## 
# Order esID by descending pref_adj within each site
es_rank <- es_rank %>%
  group_by(siteID) %>%
  mutate(esID = factor(esID, levels = esID[order(-pref_adj)])) %>%
  ungroup()%>%left_join(es_descr)%>%select(siteID,pref_adj, esID, esNAME_en)


p<-ggplot(es_rank, aes(x = esNAME_en, y = pref_adj)) +
  geom_col(fill = "steelblue") +  # Bar plot with blue fill
  facet_grid(siteID ~ ., scales = "fixed") +  # Facet by siteID, stacked vertically with fixed y-axis
  theme_minimal() +
  labs(title = "", x = "", y = "Relative preference") +
  theme(axis.text.x = element_text(size = rel(2), angle = 45, hjust = 1),
        axis.text.y = element_text(size = rel(2), angle = 0, hjust = 1),  # 3x bigger text
        strip.text = element_text(size = rel(2)),  # 3x bigger facet labels
        legend.position = "none",
        panel.spacing = unit(2, "lines"))

#ggsave(filename = paste0("pref_es.png"), plot = p, width = 20, height = 50, dpi = 300)

path<-"C:/Users/reto.spielhofer/git/NINA_pdf_paper"

#### PDF maps
library(terra)
## including the wind farms
#study_area_data
dat<-read.csv("C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/WENDY/4_case_studies/case_studies_geoprospective.csv")
dat<-dat%>%dplyr::select(siteTYPE,siteSTATUS,windfarmNAME,siteLANG,cntrID,siteID,projID,siteN_es,lng,lat)
dat_sf<-st_as_sf(dat,coords = c("lng","lat"))


##ESP
wt_esp<-dat_sf%>%filter(cntrID == "ESP",windfarmNAME !="Motilla")
st_crs(wt_esp) <- 4326
point_vect <- vect(wt_esp)  # Convert sf to terra vector
buffer_vect <- buffer(point_vect, width = 5000)  

esp<-terra::rast(paste0(path,"/tmp_ESP/PDF_tot_weight_5.6MW.tif"))
terra::crs(esp) <- "EPSG:4326" 
# min max scale
min_val <- min(terra::values(esp), na.rm = TRUE)
max_val <- max(values(esp), na.rm = TRUE)
esp_scaled <- (esp - min_val) / (max_val - min_val)
# Extract mean raster value within the buffer
extracted_values <- extract(esp_scaled, buffer_vect, fun = mean, na.rm = TRUE)


raster_df <- as.data.frame(esp_scaled, xy = TRUE)  # Extract x, y, and values
colnames(raster_df) <- c("x", "y", "scaled_PDF")  # Rename for clarity

esp<-ggplot(wt_esp) +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = scaled_PDF)) +  # Raster layer
  scale_fill_viridis_c(option = "plasma") +  # Nice color scale
  geom_sf(fill = NA, color = "blue", size = 5) + 
  geom_sf_text(data = wt_esp, aes(label = windfarmNAME), color = "white", size = 4) + 
  # Outline only
  #labs(title = "PDF Saragossa & wind farms", fill = "scaled_PDF_5MW") +
  theme_minimal() +
  theme(legend.title = element_blank()) 
ggsave(filename = paste0("pdf_ESP_5.6MW.png"), plot = esp, width = 10, height = 10, dpi = 300)


## ggplot with histogram of PDF values and the two values
g<-ggplot(raster_df, aes(x = scaled_PDF)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +  # Histogram
  geom_vline(data= extracted_values, aes(xintercept = PDF_tot_weight_5.6MW), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = mean(raster_df$scaled_PDF)), color = "black", linetype = "dashed", size = 1.2) +# Vertical line at mean
  labs(title = "", x = "PDF value", y = "Count") +
  theme_minimal()
ggsave(filename = paste0("hist_ESP_5_6MW.png"), plot = g, width = 10, height = 5, dpi = 300)


#ITA
wt_ita<-dat_sf%>%filter(cntrID == "ITA",windfarmNAME !="Motilla")
st_crs(wt_ita) <- 4326
point_vect <- vect(wt_ita)  # Convert sf to terra vector
buffer_vect <- buffer(point_vect, width = 5000)  
ita<-terra::rast(paste0(path,"/tmp_ITA/PDF_tot_weight_5MW.tif"))
terra::crs(ita) <- "EPSG:4326" 

# min max scale
min_val <- min(terra::values(ita), na.rm = TRUE)
max_val <- max(values(ita), na.rm = TRUE)
ita_scaled <- (ita - min_val) / (max_val - min_val)
extracted_values <- extract(ita_scaled, buffer_vect, fun = mean, na.rm = TRUE)
raster_df <- as.data.frame(ita_scaled, xy = TRUE)  # Extract x, y, and values
colnames(raster_df) <- c("x", "y", "scaled_PDF")  # Rename for clarity


ita<-ggplot(wt_ita) +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = scaled_PDF)) +  # Raster layer
  scale_fill_viridis_c(option = "plasma") +  # Nice color scale
  geom_sf(fill = NA, color = "blue", size = 5) + 
  geom_sf_text(data = wt_ita, aes(label = windfarmNAME), color = "black", size = 6) + 
  # Outline only
  #labs(title = "PDF Saragossa & wind farms", fill = "scaled_PDF_5MW") +
  theme_minimal() +
  theme(legend.title = element_blank()) 
ggsave(filename = paste0("pdf_ITA_5MW.png"), plot = ita, width = 10, height = 10, dpi = 300)


g<-ggplot(raster_df, aes(x = scaled_PDF)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +  # Histogram
  geom_vline(data= extracted_values, aes(xintercept = PDF_tot_weight_5MW), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = mean(raster_df$scaled_PDF)), color = "black", linetype = "dashed", size = 1.2) +# Vertical line at mean
  labs(title = "", x = "PDF value", y = "Count") +
  theme_minimal()
ggsave(filename = paste0("hist_ITA_5MW.png"), plot = g, width = 10, height = 5, dpi = 300)



#GRC
wt_grc<-dat_sf%>%filter(cntrID == "GRC")
st_crs(wt_grc) <- 4326
point_vect <- vect(wt_grc)  # Convert sf to terra vector
buffer_vect <- buffer(point_vect, width = 5000)  
grc<-terra::rast(paste0(path,"/tmp_GRC/PDF_tot_weight_5MW.tif"))
terra::crs(grc) <- "EPSG:4326" 
# min max scale
min_val <- min(terra::values(grc), na.rm = TRUE)
max_val <- max(values(grc), na.rm = TRUE)
grc_scaled <- (grc - min_val) / (max_val - min_val)
extracted_values <- extract(grc_scaled, buffer_vect, fun = mean, na.rm = TRUE)
raster_df <- as.data.frame(grc_scaled, xy = TRUE)  # Extract x, y, and values
colnames(raster_df) <- c("x", "y", "scaled_PDF")  # Rename for clarity


grc<-ggplot(wt_grc) +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = scaled_PDF)) +  # Raster layer
  scale_fill_viridis_c(option = "plasma") +  # Nice color scale
  geom_sf(fill = NA, color = "blue", size = 5) + 
  geom_sf_text(data = wt_grc, aes(label = windfarmNAME), color = "white", size = 4) + 
  # Outline only
  #labs(title = "PDF Saragossa & wind farms", fill = "scaled_PDF_5MW") +
  theme_minimal() +
  theme(legend.title = element_blank()) 
ggsave(filename = paste0("pdf_GRC_5MW.png"), plot = grc, width = 10, height = 10, dpi = 300)


g<-ggplot(raster_df, aes(x = scaled_PDF)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +  # Histogram
  geom_vline(data= extracted_values, aes(xintercept = PDF_tot_weight_5MW), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = mean(raster_df$scaled_PDF)), color = "black", linetype = "dashed", size = 1.2) +# Vertical line at mean
  labs(title = "", x = "PDF value", y = "Count") +
  theme_minimal()
ggsave(filename = paste0("hist_GRC_5MW.png"), plot = g, width = 10, height = 5, dpi = 300)
