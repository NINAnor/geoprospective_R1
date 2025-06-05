#### test extrapolation RF / SSD sampling strategy

# Load required libraries
library(sf)
library(raster)
library(sp)
library(SSDM)
library(dplyr)
library(bigrquery)

setwd("C:/Users/reto.spielhofer/git/wendy_geopros_R1")
cred_path<-paste0("dev/gcs_keys/eu-wendy_key.json")
dataset <- "wendy_prod"
bq_auth(
  path = cred_path
)

project_id<-"eu-wendy"
con_admin<-data.frame(
  project = project_id,
  dataset = dataset,
  billing = project_id
)

con_admin <- dbConnect(
  bigrquery::bigquery(),
  project = con_admin$project,
  dataset = con_admin$dataset,
  billing = con_admin$billing
)

site<-tbl(con_admin, "study_site")
# site<-site%>%collect()%>%filter(projID == bqprojID & siteID == site_id)
# site<-site%>%collect()%>%filter(siteID == "FRA_test")
site<-site%>%collect()%>%filter(siteID == "NOR")
sf_stud_geom <- sf::st_as_sf(site, wkt = "geometry" )%>%st_set_crs(4326)

#### the example training polys
train_pols<-tbl(con_admin, "ind_polys_R1")
# train_pols<-train_pols%>%collect()%>%filter(userID == "8xYE2MuH")
train_pols<-train_pols%>%collect()%>%filter(userID == "k4n4VPnLKzbJ")
polygons <- sf::st_as_sf(train_pols, wkt = "geometry" )%>%st_set_crs(4326)

# Set working directory (change as needed)
setwd("C:/Users/reto.spielhofer/git/wendy_geopros_R1/dev/env_var_NOR")

# Load three raster layers
acc <- raster("acc.tif")
dem <- raster("dem.tif")
int <- raster("int.tif")
lulc <- raster("lulc.tif")

# Stack rasters for prediction
pred_stack <- stack(acc, dem, int, lulc)



#################### introduce NA raster values
# Step 3: Convert sf Polygon to Raster Mask ----------------------------
r_mask <- rasterize(polygons, dem, mask=TRUE)  # Create a mask with NA inside polygon

# Step 4: Randomly Assign NA Values Within the Polygon -----------------
set.seed(123)  # Ensure reproducibility
for (i in 1:4) {  # Loop over each layer in raster stack
  r_layer <- pred_stack[[i]]  # Extract layer
  cells <- which(!is.na(values(r_layer) * values(r_mask)))  # Identify non-NA pixels inside polygon
  na_cells <- sample(cells, size = round(0.3 * length(cells)))  # Randomly select 30% of cells
  values(r_layer)[na_cells] <- NA  # Assign NA to selected cells
  pred_stack[[i]] <- r_layer  # Replace the layer with updated values
}


# raster_extent <- extent(pred_stack)
# raster_area <- (raster_extent@xmax - raster_extent@xmin) * (raster_extent@ymax - raster_extent@ymin)

# # Define max polygon area (10% of raster)
# max_polygon_area <- raster_area * 0.001
# 
# # Generate two polygons that sum up to max 10% of raster area
# poly1_area <- max_polygon_area * 0.5  # 50% of the allowed area
# poly2_area <- max_polygon_area * 0.5  # 50% of the allowed area
# 
# # Function to create a polygon of a given area within raster extent
# create_polygon <- function(r_extent, target_area,raster_stack) {
#   x_min <- r_extent@xmin
#   x_max <- r_extent@xmax
#   y_min <- r_extent@ymin
#   y_max <- r_extent@ymax
#   
#   # Define width and height based on target area
#   width <- sqrt(target_area)
#   height <- target_area / width
#   
#   # Randomly select a starting point within bounds
#   x_start <- runif(1, x_min, x_max - width)
#   y_start <- runif(1, y_min, y_max - height)
#   
#   # Create polygon
#   poly <- st_as_sf(st_sfc(st_polygon(list(rbind(
#     c(x_start, y_start),
#     c(x_start + width, y_start),
#     c(x_start + width, y_start + height),
#     c(x_start, y_start + height),
#     c(x_start, y_start)
#   )))), crs = crs(raster_stack))
#   
#   return(poly)
# }
# 
# # Create two polygons
# poly1 <- create_polygon(raster_extent, poly1_area,pred_stack)
# poly2 <- create_polygon(raster_extent, poly2_area,pred_stack)
# # Combine polygons into one object
# polygons <- rbind(poly1, poly2)


##### import a PAREUS test polygon



plot(pred_stack[[1]], main="Raster Layer with Random NAs Inside Polygon")
plot(st_geometry(polygons), add = TRUE, col = rgb(1, 0, 0, 0.5))  # Transparent red

# Sample points within polygons
# sample_points <- function(polygon, num_points) {
#   st_sample(polygon, num_points, type = "random") %>%
#     st_as_sf() %>%
#     st_coordinates() %>%
#     as.data.frame() %>%
#     setNames(c("lon", "lat"))
# }
# 
# # Define number of points per polygon
# num_points_poly1 <- 10
# num_points_poly2 <- 5
# 
# # Generate sampled points
# points1 <- sample_points(poly1, num_points_poly1)
# points2 <- sample_points(poly2, num_points_poly2)
# points1$pol_nr<-"pol1"
# points2$pol_nr<-"pol2"
# # Combine sample points
# pts <- rbind(points1, points2)
# pts$SPECIES <- "pres"

#### testing with absence pts
A_roi<-as.numeric(sf_stud_geom$siteAREAkm2*10^6)
#N cells
all_back_pts<- round(A_roi/1000^2,0)

# ## although zooming on the map while drawing is limited, we assure that at least 10pts are within a poly
min_in_pts<-100000

##### test presence absence modelling
polygons <- polygons[-3, ]

# Modify 'esVal' of the second row (row index 2) given the es value would be - thus es absence
polygons$es_value[2] <- -3 



# Initialize empty object
pts_in <- NULL

for (i in 1:nrow(polygons)) {
  # Get polygon area
  A_tmp <- as.numeric(st_area(polygons[i,]))
  tmp_ratio <- A_tmp / A_roi
  tmp_pts <- round(all_back_pts * tmp_ratio, 0)
  
  # Extract es_value as numeric
  tmp_es_val <- as.numeric(polygons[i,]$es_value)
  
  # Adjust points based on es_value
  if (tmp_es_val > 0) {
    tmp_es_val <- (1 + tmp_es_val) / 5  # Scale 0-5
    n_pts <- round(tmp_pts * tmp_es_val, 0)
    inside_flag <- 1
  } else {
    tmp_es_val <- (1 + abs(tmp_es_val)) / 5  # Ensure valid scaling
    n_pts <- round(tmp_pts * tmp_es_val, 0)
    inside_flag <- 0
  }
  
  # Ensure at least `min_in_pts` are sampled
  n_pts <- max(n_pts, min_in_pts)
  
  # Sample points within the polygon
  if (n_pts > 0) {
    sampled_pts <- st_sample(polygons[i,], n_pts, type = "random")
    
    if (length(sampled_pts) > 0) {
      sampled_pts <- st_as_sf(sampled_pts)
      sampled_pts$inside <- inside_flag
      
      # Merge with existing points
      if (is.null(pts_in)) {
        pts_in <- sampled_pts
      } else {
        pts_in <- rbind(pts_in, sampled_pts)
      }
    }
  }
}


###### or like original mapping process
for (i in 1:nrow(polygons)) {
  A_tmp <- as.numeric(st_area(polygons[i,]))
  tmp_ratio<-A_tmp/A_roi
  tmp_pts<-round(all_back_pts*tmp_ratio,0)
  
  if(tmp_pts<=min_in_pts){
    tmp_pts<-min_in_pts
  }else{
    tmp_pts<-tmp_pts
  }
  # npts in this poly must be max_pts*tmp_ratio*es_value
  #+1 its 0-5 scale
  tmp_es_val<-((1+polygons[i,]$es_value)/5)
  tmp_pts = st_sample(polygons[i,], round(tmp_pts*tmp_es_val,0),type="random")
  tmp_pts<-st_as_sf(tmp_pts)
  tmp_pts$inside<-rep(1,nrow(tmp_pts))
  if(i==1){
    pts_in<-tmp_pts
  }else{
    pts_in<-rbind(pts_in,tmp_pts)
  }
  
}

###########
pred_w<-raster::stack(dem*1, lulc*1, int*1, acc*0.2)
pts_in<-st_transform(pts_in,st_crs(dem))
# Extract values from the raster stack
extracted_values <- terra::extract(pred_stack, pts_in)

# Combine with original point data
pts_in <- cbind(pts_in, extracted_values)

pts_in <- cbind(pts_in, st_coordinates(pts_in))  # Adds X (lon) & Y (lat)
colnames(pts_in)[colnames(pts_in) %in% c("X", "Y")] <- c("lon", "lat")
pts_in <- st_drop_geometry(pts_in)

pts_in <- na.omit(pts_in)
pts_in<-pts_in%>%dplyr::select(inside, lon, lat)

if(nrow(pts_in)>1000){
  pts_in<-pts_in[sample(nrow(pts_in), 1000), ]
}





# Train SSD model (Species Distribution Model)
      SDM <- SSDM::modelling('RF', pts_in,  
                             pred_w, Xcol = 'lon', Ycol = 'lat',cv = "holdout",
                             cv.param = c(0.7, 2),
                             final.fit.data = "all")

      
prediction_SDM<-SDM@projection
# plot
plot(prediction_SDM)
plot(st_geometry(polygons), add = TRUE, col = rgb(1, 0, 0, 0.5)) 
# plot(st_geometry(pts$lon,pts$lat), add=T)
SDM@evaluation
########## RF

######### systematic evaluation with algo and pts size
# Sample size values to iterate over
sample_sizes <- c(10, 50, 100, 200, 600, 3000, 6000, 12000)

# Algorithms to test
algorithms <- c("MARS")

# Initialize results dataframe
results <- data.frame(SampleSize = integer(),
                      Algorithm = character(),
                      AUC = numeric(),
                      Kappa = numeric(),
                      T_diff = numeric(),
                      stringsAsFactors = FALSE
                      )

# Function to sample points within polygons
sample_points <- function(polygon, num_points) {
  st_sample(polygon, num_points, type = "random") %>%
    st_as_sf() %>%
    st_coordinates() %>%
    as.data.frame() %>%
    setNames(c("lon", "lat"))
}

# Loop over sample sizes and algorithms
for (n in sample_sizes) {
  print("----------new sample size")
  pts <- do.call(rbind, lapply(1:nrow(polygons), function(i) {
    sample_points(polygons[i, ], round(n / nrow(polygons),0))
  }))
  
  pts$SPECIES <- "pres"  # Assign presence label
  
  for (algo in algorithms) {
    # Train model
    startT<-Sys.time()
    SDM <- SSDM::modelling(algo, pts, pred_stack, Xcol = "lon", Ycol = "lat",cv = "holdout",
                           cv.param = c(0.7, 2),
                           final.fit.data = "all")
    endT<-Sys.time()
    
    # Extract evaluation metrics
    auc <- SDM@evaluation$AUC
    kappa <- SDM@evaluation$Kappa
    
    # Append results
    results <- rbind(results, data.frame(SampleSize = n,
                                         Algorithm = algo,
                                         AUC = auc,
                                         Kappa = kappa,
                                         T_diff = as.numeric(endT-startT)))
  }
}

# Save results as CSV
write.csv(results, "model_performance_FRA_test.csv", row.names = FALSE)

# Plot AUC and Kappa for different sample sizes and models
ggplot(results, aes(x = SampleSize, y = AUC, color = Algorithm, group = Algorithm)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +  # Log scale for better visualization
  labs(title = "AUC Performance Across Sample Sizes",
       x = "Sample Size",
       y = "AUC") +
  theme_minimal()

#Kappa
ggplot(results, aes(x = SampleSize, y = Kappa, color = Algorithm, group = Algorithm)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +  # Log scale for better visualization
  labs(title = "Kappa Performance Across Sample Sizes",
       x = "Sample Size",
       y = "Kappa") +
  theme_minimal()

#Time
ggplot(results, aes(x = SampleSize, y = T_diff, color = Algorithm, group = Algorithm)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +  # Log scale for better visualization
  labs(title = "Kappa Performance Across Sample Sizes",
       x = "Sample Size",
       y = "TIME") +
  theme_minimal()



