update_polygon_area_and_check <- function(polygon_sf, modified) {
  #print("-----------")
  
  if(site_type == "onshore"){
    resolution = 250^2
  }else{
    resolution = 500^2
  }
  
  #with res of 250m grid we can sample at least 10 pts with variaton within 0.6km2
  A_min<-0.7
  #A_max<-0.05*round(as.numeric(st_area(sf_stud_geom)),0)
  A_max<-A_min*20
  
  max_rectangles = 5
  
  
  area_km2 <- st_area(st_transform(polygon_sf, 3857)) / 1e6 # Transform to meters first
  area_km2 <- as.numeric(area_km2)
  #print(polygon_sf)
  
  leaf_id<-polygon_sf$leaflet_id
  #print(paste0("the new one", leaf_id))
  
  # Get the existing polygons
  existing_polygons <- drawn_polygons()
  
  
  #when modified adjust existing pols before intersection check
  if(modified == TRUE){
    #print("yes i am modified")
    #existing_polygons<-existing_polygons%>%filter(leaflet_id != leaf_id)
    existing_polygons <- existing_polygons %>%
      group_by(leaflet_id) %>%
      filter(leaflet_id != leaf_id & time_stamp == max(time_stamp)) %>%
      ungroup()
    
  }else{
    #print("just drawn not modified")
  }
  
  
  # Check for intersection but not consider the empty starting polygon
  intersects <- FALSE
  if (nrow(existing_polygons%>%filter(leaflet_id != 0)) > 0) {
    #print("fourth")
    # Union of existing polygons to create a single geometry
    existing_union <- st_union(existing_polygons)
    intersects <- st_intersects(polygon_sf, existing_union, sparse = FALSE)[1]
  }
  
  
  
  #print(intersects)
  # Check if the polygon is completely within the study area
  within_study_area <- st_within(polygon_sf, sf_stud_geom, sparse = FALSE)[1]
  
  if (area_km2 < A_max && !intersects && within_study_area) {
    #print("fifth")
    # Display success popup alert for valid polygon
    polygon_sf$valid = TRUE
    cleaned_pols<-st_sf(rbind(existing_polygons,polygon_sf))%>%filter(leaflet_id != 0)
    
    #print(paste0("The cleaned poly to be added: ",nrow(cleaned_pols)))
    
    drawn_polygons(cleaned_pols)
    #drawn_polygons(rbind(drawn_polygons(), cleaned_pols))
    #print(paste0("and this should be the sum +1draw, +0 mod, -1 del of original and new values:" ,nrow(drawn_polygons())))
    
    shinyalert(
      title = "Valid rectangle!",
      text = paste("The area is", round(area_km2, 2), "km²."),
      type = "success",
      showCancelButton = TRUE,
      cancelButtonText = "Draw further rectangles or make edits",
      confirmButtonText = "Done with mapping",
      closeOnClickOutside = FALSE,
      callbackR = function(confirm) {
        if (confirm) {
          
          update_final_map()  # Update the final map with annotations
          update_rectangle_ids()  # Update the rectangle ID display
          
        } else {
          # Save the polygon and allow further drawing when "Proceed" is clicked
          
          drawn_polygons(cleaned_pols)
          
          
          if (nrow(cleaned_pols) == max_rectangles) {
            # Disable drawing if the max has been reached
            leafletProxy("map") %>%
              removeDrawToolbar() %>%
              add_edit_toolbar(.)
            
            shinyalert(
              title = "Maximum rectangles reached!",
              text = "You cannot draw more than 5 rectangles.",
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "Done with mapping",
              cancelButtonText = "Make edits",
              closeOnClickOutside = FALSE,
              callbackR = function(confirm) {
                if (confirm) {
                  # Save and proceed
                  
                  drawn_polygons(cleaned_pols)
                  update_rectangle_ids()
                  update_final_map()
                } else {
                  # Allow editing polygons
                  leafletProxy("map") %>%
                    removeDrawToolbar() %>%
                    add_edit_toolbar(.)
                }
              }
            )
          }else{
            leafletProxy("map") %>%
              removeDrawToolbar() %>%
              add_full_toolbar(.)
          }
        }
      }
    )
    
    
  } else if (intersects) {
    #       
    shinyalert(
      title = "No overlay!",
      text = "This rectangle overlays with another rectangle. Please modify or delete your last drawn rectangle.",
      type = "error",
      showCancelButton = FALSE,
      confirmButtonText = "Edit rectangles",
      closeOnClickOutside = FALSE,
      callbackR = function(confirm) {
        if (confirm) {
          ## update the reactive value with the intersection polygon... yes that's wrong but like this either first or second polygon can be modified
          cleaned_pols<-st_sf(rbind(existing_polygons,polygon_sf))%>%filter(leaflet_id != 0)
          drawn_polygons(cleaned_pols)
          
          leafletProxy("map") %>%
            removeDrawToolbar() %>%
            add_edit_toolbar(.)
          
        }
      })
    
    
  } else if (!within_study_area) {
    # Display error popup alert for out of study area
    shinyalert(
      title = "Out of Study Area!",
      text = "The rectangle must be completely within the defined study area.",
      type = "error"
    )
    
    
    # Disable drawing but keep edit/remove active
    leafletProxy("map") %>%
      removeDrawToolbar() %>%
      add_edit_toolbar(.)
    
  } else {
    # Display error popup alert for large area
    shinyalert(
      title = "Too large!",
      text = paste("Rectangle area is", round(area_km2, 2), "km². Must be smaller than 100 km²."),
      type = "error"
    )
    
    
    # Disable drawing but keep edit/remove active
    leafletProxy("map") %>%
      removeDrawToolbar() %>%
      add_edit_toolbar(.)
  }
  
}