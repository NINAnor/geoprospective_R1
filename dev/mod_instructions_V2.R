#' training UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

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
mod_instructions_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      uiOutput(ns("task_0")),
      uiOutput(ns("task_1")),
      uiOutput(ns("task_2"))
      
      
    )#/main panel
    
  )
}

#' training Server Functions
#'
#' @noRd
mod_instructions_server <- function(id,sf_stud_geom,userID,site_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv1<-reactiveValues(
      u = reactive({})
    )
    ## ES shortly explained
    output$task_0<-renderUI({
      tagList(
        value_box(title="",
                  h5(paste0("During the study you are going to use an interactive web map of ",sf_stud_geom$siteNAME,". The next section shows you how to use the map and how to indicate areas you benefit from nature.")),
                  br(),
        ),
        
        br(),
        actionButton(ns("to_task1"),"How to map", style="color: black; background-color: #31c600; border-color: #31c600")
      )
    })
    
    output$es_quest_how<-renderUI(tagList(
      value_box(
        title = "",
        value = paste0("For each rectangle indicate, how well you think they are suited for a sunday hike?"),
        theme = value_box_theme(bg = orange, fg = "black"),
        showcase = bs_icon("question-octagon-fill")
      )
    ))

    
    # Function to add the drawing toolbar with only editing and removing options
    add_edit_toolbar <- function(map) {
      map %>%
        addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          rectangleOptions = FALSE,  # Disable drawing new rectangles
          editOptions = list(edit = TRUE, remove = TRUE)  # Keep edit/remove options enabled
        )
    }
    
    # Function to add the full drawing toolbar (including rectangle drawing)
    add_full_toolbar <- function(map) {
      map %>%
        addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          rectangleOptions = drawRectangleOptions(
            showArea = TRUE,
            shapeOptions = drawShapeOptions(
              clickable = TRUE
            )
          ),  # Enable drawing new rectangles
          editOptions = list(edit = TRUE, remove = TRUE, clearAll = FALSE)# Keep edit/remove options enabled
        )
    }
    
    output$map <- renderLeaflet({
      leaflet(sf_stud_geom) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik,options = tileOptions(minZoom = 8, maxZoom = 15),group = "Openstreet map")%>%
        addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 15),group = "World image")%>%
        add_full_toolbar(.) %>%
        addLayersControl(
          overlayGroups = c("drawn"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        # Add study area to the map
        addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        addLayersControl(baseGroups = c("Openstreet map","World image"),
                         options = layersControlOptions(collapsed = FALSE))
    })
    
    
    
    
    
    observeEvent(input$to_task1,{
      updateProgressBar(session = session, id = "pb", value = 20)
      removeUI(selector = paste0("#",ns("task_0")))

      
      output$task_1<-renderUI({
        tagList(
          # useShinyalert(force = TRUE),
          h3("Training session"),
          br(),
          value_box(
            title = "",
            value ="",
            h4(HTML("For training purposes, draw a <b>maximum</b> of five rectangles that show areas suitable for a Sunday hike in the study area")),
            theme = value_box_theme(bg = orange, fg = "black"),
            showcase = bs_icon("question-octagon-fill"),
            showcase_layout = "left center"
          ),
          br(),
          h5(HTML("
          <ul>
            <li>
              Start drawing rectangles on the map, using the rectangle button.
              <br>
              <img src='draw_btn.png' alt='Map drawing' style='width:40px;'>
            </li>
          </ul>
        ")),
          h5(HTML("<ul><li>The minimum area of a rectangle is 62.5 ha or 70 soccer fields.</li></ul>")),
          h5(HTML("<ul><li>Try to draw the rectangle as precise as possible.</li></ul>")),
          h5(HTML("<ul><li>You will see the [ha] during you draw the rectangle and if your drawn rectangle is valid or not to proceed.</li></ul>")),
          # br(),
          tags$head(
            tags$style(HTML("
      .leaflet-left .leaflet-control {
        visibility: visible; /* Make controls visible */
      }
    "))
          ),
          br(),
          fluidRow(
            actionButton(ns("help0"),"How to use the map?"),
            actionButton(ns("help1"),"How to draw a rectangle?"),
            actionButton(ns("help2"),"How to delete and modify a rectangle?")
          ),
          br(),
          leafletOutput(ns("map"), height = 500)
          #uiOutput(ns("rating")),

        )
      })
      shinyalert(  title = "Training session",
                   text = paste0("You now have the possibility to explore the map and learn how to draw rectangles. This data won`t be stored."),
                   type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE,
                   showCancelButton = FALSE,
                   showConfirmButton = TRUE,
                   animation = "slide-from-bottom",
                   size = "l")
      
    })
    
    observeEvent(input$help0,{
      showModal(modalDialog(
        title = "How to use the map",
        easyClose = T,
        h4("The orange borders indicate the study area you should focus on. It is not possible to store a rectangle that is not compleatly within the study area."),
        # br(),
        # HTML('<iframe src="1_background_map.jpg""></iframe>'),
        br(),
        h4("-You can move the map pressing and holding the left key of your mouse and move the mouse"),
        h4("-You can zoom in and out with the +/- buttons at the left side of the map or you can use the mouse wheel")
      ))
    })
    
    observeEvent(input$help1,{
      showModal(modalDialog(
        easyClose = T,
        size = "l",
        title = "",
        h3("How draw a rectangle"),
        h5("To define your areas, you can draw a maximum of five rectangles on the map."),
        h5("-click the rectangle button at the left side of the map"),
        h5("-click on the map where you want to place a corner of the rectangle, press and hold the left mouse key "),
        h5("-draw your rectangle and release the mouse"),
        br(),
        HTML('<iframe src="https://drive.google.com/file/d/1L-Zt4FETqCQuJmzg-Ff2qr_Vc7yn4LWr/preview" width="480" height="360" allow="autoplay"></iframe>'),
      ))
    })
    
    observeEvent(input$help2,{
      showModal(modalDialog(
        easyClose = T,
        size = "l",
        title = "",
        h3("Remove rectangles"),
        h5("If necessary, you can remove rectangles as shown in the animation. Important: press “save areas” to confirm your answers."),
        br(),
        h3("Modify rectangles"),
        h5("Click on the modify button on the right. You can now move the whole rectangle or change its shape."),
        br(),
        HTML('<iframe src="https://drive.google.com/file/d/19Z-2agDpCJU87l4L7O8YRlafHPzVnliU/preview" width="480" height="360" allow="autoplay"></iframe>'),
        
      ))
    })
    
    
    ######## the same logic as for mapping in R1
    drawn_polygons <- reactiveVal(st_sf(
      leaflet_id = integer(1),  # Two rows (features)
      time_stamp = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
      valid = FALSE,
      geometry = st_sfc(st_polygon(),crs = 4326)# Empty POLYGON geometry
    )) 
    
    final_saved_polygons <- reactiveVal(NULL) #
    
    # Function to add the drawing toolbar with only editing and removing options
    add_edit_toolbar <- function(map) {
      map %>%
        addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          rectangleOptions = FALSE,  # Disable drawing new rectangles
          editOptions = list(edit = TRUE, remove = TRUE)  # Keep edit/remove options enabled
        )
    }
    
    # Function to add the full drawing toolbar (including rectangle drawing)
    add_full_toolbar <- function(map) {
      map %>%
        addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          rectangleOptions = drawRectangleOptions(
            showArea = TRUE,
            shapeOptions = drawShapeOptions(
              clickable = TRUE
            )
          ),  # Enable drawing new rectangles
          editOptions = list(edit = TRUE, remove = TRUE, clearAll = FALSE)# Keep edit/remove options enabled
        )
    }
    
    output$map <- renderLeaflet({
      leaflet(sf_stud_geom) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik,options = tileOptions(minZoom = 8, maxZoom = 15),group = "Openstreet map")%>%
        addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 15),group = "World image")%>%
        add_full_toolbar(.) %>%
        addLayersControl(
          overlayGroups = c("drawn"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        # Add study area to the map
        addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0)%>%
        addLayersControl(baseGroups = c("Openstreet map","World image"),
                         options = layersControlOptions(collapsed = FALSE))
    })
    
    # Function to calculate area, check for intersections, and ensure within study area
    update_polygon_area_and_check <- function(polygon_sf, modified) {
      #print("-----------")
      
      area_ha <- st_area(st_transform(polygon_sf, 3857)) /10000 # Transform to meters first
      area_ha <- as.numeric(area_ha)
      #print(polygon_sf)
      
      leaf_id<-polygon_sf$leaflet_id
      #print(paste0("the new one", leaf_id))
      
      # Get the existing polygons
      existing_polygons <- drawn_polygons()
      #
      
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
      
      if (area_ha > 60 && area_ha < 1200 && !intersects && within_study_area) {
        #print("fifth")
        # Display success popup alert for valid polygon
        polygon_sf$valid = TRUE
        cleaned_pols<-st_sf(rbind(existing_polygons,polygon_sf))%>%filter(leaflet_id != 0)
        
        #print(paste0("The cleaned poly to be added: ",nrow(cleaned_pols)))
        area_ha2<-round(area_ha,1)
        drawn_polygons(cleaned_pols)
        #drawn_polygons(rbind(drawn_polygons(), cleaned_pols))
        #print(paste0("and this should be the sum +1draw, +0 mod, -1 del of original and new values:" ,nrow(drawn_polygons())))

        
        valid_text <- glue("
          <h4>
    
              The selected area is <b>{area_ha2} hectares</b>.
               </h4>
            <h5>
              <li>
                You can draw further rectangles, modify or delete rectangles using the buttons on the left side of the map.
                <br>
                <img src='draw_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>
              <li>
              Or you can finish mapping and evaluate the areas in the next step.
              </li>
          </h5>
        ")
        

        shinyalert(
          title = "Valid rectangle!",
          text = HTML(valid_text),
          html = TRUE,
          type = "success",
          showCancelButton = TRUE,
          cancelButtonText = "Draw further rectangles or make edits",
          confirmButtonText = "Finish mapping - evaluate areas",
          closeOnClickOutside = FALSE,
          callbackR = function(confirm) {
            if (confirm) {
              # Save the polygon and switch tab and display the map with annotations
              #drawn_polygons(c(drawn_polygons(), polygon_sf))  # Update the reactive value
              
              #print(drawn_polygons())
              #drawn_polygons(cleaned_pols)
              
              update_final_map()  # Update the final map with annotations
              update_rectangle_ids()  # Update the rectangle ID display
              
              
            } else {
              # Save the polygon and allow further drawing when "Proceed" is clicked
              #drawn_polygons(c(drawn_polygons(), polygon_sf))  # Update the reactive value
              drawn_polygons(cleaned_pols)
              

              if (nrow(cleaned_pols) == max_rectangles) {
                # Disable drawing if the max has been reached
                leafletProxy("map") %>%
                  removeDrawToolbar() %>%
                  add_edit_toolbar(.)
                max_text <- glue("
                  <h4>
            
                      <b>Maximum of {max_rectangles} rectangles is reached</b>.
                    </h4>
                    <h5>
                      <li>
                        Continue with the evaluation of your areas.
                        <br>
                      </li>
                      <li>
                      Or with modifying, removing rectangles using the buttons on the left side of the map.
                      <br>
                      <img src='edit_btn.png' alt='Edit buttons on map' style='width:40px;'>
                      </li>
                  </h5>
                ")
                shinyalert(
                  title = "Maximum rectangles reached!",
                  text = HTML(max_text),
                  html = TRUE,
                  type = "warning",
                  showCancelButton = TRUE,
                  confirmButtonText = "Finish mapping - evaluate areas",
                  cancelButtonText = "Make edits",
                  closeOnClickOutside = FALSE,
                  callbackR = function(confirm) {
                    if (confirm) {
                      # Save and proceed
                      # cleaned_pols<-st_sf(rbind(existing_polygons,polygon_sf))%>%filter(leaflet_id != 0)
                      #
                      drawn_polygons(cleaned_pols)
                      updateTabsetPanel(session, "inTabset", selected = "p1")
                      hideTab(inputId = "inTabset", target = "p0")
                      showTab(inputId = "inTabset", target = "p1")
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
        overlay_text <- glue("
          <h4>
    
              Please remove the overlay of the last drawn rectangles.
               </h3>
            </h4>
            <h5>
              <li>
                You must modify or delete rectangles using the buttons on the left side of the map to continue.
                <br>
                <img src='edit_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>
          </h5>
        ")
        shinyalert(
          title = "No overlay!",
          text = HTML(overlay_text),
          html = TRUE,
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
        out_text <- glue("
          <h4>
    
              Please place your rectangle inside the study area.
            </h4>
            <h5>
              <li>
                You must modify or delete the last rectangle using the buttons on the left side of the map to continue.
                <br>
                <img src='edit_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>
          </h5>
        ")
        shinyalert(
          title = "Out of Study Area!",
          text = HTML(out_text),
          html = TRUE,
          type = "error"
        )
        
        
        # Disable drawing but keep edit/remove active
        leafletProxy("map") %>%
          removeDrawToolbar() %>%
          add_edit_toolbar(.)
        
      } else if (area_ha > 1200) {
        area_ha2<-round(area_ha,1)
        # Display error popup alert for large area
        large_text <- glue("
          <h4>
    
              Your polygon is too big <b>{area_ha2} hectares</b>, maximum size allowed: 1200 hectares.
            </h4>
            <h5>
              <li>
                You must modify or delete the last rectangle using the buttons on the left side of the map to continue.
                <br>
                <img src='edit_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>
          </h5>
        ")
        shinyalert(
          title = "Too large!",
          text = HTML(large_text),
          html = TRUE,
          type = "error"
        )
        
        
        # Disable drawing but keep edit/remove active
        leafletProxy("map") %>%
          removeDrawToolbar() %>%
          add_edit_toolbar(.)
      } else {
        area_ha2<-round(area_ha,1)
        small_text <- glue("
          <h4>
    
              Your polygon is too small  <b>{area_ha2} hectares</b>, minimum size allowed: 65 hectares.
            </h4>
            <h5>
              <li>
                You must modify or delete the last rectangle using the buttons on the left side of the map to continue.
                <br>
                <img src='edit_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>
          </h5>
        ")
        shinyalert(
          title = "Too small!",
          text = HTML(small_text),
          html = TRUE,
          type = "error"
        )
        
        
        # Disable drawing but keep edit/remove active
        leafletProxy("map") %>%
          removeDrawToolbar() %>%
          add_edit_toolbar(.)
      }

    }
    
    # Observe draw events (when a new feature is created)
    observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature
      print(feature)
      #print(feature$properties$layerId)
      if (feature$geometry$type == "Polygon") {
        coords <- feature$geometry$coordinates[[1]]
        
        # Convert coordinates to sf object (polygon)
        polygon <- st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))
        polygon_sf <- st_sfc(polygon, crs = 4326)
        
        tmp_leaflet_id <- as.integer(feature$properties$`_leaflet_id`)
        #print(tmp_leaflet_id)
        polygon_sf <- st_sf(leaflet_id = tmp_leaflet_id, time_stamp = Sys.time(), valid = FALSE, geometry = polygon_sf)
        # Update the area and check for intersections with the large fct
        update_polygon_area_and_check(polygon_sf, modified =  F)
      }
    })
    
    
    # Observe edit events (when a feature is modified)
    observeEvent(input$map_draw_edited_features, {
      edited_features <- input$map_draw_edited_features$features
      ## and the "old polygons" from which the respective modified ids must be removed and replaced
      
      #a<-existing_polygons
      
      #print(existing_polygons)
      #print(a$properties$`_leaflet_id`)
      #print(edited_features)
      if (length(edited_features) > 0) {
        #print("first")
        for (feature in edited_features) {
          #edited_ids<-feature$properties$`_leaflet_id`
          #print("second")
          if (feature$geometry$type == "Polygon") {
            #print("third")
            coords <- feature$geometry$coordinates[[1]]
            
            # Convert coordinates to sf object (polygon)
            polygon <- st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))
            polygon_sf <- st_sfc(polygon, crs = 4326)
            
            edited_ids <- as.integer(feature$properties$`_leaflet_id`)
            print(edited_ids)
            polygon_sf <- st_sf(leaflet_id = edited_ids, time_stamp = Sys.time(), valid = FALSE, geometry = polygon_sf)
            
            # Update the area and check for intersections
            update_polygon_area_and_check(polygon_sf, modified = T)
          }
        }## / close for
        
      }
    })
    
    
    # Observe delete events (when a feature is deleted)
    observeEvent(input$map_draw_deleted_features, {
      deleted_features <- input$map_draw_deleted_features$features
      #print("---- deleted features::")
      #print(deleted_features)
      
      existing_polygons <- drawn_polygons()
      #print("---- existing features::")
      #print(existing_polygons)
      
      #print(paste0("N del features: ", length(deleted_features)))
      
      
      if (length(deleted_features) > 0) {
        # When a polygon is deleted, re-enable the drawing toolbar
        leafletProxy("map") %>%
          removeDrawToolbar() %>%
          add_full_toolbar(.)
        
        leaflet_ids <- numeric(length(deleted_features))
        
        # Loop through the features to access _leaflet_id
        for (i in seq_along(deleted_features)) {
          leaflet_ids[i] <- deleted_features[[i]]$properties$`_leaflet_id`
        }
        
        # Print the populated _leaflet_id vector
        #print(leaflet_ids)
        
        
        updated_poly <- existing_polygons%>%filter(!leaflet_id %in% leaflet_ids)
        # Optionally, show a message that the polygon was deleted
        
        
        # Update the drawn polygons reactive value
        # existing_polygons <- drawn_polygons()
        #updated_poly <- existing_polygons%>%filter(!leaflet_id %in% del_ids) # Remove rectangle(s)
        drawn_polygons(updated_poly)
        updated_poly<-drawn_polygons()
        
        
        # print(paste0("existing N poly after del: ",nrow(updated_poly)))
        #print(paste0("existing N poly before del: ",nrow(existing_polygons)))
        
        
        if(nrow(updated_poly)==0){
          del_all_text <- glue("
          <h4>
    
              Draw at least one rectangle on the map using the rectangle button.
            </h4>
            <h5>
              <li>
                <img src='draw_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>
          </h5>
        ")
          shinyalert(
            title = "Polygon Deleted",
            text = HTML(del_all_text),
            html = T,
            type = "info",
            showCancelButton = F,
            confirmButtonText = "Draw rectangle",
            #cancelButtonText = "Draw at least one rectangle",
            closeOnClickOutside = FALSE,
            callbackR = function(confirm) {
              # clear drawn polygons since there is nothing else
              drawn_polygons(st_sf(
                leaflet_id = integer(1),  # Two rows (features)
                time_stamp = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
                valid = FALSE,
                geometry = st_sfc(st_polygon(),crs = 4326)# Empty POLYGON geometry
              ))
              #print(drawn_polygons)
              
            }
          )
        }else{
          del_text <- glue("
            <h5>
              <li>
                You can draw further rectangles, modify or delete rectangles using the buttons on the left side of the map.
                <br>
                <img src='drawn_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>

          </h5>
        ")
          shinyalert(
            title = "Polygon Deleted",
            text = HTML(del_text),
            html = T,
            type = "info",
            showCancelButton = TRUE,
            confirmButtonText = "Finish mapping - evaluate areas",
            cancelButtonText = "Draw further rectangles or make edits",
            closeOnClickOutside = FALSE,
            callbackR = function(confirm) {
              if (confirm) {
                # Save and proceed
                
                drawn_polygons(updated_poly)
                updateTabsetPanel(session, "inTabset", selected = "p1")
                hideTab(inputId = "inTabset", target = "p0")
                showTab(inputId = "inTabset", target = "p1")
                update_final_map()
              } else {
                drawn_polygons(updated_poly)
                # Allow editing polygons
                leafletProxy("map") %>%
                  removeDrawToolbar() %>%
                  add_full_toolbar(.)
              }
            }
          )
        }

      }
    })
    
    
    update_final_map <- function() {
      removeUI(selector = paste0("#",ns("task_1")))
        shinyalert(  title = "Evaluating your rectangles",
                     text = "As soon as you have saved all rectangles, they will appear on the map again with a red number. Below you will find a slider for each rectangle with a corresponding number. With the slider you are now asked to rate each rectangle how well it is suited for a Sunday hike.",
                     type = "info",
                     closeOnEsc = TRUE,
                     closeOnClickOutside = TRUE,
                     showCancelButton = FALSE,
                     showConfirmButton = TRUE,
                     animation = "slide-from-bottom",
                     size = "l")
      
      drawn_sf <- drawn_polygons()  # Retrieve the stored polygons
      output$task_2<-renderUI( 
        tagList(
          uiOutput(ns("es_quest_how")),
          br(),
          leafletOutput(ns("map_res")),
          br(),
          uiOutput(ns("slider_container")),
          actionButton(ns("sub3"),"End training", style="color: black; background-color: #31c600; border-color: #31c600")
        )
      )
      
      removeUI(
        selector = paste0("#",ns("map")))
      
      # removeUI(
      #   selector = paste0("#",ns("es_quest_where")))
      
      output$map_res <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik,options = tileOptions(minZoom = 8, maxZoom = 15),group = "Openstreet map")%>%
          addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 15),group = "World image")%>%
          addPolygons(data = drawn_sf, color = "green", weight = 2, fillOpacity = 0.4, group = "drawn") %>%
          addLabelOnlyMarkers(
            lng = st_coordinates(st_centroid(drawn_sf))[, 1], 
            lat = st_coordinates(st_centroid(drawn_sf))[, 2], 
            label = paste("ID:", seq_along(drawn_sf$geometry)), 
            labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list('color' = 'red'))
          )%>%
          addLayersControl(baseGroups = c("Openstreet map","World image"),
                           options = layersControlOptions(collapsed = FALSE))
        
      })
    }#/final_map function
    
    # Function to update the rectangle IDs display
    update_rectangle_ids <- function() {
      ids <- paste("Rectangle IDs:", seq_along(drawn_polygons()), collapse = ", ")
      # You can display this somewhere in the UI if needed
    }
    
    output$slider_container <- renderUI({
      drawn_sf <- drawn_polygons() 
      tagList(
        paste0("The number for each rectangle in the map corresponds to the number of the slider. For each individual rectangle, how suitable do you think the area is for a sunday hike? 0 = unsuitable, 5 = very suitable "),
        br(),
        lapply(seq_along(drawn_sf$geometry), function(i) {
          sliderInput(
            inputId = ns(paste0("slider_", i)),
            label = paste("Rectangle ID:", i),
            min = 0, max = 5, value = 3  # Default value set to 3, can be customized
          )
        })
        
      )
      
    })

    
    observeEvent(input$sub3,{
      rv1$u <-reactive({1})
    })
    
    # play back the value of the confirm button to be used in the main app
    cond <- reactive({rv1$u()})
    
    return(cond)
  })
}

## To be copied in the UI
# mod_training_ui("training_1")

## To be copied in the server
# mod_training_server("training_1")
