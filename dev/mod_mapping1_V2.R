#' delphi_round1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' 
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
mod_delphi_round1_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    mainPanel(
      value_box(
        title = "",
        value = uiOutput(ns("title_es"))
        
      ),
      br(),
      actionButton(ns("alert"),
                   label = "Explain me this nature benefit"),
      
      br(),
      # questions of importance
      uiOutput(ns("imp_text")),
      br(),
      sliderInput(ns("imp_own"), "... for you personally in the study area?",
                  min = 0, max = 5, value = 3),
      
      sliderInput(ns("imp_other"), "...for the whole society in general in the study area?",
                  min = 0, max = 5, value = 3),
      
      br(),
      # are you able to map the ES?
      uiOutput(ns("map_poss")),
      br(),
      # if ES not mappable
      conditionalPanel(
        condition = "input.map_poss == 'No'", ns = ns ,
        tagList(
          value_box(
            title = "",
            value = "Would you trust an expert evaluation regarding suitable areas for this nature benefit?",
            theme = value_box_theme(bg = orange, fg = "black"),
            showcase = bs_icon("question-octagon-fill")
          ),
          selectizeInput(ns("expert_map"),label="" ,choices = c("Yes","No"),options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')))%>%
            
            shinyInput_label_embed(
              icon("info") %>%
                bs_embed_tooltip(title = "An expert evaluation could be either a physical model, including various indicators or expert judgements based on their knowledge",placement = "right")
            ),
          use_bs_tooltip()
        )
        
        
        # actionButton(ns("submit2"),"save")
      ),
      
      conditionalPanel(
        condition = "input.expert_map != ''", ns=ns,
        actionButton(ns("confirm"), "Next task", style="color: black; background-color: #31c600; border-color: #31c600")
      )
    )
  )
}

callback <- c(
  '$("#remove").on("click", function(){',
  '  table.rows(".selected").remove().draw();',
  '});'
)

#' delphi_round1 Server Functions
#'
#' @noRd
mod_delphi_round1_server <- function(id, sf_stud_geom, rand_es_sel, order, userID, site_id, site_type, var_lang, pred){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mapTIME_start <-Sys.time()
    
    order<-as.numeric(order)
    rand_es_sel<-rand_es_sel[order,]

    rv1<-reactiveValues(
      u = reactive({})
    )
    a<-paste0("esNAME_",var_lang)
    ## descriptives of ecosystem services
    output$title_es<-renderUI(h3(dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang)))))

    observeEvent(input$alert,{
      showModal(modalDialog(
        title = "",
        h4(dplyr::select(rand_es_sel,contains(paste0("esDESC_lay_",var_lang)))),
        
      ))
    })
    
    glue1<-glue("<ul><li>The minimum area of a rectangle is {A_min} ha or 70 soccer fields.</li></ul>")
    
    output$es_quest_where<-renderUI(
      tagList(
        value_box(
          title = "",
          value = dplyr::select(rand_es_sel,contains(paste0("esQUEST_",var_lang))),
          h5("Draw and modify rectangles inside the orange bounds of the study area."),
          h5("Draw a maximum of five rectangles"),
          br(),
          h5(glue1),
          h5("You will see the [ha] during you draw the rectangle. In addition, the app indicates if your last drawn polygon is too small or too big."),
          theme = value_box_theme(bg = orange, fg = "black"),
          showcase = bs_icon("question-octagon-fill"),
          
        )
      )
    )
    output$es_quest_how<-renderUI(tagList(
      value_box(
        title = "",
        value = paste0("For each rectangle indicate, how well you think they are suited for ",dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang)))),
        theme = value_box_theme(bg = orange, fg = "black"),
        showcase = bs_icon("question-octagon-fill")
      )
    ))
    output$imp_accText<-renderUI(
      tagList(
        value_box(
          title = "",
          value = paste0("How important is an easy access (by foot, bike, car) to your rectangles to benefit from ", dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang))),"?"),
          h5("0 = not important at all - 5 = very important"),
          theme = value_box_theme(bg = orange, fg = "black"),
          showcase = bs_icon("question-octagon-fill")
        )
      ))
    
    output$blog_descr<-renderUI(
      tagList(
        value_box(
          title = "",
          value = paste0("Briefly explain to other study participants why your selected areas are good to benefit from ",dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang)))),
          h5("Do this in a short (max 250 characters), blog-like statement"),
          theme = value_box_theme(bg = orange, fg = "black"),
          showcase = bs_icon("question-octagon-fill")
        )
      ))
    
    output$imp_text<-renderUI(
      tagList(
        value_box(
          title = "",
          value = paste0("How important are the benefits of ", dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang))),"..."),
          h5("0 = not important at all - 5 = very important"),
          theme = value_box_theme(bg = orange, fg = "black"),
          showcase = bs_icon("question-octagon-fill")
        )
      ))

    
    
    # UI rendered to ask if able to map ES
    output$map_poss<-renderUI({
      tagList(
        value_box(
          title = "",
          value = paste0("Are you able to map areas that are well suited for ", dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang)))," according to you in the study area?"),
          theme = value_box_theme(bg = orange, fg = "black"),
          showcase = bs_icon("question-octagon-fill")
        ),
        selectizeInput(ns("map_poss"),label="",choices = c("Yes","No"),options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        ))
        
      )
      
    })
    
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
          polygonOptions = draw_pol,
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
    

    observeEvent(input$confirm,{
      # removeNotification(id="note1")
      rv1$u <-reactive({1})
    })

    
    observeEvent(input$map_poss,{
      if(input$map_poss == "Yes"){

        insertUI(selector =paste0("#",ns("map_poss")),
                 where = "afterEnd",
                 ui=tagList(
                   uiOutput(ns("es_quest_where")),
                   br(),
                   leafletOutput(ns("map")),
                   uiOutput(ns("rating"))
                   
                 )
        )
        
        update_polygon_area_and_check <- function(polygon_sf, modified) {
          #print("-----------")
          
          area_ha <- st_area(st_transform(polygon_sf, 3857)) /10000 # Transform to meters and then to ha
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
          
          ### check for self intersection
          check_valid<-st_is_valid(polygon_sf)
          print(check_valid)
          
          # Check for intersection but not consider the empty starting polygon
          intersects <- FALSE
          if (nrow(existing_polygons%>%filter(leaflet_id != 0)) > 0 && check_valid == T) {
            #print("fourth")
            # Union of existing polygons to create a single geometry
            existing_union <- st_union(existing_polygons)
            intersects <- st_intersects(polygon_sf, existing_union, sparse = FALSE)[1]
          }
          
          
          
          #print(intersects)
          # Check if the polygon is completely within the study area
          if(check_valid == T){
            within_study_area <- st_within(polygon_sf, sf_stud_geom, sparse = FALSE)[1]
          } else {
            within_study_area <- FALSE
          }
          
          
          
          
          if (area_ha > A_min && area_ha < A_max && !intersects && within_study_area && check_valid == T) {
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
            
            
          } else if (intersects  & check_valid == TRUE) {
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
            
            
          } else if (!within_study_area & check_valid == TRUE) {
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
              type = "error",
              showCancelButton = FALSE,
              confirmButtonText = "Edit rectangles",
              closeOnClickOutside = FALSE,
            )
            
            
            # Disable drawing but keep edit/remove active
            leafletProxy("map") %>%
              removeDrawToolbar() %>%
              add_edit_toolbar(.)
            
          } else if (area_ha > A_max) {
            area_ha2<-round(area_ha,1)
            # Display error popup alert for large area
            large_text <- glue("
          <h4>
    
              Your polygon is too big <b>{area_ha2} hectares</b>, maximum size allowed: {A_max} hectares.
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
              type = "error",
              showCancelButton = FALSE,
              confirmButtonText = "Edit rectangles",
              closeOnClickOutside = FALSE,
            )
            
            
            # Disable drawing but keep edit/remove active
            leafletProxy("map") %>%
              removeDrawToolbar() %>%
              add_edit_toolbar(.)
          }else if(check_valid == F){
            selfint_text <- glue("
          <h4>
    
              Please do not draw a polygon with overcrossing lines!
            </h4>
            <h5>
              <li>
                You must modify or delete the last polygon using the buttons on the left side of the map to continue.
                <br>
                <img src='edit_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>
          </h5>
        ")
            shinyalert(
              title = "Self intersection!",
              text = HTML(selfint_text),
              html = TRUE,
              type = "error",
              showCancelButton = FALSE,
              confirmButtonText = "Edit rectangles",
              closeOnClickOutside = FALSE,
            )
            
            
            # Disable drawing but keep edit/remove active
            leafletProxy("map") %>%
              removeDrawToolbar() %>%
              add_edit_toolbar(.)
            
          } else {
            area_ha2<-round(area_ha,1)
            small_text <- glue("
          <h4>
    
              Your polygon is too small  <b>{area_ha2} hectares</b>, minimum size allowed: {A_min} hectares.
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
              type = "error",
              showCancelButton = FALSE,
              confirmButtonText = "Edit rectangles",
              closeOnClickOutside = FALSE,
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
          if (length(edited_features) > 0) {

            for (feature in edited_features) {

              if (feature$geometry$type == "Polygon") {

                coords <- feature$geometry$coordinates[[1]]
                polygon <- st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE)))
                polygon_sf <- st_sfc(polygon, crs = 4326)
                
                edited_ids <- as.integer(feature$properties$`_leaflet_id`)
                polygon_sf <- st_sf(leaflet_id = edited_ids, time_stamp = Sys.time(), valid = FALSE, geometry = polygon_sf)
                update_polygon_area_and_check(polygon_sf, modified = T)
              }
            }## / close for
            
          }
        })
        
        
        # Observe delete events (when a feature is deleted)
        observeEvent(input$map_draw_deleted_features, {
          deleted_features <- input$map_draw_deleted_features$features

          existing_polygons <- drawn_polygons()

          if (length(deleted_features) > 0) {
            leafletProxy("map") %>%
              removeDrawToolbar() %>%
              add_full_toolbar(.)
            
            leaflet_ids <- numeric(length(deleted_features))
            
            # Loop through the features to access _leaflet_id
            for (i in seq_along(deleted_features)) {
              leaflet_ids[i] <- deleted_features[[i]]$properties$`_leaflet_id`
            }
            
            updated_poly <- existing_polygons%>%filter(!leaflet_id %in% leaflet_ids)
            drawn_polygons(updated_poly)
            updated_poly<-drawn_polygons()

            if(nrow(updated_poly)==0 | nrow(existing_polygons) == 0){
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
                type = "info",
                showCancelButton = F,
                confirmButtonText = "Draw at least one rectangle",
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

                }
              )
            }else{
              del_text <- glue("
            <h5>
              <li>
                You can draw further rectangles, modify or delete rectangles using the buttons on the left side of the map.
                <br>
                <img src='draw_btn.png' alt='Edit buttons on map' style='width:40px;'>
              </li>

          </h5>
        ")
              shinyalert(
                title = "Polygon Deleted",
                text = HTML(del_text),
                type = "info",
                showCancelButton = TRUE,
                confirmButtonText = "Finish mapping - evaluate areas",
                cancelButtonText = "Draw further rectangles or make edits",
                closeOnClickOutside = FALSE,
                callbackR = function(confirm) {
                  if (confirm) {
                    # Save and proceed
                    
                    drawn_polygons(updated_poly)

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
            # Update the reactive value
            #print(nrow(new_polygons))
            
            # Update the max rectangles counter after deletion
            # output$max_rectangles_info <- renderText({
            #   current_count <- length(new_polygons)  # Count after deletion
            #   paste("Rectangles Drawn: ", current_count, "/", max_rectangles)
            # })
          }
          
          #feature <- input$map_draw_new_feature
          #existing_polygons<-existing_polygons%>%filter(leaflet_id != leaf_id)
        })

      }#/if yes
    })#/map_poss
    
    
    ## if mapping not possible: (save results )
    observeEvent(input$confirm,{
      
      if(input$expert_map !=""){
        show_modal_spinner(color = green,text = "update data base")
        train_param<-list(
          esID = rand_es_sel$esID,
          userID = userID,
          siteID = site_id,
          imp_acc= as.integer(0),
          imp_nat= as.integer(0),
          imp_lulc = as.integer(0),
          imp_own = as.integer(input$imp_own),
          imp_other = as.integer(input$imp_other),
          rel_training_A = as.numeric(0.000001),
          n_poly = as.integer(0),
          blog = "NA",
          poss_mapping = "No",
          expert_trust = input$expert_map,
          mapping_order = as.numeric(order),
          extrap_AUC = as.numeric(0.00001),
          extrap_KAPPA = as.numeric(0.0001),
          extrap_propC = as.numeric(0.0001),
          # extrap_demIMP = as.numeric(0),
          # extrap_accIMP = as.numeric(0),
          # extrap_lulcIMP = as.numeric(0),
          # extrap_natIMP = as.numeric(0),
          mapTIME_h = as.numeric((Sys.time()-mapTIME_start)/3600)
        )
        train_param<-as.data.frame(train_param)
        # insert_upload_job(table_con$project, table_con$dataset, "es_mappingR1", train_param)
        es_mapping_tab = bq_table(project = project_id, dataset = dataset, table = 'es_mappingR1')
        bq_table_upload(x = es_mapping_tab, values = train_param, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
        
        removeUI(
          selector = paste0("#",ns("expert_map"))
        )
        remove_modal_spinner()
      }
      
      
    })

    

    #remove mapping question as soon as decided
    observeEvent(input$map_poss,{
      if(input$map_poss !=""){
        removeUI(
          selector = paste0("#",ns("map_poss"))
        )
        removeUI(
          selector =  paste0("div:has(> #",ns("imp_own"),")")
        )

        removeUI(
          selector =  paste0("div:has(> #",ns("imp_other"),")")
        )
        removeUI(
          selector = paste0("#",ns("imp_text")))
        
      }
    })
    
    ### confirm the drawings and render the sliders with the leaflet map
    
    update_final_map <- function() {
      
      drawn_sf <- drawn_polygons()  # Retrieve the stored polygons
      output$rating<-renderUI( 
        tagList(
          uiOutput(ns("es_quest_how")),
          br(),
          leafletOutput(ns("map_res")),
          br(),
          uiOutput(ns("slider_container")),
          br(),
          uiOutput(ns("imp_accText")),
          sliderInput(ns("imp_acc"), "",
                      min = 0, max = 5, value = 3)%>%
            shinyInput_label_embed(
              icon("info") %>%
                bs_embed_tooltip(title = "Ask yourself how important an easy access to the area is necessary to use or profit from this nature benefit. Is it important for you to be able to reach your areas without effort? 1 = not important at all, 5 = very important",placement = "right")),
          use_bs_tooltip(),
          br(),
          uiOutput(ns("blog_descr")),
          textInput(ns("blog"), label = "")%>%
            shinyInput_label_embed(
              icon("info") %>%
                bs_embed_tooltip(title = "Make it clear to other people why your areas are important. Please do this in a short blog-like statement.",placement = "right")),
          use_bs_tooltip() ,
          br(),
          uiOutput(ns("cond_1"))
          # conditionalPanel(
          #   condition = "input.blog != ''", ns=ns,
          #   actionButton(ns("submit"),"save values", style="color: black; background-color: #31c600; border-color: #31c600")
          # )
        )
      )
      
      ############
      output$cond_1<-renderUI({
        validate(
          need(input$blog != '', 'Provide an explanation why your areas are important'),
        )
        actionButton(ns('submit'), 'proceed', style="color: black; background-color: #31c600; border-color: #31c600")
      })
      ############
      
      removeUI(
        selector = paste0("#",ns("map")))
      
      removeUI(
        selector = paste0("#",ns("es_quest_where")))
      
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
        value_box(
          title = "",
          value = paste0("For each individual rectangle, how suitable do you think the area is for ",dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang))),"?"),
          h5("The number for each rectangle in the map corresponds to the number of the slider"),
          h5("0 = unsuitable, 5 = very suitable"),
          theme = value_box_theme(bg = orange, fg = "black"),
          showcase = bs_icon("question-octagon-fill")
        ),
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
    
    ############ And the final extrapolation of the rectangles:
    
    ## keep mapping time
    mapTIME_end <-eventReactive(input$submit,{
      mapTIME_end <-Sys.time()
    })
    ## remove map UI and sliders show result
    observeEvent(input$submit, {
      insertUI(
        selector = paste0("#",ns("submit")),
        where = "afterEnd",
        ui = tagList(
          bslib::value_box(
            title= "",
            value = paste0("Based on your inputs, we calculated a map of the study area that shows the probability to benefit from ",dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang)))),
            showcase_layout = "left center",
            theme = value_box_theme(bg = blue, fg = "black"),
            showcase = bs_icon("check-square"),
            h5("Red colors indicate areas of higher probability, blue colors lower probability to benefit"),
          ),
          br(),
          leafletOutput(ns("res_map")),
          br(),
          uiOutput(ns("btn_cond"))
          
        )
      ) ## insert ui
      
      removeUI(
        selector = paste0("#",ns("map_res"))
      )
      removeUI(
        selector =  paste0("div:has(> #",ns("imp_acc"),")")
      )
      removeUI(
        selector = paste0("#",ns("imp_accText")))
      removeUI(
        selector = paste0("#",ns("es_quest_how"))
      )
      removeUI(
        selector = paste0("#",ns("slider_container"))
      )
      removeUI(
        selector = paste0("#",ns("blog_descr"))
      )
      removeUI(
        selector = paste0("#",ns("blog"))
      )
      removeUI(
        selector =  paste0("div:has(> #",ns("blog"),")")
      )
      removeUI(
        selector =  paste0("div:has(> #",ns("select"),")")
      )
      removeUI(
        selector = paste0("#",ns("submit"))
      )
      removeUI(
        selector =  paste0("div:has(> #",ns("pull-right"),")")
      )
      removeUI(
        selector = paste0("div:has(>> #",ns("blog"),")")
      )
      
      
    })

    # prediction<-eventReactive(input$submit, {
    observeEvent(input$submit, {
      show_modal_progress_line(text = "fetch data", color = green)
      req(mapTIME_end)
      mapTIME_end<-mapTIME_end()
      
      polygon <- drawn_polygons()
      #print(drawn_sf)
      
      # Collect slider values and ensure they are numeric
      slider_values <- sapply(1:nrow(polygon), function(i) {
        slider_val <- input[[paste0("slider_", i)]]
        if (is.null(slider_val)) {
          return(NA)  # Return NA if the slider value is not set
        }
        return(slider_val)  # Return the slider value
      })
      
      polygon$es_value <- slider_values
      
      polygon$esID <- rep(rand_es_sel$esID,nrow(polygon))
      polygon$userID <- rep(userID,nrow(polygon))
      polygon$siteID <- rep(site_id,nrow(polygon))
      polygon$delphi_round<-rep(1, nrow(polygon))
      update_modal_progress(0.1, text = "update data base")
      n_polys <-nrow(polygon)
      polygon<-st_as_sf(polygon)
      train_area<-as.numeric(sum(st_area(polygon)))
      
      #create new polygon object with wkt as geometry
      polygons<-polygon%>%st_drop_geometry()
      polygons$geometry<-st_as_text(polygon$geometry)
      update_modal_progress(0.15, text = "update data base")
      #save it on bq
      poly_table = bq_table(project = project_id, dataset = dataset, table = 'ind_polys_R1')
      bq_table_upload(x = poly_table, values = polygons, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
      
      
      ################### not for the moment, just upload polygons to bq
      ############ training pts
      update_modal_progress(0.2, text = "update data base")

      # 
      # ## N background (outside poly points) according to area of extrapolation
      A_roi<-as.numeric(sf_stud_geom$siteAREAkm2*10^6)
      # 
      # # max pts for efficient extrapolation each cell
      all_back_pts<- round(A_roi/resolution,0)
      # 
      # ## although zooming on the map while drawing is limited, we assure that at least 10pts are within a poly
      min_in_pts<-10
      
      # # inside pts are area + es value weighted
      for (i in 1:nrow(polygon)) {
        A_tmp <- as.numeric(st_area(polygon[i,]))
        tmp_ratio<-A_tmp/A_roi
        tmp_pts<-round(all_back_pts*tmp_ratio,0)
        
        if(tmp_pts<=min_in_pts){
          tmp_pts<-min_in_pts
        }else{
          tmp_pts<-tmp_pts
        }
        # npts in this poly must be max_pts*tmp_ratio*es_value
        #+1 its 0-5 scale
        tmp_es_val<-((1+polygon[i,]$es_value)/5)
        tmp_pts = st_sample(polygon[i,], round(tmp_pts*tmp_es_val,0),type="random")
        tmp_pts<-st_as_sf(tmp_pts)
        tmp_pts$inside<-rep(1,nrow(tmp_pts))
        if(i==1){
          pts_in<-tmp_pts
        }else{
          pts_in<-rbind(pts_in,tmp_pts)
        }
        
      }
      # weight predictors
      pred<-load_var(path=pred)
      # pred_w<-stack(pred$dem*1, pred$eii*1, pred$acc*as.numeric(input$imp_acc))
      weight_access <- as.numeric(as.numeric(input$imp_acc)/5)
      pred_w<-raster::stack(pred$dem*1, pred$lulc*1, pred$int*1, pred$acc*weight_access)
      # pred_w<-c(rast(pred$acc),rast(pred$dem))
      
      
      pts_in<-st_transform(pts_in,st_crs(pred))
      pts <- do.call(rbind, st_geometry(pts_in)) %>% 
        as_tibble() %>% setNames(c("lon","lat"))
      pts$SPECIES<-rep("pres",nrow(pts))
      
      
      if(nrow(pts)>3000){
        pts<-pts[sample(nrow(pts), 3000), ]
      }
      
      
      
      ############ save map on gcs within studID folder
      update_modal_progress(0.4, text = "train model")
      SDM <- SSDM::modelling('MARS', pts, 
                             pred_w, Xcol = 'lon', Ycol = 'lat')
      
      train_param <-
        list(
          esID = rand_es_sel$esID,
          userID = userID,
          siteID = site_id,
          imp_acc= as.integer(input$imp_acc),
          imp_nat= as.integer(0),
          imp_lulc = as.integer(0),
          imp_own = as.integer(input$imp_own),
          imp_other = as.integer(input$imp_other),
          rel_training_A = as.numeric(sum(st_area(polygon)))/A_roi,
          n_poly = as.integer(n_polys),
          blog = input$blog,
          poss_mapping = "Yes",
          expert_trust = "No",
          mapping_order = as.numeric(order),
          extrap_AUC = as.numeric(SDM@evaluation$AUC),
          extrap_KAPPA = as.numeric(SDM@evaluation$Kappa),
          extrap_propC = as.numeric(SDM@evaluation$prop.correct),
          # extrap_demIMP = SDM@variable.importance$dem,
          # extrap_accIMP = SDM@variable.importance$acc,
          # extrap_lulcIMP = SDM@variable.importance$lulc,
          # extrap_natIMP = SDM@variable.importance$int,
          mapTIME_h = as.numeric((Sys.time()-mapTIME_start)/3600)
          
        )
      train_param<-as.data.frame(train_param)
      
      update_modal_progress(0.6, text = "evaluate model & update data base")
      # write to bq
      es_mapping_tab = bq_table(project = project_id, dataset = dataset, table = 'es_mappingR1')
      bq_table_upload(x = es_mapping_tab, values = train_param, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
      
      prediction<-SDM@projection
      
      update_modal_progress(0.8, text = "save your map")
      temp_file <- tempfile(fileext = paste0(rand_es_sel$esID,"_",userID,".tif"))
      writeRaster(prediction, filename = temp_file, format = "GTiff")
      
      file_name <-paste0(site_id,"/3_ind_R1/",rand_es_sel$esID,"/",userID)
      gcs_upload(temp_file, bucket_name, name = file_name, predefinedAcl = "bucketLevel")
      file.remove(temp_file)
      
      update_modal_progress(0.9, text = "draw map")
      
      prediction[prediction < 0.15] <- NA
      
      
      bins <- c(0.15, 0.25, 0.5, 0.75, 1)
      colors <- c("#0000FF", "#00FFFF", "#FFFFFF", "#FF7F7F", "#FF0000")
      labels <- c("Low", "Moderate", "High", "Very High")
      
      # Create color palette function
      color_palette <- colorBin(palette = colors, domain = values(prediction), bins = bins, na.color = "transparent")
      

      output$res_map <- renderLeaflet({
        leaflet(sf_stud_geom)%>%
          addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0)%>%
          addProviderTiles(providers$OpenStreetMap.Mapnik,options = tileOptions(minZoom = 8, maxZoom = 15),group = "Openstreet map")%>%
          addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 15),group = "World image")%>%
          addRasterImage(prediction,colors = color_palette, opacity = 0.6)%>%
          addLegend(
            pal = color_palette,
            values = values(prediction),
            labels= labels,
            title = paste0("Probability to benefit from ",dplyr::select(rand_es_sel,contains(paste0("esNAME_",var_lang)))),
            position = "bottomright"
          )
        # addLayersControl(baseGroups = c("Openstreet map","World image"),
        #                  options = layersControlOptions(collapsed = T))
      })
      remove_modal_progress()
      output$btn_cond<-renderUI({
        req(train_param)
        actionButton(ns("confirm2"), "Next task", style="color: black; background-color: #31c600; border-color: #31c600")
      })
      
    })
    
    
    #modify reactive value to trigger cond
    observeEvent(input$confirm2,{
      # removeNotification(id="note1")
      rv1$u <-reactive({1})
    })
    # play back the value of the confirm button to be used in the main app
    cond <- reactive({rv1$u()})
    
    return(cond)
  })
}

## To be copied in the UI
# mod_delphi_round1_ui("delphi_round1_1")

## To be copied in the server
# mod_delphi_round1_server("delphi_round1_1")
