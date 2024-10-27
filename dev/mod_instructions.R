#' training UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
                  h5(paste0("During the study you are going to use an interactive web map of ",sf_stud_geom$siteNAME,". The next section shows you how to interact with the map.")),
                  br(),
        ),

        br(),
        actionButton(ns("to_task1"),"How to map", style="color: black; background-color: #31c600; border-color: #31c600")
      )
    })
    

## background map to modify
    map_training<-leaflet(sf_stud_geom)%>%
      addPolygons(color = "orange", weight = 3, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0)%>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,options = tileOptions(minZoom = 8, maxZoom = 15),group = "Openstreet map")%>%
      addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 15),group = "World image")%>%
      addDrawToolbar(targetGroup='drawPoly',
                     polylineOptions = F,
                     polygonOptions = F,
                     circleOptions = F,
                     markerOptions = F,
                     circleMarkerOptions = F,
                     rectangleOptions = drawRectangleOptions(
                       showArea = TRUE,
                       shapeOptions = drawShapeOptions(
                         clickable = TRUE
                       )
                     ),
                     singleFeature = FALSE,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
      addLayersControl(baseGroups = c("Openstreet map","World image"),
                       options = layersControlOptions(collapsed = FALSE))
    
    map_res<-leaflet(sf_stud_geom)%>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,options = tileOptions(minZoom = 8, maxZoom = 15),group = "Openstreet map")%>%
      addProviderTiles(providers$Esri.WorldImagery,options = tileOptions(minZoom = 8, maxZoom = 15),group = "World image")%>%
      addDrawToolbar(targetGroup='drawPoly',
                     polylineOptions = F,
                     polygonOptions = F,
                     circleOptions = F,
                     markerOptions = F,
                     circleMarkerOptions = F,
                     rectangleOptions = F,
                     singleFeature = FALSE,
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
      addLayersControl(baseGroups = c("Openstreet map","World image"),
                       options = layersControlOptions(collapsed = FALSE))
    
    rv<-reactiveValues(
      edits = reactive({})
    )
    
    
    
    
    
    observeEvent(input$to_task1,{
      updateProgressBar(session = session, id = "pb", value = 20)
      removeUI(selector = paste0("#",ns("task_0")))
      rv$edits<-callModule(
        module = editMod,
        leafmap = map_training,
        id = "map_sel")
      
      output$task_1<-renderUI({
        tagList(
          # useShinyalert(force = TRUE),
          h3("Training session"),
          br(),
          h4("For training purposes, draw a maximum of five rectangles that show areas suitable for a Sunday hike in the study area"),
          br(),
          h5("The minimum area of a rectangle is 62.5 ha or 70 soccer fields."),
          h5("Try to draw the rectangle as precise as possible"),
          h5("You will see the [ha] during you draw the rectangle. In addition, the app indicates if your last drawn rectangle was too small or too big."),
          # br(),
          tags$head(
            tags$style(HTML("
      .leaflet-left .leaflet-control {
        visibility: visible; /* Make controls visible */
      }
    "))
          ),
          editModUI(ns("map_sel")),
          fluidRow(
            actionButton(ns("help0"),"How to use the map?"),
            actionButton(ns("help1"),"How to draw a rectangle?"),
            actionButton(ns("help2"),"How to delete and modify a rectangle?")
          ),
          br(),
          htmlOutput(ns("overlay_result")),
          br(),
          uiOutput(ns("btn1"))
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
        h4("The orange borders indicate the study area you should focus on"),
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
    
    observe({
      req(rv$edits)
      rectangles <- rv$edits()$finished
      
      # if(is.null(rectangles) | rectangles == ""){
      #   n_poly<-0
      # }else{
      #   n_poly<-nrow(as.data.frame(rectangles))
      # } 
      
      n_poly<-nrow(as.data.frame(rectangles))
      
      
      
      if(site_type == "onshore"){
        resolution = 250^2
      }else{
        resolution = 500^2
      }
      
      #with res of 250m grid we can sample at least 10 pts with variaton within 0.6km2
      A_min<-resolution*10
      #A_max<-0.05*round(as.numeric(st_area(sf_stud_geom)),0)
      A_max<-A_min*20
      if(n_poly == 0){
        output$overlay_result <- renderText({
          paste("<font color=\"#FF0000\"><b><li>Draw at least one rectangle<li/></b></font>")
        })
      } else if(n_poly==1){
        n_within<-nrow(as.data.frame(st_within(rectangles,sf_stud_geom)))
        if(n_within < n_poly){
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>Place your rectangle completely within the study area<li/></font>")
          })
          # shinyalert(  title = "",
          #              text = "Place your rectangle inside the orange borders",
          #              type = "warning",
          #              closeOnEsc = TRUE,
          #              closeOnClickOutside = TRUE,
          #              showCancelButton = F,
          #              showConfirmButton = TRUE,
          #              animation = "slide-from-bottom",
          #              size = "s")
          removeUI(
            selector = paste0("#",ns("proc5")))
        }else{
          area<-round(as.numeric(st_area(rectangles)),0)
          min_train<-min(area)
          max_train<-max(area)
          if(min_train<A_min & max_train<=A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>The area of the rectangle is too small<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("proc5")))
            
          }else if(min_train>A_min & max_train>A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>The area of the rectangle is too big<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("proc5")))
            
            
          }else{
            output$btn1<-renderUI(
              actionButton(ns("proc5"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further rectangles"
            })
            
          }
          
        }
        
      }else if(n_poly<=5 & n_poly>1){
        n_within<-nrow(as.data.frame(st_within(rectangles,sf_stud_geom)))
        n_inter<-nrow(as.data.frame(st_intersects(rectangles)))
        q=n_inter-n_poly
        if(q!=0 & n_within<n_poly){
          removeUI(
            selector = paste0("#",ns("proc5")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b><li>Place your last rectangle completely into the the study area<li/><li>Remove overlays<li/></font>")
            
          })
        }else if(q==0 & n_within<n_poly){
          removeUI(
            selector = paste0("#",ns("proc5")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>Place your last rectangle completely into the the study area<li/></font>")
            
          })
        }else if(q!=0 & n_within==n_poly){
          removeUI(
            selector = paste0("#",ns("proc5")))
          output$overlay_result <- renderText({
            paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>Remove overlays from last rectangle<li/></font>")
            
          })
        }else if(q==0 & n_within==n_poly){
          area<-round(as.numeric(st_area(rectangles)),0)
          min_train<-min(area)
          max_train<-max(area)
          if(min_train<A_min & max_train<=A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>The area of the last rectangle was too small<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("proc5")))
            
          }else if(min_train>A_min & max_train>A_max){
            output$overlay_result <- renderText({
              paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>The area of the last rectangle was too big<li/></font>")
            })
            removeUI(
              selector = paste0("#",ns("proc5")))
            
            
          }else{
            output$btn1<-renderUI(
              actionButton(ns("proc5"),"save")
            )
            output$overlay_result <- renderText({
              "Save or draw further rectangles"
            })
            
          }
        }
      }else{
        output$overlay_result <- renderText({
          paste("<font color=\"#FF0000\"><b>","You can`t save the rectangles:","</b> <li>Draw a maximum of five rectangles<li/></font>")
        })
        removeUI(
          selector = paste0("#",ns("proc5")))
      }
      
    })
    
    
    # description 3
    observeEvent(input$proc5,{
      
      tbl<-rv$edits()$finished
      tbl<-tbl%>%st_drop_geometry()
      tbl$value_es<-rep(NA,(nrow(tbl)))
      polygon<-rv$edits()$finished
      
      # do not give possibility to submit map without polygons
      req(polygon, cancelOutput = FALSE)

      
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
      
      output$task_2<-renderUI({
        tagList(
          h3("Training session"),
          br(),
          value_box(
            title = "",
            value = "How well are the rectangles suited for a Sunday hike?",
            theme = value_box_theme(bg = orange, fg = "black"),
            showcase = bs_icon("question-octagon-fill")
          ),
          br(),
          leafletOutput(ns("map_res")),
          br(),
          uiOutput(ns("slider")),
          br(),
          value_box(
            title = "",
            value = "Based on your rectangles and evaluations we will dynamically calculate a map that shows the suitability for a Sunday hike",
            theme = value_box_theme(bg = blue, fg = "black"),
            showcase = bs_icon("hand-thumbs-up")),
          br(),
          actionButton(ns('sub3'), 'start main part', style="color: black; background-color: #31c600; border-color: #31c600")
        )

      })
      cent_poly <- st_centroid(polygon)
      output$map_res<-renderLeaflet(map_res %>%
                                      addPolygons(data=polygon) %>%
                                      addLabelOnlyMarkers(data = cent_poly,
                                                          lng = ~st_coordinates(cent_poly)[,1], lat = ~st_coordinates(cent_poly)[,2], label = cent_poly$`_leaflet_id`,
                                                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,
                                                                                      style = list(
                                                                                        "color" = "red",
                                                                                        "font-family" = "serif",
                                                                                        "font-style" = "bold",
                                                                                        "font-size" = "20px"
                                                                                      ))))
      
      ## create a slider for each of the polygons
      
      output$slider <- shiny::renderUI({
        ns <- session$ns
        tagList(
          "The number of each rectangle in the map corresponds to the number of the slider. For each individual rectangle, how suitable do you think the area is for a Sunday hike? 1 = not suitable, 2 = little suitable, 3 = suitable, 4 = very suitable, 5 = extremely suitable",
          br(),
          br(),
          lapply(1:nrow(tbl),function(n){
            polynr <- tbl[n,]$`_leaflet_id`
            id<-paste0("id_",polynr)
            lable<-paste0("Rectangle Nr. in map: ",polynr)
            sliderInput(ns(id),lable, min = 1, max = 5, step = 1, value = 3)
          })
        )
        
      })
      
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
