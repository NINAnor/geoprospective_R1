#' questionnaire UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
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
mod_questionnaire_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      br(),
      # layout_columns(

        # numericInput(ns("age"),
        #                "How old are you?",
        #                NULL,
        #                min=10,
        #                max=110,
        #                step=1),
      selectizeInput(ns("age"),
                     "How old are you?",
                     choices = c("18-39" =1,
                                 "40-59"= 2,
                                 "50-69" = 3,
                                 "70+" = 4,
                                 "Prefer not to say"=5),
                     options = list(
                       placeholder = 'Please select an option below',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      ),
      
      uiOutput(ns("cond_b0")),
        br(),

        selectizeInput(ns("gender"),
                         "What is your gender?",
                         choices = c("Female" ="female",
                                     "Male" = "male",
                                     "Gender neutral" = "neut",
                                     "Prefer not to say"="no_answ"),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          ),
      uiOutput(ns("cond_b1")),
      br(),
      
      selectizeInput(ns("edu"),
                         "What is the highest level of education you have fulfilled?",
                         choices = c("Primary school (up to age of 12)" = "prim",
                                     "Secondary school (up to age of 16) " ="sec",
                                     "High school (up to the age of 19)" = "tertiary",
                                     "Higher education" = "high",
                                     "Prefer not to say"="no_answ"),
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }'))
                         
          ),
      uiOutput(ns("cond_b2")),
      br(),
      
      selectizeInput(ns("work"),
                                 "In which economic sector do you currently work?",
                                 choices = c("Student or school" = "school",
                                   "Primary sector (Farming, logging, fishing, forestry and mining)" = "first",
                                             "Secondary sector (Manufacturing, Construction, Reparing)"= "second",
                                             "Tertiary sector (Trading, Finance, Administration)"= "third",
                                             "Quaternary sector (Media, research and development, education, design)" =  "fourth",
                                             "Prefer not to say" = "no_w"),
                                 options = list(
                                   placeholder = 'Please select an option below',
                                   onInitialize = I('function() { this.setValue(""); }')
                                 )),
      uiOutput(ns("cond_b3")),
      br(),
      br(),
      h3("Study area"),
      if(site_type == "onshore"){
        fluidRow(h5("The orange border shows the area in which you are going to map different benefits from nature"))
      }else{
        fluidRow(h5("The orange border delineates the offshore area in which you are going to map different benefits from nature"))
      }
      ,
      br(),
      tags$head(
        tags$style(HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }
    "))
      ),
      leafletOutput(ns("map_stud")),
      br(),
      uiOutput(ns("cond_map")),
      br(),

      uiOutput(ns("cond_b5"))
    ) #/main panel
  )
}

#' questionnaire Server Functions
#'
#' @noRd
mod_questionnaire_server <- function(id, user_id, site_id, sf_stud_geom, site_type, table_con,grd){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rv1<-reactiveValues(
      u = reactive({})
    )
    # print(user_id)
    shinyalert(  title = "About you",
                 text = "First, we have some general questions to get to know you. Please provide an answer for all the following questions. Your information will be anonymized for further use. ",
                 type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 showCancelButton = FALSE,
                 showConfirmButton = TRUE,
                 animation = "slide-from-bottom",
                 size = "s")
    output$map_stud<-renderLeaflet({
      if(site_type == "onshore"){
        leaflet(sf_stud_geom) %>%
          addPolygons(color = orange, weight = 3, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0)%>%
          addProviderTiles(providers$OpenStreetMap.Mapnik,options = tileOptions(minZoom = 8, maxZoom = 15))
      }else{
        leaflet(sf_stud_geom) %>%
          addPolygons(color = orange, weight = 3, smoothFactor = 2,
                      opacity = 1.0, fillOpacity = 0)%>%
          addProviderTiles(providers$OpenStreetMap.Mapnik)%>%
          setView(lat = 59.3233563, lng = 4.8832060, zoom= 10)
      }
     
    })
    
    output$cond_b0<-renderUI({
      validate(
        need(input$age != '', 'Select an age category'),
      )
    })
    
    output$cond_b1<-renderUI({
      validate(
        need(input$gender != '', 'Select a gender')
      )
    })
    
    output$cond_b2<-renderUI({
      validate(
        need(input$edu != '', 'Select an education')
      )
    })
    output$cond_b3<-renderUI({
      validate(
        need(input$work != '', 'Select a working industry')
      )
    })
    output$cond_b4<-renderUI({
      validate(
        need(input$length_liv != '', 'Provide a number of years you live in the area'),
      )
    })

    output$cond_b5<-renderUI({
      if(site_type == "onshore"){
        validate(
          # need(input$age >= 10 && input$age <= 110, ""),
          need(input$age != '', ''),
          need(input$gender != '', ''),
          need(input$edu != '', ''),
          need(input$work != '', ''),
          need(input$length_liv != '', '')
          #need(input$age >= input$length_liv, "You can't live longer in an area than you are old")
        )
        actionButton(ns('sub_quest'), 'submit answers', style="color: black; background-color: #31c600; border-color: #31c600")
        
      }else{
        validate(
          # need(input$age >= 10 && input$age <= 110, ""),
          need(input$age != '', ''),
          need(input$gender != '', ''),
          need(input$edu != '', ''),
          need(input$work != '', ''),

        )
        actionButton(ns('sub_quest'), 'submit answers', style="color: black; background-color: #31c600; border-color: #31c600")
      }

    })
    if(site_type == "onshore"){
      output$cond_map<-renderUI({
        tagList(            numericInput(ns("length_liv"),
                                         paste0("How many years have you lived in ",sf_stud_geom$siteNAME ,"?"),
                                         NULL,
                                         min=0,
                                         max=110,
                                         step=1)%>%
          # selectInput(ns("liv_in_area"),"Do you currently live in the study area?", choices = c("","yes","no"),selected = "")%>%
            
            shinyInput_label_embed(
              icon("info") %>%
                bs_embed_tooltip(title = "If you don't live in the area select 0 years.",placement = "right")
            ),
          use_bs_tooltip(),
          uiOutput(ns("cond_b4")),
          uiOutput(ns("cond2"))
        )
      })
      observeEvent(input$length_liv,{
        req(input$length_liv)
        if(input$length_liv >0){
          output$cond2<-renderUI({
            removeUI(
              selector = paste0("#",ns("map_stud"))
            )
            tagList(
              h5("Please select the rectangle in which you currently live."),
              br(),
              mapedit::selectModUI(ns("map_living")),
              
              
            )
          })
          
        }
      })
      
      map_liv<-eventReactive(input$length_liv,{
        req(grd)
        if(input$length_liv >0){
          
          map_liv<- leaflet() %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik, options = tileOptions(minZoom = 8, maxZoom = 15))%>%
            addFeatures(st_sf(grd), layerId = ~seq_len(length(grd)))
          
        }
      })
    }else{
      
    }





    liv_pol <- callModule(module=selectMod,
                          leafmap=map_liv(),
                          id="map_living")








    observeEvent(input$sub_quest,{
      show_modal_spinner(
        color = green,
        text="update data base"
      )
      rv1$u <-reactive({1})
      if(site_type == "onshore"){
        
        if(input$length_liv >0){
          liv_in_area <- "yes"
          length_liv<-input$length_liv
          liv_pol<-liv_pol()
          liv_pol<-st_sf(grd[as.numeric(liv_pol[which(liv_pol$selected==TRUE),"id"])])
          # only take first poly if user selected multiple
          liv_pol<-liv_pol[1,]
          cent<-st_centroid(liv_pol)
          user_lat <- st_coordinates(cent)[2]
          user_lng <- st_coordinates(cent)[1]
        }else{
          liv_in_area <- "no"
          length_liv <-as.integer(0)
          user_lat <- as.numeric(999.1)
          user_lng <- as.numeric(999.1)
        }}else{
        liv_in_area<- "no"
        length_liv <-as.integer(0)
        user_lat <- as.numeric(999.1)
        user_lng <- as.numeric(999.1)
      }


      quest<-  data.frame(
        userID = user_id,
        siteID = site_id,
        edu = input$edu,
        fam = "AA",
        gen = as.character(input$gender),
        age = as.integer(input$age),
        work = input$work,
        userLAT = user_lat,
        userLNG = user_lng,
        liv_in_area = liv_in_area,
        length_liv = as.integer(length_liv),
        pref_land_type = "AA",
        res_1 = as.integer(999),
        res_2 = as.integer(999),
        res_3 = as.integer(999),
        res_4 = as.integer(999),
        res_5 = as.integer(999),
        res_6 = as.integer(999),
        res_7 = as.integer(999),
        res_8 = as.integer(999),
        res_9 = as.integer(999),
        res_10 = as.integer(999),
        res_11 = as.integer(999),
        res_12 = as.integer(999),
        res_13 = as.integer(999),
        res_14 = as.integer(999),
        res_15 = as.integer(999),
        res_16 = as.integer(999),
        res_17 = as.integer(999),
        res_18 = as.integer(999),
        res_19 = as.integer(999),
        res_20 = as.integer(999)
      )
      # insert_upload_job(table_con$project, table_con$dataset, "mapper", quest)
      mapper_tab = bq_table(project = project_id, dataset = dataset, table = 'mapper')
      bq_table_upload(x = mapper_tab, values = quest, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
      remove_modal_spinner()

    })
    # play back the value of the confirm button to be used in the main app
    cond <- reactive({rv1$u()})

    return(cond)

  })
}

## To be copied in the UI
# mod_questionnaire_ui("questionnaire_1")

## To be copied in the server
# mod_questionnaire_server("questionnaire_1")
