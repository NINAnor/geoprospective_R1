## server.R

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

function(input, output, session) {
  hideTab(inputId = "inTabset", target = "p1")
  hideTab(inputId = "inTabset", target = "p2")
  hideTab(inputId = "inTabset", target = "p3")
  hideTab(inputId = "inTabset", target = "p4")

  
  rv<-reactiveValues(
    u = reactive({}),
    v = reactive({}),
    w = reactive({}),
    x = reactive({}),
    y = reactive({}),
    z = reactive({})
  )
  
  #checkbox gdpr
  observeEvent(input$to_data,{
    updateTabsetPanel(session, "inTabset",
                      selected = "p1")
    hideTab(inputId = "inTabset",
            target = "p0")
    showTab(inputId = "inTabset", target = "p1")
    
  })
  
  observeEvent(input$gdpr,{
    if(input$gdpr == TRUE){
      #render an alert

      #remove checkbox and render email
      removeUI(
        selector = "div:has(>> #gdpr)"
      )

      output$cond_0<-renderUI({
        tagList(bslib::value_box(
          title= "",
          showcase_layout = "left center",
          theme = "white",
          showcase = bs_icon("pencil"),
            br(),
            br(),
            h5("If you would like to participate in the second session please provide your email address below. We won`t use your email for any other purpose"),
            textInput("email","")

        ),
        actionButton("sub1", "Start the study", style="color: black; background-color: #31c600; border-color: #31c600"),
        #uiOutput("cond_1")
        )
      #)#/tagList



      })#/ui
    } #/if
  })
  
  
  # here check if user is not already present in DB
  valid_mail<- eventReactive(input$sub1,{
    if(input$email == ""){
      valid_mail<-T
    }else{
      # print("check mail")
      show_modal_spinner(
        color = green,
        text = "check your mail"
      )
      check_tab <- bq_table(project_id, dataset, "user_conf")
      if(bq_table_exists(check_tab)){

        user_conf<-tbl(con_admin, "user_conf")%>%collect()%>%filter(siteID == site_id)

          if(input$email %in% user_conf$userMAIL){
            #print("check mail3 user present")
            valid_mail<-F
            
          }else{
            #print("check mail3 user not pres")
            valid_mail<-T
          }
      
      }else{
        valid_mail<-T
      }
      remove_modal_spinner()
    }
    valid_mail<-valid_mail
  })
  
  userID<-eventReactive(input$sub1,{
    req(valid_mail)
    nchar<-round(runif(1,8,13),0)
    print(userID)
    userID<-stri_rand_strings(1, nchar, pattern = "[A-Za-z0-9]")
  })
  
  
  observeEvent(input$sub1,{
    req(userID)
    req(valid_mail)
    valid_mail<-valid_mail()
    userID<-userID()
    
    if(valid_mail == T){
      show_modal_spinner(
        color = green,
        text = "Access study and load data"
      )
      
      updateTabsetPanel(session, "inTabset",
                        selected = "p3")
      hideTab(inputId = "inTabset",
              target = "p1")
      
      user_conf_df<-data.frame(
        userID = userID,
        userMAIL = input$email,
        userTLOG = Sys.time(),
        siteID = site_id,
        projID = project_id
      )
      
      #upload bq
      user_conf_tab = bq_table(project = project_id, dataset = dataset, table = 'user_conf')
      bq_table_upload(x = user_conf_tab, values = user_conf_df, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
      updateProgressBar(session = session, id = "pb", value = 5)
      
      
      #download predictors if not already in the app folder:
      path<-paste0("env_var_",site_id)
      if(!dir.exists(paths = path)){
        show_modal_spinner(text = "fetch geodata from server", color = green)
        dir.create(file.path(path))
        ## download the prepared env data from gcs
        
        lulc<-paste0("gs://",bucket_name,"/",site_id,"/2_env_var/lulc.tif")
        gcs_get_object(lulc, saveToDisk = paste0(path,"/lulc.tif"),overwrite = T)
        
        dem<-paste0("gs://",bucket_name,"/",site_id,"/2_env_var/dem.tif")
        gcs_get_object(dem, saveToDisk = paste0(path,"/dem.tif"),overwrite = T)
        
        acc<-paste0("gs://",bucket_name,"/",site_id,"/2_env_var/acc.tif")
        gcs_get_object(acc, saveToDisk = paste0(path,"/acc.tif"),overwrite = T)
        
        int<-paste0("gs://",bucket_name,"/",site_id,"/2_env_var/int.tif")
        gcs_get_object(int, saveToDisk = paste0(path,"/int.tif"),overwrite = T)
        
        
        ## align raster
        lulc<-terra::rast(paste0(path,"/lulc.tif"))%>%terra::project(crs("+init=epsg:4326"))
        dem<-terra::rast(paste0(path,"/dem.tif"))%>%terra::project(crs("+init=epsg:4326"))
        acc<-terra::rast(paste0(path,"/acc.tif"))%>%terra::project(crs("+init=epsg:4326"))
        int<-terra::rast(paste0(path,"/int.tif"))%>%terra::project(crs("+init=epsg:4326"))
        
        
        lulc<-terra::resample(lulc,dem)
        acc<-terra::resample(acc,dem)
        int<-terra::resample(int,dem)
        
        terra::writeRaster(dem,paste0(path,"/dem.tif"),overwrite=T)
        terra::writeRaster(lulc,paste0(path,"/lulc.tif"),overwrite=T)
        terra::writeRaster(acc,paste0(path,"/acc.tif"),overwrite=T)
        terra::writeRaster(int,paste0(path,"/int.tif"),overwrite=T)
        
      }
      remove_modal_spinner()
      
      

      showTab(inputId = "inTabset", target = "p3")
      rv$u<-mod_questionnaire_server("questionnaire",userID, site_id, sf_stud_geom, site_type, con_admin, grd)
    }else{
      shinyalert(
        title = "Email already present",
        text = "You can't use the same email address several times",
        type = "error",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        animation = "slide-from-bottom",
        size = "s"
      )
      
    }
    
  })
  
  

  observeEvent(rv$u(),{
    updateProgressBar(session = session, id = "pb", value = 15)
    userID<-userID()
    # num_tabs<-num_tabs()
    updateTabsetPanel(session, "inTabset",
                      selected = "p4")
    hideTab(inputId = "inTabset",
            target = "p3")
    showTab(inputId = "inTabset", target = "p4")
    rv$v<-mod_instructions_server("training_1",sf_stud_geom,userID,site_id)
  })
  
  #######################
  ### create N tabs
  
  observeEvent(rv$v(),{
    updateProgressBar(session = session, id = "pb", value = 25)
    hideTab(inputId = "inTabset",
            target = "p4")
    
    
    

    # num_tabs<-num_tabs()
    shinyalert(  title = "Mapping benefits of nature",
                 text = "After the training session you will now try to map different benefits from nature",
                 type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 showCancelButton = FALSE,
                 showConfirmButton = TRUE,
                 animation = "slide-from-bottom",
                 size = "s")
    output$tabs <- renderUI({
      do.call(tabsetPanel, c(id="tabs_content",
                             lapply(1:num_tabs, function(i) {
                               tabPanel(title = paste("Mapping ", i), value = paste0("map_", i),
                                        mod_delphi_round1_ui(paste0("mapping_",i))
                                        
                               )#/tabpanel
                             })#/lapply
      ))#/do.call
    })#/UI render
  })

  pred<-eventReactive(rv$v(),{
    path<-paste0("env_var_", site_id)
    pred<-paste0(path,"/")
  })
  
  
  ## hide tabs
  observeEvent(input$tabs_content, {
    # num_tabs<-num_tabs()
    rand_es_sel<-stud_es[sample(nrow(stud_es)),]
    pred<-pred()
    # rand_es_sel<-stud_es
    # sf_stud_geom<-sf_stud_geom()
    # comb<-comb()
    userID<-userID()
    # site_id<-site_id()
    # bands<-bands()
    # site_type<-site_type()
    
    for (i in 2:num_tabs) {
      shinyjs::runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
    }
    
    lapply(1:num_tabs, function(i) {
      rv<-reactiveValues(
        a = reactive({})
      )
      
      
      rv$a<-mod_delphi_round1_server(paste0("mapping_",i),
                                     sf_stud_geom,
                                     # comb,
                                     # bands,
                                     rand_es_sel,
                                     as.numeric(i),
                                     userID,
                                     site_id,
                                     # table_con,
                                     site_type,
                                     var_lang,
                                     pred)
      #reactive value from module as event
      observeEvent(rv$a(), {
        updateProgressBar(session = session, id = "pb", value = 25+i*15)
        next_tab <- i+1
        shinyjs::runjs(paste("$('.nav-tabs li:nth-child(", next_tab, ")').show();"))
        if(next_tab<=num_tabs){
          updateTabsetPanel(session, "tabs_content", selected=paste0("map_",next_tab) )
          shinyjs::runjs(paste("$('.nav-tabs li:nth-child(", i, ")').hide();"))
        }else{
          removeUI("#tabs")
          output$ahp_group<-renderUI({
            tagList(
              mod_ahp_group_ui("ahp_group_1")
            )
          })

        }
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  }, once = TRUE)
  
  
  rv$w <- mod_ahp_group_server("ahp_group_1", isolate(userID()), site_id, area_name)
  
  observeEvent(rv$w(),{
    removeUI("#ahp_group")
    updateProgressBar(session = session, id = "pb", value = 90)
    output$ahp_single<-renderUI({
      tagList(
        mod_ahp_single_ui("ahp_single_1")

      )
    })
    rv$x <- mod_ahp_single_server("ahp_single_1", isolate(userID()), site_id, stud_all)
  })
  
  
  observeEvent(rv$x(),{
    removeUI("#ahp_single")
    if(isTRUE(wind_lca_questions)){
      #removeUI("#ahp_single")
      updateProgressBar(session = session, id = "pb", value = 95)
      output$ahp_dist<-renderUI({
        tagList(
          mod_dist_impact_ui("ahp_dist_1")
          
        )
      })
      rv$y <- mod_dist_impact_server("ahp_dist_1", isolate(userID()), site_id, stud_all)
    }else{
      
      updateProgressBar(session = session, id = "pb", value = 100)
      output$final<-renderUI({
        tagList(
          bslib::value_box(
            title= "",
            showcase_layout = "left center",
            theme = value_box_theme(bg = blue, fg = "black"),
            showcase = bs_icon("check-square"),
            h5("This is the end of the first session of the study, you can now close the browser. Thank you very much for your participation. In case you provided your email, we will contact you soon for the second session."),
            h5(HTML(paste0(
              'More information about the <a href="', 
              setting$project.url, 
              '">', 
              setting$project_name, 
              ' project</a>'
            )))
          )
          
        )
      })
    }

  })
  
  
  observeEvent(rv$y(),{
    removeUI("#ahp_dist")
    updateProgressBar(session = session, id = "pb", value = 100)
    output$final<-renderUI({
      tagList(
        bslib::value_box(
          title= "",
          showcase_layout = "left center",
          theme = value_box_theme(bg = blue, fg = "black"),
          showcase = bs_icon("check-square"),
          h5("This is the end of the first session of the study, you can now close the browser. Thank you very much for your participation. In case you provided your email, we will contact you soon for the second session."),
          h5(HTML('More information about the <a href="https://wendy-project.eu" target="_blank">EU-WENDY project</a>'))
       )

    )
  })

  })
  
  
  
}#/server