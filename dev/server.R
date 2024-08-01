## server

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
      # output$alertUI<-renderUI(h4("-click the rectangle button"))
    
    if(input$gdpr == TRUE){

      

      #remove checkbox and render email
      removeUI(
        selector = "div:has(>> #gdpr)"
      )
      output$cond_0<-renderUI({
        tagList(bslib::value_box(
          title= "",
          showcase_layout = "left center",
          theme = "teal",
          showcase = bs_icon("pencil"),
            br(),
            br(),
            h5("To be able to contact you as soon as the second mapping session is open, we ask you to provide your email address in this first session. We will not use your email address for any other purpose and will anonymize all your data. However, if you don’t want to provide your email address you are welcome to complete just the first mapping round."),
            textInput("email","")

        ),
        actionButton("check_mail","submit"),
        uiOutput("cond_1"))

          
        
      })#/ui
    }#/if
  })
  
  
  ## here check if user is not already present in DB
  observeEvent(input$check_mail,{
    #if no email is provided
    if(input$email == ""){
      removeUI(
        selector = "#check_mail")
      output$cond_1<-renderUI({
        tagList(
          br(),
          actionButton("sub1","Start the study", class='btn-primary')
        )
      })
    }else{
      show_modal_spinner(
        text = "check mail"
      )
      req(site_id)
      check_tab <- bq_table(project_id, bqprojID, "user_conf")
      if(bq_table_exists(check_tab)==T){
        user_conf<-tbl(con_admin, "user_conf")%>%collect()
        user_conf<-user_conf%>%filter(siteID == site_id)
        
        #email already present
        if(input$email %in% user_conf$userMAIL){
          output$cond_1<-renderUI({
            h5("email for this study already present")
          })
        }else{
          removeUI(
            selector = "#check_mail")
          output$cond_1<-renderUI({
            tagList(
              br(),
              actionButton("sub1","Start the study", class='btn-primary')
            )
          })
        }
      }else{
        removeUI(
          selector = "#check_mail")
        output$cond_1<-renderUI({
          tagList(
            br(),
            actionButton("sub1","Start the study", class='btn-primary')
          )
        })
      }
      remove_modal_spinner()
    }
  })
  
  ##create a user ID as soon as start is pressed
  userID<-eventReactive(input$sub1,{
    nchar<-round(runif(1,8,13),0)
    userID<-stri_rand_strings(1, nchar, pattern = "[A-Za-z0-9]")
  })
  
  ## save user data in the DB and open the questionnaire module
  observeEvent(input$sub1,{
    show_modal_spinner(
      text = "update data base"
    )
    req(userID)
    userID<-userID()
    
    ## save user_conf
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
    remove_modal_spinner()
    
    #download predictors if not already in the app folder:
    path<-paste0("env_var_",site_id)
    if(!dir.exists(paths = path)){
      show_modal_spinner(text = "fetch geodata from server")
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
      remove_modal_spinner()
    }
    
  })
  
  
  observeEvent(input$sub1,{
    userID<-userID()
    updateTabsetPanel(session, "inTabset",
                      selected = "p3")
    hideTab(inputId = "inTabset",
            target = "p1")
    showTab(inputId = "inTabset", target = "p3")
    rv$u<-mod_questionnaire_server("questionnaire",userID, site_id, sf_stud_geom, site_type, con_admin, grd)
  })
  ## chreate a randomized order of ES
  # stud_es<-eventReactive(rv$u(),{
  #   stud_es<-stud_es[sample(nrow(stud_es)),]
  # })
  
  ### instructions
  # training module
  # observeEvent(rv$u(),{
  #   userID<-userID()
  #   # num_tabs<-num_tabs()
  #   updateTabsetPanel(session, "inTabset",
  #                     selected = "p4")
  #   hideTab(inputId = "inTabset",
  #           target = "p3")
  #   showTab(inputId = "inTabset", target = "p4")
  #   rv$v<-mod_instructions_server("training_1",sf_stud_geom,userID,site_id)
  # })
  
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
                 text = "After this training session you will now try to map different benefits from nature",
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
    updateProgressBar(session = session, id = "pb", value = 95)
    output$ahp_dist<-renderUI({
      tagList(
        mod_dist_impact_ui("ahp_dist_1")
        
      )
    })
    rv$y <- mod_dist_impact_server("ahp_dist_1", isolate(userID()), site_id, stud_all)
  })
  
  
  observeEvent(rv$y(),{
    removeUI("#ahp_dist")
    updateProgressBar(session = session, id = "pb", value = 100)
    output$final<-renderUI({
      tagList(
        bslib::value_box(
          title= "",
          showcase_layout = "left center",
          theme = "success",
          showcase = bs_icon("check-square"),
          h5("This is the end of the first session of the study, you can now close the browser. Thank you very much for your participation. In case you provided your email, we will contact you soon for the second session."),
       )

    )
  })

  })
  
  
  
}#/server