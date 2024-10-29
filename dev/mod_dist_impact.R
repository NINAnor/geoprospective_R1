#' dist_impact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dist_impact_ui <- function(id){
  ns <- NS(id)
  tagList(
    value_box(
      title = "",
      value = "Impacts of wind energy on benefits of nature",
      h4("Imagine that you are visiting a certain location in the study area and that you can see and/or hear a wind turbine from there. According to you, how much are the following different benefits of nature affected by the visual, acoustic or combined impact of the wind turbine?"),
      theme = value_box_theme(bg = orange, fg = "black"),
      showcase = bs_icon("question-octagon-fill")
    ),
    br(),
    h4("Remember: Just focus on visual and/or acoustic disturbance, not on the footprint from the turbine foundation!"),
    h5("0 = The benefit is not affected at all by the wind turbine(s), 1 = The area is completely unsuitable to benefit from nature"),
    br(),
    uiOutput(ns("slider_impact")),
    br(),
    actionButton(ns("conf"), "submit", style="color: black; background-color: #31c600; border-color: #31c600")
    )
}

#' dist_impact Server Functions
#'
#' @noRd
mod_dist_impact_server <- function(id, userID, site_id, stud_all){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    shinyalert(  title = "Impacts of wind energy on nature benefits",
                 text = "As a last part in this session, you are going to rate the impact of wind turbines on different nature benefits.",
                 type = "info",
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 showCancelButton = FALSE,
                 showConfirmButton = TRUE,
                 animation = "slide-from-bottom",
                 size = "s")
    ## change it afterwards to the correct es!!
    stud_es<-stud_all%>%filter(esID == "recr" | esID == "wild_hunt" | esID == "wild_col"| esID == "habitat"| esID == "sense"| esID == "aest")
    
    
    # output$slider_impact<-renderUI({
    #   lapply(1:nrow(stud_es),function(n){
    #     inputid<-paste0(stud_es$esID[n],"_impact")
    #     es<-stud_es%>%slice(n)
    #     label<-paste0("Impacts of wind energy on ",es%>%dplyr::select(contains(paste0("esNAME_",var_lang))))
    #     lay_descr<-es%>%dplyr::select(contains(paste0("esDESCR_lay_",var_lang)))
    # 
    #     tagList(
    #       fluidRow(
    #         column(4,
    #                sliderInput(
    #                  inputId = ns(inputid),
    #                  label = label,
    #                  min = 0,
    #                  max = 1,
    #                  value =.5,
    #                  step = .1,
    #                  ticks = T
    #                )#/slider  
    #                ),
    #         column(1),
    #         column(7,
    #                h5(lay_descr)
    #                )
    #       )
    #     )
    # 
    #   })#/lapply
    # })#/slider

    ####
    output$slider_impact <- renderUI({
      lapply(1:nrow(stud_es), function(n) {
        es<-stud_es%>%slice(n)
        inputid<-paste0(es$esID,"_impact")
        label<-paste0("Impacts of wind energy on ",es%>%dplyr::select(contains(paste0("esNAME_",var_lang))))
        # Slider with a modal button for each row
        fluidRow(
          column(
            width = 8,
            sliderInput(
              inputId = ns(inputid),
              label = label,
              min = 0, max = 1, value = 0.5,
              step = .1,
              ticks = T
            )
          ),
          column(
            width = 4,
            actionButton(inputId = ns(paste0("modal_button_", n)), label = "Show Details")
          ),
          # Define bsModal for each row with column information
          bsModal(
            id = ns(paste0("modal_", n)),
            title = "",
            trigger = ns(paste0("modal_button_", n)),
            size = "medium",
            h4(es%>%dplyr::select(contains(paste0("esDESCR_lay_",var_lang)))),
            verbatimTextOutput(paste0("modal_text_", n))
          )
        )
      })
    })
    

    
    ###
    observeEvent(input$conf,{
      show_modal_spinner(color = green,text = "update data base")
      val_list<-list()

      res<-lapply(1:nrow(stud_es),function(a){
        var<-paste0(stud_es$esID[a],"_impact")
        val_list[[a]]<-input[[var]]
        return(val_list)
      })
      val_vec <- unlist(res)
      stud_es$impact_val<-val_vec


      stud_es$userID<-rep(userID,nrow(stud_es))
      stud_es$siteID<-rep(site_id,nrow(stud_es))
      stud_es<-stud_es%>%dplyr::select(userID, siteID, esID, impact_val)
      colnames(stud_es)<-c("userID","siteID","esID","impact_val")
      es_mapping_tab = bq_table(project = project_id, dataset = dataset, table = 'es_impact')
      bq_table_upload(x = es_mapping_tab, values = stud_es, create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_APPEND')
      remove_modal_spinner()

    })

    cond <- reactive({input$conf})

    return(cond)

  })
}

## To be copied in the UI
# mod_dist_impact_ui("dist_impact_1")

## To be copied in the server
# mod_dist_impact_server("dist_impact_1")
