fluidPage(theme = shinytheme("flatly"),
  useShinydashboard(),
  useShinyjs(),

  titlePanel(title = div(img(src=paste0(project_id,".PNG"), width ='120'), paste0("Benefits from Nature in ",sf_stud_geom$siteNAME), h4("Session I")), windowTitle = "Nature benefits I"),
  shinyWidgets::progressBar(id = "pb", value = 0, display_pct = TRUE),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "", value = "p0",
                       bslib::value_box(
                         title="",
                         value = "",
                         h3("Welcome and many thanks for your interest in this study"),
                         showcase_layout = "left center",
                         theme =  "white"
                       ),
                       bslib::value_box(
                         title = "",
                         value = "",
                         h5(paste0("With your information, you help us to understand where in the ",sf_stud_geom$siteNAME," region you and other people benefit from nature. This will be useful for those who plan and decide where development of infrastructure can best occur or should not happen to maintain our benefits from nature. In addition, you help to sustain biodiversity and increase human well-being in ", sf_stud_geom$siteNAME)),
                         showcase = bs_icon("exclamation-octagon-fill"),
                         theme = "teal"
                       ),
                       bslib::value_box(
                         title = "",
                         value = "",
                         h5(paste0("After some general questions about yourself, we ask you to map areas that give you different benefits from nature   on an interactive map of ",sf_stud_geom$siteNAME," (instructions are provided). In an optional  second session, for which you will receive a new link in a few days, you will see where the benefits from nature can be found for ",sf_stud_geom$siteNAME," based on all participants’ inputs, and if you wish, you can modify and update your mapping based on the insights you got from their mapping")),
                         br(),
                         h5("Each of these mapping sessions takes about 30 minutes to complete"),
                         showcase = bs_icon("clock-history"),
                         theme = "white"
                       ),
                       br(),
                       actionButton("to_data","Next")

              ),
              tabPanel("",value="p1",
                       br(),
                       bslib::value_box(
                         title= "",
                         showcase_layout = "left center",
                         showcase = bs_icon("database-lock"),
                         h3("Data protection"),
                         value= "",
                         h5("-During the following task, we will store your answers for further analysis."),
                         h5("-Within this study we do not collect any sensitive information"),
                         h5("- We do not present and publish your individual data but statistics and summaries of all participants"),
                         h5("- We rigorously anonymize data for analysis and publication"),
                         h5("- Your email address will not be shared within any other context outside this study and will be deleted after the survey is completed  date:XY"),
                         h5("- We reuse and share only anonymized data for within the research project WENDY"),
                         h5("- We store the data on a google big query database owned by the Norwegian Institute of Nature research (NINA)."),
                         h5("- You have the right to withdraw your data at any time and thus exclude it from the study."),
                         br(),
                         h5("LINK TO CONSENT FORM"),
                         br(),
                         checkboxInput("gdpr","I read and understood the form, use and storage of my data",value = F),
                         br(),
                         
                         theme = "white"
                       ),
                       br(),
                       uiOutput("cond_0")
                       ),
              
              tabPanel("", value= "p2",
                       br(),
                        actionButton("to_quest","next")
                       ),

              tabPanel(title = "", value = "p4",
                       mod_instructions_ui("training_1")
              ),
              tabPanel(title = "", value = "p3",
                       mod_questionnaire_ui("questionnaire")
              ),
  ),
  uiOutput("tabs"),
  uiOutput("ahp_group"),
  uiOutput("ahp_single"),
  uiOutput("ahp_dist"),
  uiOutput("final")
  
)