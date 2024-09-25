fluidPage(theme = shinytheme("flatly"),
  useShinydashboard(),
  useShinyjs(),

  titlePanel(title = div(img(src=paste0(project_id,".PNG"), width ='120'), paste0("Benefits from Nature in ",sf_stud_geom$siteNAME, " session I")), windowTitle = "Nature benefits I"),
  shinyWidgets::progressBar(id = "pb", value = 0, display_pct = TRUE),
  tabsetPanel(id = "inTabset",
              tabPanel(title = "", value = "p0",
                       bslib::value_box(
                         title="",
                         value = "",
                         h3("Welcome and many thanks for your interest in this study"),
                         showcase_layout = "left center",
                         theme =  "white",
                       ),
                       value_box(title="",
                                 h4("The landscapes in which we live consist of a mix of different land-use such as for example urban settlements and natural areas including agriculture, forestry and natural protected areas. Landscapes change continuously, shaped by our activities as well as natural processes."),
                                 br(),
                                 h4("Human activities and natural processes may reduce the benefits we gain from nature, such as berry-picking, outdoor recreation or flood protection. To ensure that we can keep on benefitting from the nature surrounding us, it is important to understand how and where people actually benefit from nature."),
                                 showcase = bs_icon("book"),
                                 theme = value_box_theme(bg = "#4dd5ff", fg = "black")
                       ),

                       bslib::value_box(
                         title = "",
                         value = "",
                         h4(paste0("With your information, you help us to understand where in the ",sf_stud_geom$siteNAME," region you and other people benefit from nature. This will be useful for those who plan and decide where development of infrastructure can best occur or should not happen to maintain our benefits from nature. In addition, you help to sustain biodiversity and increase human well-being in ", sf_stud_geom$siteNAME)),
                         showcase = bs_icon("exclamation-octagon-fill"),
                         theme = value_box_theme(bg = "#ffa626", fg = "black")
                       ),

                       br(),
                       actionButton("to_data","Next",style="color: black; background-color: #31c600; border-color: #31c600")

              ),
              tabPanel("",value="p1",
                       bslib::value_box(
                         title = "",
                         h3("Procedure"),
                         value = "",
                         br(),
                         h5(HTML("<b>The whole study consists of two sessions</b>")),
                         br(),
                         h5(paste0("In this first session you are going to answer some general questions about yourself. We then ask you to map areas that give you different benefits from nature on an interactive map of ",sf_stud_geom$siteNAME," (instructions are provided). The first round closes with some comparison between nature values.")),
                         br(),
                         h5(paste0("In an optional second session, for which you will receive a new link in a few days, you will see where the benefits from nature can be found for ",sf_stud_geom$siteNAME," based on all participants’ anonymized inputs. We will then ask you to modify and update your mapping of nature values based on the insights you got from the other participants.")),
                         br(),
                         h5("Each of these mapping sessions takes about 30-45 minutes to complete"),
                         showcase = bs_icon("clock-history"),
                         theme = "white"
                       ),
                       br(),
                       bslib::value_box(
                         title= "",
                         showcase_layout = "left center",
                         showcase = bs_icon("database-lock"),
                         h3("Data protection summary"),
                         value= "",
                         h5("- During the following task, we will store your personal data for further analysis. The data collection is based on consent."),
                         h5("- Within this study we do not collect any sensitive information"),
                         h5("- We do not present and publish your individual data but statistics and summaries of all participants"),
                         h5("- We rigorously anonymise data for analysis and publication"),
                         h5("- Your individual data will be deleted after the survey is completed (31/09/2025)"),
                         h5("- We reuse and share only anonymised data for within the research project WENDY"),
                         h5("- We store the data on a google big query database hosted in Europe and owned by the Norwegian Institute of Nature research (NINA)."),
                         br(),
                         h5("LINK TO CONSENT FORM"),
                         br(),
                         checkboxInput("gdpr","I confirm that I have read and understood the information provided and agree to the collection and processing of my personal data as described. (If you do not want to give consent you can now close the study.)",value = F),
                         br(),
                         theme = value_box_theme(bg = "#ffa626", fg = "black")
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