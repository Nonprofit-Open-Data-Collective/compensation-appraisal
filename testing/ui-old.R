ui<- bootstrapPage(
  tags$head(tags$style(HTML('.navbar-static-top {background-color: blue;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: blue;}'))),

  navbarPage("CEO Pay Apprisial",
             theme = bs_theme(
               bg = "#f5f5f5",
               fg = "#0a4c6a",
               primary = "#fdbf11",
               base_font = font_google("Prompt"),
               code_font = font_google("JetBrains Mono")
             ),
             
             # ##################################################
             #   ###  Data tabPanel
             # ##################################################
             tabPanel("Glide Testing", 
                      titlePanel("CEO Compensation Apprisial Tool!"),
                      
                      glide(
                        shinyglide::screen( #Directions
                           HTML('Welcome to the CEO Compensation Apprasial Tool. Here is how it works.
                                <br> 
                                PUT DIRECTIONS HERE
                                <br>
                                PUT SOMETHING ABOUT HOW WE USE THE NTEE-CC SYSTEM FOR CLASSIFICATION
                                <br>
                                SCROLL DOWN')
                         ), #end directions screen
                        shinyglide::screen( #Org inputs screen
                          h4("Tell us about your orginization."), 
                          wellPanel(
                            #org.State
                            selectInput("org.State", 
                                        label = "What state are you located in?",
                                        choices = sort(c(datasets::state.abb, "PR", "DC"))), 
                            # #org.loc
                            selectInput("org.loc",
                                        "What type of city is your orginization located in?",
                                        choices = c("Rural", "Suburban", "Urban")),
                            # org.MajorGroup
                            selectInput("org.MajorGroup",
                                        label = htmltools::HTML("What broad category best describes the work your orginization does?"),
                                        choices = c("Arts, Culture, and Humanities" = 1,
                                                    "Education" = 2,
                                                    "Environment and Animals" = 3,
                                                    "Health" = 4,
                                                    "Human Services" = 5,
                                                    "International, Foreign Affairs" = 6,
                                                    "Public, Societal Benefit" = 7,
                                                    "Religion Related" = 8,
                                                    "Mutual/Membership Benefit" = 9,
                                                    "Unknown/Unclassified"= 10)) %>%
                               shinyhelper::helper(type = "markdown",
                                                content = "MajorGroup"),
                            #NTEE
                            selectInput("org.NTEE",
                                        label = htmltools::HTML("What major group best describes the work your orginization does?"),
                                        choices = LETTERS) %>%
                              shinyhelper::helper(type = "markdown",
                                                  content = "NTEE"),
                            #NTEE.CC
                             textInput("org.NTEE.CC",
                                       label = htmltools::HTML("What NTEE common code best describes the work your orginization does?"),
                                       value = "A01") %>%
                            shinyhelper::helper(type = "markdown",
                                                content = "NTEECC"),
                            # # org.Hosp
                            "Is your orginization a Hospital?",
                            switchInput("org.HOSP", label = NA,
                                        value = FALSE,
                                        onLabel = "Yes",
                                        offLabel = "No"),
                            # org.Univ
                            "Is your orginization a University?",
                            switchInput("org.UNIV", label = NA,
                                        value = FALSE,
                                        onLabel = "Yes",
                                        offLabel = "No"),
                            # org.TotalExpense
                            numericInput("org.TotalExpense", label = "What are your orginizations anaual expenses?", #are or is?
                                         value = 1000000,
                                         min = 0),
                            # org.TotalEmployee
                            numericInput("org.TotalEmployee", label = "How many full time equivalent employees does your orginization have?",
                                         value = 25,
                                         min = 0),
                            #org.EZQual
                            "Do you qualify to file IRS Form 990EZ?",
                            switchInput("org.EZQual", label = NA,
                                        value = FALSE,
                                        onLabel = "Yes",
                                        offLabel = "No")  %>%
                              shinyhelper::helper(type = "markdown",
                                                  content = "EZQual")


                          )#end wellPannel
                        ), #end org inputs screen
                        shinyglide::screen( #Search criteria screen
                          h4("What types of orginizations to you want to compare yourself to?"),
                          HTML("What is the differnce between hard and soft matching? 
                               <br>
                               PUT INSTRUCTIONS HERE ABOUT HARD AND SOFT MATCHING"),
                          wellPanel(
                            fluidRow(
                            column(6,
                                  #search.FormYr
                                    pickerInput(
                                      inputId = "search.FormYr",
                                      label = "I want to include organizations whose latest IRS filing year was in the following years: ",
                                      choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
                                                     "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                     "2015" = 2015, "2016" = 2016, "2017" = "2017",
                                                     "2018" = 2018, "2019" = 2019),
                                      multiple = TRUE,
                                      selected = 2019,
                                      options = list(
                                        `actions-box` = TRUE,
                                        `deselect-all-text` = "None",
                                        `select-all-text` = "All",
                                        `none-selected-text` = "NA"
                                      )
                                    ), 
                                   ),#end column
                            column(6, 
                                   "Do you want to hard or soft matching on IRS filing year?",
                                   switchInput("hard.FormYr",
                                               label = NA,
                                               value = FALSE,
                                               onLabel = "Hard",
                                               offLabel = "Soft"))
                            )#end fluid row 
                          ), #end well Panel
                          wellPanel(
                            fluidRow(
                              column(6,
                                     #search.state
                                     pickerInput(
                                       inputId = "search.state",
                                       label = "I want to include orginizations who live in the following states:",
                                       choices = sort(c(state.abb, "PR", "DC")),
                                       multiple = TRUE,
                                       selected = sort(c(state.abb, "PR", "DC")),
                                       options = list(
                                         `actions-box` = TRUE,
                                         `deselect-all-text` = "None",
                                         `select-all-text` = "All",
                                         `none-selected-text` = "NA"
                                       )
                                     )
                              ),#end column
                              column(6, 
                                     "Do you want to hard or soft matching on state?",
                                     switchInput("hard.state",
                                                 label = NA,
                                                 value = FALSE,
                                                 onLabel = "Hard",
                                                 offLabel = "Soft"))
                            )#end fluid row 
                          ), #end well Panel
                          wellPanel(
                            fluidRow(
                              column(6,
                                     #search.loc
                                     pickerInput(
                                       inputId = "search.loc",
                                       label = "I want to include orginizations who live in the following types of cities:",
                                       choices = list("Rural" = "Rural",
                                                      "Suburban" = "Suburban",
                                                      "Urban" = "Urban"),
                                       multiple = TRUE,
                                       selected = c("Rural", "Suburban", "Urban"),
                                     )
                              ),#end column
                              column(6, 
                                     "Do you want to hard or soft matching on city types?",
                                     switchInput("hard.loc",
                                                 label = NA,
                                                 value = FALSE,
                                                 onLabel = "Hard",
                                                 offLabel = "Soft"))
                            )#end fluid row 
                          ), #end well Panel
                          wellPanel(
                            fluidRow(
                              column(6,
                                     # search.MajorGroup
                                     pickerInput(
                                       inputId = "search.MajorGroup",
                                       label = "I want to include orginizations in the following braod categories:",
                                       choices = c("Arts, Culture, and Humanities" = 1,
                                                   "Education" = 2,
                                                   "Environment and Animals" = 3,
                                                   "Health" = 4,
                                                   "Human Services" = 5,
                                                   "International, Foreign Affairs" = 6,
                                                   "Public, Societal Benefit" = 7,
                                                   "Religion Related" = 8,
                                                   "Mutual/Membership Benefit" = 9,
                                                   "Unknown/Unclassified"= 10), #showing up as 1:10 and idk why
                                       multiple = TRUE,
                                       selected = 1:10,
                                       options = list(
                                         `actions-box` = TRUE,
                                         `deselect-all-text` = "None",
                                         `select-all-text` = "Yeah, all!",
                                         `none-selected-text` = "NA"
                                       )
                                     ),
                              ),#end column
                              column(6, 
                                     "Do you want to hard or soft matching on city types?",
                                     switchInput("hard.MajorGroup",
                                                 label = NA,
                                                 value = FALSE,
                                                 onLabel = "Hard",
                                                 offLabel = "Soft"))
                            )#end fluid row 
                          ), #end well Panel
                          
                          
                          
                          column(6, #junk col
                                 "second column"
                                 ) #end junk column

                        ) ,  #end Search Criteria screen
                          
                      
                        
                        
                        
                        
                        

                        #   #                      # search.MajorGroup
                        #   #                      pickerInput(
                        #   #                        inputId = "search.MajorGroup",
                        #   #                        label = "Major Group",
                        #   #                        choices = c("Arts, Culture, and Humanities" = 1,
                        #   #                                    "Education" = 2,
                        #   #                                    "Environment and Animals" = 3,
                        #   #                                    "Health" = 4,
                        #   #                                    "Human Services" = 5,
                        #   #                                    "International, Foreign Affairs" = 6,
                        #   #                                    "Public, Societal Benefit" = 7,
                        #   #                                    "Religion Related" = 8,
                        #   #                                    "Mutual/Membership Benefit" = 9,
                        #   #                                    "Unknown/Unclassified"= 10), #showing up as 1:10 and idk why
                        #   #                        multiple = TRUE,
                        #   #                        selected = 1:10,
                        #   #                        options = list(
                        #   #                          `actions-box` = TRUE,
                        #   #                          `deselect-all-text` = "None",
                        #   #                          `select-all-text` = "Yeah, all!",
                        #   #                          `none-selected-text` = "NA"
                        #   #                        )
                        #   #                      ),
                        #   #                      #NEED TO ADD NTEE
                        #   #                      #NEED TO ADD NTEE.CC both NTEE and NTEE.CC need to be reactive to inside Major Group
                        #   #                      # select.HOSP
                        #   #                      # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                        #   #                      radioButtons("search.HOSP", label = "Do you want to compare to Orginizations that are hospitals?",
                        #   #                                   choices = list("No, I do not want to compare with hospitals." = 2,
                        #   #                                                  "Yes, I want to compare to both hospitals and non-hospitals." = NA,
                        #   #                                                  "Yes, but I only want to compare to hospitals." = 1),
                        #   #                                   selected = 2),
                        #   #                      # select.UNIV
                        #   #                      # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                        #   #                      radioButtons("search.UNIV", label = "Do you want to compare to Orginizations that are universities?",
                        #   #                                   choices = list("No, I do not want to compare with universities." = 2,
                        #   #                                                  "Yes, I want to compare to both universities and non-universities." = NA,
                        #   #                                                  "Yes, but I only want to compare to universities." = 1),
                        #   #                                   selected = 2),
                        #   #                      # search.TotalExpenses
                        #   #                      numericRangeInput("search.TotalExpenses", "Range of Total Expenses",
                        #   #                                        value = c(-Inf, Inf) #can we get commas on these values?
                        #   #                                        ),
                        #   #                      # search.TotalEmployees
                        #   #                      numericRangeInput("search.TotalEmployees", "Range of Total Employees",
                        #   #                                        value = c(0, 1000), #can we get commas on these values?
                        #   #                                        min = 0,
                        #   #                                        max = 1000)
                        #   # 
                        #   # 
                        #   
                        #   
                        #   
                        #   
                        #   
                        #   
                        # ), #End Search criteria screen
                        # 
                        # 
                        # 
                        # 
                        
                        #test screen
                        shinyglide::screen(
                          textOutput("test1")
                        ) #end test screen
                      )#end glide page 
                      
                      
                      
                      
                      # 
                      # fluidRow(
                      #   
                      #   column(6,
                      #          wellPanel(
                      #            sliderInput(
                      #              "bins", label = "Number of bins:",
                      #              min = 1, value = 30, max = 50
                      #            ),
                      #            sliderInput(
                      #              "bins", label = "Number of bins:",
                      #              min = 1, value = 30, max = 50
                      #            )
                      #          )  ,
                      #          
                      #   ),
                      #   
                      #   column(6,
                      #          sliderInput(
                      #            "bins", label = "Number of bins:",
                      #            min = 1, value = 30, max = 50
                      #          )
                      #   )
                      # ),
                      ), #end Glide Testing
             # ##################################################
             #   ###  Data tabPanel
             # ##################################################
               tabPanel("Data",
                        "This is where we will put links to our data."
               ), #end Gender Pay Differences tabPanel

             # ##################################################
             #   ###  Contact tabPanel
             # ##################################################
               tabPanel("Contact",
                        "This is where we will put our contact information."
               ) #end Gender Pay Differences tabPanel
             # 
             
  ), #end navbarPage
  

)#end bootstrap page 



# ui <- navbarPage("CEO Compensation Tool",
# 
# #############################################
# ### Glide Pannel Testing
# #############################################
#   tabPanel("Glide Testing",
#            fluidPage(theme = shinytheme("flatly")),
#            tags$head(tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
#           glide(
#             shinyglide::screen( #Directions
#                HTML('Welcome to the CEO Compensation Apprasial Tool. Here is how it works.
#                     <br> 
#                     PUT DIRECTIONS HERE')
#              ), #end Directions screen
#             shinyglide::screen( #Organizations Input
#                "Tell us about your orginization:",
#                selectInput("org.FormYr", label = "IRS Filing Year", 
#                            choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
#                                           "2012" = 2012, "2013" = 2013, "2014" = 2014, 
#                                           "2015" = 2015, "2016" = 2016, "2017" = "2017", 
#                                           "2018" = 2018, "2019" = 2019),
#                            selected = 2019), 
#                # org.FormType
#                "Do you qualify to file a 990EZ?  "%>%
#                  shinyhelper::helper(type = "inline",
#                                      title = "HERE IS SOME HELP",
#                                      content = "This is where we will put content about EZ qualifications"),
#                switchInput("org.FormType", label = NA, #### NEEDD TO UPDATE HOW SERVER READS THIS
#                            value = FALSE,
#                            onLabel = "Yes",
#                            offLabel = "No", ), 
#                # org.State
#                selectInput("org.State", label = "State",
#                            choices = state.abb), 
#                # org.MajorGroup
#                selectInput("org.MajorGroup", label = htmltools::HTML("Sector <u style=\"color:white;\"> hold </u>") %>%
#                              shinyhelper::helper(type = "inline",
#                                                  title = "Secor",
#                                                  content = "Choose the Orginiztion that best fits your orginization. You will be able to be more specific about your orignization in the Subsector"),
#                            choices = c("Arts, Culture, and Humanities" = 1,
#                                        "Education" = 2,
#                                        "Environment and Animals" = 3,
#                                        "Health" = 4,
#                                        "Human Services" = 5,
#                                        "International, Foreign Affairs" = 6,
#                                        "Public, Societal Benefit" = 7,
#                                        "Religion Related" = 8,
#                                        "Mutual/Membership Benefit" = 9, 
#                                        "Unknown/Unclassified"= 10)),
#                #NEED TO ADD NTEE
#                selectInput("org.NTEE", label = htmltools::HTML("NTEE Codes <u style=\"color:white;\"> hold </u>") %>%
#                              shinyhelper::helper(type = "markdown",
#                                                  title = "HERE IS SOME HELP",
#                                                  content = "Clusters"),
#                            choices = LETTERS),
#                #NEED TO ADD NTEE.CC
#                 # textInput("org.NTEE.CC", label = htmltools::HTML("NTEE Codes <u style=\"color:white;\"> hold </u>") %>%
#                 #                                  shinyhelper::helper(type = "inline",
#                 #                                                      title = "HERE IS SOME HELP",
#                 #                                                      content = "This is where we will put content about NTEE CODES"),
#                 #          value = "A01")) ,
# 
#                # # org.Hosp 
#                "Are you a Hospital?",
#                switchInput("org.HOSP", label = NA,
#                            value = FALSE,
#                            onLabel = "Yes",
#                            offLabel = "No"),
#                # org.Univ 
#                "Are you a University?",
#                switchInput("org.UNIV", label = NA,
#                            value = FALSE,
#                            onLabel = "Yes",
#                            offLabel = "No"),
#                # org.TotalExpense
#                numericInput("org.TotalExpense", label = "Anual Expenses", 
#                             value = 1000000),
#                # org.TotalEmployee
#                numericInput("org.TotalEmployee", label = "Full Time Employees", 
#                             value = 25,
#                             min = 0)
#              ), #End screen
#             shinyglide::screen(
#               "What types of orginizations to you want to compare to?",
#                  #search.FormYr
#                 pickerInput(
#                   inputId = "search.FormYr",
#                   label = "IRS Filing Year",
#                   choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
#                                            "2012" = 2012, "2013" = 2013, "2014" = 2014,
#                                            "2015" = 2015, "2016" = 2016, "2017" = "2017",
#                                            "2018" = 2018, "2019" = 2019),
#                   multiple = TRUE,
#                   selected = 2019,
#                   options = list(
#                     `actions-box` = TRUE,
#                     `deselect-all-text` = "None",
#                     `select-all-text` = "Yeah, all!",
#                     `none-selected-text` = "NA"
#                   )
#                 ), 
#                      # search.FormType
#                      checkboxGroupInput("search.FormType", label = "IRS Form",
#                                  choices = list("990" = "990", "990EZ" = "990EZ"),
#                                  selected= "990EZ"),
#                      # search.state
#                      pickerInput(
#                        inputId = "search.State",
#                        label = "States",
#                        choices = state.abb,
#                        multiple = TRUE,
#                        selected = state.abb,
#                        options = list(
#                          `actions-box` = TRUE,
#                          `deselect-all-text` = "None",
#                          `select-all-text` = "Yeah, all!",
#                          `none-selected-text` = "NA"
#                        )
#                      ),
#                      # search.MajorGroup
#                      pickerInput(
#                        inputId = "search.MajorGroup",
#                        label = "Major Group",
#                        choices = c("Arts, Culture, and Humanities" = 1,
#                                    "Education" = 2,
#                                    "Environment and Animals" = 3,
#                                    "Health" = 4,
#                                    "Human Services" = 5,
#                                    "International, Foreign Affairs" = 6,
#                                    "Public, Societal Benefit" = 7,
#                                    "Religion Related" = 8,
#                                    "Mutual/Membership Benefit" = 9,
#                                    "Unknown/Unclassified"= 10), #showing up as 1:10 and idk why
#                        multiple = TRUE,
#                        selected = 1:10,
#                        options = list(
#                          `actions-box` = TRUE,
#                          `deselect-all-text` = "None",
#                          `select-all-text` = "Yeah, all!",
#                          `none-selected-text` = "NA"
#                        )
#                      ),
#                      #NEED TO ADD NTEE
#                      #NEED TO ADD NTEE.CC both NTEE and NTEE.CC need to be reactive to inside Major Group
#                      # select.HOSP
#                      # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
#                      radioButtons("search.HOSP", label = "Do you want to compare to Orginizations that are hospitals?",
#                                   choices = list("No, I do not want to compare with hospitals." = 2,
#                                                  "Yes, I want to compare to both hospitals and non-hospitals." = NA,
#                                                  "Yes, but I only want to compare to hospitals." = 1),
#                                   selected = 2),
#                      # select.UNIV
#                      # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
#                      radioButtons("search.UNIV", label = "Do you want to compare to Orginizations that are universities?",
#                                   choices = list("No, I do not want to compare with universities." = 2,
#                                                  "Yes, I want to compare to both universities and non-universities." = NA,
#                                                  "Yes, but I only want to compare to universities." = 1),
#                                   selected = 2),
#                      # search.TotalExpenses
#                      numericRangeInput("search.TotalExpenses", "Range of Total Expenses",
#                                        value = c(-Inf, Inf) #can we get commas on these values?
#                                        ),
#                      # search.TotalEmployees
#                      numericRangeInput("search.TotalEmployees", "Range of Total Employees",
#                                        value = c(0, 1000), #can we get commas on these values?
#                                        min = 0,
#                                        max = 1000)
# 
# 
#             ), #end screen
#             shinyglide::screen( 
#               "Similar Orginizations Here is a list of instructions.Similar Orginizations Here is a list of instructions.Similar Orginizations Here is a list of instructions..Similar Orginizations Here is a list of instructions.Similar Orginizations Here is a list of instructions.Similar Orginizations Here is a list of instructions.vvSimilar Orginizations Here is a list of instructions", 
#               #DT::dataTableOutput("dat.filterd.table")
#               box(style='width:1000px;overflow-x: scroll;height:600px;overflow-y: scroll;',
#                   DT::dataTableOutput("dat.filterd.table", width = "100%")
#               )#end box
# 
#               ),
#             shinyglide::screen(
#               "Here is where we will put suggested ceo compensations"
#             ),
#                                                     
#   
#              height = "100%" # need to adjust this as we go
#            ) #end glide 
#           ), #end tab panel,
# #   
# # screen(  #Organization inputs 
# #   "Tell us about your orginization:",
# #   selectInput("org.MajorGroup", label = "Sector",
# #               choices = c("Arts, Culture, and Humanities" = 1,
# #                           "Education" = 2,
# #                           "Environment and Animals" = 3,
# #                           "Health" = 4,
# #                           "Human Services" = 5,
# #                           "International, Foreign Affairs" = 6,
# #                           "Public, Societal Benefit" = 7,
# #                           "Religion Related" = 8,
# #                           "Mutual/Membership Benefit" = 9, 
# #                           "Unknown/Unclassified"= 10)),%>%
# #     helper(icon = "exclamation",
# #            colour = "red",
# #            type = "inline",
# #            content = c("THIS IS INLINE TEXT")),
# #   
# #   uiOutput(outputId = "dynamicUI"),
# #   
# # ), #End orginization screen ,
#            
# ##################################################
#   ### 1st Page under header CEO Compensation
# ##################################################
#   # tabPanel("CEO Compensation",
#   # 
#   # 
#   # 
#   #         ### Side Bar on CEO Compensation page
#   #         pageWithSidebar(
#   #           headerPanel('Apply Orginization Filters'),
#   #           sidebarPanel(width = 4,
#   #                        tabsetPanel(type = "tabs",
#   #                                    ### Input Organization Characteristics
#   #                                    tabPanel("You Organization",
#   #                                             "Input your organizations characteristics.",
#   # 
#   #                                              # org.FormYr
#   #                                              selectInput("org.FormYr", label = "IRS Filing Year",
#   #                                                          choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
#   #                                                                         "2012" = 2012, "2013" = 2013, "2014" = 2014,
#   #                                                                         "2015" = 2015, "2016" = 2016, "2017" = "2017",
#   #                                                                         "2018" = 2018, "2019" = 2019),
#   #                                                          selected = 2019),
#   #                                             # org.FormType
#   #                                             selectInput("org.FormType", label = "What type of IRS Form did you file?",
#   #                                                         choices = list("990" = "990", "990EZ" = "990EZ")),
#   #                                             # org.State
#   #                                             selectInput("org.State", label = "State",
#   #                                                         choices = state.abb),
#   #                                             # org.MajorGroup
#   #                                             selectInput("org.MajorGroup", label = "Major Group",
#   #                                                         choices = c("Arts, Culture, and Humanities" = 1,
#   #                                                                     "Education" = 2,
#   #                                                                     "Environment and Animals" = 3,
#   #                                                                     "Health" = 4,
#   #                                                                     "Human Services" = 5,
#   #                                                                     "International, Foreign Affairs" = 6,
#   #                                                                     "Public, Societal Benefit" = 7,
#   #                                                                     "Religion Related" = 8,
#   #                                                                     "Mutual/Membership Benefit" = 9,
#   #                                                                     "Unknown/Unclassified"= 10)),
#   #                                             #NEED TO ADD NTEE
#   #                                             "NEED TO ADD LINK TO NTEE AND NETT.CC CODES",
#   #                                             selectInput("org.NTEE", label = "NTEE Code",
#   #                                                         choices = LETTERS),
#   #                                             #NEED TO ADD NTEE.CC
#   #                                             textInput("org.NTEE.CC", label = "NTEE-CC Code",
#   #                                                       value = "A01"),
#   #                                             # org.Hosp
#   #                                             "Are you a Hospital?",
#   #                                             switchInput("org.HOSP", label = NA,
#   #                                                         value = FALSE,
#   #                                                         onLabel = "Yes",
#   #                                                         offLabel = "No"),
#   #                                             # org.Univ
#   #                                             "Are you a University?",
#   #                                             switchInput("org.UNIV", label = NA,
#   #                                                         value = FALSE,
#   #                                                         onLabel = "Yes",
#   #                                                         offLabel = "No"),
#   #                                             # org.TotalExpense
#   #                                             numericInput("org.TotalExpense", label = "Anual Expenses",
#   #                                                          value = 1000000),
#   #                                             # org.TotalEmployee
#   #                                             numericInput("org.TotalEmployee", label = "Full Time Employees",
#   #                                                          value = 25,
#   #                                                          min = 0),
#   #                                             #submitButton("Submit")
#   #                                    ),
#   # 
#   #                                    ### Comparison Filters
#   #                                    tabPanel("Comparison Filters", "What types of orginizations to you want to compare to?",
#   # 
#   #                                             #search.FormYr
#   #                                             pickerInput(
#   #                                               inputId = "search.FormYr",
#   #                                               label = "IRS Filing Year",
#   #                                               choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
#   #                                                                        "2012" = 2012, "2013" = 2013, "2014" = 2014,
#   #                                                                        "2015" = 2015, "2016" = 2016, "2017" = "2017",
#   #                                                                        "2018" = 2018, "2019" = 2019),
#   #                                               multiple = TRUE,
#   #                                               selected = 2019,
#   #                                               options = list(
#   #                                                 `actions-box` = TRUE,
#   #                                                 `deselect-all-text` = "None",
#   #                                                 `select-all-text` = "Yeah, all!",
#   #                                                 `none-selected-text` = "NA"
#   #                                               )
#   #                                             ),
#   #                                             # search.FormType
#   #                                             checkboxGroupInput("search.FormType", label = "IRS Form",
#   #                                                         choices = list("990" = "990", "990EZ" = "990EZ")),
#   #                                             # search.state
#   #                                             pickerInput(
#   #                                               inputId = "search.State",
#   #                                               label = "States",
#   #                                               choices = state.abb,
#   #                                               multiple = TRUE,
#   #                                               selected = state.abb,
#   #                                               options = list(
#   #                                                 `actions-box` = TRUE,
#   #                                                 `deselect-all-text` = "None",
#   #                                                 `select-all-text` = "Yeah, all!",
#   #                                                 `none-selected-text` = "NA"
#   #                                               )
#   #                                             ),
#   #                                             # search.MajorGroup
#   #                                             pickerInput(
#   #                                               inputId = "search.MajorGroup",
#   #                                               label = "Major Group",
#   #                                               choices = c("Arts, Culture, and Humanities" = 1,
#   #                                                           "Education" = 2,
#   #                                                           "Environment and Animals" = 3,
#   #                                                           "Health" = 4,
#   #                                                           "Human Services" = 5,
#   #                                                           "International, Foreign Affairs" = 6,
#   #                                                           "Public, Societal Benefit" = 7,
#   #                                                           "Religion Related" = 8,
#   #                                                           "Mutual/Membership Benefit" = 9,
#   #                                                           "Unknown/Unclassified"= 10), #showing up as 1:10 and idk why
#   #                                               multiple = TRUE,
#   #                                               selected = 1:10,
#   #                                               options = list(
#   #                                                 `actions-box` = TRUE,
#   #                                                 `deselect-all-text` = "None",
#   #                                                 `select-all-text` = "Yeah, all!",
#   #                                                 `none-selected-text` = "NA"
#   #                                               )
#   #                                             ),
#   #                                             #NEED TO ADD NTEE
#   #                                             #NEED TO ADD NTEE.CC both NTEE and NTEE.CC need to be reactive to inside Major Group
#   #                                             # select.HOSP
#   #                                             # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
#   #                                             radioButtons("search.HOSP", label = "Do you want to compare to Orginizations that are hospitals?",
#   #                                                          choices = list("No, I do not want to compare with hospitals." = 2,
#   #                                                                         "Yes, I want to compare to both hospitals and non-hospitals." = NA,
#   #                                                                         "Yes, but I only want to compare to hospitals." = 1),
#   #                                                          selected = 2),
#   #                                             # select.UNIV
#   #                                             # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
#   #                                             radioButtons("search.UNIV", label = "Do you want to compare to Orginizations that are universities?",
#   #                                                          choices = list("No, I do not want to compare with universities." = 2,
#   #                                                                         "Yes, I want to compare to both universities and non-universities." = NA,
#   #                                                                         "Yes, but I only want to compare to universities." = 1),
#   #                                                          selected = 2),
#   #                                             # search.TotalExpenses
#   #                                             numericRangeInput("search.TotalExpenses", "Range of Total Expenses",
#   #                                                               value = c(-Inf, Inf) #can we get commas on these values?
#   #                                                               ),
#   #                                             # search.TotalEmployees
#   #                                             numericRangeInput("search.TotalEmployees", "Range of Total Employees",
#   #                                                               value = c(0, 1000), #can we get commas on these values?
#   #                                                               min = 0,
#   #                                                               max = 1000),
#   #                                             #submitButton("Update filters"),
#   # 
#   #                                    )
#   #                         ), # end internal tabsetPanel
#   # 
#   # 
#   #           ), #end sidebarPanel
#   # 
#   # 
#   #             mainPanel(
#   #               column(12,
#   #                      submitButton("Update"),
#   #                      ### Internal tabsetPanel for suggested compensation
#   #                      tabsetPanel(type = "tabs",
#   #                                     tabPanel("Instructions",
#   #                                              "Here is where we will put the user instructions for this tool",
#   #                                              "First fill our your orginizations characteristics, make sure to add ntee code link, make sure we add something about full time vs full time equivalent employees",
#   #                                              "Then fill our your search critera.",
#   #                                              "Click Update",
#   #                                              "We will give a suggested pay range.",
#   #                                              "And output the top 10 orgs closest to you",
#   #                                              "We also have more detailed information about the model.",
#   #                                              "Add something about the inflation adjustment.",
#   #                                              "Add information about the Metropolitin/Subruban/Rural coding."),
#   #                                     tabPanel("Suggested Compensation",
#   #                                              textOutput("ceo.suggest"),
#   #                                              "This is where we will put the suggested compensation."),
#   #                                     tabPanel("Similar Orginizations",
#   #                                              DT::dataTableOutput("similar"),
#   #                                              #tableOutput("similar"),
#   #                                              "This is where we will put our list of similar orgs. "),
#   #                                     tabPanel("Model", "This is where we will put more detailed information about the model specifications.")
#   #                      ), # end internal tabsetPanel
#   #                      "Text here will be on every page."
#   #                ) #end column
#   #              ) #end mainPanel
#   # 
#   #         ) #end pageWithSidebar
#   # 
#   #       ), #end CEO Compensation tabPanel
# 
# # ##################################################  
# #   ### Gender Pay gap difference in diference model tabPanel
# # ##################################################
# # 
# #   tabPanel("Gender Transitional Pay Gap Model ",
# #             "This is where we will put the difference in difference model for gender transitional pay gap. "
# #   ), #end tabPanel for Gender transitional pay gap
# # 
# # ##################################################   
# #   ###  Gender Pay Differences tabPanel
# # ##################################################
# 
#   tabPanel("Gender Pay Gap",
#            pageWithSidebar(
#              headerPanel('Gender Pay Gap'),
#              sidebarPanel(width = 4,
#                           h3("Filters"),
#                           "What types of orginizations to you want to look at?",
#                           tabsetPanel(type = "tabs",
#                                       tabPanel("Filter Criteria",
#                                                #search.FormYr
#                                                pickerInput(
#                                                  inputId = "filter.gender.FormYr",
#                                                  label = "IRS Filing Year",
#                                                  choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
#                                                                 "2012" = 2012, "2013" = 2013, "2014" = 2014,
#                                                                 "2015" = 2015, "2016" = 2016, "2017" = 2017,
#                                                                 "2018" = 2018, "2019" = 2019),
#                                                  multiple = TRUE,
#                                                  selected = 2009:2019,
#                                                  options = list(
#                                                    `actions-box` = TRUE,
#                                                    `deselect-all-text` = "None",
#                                                    `select-all-text` = "Yeah, all!",
#                                                    `none-selected-text` = "NA"
#                                                  )
#                                                )%>%
#                                                  helper(type = "inline",
#                                                         title = "Inline Help",
#                                                         content = c("This helpfile is defined entirely in the UI!",
#                                                                     "This is on a new line.",
#                                                                     "This is some <b>HTML</b>."),
#                                                         size = "s"),
# 
#                                                # search.state
#                                                pickerInput(
#                                                  inputId = "filter.gender.State",
#                                                  label = "States",
#                                                  choices = state.abb,
#                                                  multiple = TRUE,
#                                                  selected = state.abb,
#                                                  options = list(
#                                                    `actions-box` = TRUE,
#                                                    `deselect-all-text` = "None",
#                                                    `select-all-text` = "Yeah, all!",
#                                                    `none-selected-text` = "NA"
#                                                  )
#                                                ),
#                                                # search.MajorGroup
#                                                pickerInput(
#                                                  inputId = "filter.gender.MajorGroup",
#                                                  label = "Major Group",
#                                                  choices = c("Arts, Culture, and Humanities" = 1,
#                                                              "Education" = 2,
#                                                              "Environment and Animals" = 3,
#                                                              "Health" = 4,
#                                                              "Human Services" = 5,
#                                                              "International, Foreign Affairs" = 6,
#                                                              "Public, Societal Benefit" = 7,
#                                                              "Religion Related" = 8,
#                                                              "Mutual/Membership Benefit" = 9,
#                                                              "Unknown/Unclassified"= 10), #showing up as 1:10 and idk why
#                                                  multiple = TRUE,
#                                                  selected = 1:10,
#                                                  options = list(
#                                                    `actions-box` = TRUE,
#                                                    `deselect-all-text` = "None",
#                                                    `select-all-text` = "Yeah, all!",
#                                                    `none-selected-text` = "NA"
#                                                  )
#                                                ),
#                                                #NEED TO ADD NTEE
#                                                #NEED TO ADD NTEE.CC both NTEE and NTEE.CC need to be reactive to inside Major Group
#                                                # select.HOSP
#                                                # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
#                                                radioButtons("filter.gender.HOSP", label = "Do you want to compare to Orginizations that are hospitals?",
#                                                             choices = list("No, I do not want to compare with hospitals." = 2,
#                                                                            "Yes, I want to include both hospitals and non-hospitals." = NA,
#                                                                            "Yes, but I only want to include hospitals." = 1),
#                                                             selected = NA),
#                                                # select.UNIV
#                                                # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
#                                                radioButtons("filter.gender.UNIV", label = "Do you want to compare to Orginizations that are universities?",
#                                                             choices = list("No, I do not want to compare with universities." = 2,
#                                                                            "Yes, I want to include both universities and non-universities." = NA,
#                                                                            "Yes, but I only want to include universities." = 1),
#                                                             selected = NA),
#                                                # search.TotalExpenses
#                                                numericRangeInput("filter.gender.TotalExpenses", "Range of Total Expenses",
#                                                                  value = c(-Inf, Inf) #can we get commas on these values?
#                                                ),
#                                                # search.TotalEmployees
#                                                numericRangeInput("filter.gender.TotalEmployees", "Range of Total Employees",
#                                                                  value = c(0, Inf), #can we get commas on these values?
#                                                                  min = 0,
#                                                                  max = 1000),
#                                                #submitButton("Update filters"),
#                                             ),
#                                       tabPanel("Graph Formatting Options",
#                                                #yaxis options
#                                                radioButtons(
#                                                  inputId = "filter.gender.graph.yaxis",
#                                                  label = "Y-axis",
#                                                  choices = list( "Major Group" = "MajorGroup",
#                                                                  "NTEE Code" = "NTEE",
#                                                                  "NTEE- CC Code" = "NTEE.CC")
#                                                ),
#                                                #method option
#                                                radioButtons(
#                                                  inputId = "filter.gender.graphs.method",
#                                                  label = "Statistics",
#                                                  choices = list("Mean" = "Mean",
#                                                                 "Median"= "Median")
#                                                             ))
#                                       )#end tabsetPanel
#              ), #end sidebarPanel
# 
# 
#              mainPanel(
#                column(12,
#                       ### Internal tabsetPanel for Gender pay differences
#                       # end internal tabsetPanel
#                       #submitButton("Update Graph"),
#                       "Disclaimer about how this is not a true gender pay gap. To see more specifics about gender pay gap look at difference in difference model.",
#                       plotlyOutput("gender.pay.graph"),
#                       #textOutput("test"),
#                ) #end column
#              ) #end mainPanel
# 
#            ),
# 
#   ), #end Gender Pay Differences tabPanel
# 
# ##################################################
#   ###  Data tabPanel
# ##################################################
#   tabPanel("Data",
#            "This is where we will put links to our data."
#   ), #end Gender Pay Differences tabPanel
# 
# ##################################################
#   ###  Contact tabPanel
# ##################################################
#   tabPanel("Contact",
#            "This is where we will put our contact information."
#   ) #end Gender Pay Differences tabPanel
# 
# #########
# )#end navbarPage
#   
#   
#   
#   
