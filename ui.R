ui<- bootstrapPage(
  tags$head(tags$style(HTML('.navbar-static-top {background-color: blue;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: blue;}'))),

  
  navbarPage("CEO Pay Appraisal",
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
                      titlePanel("CEO Compensation Appraisal Tool!"),
                      
                      glide(
                        #put button at top of screen 
                        controls_position = "top", 
                        
                        #screens
                        shinyglide::screen( #Directions
                          HTML('Welcome to the CEO Compensation Apprasial Tool. Here is how it works.
                                <br> 
                                PUT DIRECTIONS HERE
                                <br>
                                We will first ask your questions about your organization
                                <br>
                                We will then let you define your own market and find a set of orgnizations to compare you to,
                                <br>
                                Then we will give you orgs to compare yourself to, you can select/deselect.
                                <br>
                                Then we will give you a suggested CEO pay
                                <br>
                                At the end you will be able to download a pdf of this report.
                                SCROLL DOWN')
                        ), #end directions screen
                        shinyglide::screen(# Tell us about your org
                          HTML("Tell us about your organization.
                               <br>
                               We are going to ask you a series of questions about your own organization. ")
                        ), #end tell us screen 
                        
                        
                        ### Org input
                        shinyglide::screen(#org location screen
                          h4("Where is your organization located?"),
                         wellPanel( 
                           selectInput("OrgState", 
                                       label = "What state are you located in?",
                                       choices = sort(c(datasets::state.abb, "PR", "DC"))), 
                           # #org.loc
                           selectInput("OrgLoc",
                                       "What type of city is your organization located in?",
                                       choices = c("Rural", "Metropolitan")),

                         )#end WellPannel
                        ), #end #org location screen
                        
                        shinyglide::screen( #org size screen
                          h4("How large is your organization?"),
                          wellPanel(
                            # org.TotalExpense
                            shinyWidgets::currencyInput(
                              inputId = "OrgTotalExpense", 
                              label =  "What are your organizations annual expenses?",
                              value = 1000000,
                              format = "dollar"
                              ),
                            # org.TotalEmployee
                            numericInput(
                              inputId = "OrgTotalEmployee", 
                              label = "How many full time equivalent employees does your organization have?",
                              value = 25,
                              min = 0)
                          )#end Well Pannel 
                        ), #end org size screen
                        
                        shinyglide::screen( #org type 
                          h4("What type of organization do you have? "),
                          wellPanel(
                            selectInput(
                              "OrgMajorGroup",
                              label = htmltools::HTML("What broad category best describes the work your organization does?"),
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
                            shinyhelper::helper(
                              type = "markdown",
                              content = "MajorGroup"),
                            
                            #specify NTEE 
                            #groups 1, 2, 3, 6, 8, 9, 10 do not have further NTEE Divisions
                            conditionalPanel( #if we are in group 3
                              "input.OrgMajorGroup == 3 ",
                              selectInput(
                                "OrgNTEE3",
                                label = htmltools::HTML("What major group best describes the work your organization does?"),
                                choices = c("Environment" = "C", 
                                            "Animal Related" = "D"),
                                selected = NA) %>%
                              shinyhelper::helper(
                                type = "markdown",
                                content = "NTEE"),
                            ), #end Conditional Panel
                            
                            
                            conditionalPanel( #if we are in group 4
                              "input.OrgMajorGroup == 4 ",
                              selectInput(
                                "OrgNTEE4",
                                label = htmltools::HTML("What major group best describes the work your organization does?"),
                                choices = c("Health Care" = "E",
                                            "Mental Health & Crisis Intervention" = "F",
                                            "Voluntary Health Associations & Medical Disciplines" = "G",
                                            "Medical Research" = "H"),
                                selected = NA) %>%
                              shinyhelper::helper(
                                type = "markdown",
                                content = "NTEE"),
                            ), #end Conditional Panel

                            conditionalPanel( #if we are in group 5
                              "input.OrgMajorGroup == 5 ",
                              selectInput(
                                "OrgNTEE5",
                                label = htmltools::HTML("What major group best describes the work your organization does?"),
                                choices = c("Crime & Legal-Related" = "I",
                                            "Employment" = "J",
                                            "Food, Agriculture & Nutrition" = "K",
                                            "Housing & Shelter" = "L", 
                                            "Public Safety, Disaster Preparedness & Relief" = "M", 
                                            "Recreation & Sports" = "N", 
                                            "Youth Development" = "O",
                                            "Human Services" = "P"),
                                selected = NA) %>%
                              shinyhelper::helper(type = "markdown",
                                                    content = "NTEE"),
                            ), #end Conditional Panel
                            conditionalPanel( #if we are in group 7
                              "input.OrgMajorGroup == 7 ",
                              selectInput(
                                "OrgNTEE7",
                                label = htmltools::HTML("What major group best describes the work your organization does?"),
                                choices = c("Civil Rights, Social Action & Advocacy" = "R",
                                            "Community Improvement & Capacity Building" = "S",
                                            "Philanthropy, Voluntarism & Grantmaking Foundations" = "T",
                                            "Science & Technology" = "U", 
                                            "Social Science" = "V", 
                                            "Public & Societal Benefit" = "W"),
                                selected = NA) %>%
                              shinyhelper::helper(
                                type = "markdown",
                                content = "NTEE"),
                            ), #end Conditional Panel
                            
                          )#end wellPannel
                        ), #end org type screen 
                        
                        shinyglide::screen( #org other questions
                          h4("We have a few more questions about your organization."), 
                          wellPanel(
                            #org ntee-cc
                            pickerInput(
                              "OrgNTEECC",
                              label = htmltools::HTML("Does your orginzation fit any of the following speacilty descriptions?"),
                              choices = c("Alliance/Advocacy Organization" = 1,
                                          "Management and Technical Assistance" = 2,
                                          "Professional Society/Association" = 3,
                                          "Research Institute and/or Public Policy Analysis" = 5,
                                          "Monetary Support - Single Organization" = 11,
                                          "Monetary Support - Multiple Organizations" = 12,
                                          "Nonmonetary Support Not Elsewhere Classified (N.E.C.)" = 19,
                                          "I am a regular non-profit. None of these speacilties describe my organization." = NA),
                              multiple = TRUE),
                            
                            # # org.Hosp
                            "Is your organization a Hospital?",
                            switchInput(
                              "OrgHOSP", 
                              label = NA,
                              value = FALSE,
                              onLabel = "Yes",
                              offLabel = "No"),
                          
                            # org.Univ
                            "Is your organization a University?",
                            switchInput(
                              "OrgUNIV", 
                              label = NA,
                              value = FALSE,
                              onLabel = "Yes",
                              offLabel = "No")
                          
                          ) #end WellPanel
                          ), # end other questions screen
                        
                        ### Testing screen - to be deleted 
                        shinyglide::screen(
                          "this is a test screen for making sure org inputed correctly.",
                          textOutput("test1")
                        ),

                        ### Comparison set 
                        shinyglide::screen( # comparison directions 
                          HTML("We will next ask you a series of questions about orginziations you want to compare yourself to
                               <br>
                               Add details about how this allows your to define your own market,
                               <br>
                               Define hard and soft searches ")
                          ), # end comparison directions 
                        
                        shinyglide::screen( #search locations
                          h4("How do you want to consider location?"),
                          wellPanel(
                            fluidRow(
                              column(12,
                              pickerInput(
                                inputId = "LocType",
                                label = "I want to search location type by... ",
                                choices = c("State" = 1, 
                                            "City Type" = 2),
                                selected = NA
                              )
                              )#end column
                            ),  #end fluid row
                            
                            fluidRow(
                              conditionalPanel(
                                condition = "input.LocType == 1 ",
                                column(6,
                                    #search.state
                                    pickerInput(
                                      inputId = "SearchState",
                                      label = "I want to include organizations who are located in the following states:",
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
                                  switchInput("HardState",
                                              label = NA,
                                              value = FALSE,
                                              onLabel = "Hard",
                                              offLabel = "Soft")
                                )#end column
                              ), #end conditional panel 
                              
                              conditionalPanel(
                                condition = "input.LocType == 2 ",
                                column(6,
                                  #search.loc
                                  pickerInput(
                                    inputId = "SearchLoc",
                                    label = "I want to include organizations who are located in the following types of cities:",
                                    choices = list("Rural" = "Rural",
                                                   "Metropolitan" = "Metropolitan"),
                                    multiple = TRUE,
                                    selected = c("Rural", "Metropolitan"),
                                  )
                                ),#end column
                                column(6,
                                       "Do you want to hard or soft matching on city types?",
                                       switchInput("HardLoc",
                                                   label = NA,
                                                   value = FALSE,
                                                   onLabel = "Hard",
                                                   offLabel = "Soft")
                                )#end column
                              ), #end conditional panel 
                            ) #end fluid row 
                          )#end Well Panel  
                        ), #end search locations screen
                        
                        
                        shinyglide::screen( # search types
                          h4("How to do you want to filter by organization type?"),
                          wellPanel(
                            pickerInput(
                              inputId = "SearchType",
                              "Do you want to filter by ... ",
                              choices = c("Broad Category (10 options)" = 1,
                                          "Major Group (26 options)" = 2, 
                                          "Common Code ( NEED SOMETHING HERE)" = "3"),
                              selected = 1
                            ), 
                            
                            "ADD HELPER FUNCTION ABOUT BROAD CATEGORY, MAJOR GROUP, AND COMMON CODE",
                            
                            # if broad category is selected 
                            conditionalPanel(
                              condition = "input.SearchType == 1",
                              pickerInput(
                                inputId = "SearchMajorGroup",
                                label = "I want to include organizations in the following broad categories:",
                                choices = c("Arts, Culture, and Humanities" = 1,
                                            "Education" = 2,
                                            "Environment and Animals" = 3,
                                            "Health" = 4,
                                            "Human Services" = 5,
                                            "International, Foreign Affairs" = 6,
                                            "Public, Societal Benefit" = 7,
                                            "Religion Related" = 8,
                                            "Mutual/Membership Benefit" = 9,
                                            "Unknown/Unclassified"= 10), 
                                multiple = TRUE,
                                selected = 1:10,
                                options = list(
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "None",
                                  `select-all-text` = "Select All",
                                  `none-selected-text` = "NA")
                              ), #end picker input
                              "Do you want to hard or soft matching on broad category?",
                              switchInput(
                                "HardMajorGroup",
                                label = NA,
                                value = FALSE,
                                onLabel = "Hard",
                                offLabel = "Soft")
                            ),  #end conditional panel 
                            
                            #if major group is selected 
                            conditionalPanel(
                              condition = "input.SearchType == 2", 
                              pickerInput(
                                inputId = "SearchNTEE",
                                label = "I want to include organizations in the following major groups:",
                                choices = c("Animals" = "D",
                                            "Arts, Culture, and Humanities" = "A",
                                            "Civil Rights, Social Action & Advocacy" = "R",
                                            "Community Improvement & Capacity Building" = "S", 
                                            "Crime & Legal-Related" = "I",
                                            "Education" = "B",
                                            "Environment" = "C", 
                                            "Employment" = "J", 
                                            "Food, Agriculture & Nutrition" = "K", 
                                            "Health Care" = "E", 
                                            "Housing & Shelter" = "L",
                                            "Human Services" = "P",
                                            "International, Foreign Affairs & National Security" = "Q", 
                                            "Medical Research" = "H",
                                            "Mental Health & Crisis Intervention" = "F",
                                            "Mutual & Membership Benefit" = "Y", 
                                            "Philanthropy, Voluntarism & Grantmaking Foundations" = "T",
                                            "Public Safety, Disaster Preparedness & Relief" = "M",
                                            "Public & Societal Benefit" = "W",
                                            "Voluntary Health Associations & Medical Disciplines" = "G",
                                            "Recreation & Sports" = "N", 
                                            "Religion Related" = "X", 
                                            "Science & Technology" = "U", 
                                            "Social Science" = "V",
                                            "Youth Development" = "O",
                                            "Unknown, Unclassified" = "Z"),
                                multiple = TRUE,
                                selected = LETTERS,
                                options = list(
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "None",
                                  `select-all-text` = "Select All",
                                  `none-selected-text` = "NA")
                              ), #end picker input
                              "Do you want to hard or soft matching on major group?",
                              switchInput(
                                "HardNTEE",
                                label = NA,
                                value = FALSE,
                                onLabel = "Hard",
                                offLabel = "Soft")
                            ), #end conditional panel 
                            
                            #if common code is selected 
                            conditionalPanel(
                              condition = "input.SearchType == 3",
                              pickerInput(
                                "SearchNTEECC",
                                label = htmltools::HTML("Do you want to include orgnizations that fit any of the following speacilty descriptions?"),
                                choices = c("Alliance/Advocacy Organization" = 1,
                                            "Management and Technical Assistance" = 2,
                                            "Professional Society/Association" = 3,
                                            "Research Institute and/or Public Policy Analysis" = 5,
                                            "Monetary Support - Single Organization" = 11,
                                            "Monetary Support - Multiple Organizations" = 12,
                                            "Nonmonetary Support Not Elsewhere Classified (N.E.C.)" = 19,
                                            "I only want to include regular non-profits. I do not want to include any of these types of organizations." = NA),
                                multiple = TRUE),
                              "Do you want to hard or soft matching on speacilty description?",
                              switchInput(
                                "HardNTEECC",
                                label = NA,
                                value = FALSE,
                                onLabel = "Hard",
                                offLabel = "Soft"),
                             
                            "Do you want to further search by major group? (We recommend that you do not. 
                            For most searches this is not needed.
                            If you do choose to do this, it is likely your results will be very limited and the filterted data set will not have many results)",
                            switchInput(
                              inputId = "FurtherNTEE",
                              label = NA,
                              value = FALSE,
                              onLabel = "Yes", 
                              offLabel = "No"),
                            
                            #if they want to further filter by NTEE
                            conditionalPanel(
                              condition = "input.FurtherNTEE == true",
                              pickerInput(
                                inputId = "SearchNTEE2",
                                label = "I want to include organizations in the following major groups:",
                                choices = c("Animals" = "D",
                                            "Arts, Culture, and Humanities" = "A",
                                            "Civil Rights, Social Action & Advocacy" = "R",
                                            "Community Improvement & Capacity Building" = "S", 
                                            "Crime & Legal-Related" = "I",
                                            "Education" = "B",
                                            "Environment" = "C", 
                                            "Employment" = "J", 
                                            "Food, Agriculture & Nutrition" = "K", 
                                            "Health Care" = "E", 
                                            "Housing & Shelter" = "L",
                                            "Human Services" = "P",
                                            "International, Foreign Affairs & National Security" = "Q", 
                                            "Medical Research" = "H",
                                            "Mental Health & Crisis Intervention" = "F",
                                            "Mutual & Membership Benefit" = "Y", 
                                            "Philanthropy, Voluntarism & Grantmaking Foundations" = "T",
                                            "Public Safety, Disaster Preparedness & Relief" = "M",
                                            "Public & Societal Benefit" = "W",
                                            "Voluntary Health Associations & Medical Disciplines" = "G",
                                            "Recreation & Sports" = "N", 
                                            "Religion Related" = "X", 
                                            "Science & Technology" = "U", 
                                            "Social Science" = "V",
                                            "Youth Development" = "O",
                                            "Unknown, Unclassified" = "Z"),
                                multiple = TRUE,
                                selected = LETTERS,
                                options = list(
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "None",
                                  `select-all-text` = "Select All",
                                  `none-selected-text` = "NA")
                            ) #end picker input 
                            ) #end conditional Panel
                            
                            ) #end conditional Panel
                          )#end WellPannel
                        ), #end search types 
                        
                        
                        
                        
                        shinyglide::screen( #search.hosp screen
                          wellPanel(
                            pickerInput(
                              inputId = "SearchHosp",
                              label = "Do you want to include hospitals in your search?", 
                              choices = list("No, I do not want to compare with hospitals." = 1,
                                             "Yes, I want to compare to both hospitals and non-hospitals." = 2,
                                             "Yes, I exclusively want to compare to hospitals." = 3),
                              selected = 2),
                            
                            pickerInput(
                              inputId = "SearchUniv",
                              label = "Do you want to include universities in your search?", 
                              choices = list("No, I do not want to compare with universities." = 1,
                                             "Yes, I want to compare to both universities and non-universities." = 2,
                                             "Yes, I exclusively want to compare to universities." = 3),
                              selected = 2)
                          )#end WellPanel
                        ), #end search.hosp screen
                        
                        
                        shinyglide::screen( #search.total expenses
                          wellPanel(
                            "Do you want to further filter your comparison set by Total Expenses?",
                            switchInput(
                              "TotalExpenseDecide",
                              label = NA,
                              value = FALSE,
                              onLabel = "Yes",
                              offLabel = "No"),
                            
                            conditionalPanel(
                              condition = "input.TotalExpenseDecide == true", 
                              numericRangeInput(
                                "SearchTotalExpenses", 
                                "Range of total expenses I want to include:",
                                 value = c(0, Inf) #can we get commas on these values?
                                 ),
                            ) #end conditional panel
                          )#end well panel
                        ), #end total expenses screen
                        
                        shinyglide::screen(#search.TotalEmployees
                          wellPanel(
                            "Do you want to further filter your comparison set by Total Employees?",
                            switchInput(
                              "TotalEmployeeDecide",
                              label = NA,
                              value = FALSE,
                              onLabel = "Yes",
                              offLabel = "No"),
                            
                            conditionalPanel(
                              condition = "input.TotalEmployeeDecide == true", 
                              numericRangeInput(
                                "SearchTotalEmployee", 
                                "Range of total employees I want to include:",
                                value = c(0, Inf) #can we get commas on these values?
                              ),
                            ) #end conditional panel
                          )#end well panel
                        ), #end total expenses screen
                        
                        
                        ### Testing screen - to be deleted 
                        shinyglide::screen(
                          "this is a test screen for making sure search inputed correctly.",
                          textOutput("test2")
                        )
                        
                        
                      )#end glide
                      
             ), #end tabPanel
             # ##################################################
             #   ###  Gender Pay Differences tabPanel
             # ##################################################

               tabPanel("Gender Pay Gap",
                        pageWithSidebar(
                          headerPanel('Gender Pay Gap'),
                          sidebarPanel(width = 4,
                                       h3("Filters"),
                                       "What types of organizations to you want to look at?",
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Filter Criteria",
                                                            #search.FormYr
                                                            pickerInput(
                                                              inputId = "filter.gender.FormYr",
                                                              label = "IRS Filing Year",
                                                              choices = list("2009" = 2009, "2010" = 2010, "2011" = 2011,
                                                                             "2012" = 2012, "2013" = 2013, "2014" = 2014,
                                                                             "2015" = 2015, "2016" = 2016, "2017" = 2017,
                                                                             "2018" = 2018, "2019" = 2019),
                                                              multiple = TRUE,
                                                              selected = 2009:2019,
                                                              options = list(
                                                                `actions-box` = TRUE,
                                                                `deselect-all-text` = "None",
                                                                `select-all-text` = "Select All",
                                                                `none-selected-text` = "NA"
                                                              )
                                                            )%>%
                                                              helper(type = "inline",
                                                                     title = "Inline Help",
                                                                     content = c("This helpfile is defined entirely in the UI!",
                                                                                 "This is on a new line.",
                                                                                 "This is some <b>HTML</b>."),
                                                                     size = "s"),

                                                            # search.state
                                                            pickerInput(
                                                              inputId = "filter.gender.State",
                                                              label = "States",
                                                              choices = state.abb,
                                                              multiple = TRUE,
                                                              selected = state.abb,
                                                              options = list(
                                                                `actions-box` = TRUE,
                                                                `deselect-all-text` = "None",
                                                                `select-all-text` = "Select All",
                                                                `none-selected-text` = "NA"
                                                              )
                                                            ),
                                                            # search.MajorGroup
                                                            pickerInput(
                                                              inputId = "filter.gender.MajorGroup",
                                                              label = "Major Group",
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
                                                                `select-all-text` = "Select All",
                                                                `none-selected-text` = "NA"
                                                              )
                                                            ),
                                                            #NEED TO ADD NTEE
                                                            #NEED TO ADD NTEE.CC both NTEE and NTEE.CC need to be reactive to inside Major Group
                                                            # select.HOSP
                                                            # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                                                            radioButtons("filter.gender.HOSP", label = "Do you want to compare to organizations that are hospitals?",
                                                                         choices = list("No, I do not want to compare with hospitals." = 2,
                                                                                        "Yes, I want to include both hospitals and non-hospitals." = NA,
                                                                                        "Yes, but I only want to include hospitals." = 1),
                                                                         selected = NA),
                                                            # select.UNIV
                                                            # WILL NEED TO COME BACK AND CHANGE THE OPTION VALUES
                                                            radioButtons("filter.gender.UNIV", label = "Do you want to compare to organizations that are universities?",
                                                                         choices = list("No, I do not want to compare with universities." = 2,
                                                                                        "Yes, I want to include both universities and non-universities." = NA,
                                                                                        "Yes, but I only want to include universities." = 1),
                                                                         selected = NA),
                                                            # search.TotalExpenses
                                                            numericRangeInput("filter.gender.TotalExpenses", "Range of Total Expenses",
                                                                              value = c(-Inf, Inf) #can we get commas on these values?
                                                            ),
                                                            # search.TotalEmployees
                                                            numericRangeInput("filter.gender.TotalEmployees", "Range of Total Employees",
                                                                              value = c(0, Inf), #can we get commas on these values?
                                                                              min = 0,
                                                                              max = 1000),
                                                            #submitButton("Update filters"),
                                                         ),
                                                   tabPanel("Graph Formatting Options",
                                                            #yaxis options
                                                            radioButtons(
                                                              inputId = "filter.gender.graph.yaxis",
                                                              label = "Y-axis",
                                                              choices = list( "Major Group" = "MajorGroup",
                                                                              "NTEE Code" = "NTEE",
                                                                              "NTEE- CC Code" = "NTEE.CC")
                                                            ),
                                                            #method option
                                                            radioButtons(
                                                              inputId = "filter.gender.graphs.method",
                                                              label = "Statistics",
                                                              choices = list("Mean" = "Mean",
                                                                             "Median"= "Median")
                                                                         ))
                                                   )#end tabsetPanel
                          ), #end sidebarPanel


                          mainPanel(
                            column(12,
                                   ### Internal tabsetPanel for Gender pay differences
                                   # end internal tabsetPanel
                                   #submitButton("Update Graph"),
                                   "Disclaimer about how this is not a true gender pay gap. To see more specifics about gender pay gap look at difference in difference model.",
                                   plotlyOutput("gender.pay.graph"),
                                   #textOutput("test"),
                            ) #end column
                          ) #end mainPanel

                        ),

               ), #end Gender Pay Differences tabPanel

             
             
  )#end NavbarPage
)#End bookstrap page