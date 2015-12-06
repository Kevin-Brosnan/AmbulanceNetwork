##### User Interface for Ambulance Service Project #####

library(shiny)
library(devtools)
library(RgoogleMaps)
library(TeachingDemos)
library(lattice)

shinyUI(navbarPage("Ambulance Network Analysis", id="navbar", theme="css/united.css",
                   
##### Home Tab - Completed #####

         tabPanel("Home", tags$style("body {background-color: #ADD8E6;}"),
                  conditionalPanel(condition = "input.navbar=='Home'", includeHTML("home.html"))),

#### Overview SubTab - Completed ####

         tabPanel("Overview", conditionalPanel(condition = "input.navbar=='Overview'",includeHTML("overview.html"))),

#### Data SubTab - Completed ####

         tabPanel("Data", conditionalPanel(condition = "input.navbar=='Data'",
                        tabsetPanel(
                           tabPanel("Population Data", dataTableOutput("dataPop"), 
                                    HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                           tabPanel("Hospital Data", dataTableOutput("dataHosp"),
                                    HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                           tabPanel("Ambulance Stations Data", dataTableOutput("dataStations"),
                                    HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                           tabPanel("Download Data", conditionalPanel(condition = "input.dataTabs=='Download Data'",
                               sidebarPanel(
                                  radioButtons("dataset", selected = "Population Data", "Choose a dataset:", 
                                           choices = c("Population Data", "Hospital Data", "Ambulance Stations Data")),
                                  downloadButton('downloadData', 'Download')),
                               mainPanel(includeHTML("data.html"))),
                               HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                           id="dataTabs"))),

#### Analysis SubTab - Completed ####

         tabPanel("Analysis", conditionalPanel(condition = "input.navbar=='Analysis'",
                         tabsetPanel(
                            tabPanel("Population Analysis",
                                     includeHTML("analysisPop.html"),
                                     conditionalPanel(condition = "input.analysisTabs == 'Population Analysis'",
                                          sidebarPanel(
                                             selectInput("barvar", label = "Barplot Variable", selected = "Population",
                                                   choices = c("Counties", "Gender", "Age Groups")),
                                             checkboxInput("stackedbar", label = "Stacked Barplot", value = FALSE),
                                             selectInput("stackedbarvar", label = "Stacked Barplot Variable",
                                                                        selected = NULL, choices = NULL)),
                                          mainPanel(plotOutput(outputId = "popbar", width = 900, height = 500)), HTML('<br> <br>')),
                                     HTML('<footer>
                                         <br>
                                         <br>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                            tabPanel("Hospital Analysis",
                                     includeHTML("analysisHosp.html"),
                                     fluidRow(align="center",plotOutput(outputId = "hospmap", width = 900, height = 1000)),
                                     HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                            tabPanel("Ambulance Stations Analysis",
                                     includeHTML("analysisStations.html"),
                                     fluidRow(align="center",plotOutput(outputId = "stationsmap", width = 900, height = 1000)),
                                     HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                            tabPanel("Analysis of the Medical Emergency Network of Ireland",
                                     withMathJax(),
                                     includeHTML("fullAnalysis.html"),
                                     div("$$\\Delta \\text{Latitude} = \\text{Latitude}_{1} - \\text{Latitude}_{2}$$"),
                                     div("$$\\Delta \\text{Longitude} = \\text{Longitude}_{1} - \\text{Longitude}_{2}$$"),
                                     div("$$a = \\sin^{2}\\left(\\frac{\\Delta \\text{Latitude}}{2}\\right) + \\cos(\\text{Latitude}_{1})\\times\\cos(\\text{Latitude}_{2})\\times\\sin^{2}\\left(\\frac{\\Delta \\text{Longitude}}{2}\\right)$$"),
                                     div("$$c = 2 \\times atan2(\\sqrt{a}, \\sqrt{1-a})$$"),
                                     div("$$atan2(x,y) = \\begin{cases} arctan(\\frac{y}{x}) & x>0 \\\\ arctan(\\frac{y}{x}+\\pi) & y \\leq 0, \\; x<0 \\\\ arctan(\\frac{y}{x}-\\pi) & y < 0, \\; x<0 \\\\ +\\frac{\\pi}{2} & y>0, \\; x=0 \\\\ -\\frac{\\pi}{2} & y>0, \\;x=0 \\\\ \\text{undefined} & y=0, \\; x=0 \\end{cases}$$"),
                                     div("$$\\text{distance} = R \\times c$$"),
                                     includeHTML("fullAnalysis1.html"),
                                     fluidRow(align="center",plotOutput(outputId = "fullAnalysisMap", width = 900, height = 1000)),
                                     includeHTML("fullAnalysis2.html"),
                                     dataTableOutput(outputId = "distanceMatrix"),
                                     includeHTML("fullAnalysis3.html"),
                                     plotOutput(outputId = "GroupedBar"),
                                     includeHTML("fullAnalysis4.html"),
                                     HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')),
                            tabPanel("Future Work",
                                     includeHTML("futureWork.html")),
                            id="analysisTabs"))),
     

#### Documentation SubTab - Completed ####
         
         tabPanel("Docs & Sources", conditionalPanel(condition = "input.navbar=='Docs & Sources'",includeHTML("docsSources.html"))),

##### About Us Tab - Completed #####

         tabPanel("About Us", conditionalPanel(condition = "input.navbar=='About Us'",includeHTML("about.html"))),

##### Contact Us Tab - Completed #####

         tabPanel("Contact Us", conditionalPanel(condition = "input.navbar=='Contact Us'",
                                  sidebarPanel(
                                     h3("Contact Form"),
                                     textInput(inputId="name", label="Your name", value=""),
                                     tags$style(type='text/css', "#name{width:300px;}"),
                                     textInput(inputId="email", label="Your email", value=""),
                                     tags$style(type='text/css', "#email{width:300px;}"),
                                     HTML('<tr>
                                             <td valign="top">
                                                <label for="message">Your message</label>
                                             </td>
                                             <td>
                                             <textarea name="message" maxlength="1000" cols="400" rows="6"></textarea>
                                             </td>
                                          </tr>'),
                                     actionButton(inputId="submit", label="Send Comments"),
                                     uiOutput("CommentAccepted")
                                  ),
                                  
                                  mainPanel(
                                     includeHTML("contact.html")), 
                                  HTML('<footer>
                                         <br>
                                         <br>
                                         <div style="text-align: center;"><img src="images/footer.jpg" align="middle"></div>
                                         <br>
                                         </footer>')))


##### End of Script #####
   )
)