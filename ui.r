library(shiny)
library(shinydashboard)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotrix)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
  dashboardHeader(title = "Beneficios HUCA", dropdownMenuOutput("messageMenu")),
  
#########################SIDEBAR############################################
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuSubItem("Tiempo en urgencias", tabName = "urg", icon = icon("bar-chart")),
      menuSubItem("Caidas", tabName = "caidas", icon = icon("bar-chart")),
      menuSubItem("UPP", tabName = "upp", icon = icon("bar-chart")),
      menuSubItem("Errores de medicacion", tabName = "mederr", icon = icon("bar-chart")),
      menuSubItem("Paso de medicacion", tabName = "med", icon = icon("bar-chart")),  
      menuSubItem("Pruebas", tabName = "p", icon = icon("bar-chart")),  
      fluidPage(
        hr(
        ),
        dateRangeInput('dateRangeALL',
                       label = 'Fechas de solicitud de datos',
                       start = "07/01/2014", end = "07/01/2015",
                       #min = Sys.Date() - 10, max = Sys.Date() + 10,
                       separator = " a ", format = "dd/mm/yy",
                       startview = 'year', language = 'es', weekstart = 1
        ),
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        box(width = 10,
          tags$img(src="cernerlogoT.png", width = "120px", height = "60px")
        #tags$img(src="cernerlogoT.png"
      ))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
####################################DASHBOARD####################################3
      tabItem(tabName = "dashboard",
              fluidRow(
                column(1,offset = 5, h2("Dashboard")),
                column(1,offset = 3,tags$img(src="hucalogoT.png"))
              ),
              fluidRow(
                # A static valueBox
                #valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
                fluidRow(
                  column(6,offset = 1, h3("Tiempo Medio en urgencias")),
                  column(1.5,offset = 2,h3("Caidas"))
                ),
                # Dynamic valueBoxes
                
                valueBoxOutput("progressBoxED",width = 3),
                
                valueBoxOutput("approvalBoxED",width = 3),
                # Dynamic valueBoxes
                valueBoxOutput("progressBoxFalls",width = 3),
                
                valueBoxOutput("approvalBoxFalls",width = 3)
              ),
              #fluidRow(
                # Clicking this will increment the progress amount
               # box(width = 4, actionButton("count", "Increment progress"))
              #),
              fluidRow(
                column(12,
                fluidRow(
                      box(plotOutput("distPlotUrgDash")),
                      box(plotOutput("distPlotFallsDash"))
                )
                ),
                fluidRow(
                  # A static valueBox
                  #valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
                  fluidRow(
                    column(6,offset = 1, h3("Ulceras por presión")),
                    column(1.5,offset = 2,h3("Errores de medicación evitables"))
                  ),
                  # Dynamic valueBoxes
                  
                  valueBoxOutput("progressBoxUPP",width = 3),
                  
                  valueBoxOutput("approvalBoxUPP",width = 3),
                  # Dynamic valueBoxes
                  valueBoxOutput("progressBoxMEDERR",width = 3),
                  
                  valueBoxOutput("approvalBoxMEDERR",width = 3)
                ),
                fluidRow(
                  column(12,
                         fluidRow(
                           box(plotOutput("distPlotUPPDash")),
                           box(plotOutput("distPlotMEDERRDash"))
                         )
                  )
                ),
                fluidRow(
                  # A static valueBox
                  #valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
                  fluidRow(
                    column(6,offset = 1, h3("Paso de medicación de IV a VO")),
                    column(1.5,offset = 2,h3(""))
                  ),
                  # Dynamic valueBoxes
                  
                  valueBoxOutput("progressBoxMEDIV",width = 3),
                  valueBoxOutput("approvalBoxMEDIV",width = 3)
                ),
                fluidRow(
                  column(12,
                         fluidRow(
                           #box(plotOutput("distPlotUPPDash"))
                           box()
                         )
                  )
                )
              ),              
              fluidRow(
                column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
              )
              
              
      ),
#####################CAIDAS####################################################      
      # Second tab content
      tabItem(tabName = "caidas",
              fluidRow(
                column(1,offset = 5, h2("Caídas")),
                column(1,offset = 3,tags$img(src="hucalogoT.png"))
              ),
              tabsetPanel(
                tabPanel("Caídas por mes",
                         fluidRow(
                           box(plotOutput("distHistFalls")),
                           box(plotOutput("distPlotFalls"))
                         )),
                tabPanel("Porcentajes de caídas",
                         fluidRow(
                           box(plotOutput("distPlotFallsPW")),
                           box(plotOutput("distPlotFallsLO",height = 210)),
                           box(plotOutput("distPlotFallsUB",height = 210))
                         )
                         ),
                tabPanel("Caídas por horas",
                         fluidRow(
                           #box(plotOutput("distRadarFalls"))
                         )
                         )
              ), 
              fluidRow(
                column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
              )
      ),
########################UPP#############################################
      tabItem(tabName = "upp",
              fluidRow(
                column(4,offset = 4, h2("Úlceras por presión")),
                column(1,offset = 1,tags$img(src="hucalogoT.png"))
              ),
              fluidRow(
                box(plotOutput("distPlotUPP")),
                box(plotOutput("distPlotUPP2"))
                #column(5,offset = 1,box(plotOutput("distPlotUPP"))),
                #column(5,offset = 4,box(plotOutput("distPlotUPP2"))), 
              ),
              fluidRow(
                column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
              )
      ),
############################URG########################################
      tabItem(tabName = "urg",
              fluidRow(
                column(4,offset = 4, h2("Tiempo Medio de Urgencias")),
                column(1,offset = 1,tags$img(src="hucalogoT.png"))
              ),
              fluidRow(
                column(6,
                  box(
                    title = "Maximo de horas en Urgencias",
                    sliderInput("n", "Horas:", min=1, max = 120, value = 48)
                    #width = 3
                  )
                )
              ),
              
              

              fluidRow(
                box(plotOutput("distPlotUrg")),
                box(plotOutput("distHistUrg")),
                box(plotOutput("barplotUrg"))
                
              ),   
              fluidRow(
                
              ),  
              fluidRow(
                column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
              )
      ),
###################MEDIV##########################################################
      tabItem(tabName = "med",
              fluidRow(
                column(4,offset = 2, h2("Paso de medicacion IV a PO")),
                column(1,offset = 3,tags$img(src="hucalogoT.png"))
              ), 
              fluidRow(
                tabsetPanel(
                  tabPanel("Caídas",
                           # Application title
                           titlePanel("CAIDAS HUCA"),
                           box(
                             # Sidebar with a slider input for the number of bins
                             sliderInput(inputId = "num",
                                         label = "Choose a number",
                                         min = 1,
                                         max = 100,
                                         value = 30
                             )),
                           
                           # Show a plot of the generated distribution
                           box(mainPanel(
                             #plotOutput("distPlot")
                           ))
                  ),
                  tabPanel("UPP","Contents"),
                  tabPanel("Tiempo en Urgencias","Contents"),
                  tabPanel("Paso de medicación","Contents"),
                  tabPanel("Errores de medicación evitables","Contents")
                )),
              fluidRow(
                column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
              )
      ),
#####################MEDERR#######################################################
      tabItem(tabName = "mederr",
              fluidRow(
                column(4,offset = 2, h2("Errores de medicación evitables")),
                column(1,offset = 3,tags$img(src="hucalogoT.png"))
              ), 

              
              fluidRow(
                box(plotOutput("distPlotMEDERR")),
                box(plotOutput("distPlotMEDERRPOS"))
              ),
              fluidRow(
                column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
              )
      ),
tabItem(tabName = "p",
        fluidRow(
          column(4,offset = 2, h2("Errores de medicación evitables")),
          column(1,offset = 3,tags$img(src="hucalogoT.png"))
        ), 
        
        
        fluidRow(
          box(plotOutput("distRadarFalls")),
          box(plotOutput("ggplotUrg"))
          
        ),
        fluidRow(
          column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
        )
)
    )
  )
))
