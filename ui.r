library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotrix)
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Beneficios HUCA",  dropdownMenuOutput("notifMenu"),dropdownMenuOutput("taskMenu")),
    
    #########################SIDEBAR############################################
    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Tiempo en urgencias", tabName = "urg", icon = icon("bar-chart")),
        menuItem("Caídas", tabName = "caidas", icon = icon("bar-chart")),
        menuItem("UPP", tabName = "upp", icon = icon("bar-chart")),
        menuItem("Errores de medicación", tabName = "mederr", icon = icon("bar-chart")),
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
          box(tags$img(src="legend2.png"),
              width = 12,
              title = "Leyenda"),
          box(width = 10,
              tags$img(src="cernerlogoT.png", width = "120px", height = "60px")
              #tags$img(src="cernerlogoT.png"
          ))
      )
    ),
    ## Body content
    dashboardBody(
      tabItems(
        ####################################DASHBOARD####################################3
        tabItem(tabName = "dashboard",
                fluidRow(
                  column(1,offset = 0,tags$img(src="hosplogoT.png")),
                  column(1,offset = 4, h2("Dashboard"))
                ),
                fluidRow(
                  fluidRow(
                    column(6,offset = 1, h3("Tiempo Medio en urgencias")),
                    column(1.5,offset = 3,h3("Caídas"))
                  ),
                  
                  valueBoxOutput("progressBoxED",width = 3),
                  valueBoxOutput("approvalBoxED",width = 3),
                  valueBoxOutput("progressBoxFalls",width = 3),
                  valueBoxOutput("approvalBoxFalls",width = 3)
                ),
                fluidRow(
                  column(12,
                         fluidRow(
                           box(plotOutput("distPlotUrgDash"),title = "Tiempo medio en urgencias",background = "navy", solidHeader = TRUE),
                           box(plotOutput("distPlotFallsDash"),title = "Caídas",background = "navy", solidHeader = TRUE)
                         )
                  )
                ),
                fluidRow(
                  fluidRow(
                    column(6,offset = 1, h3("Ulceras por presión")),
                    column(1.5,offset = 2,h3("Errores de medicación evitables"))
                  ),
                  valueBoxOutput("progressBoxUPP",width = 3),
                  valueBoxOutput("approvalBoxUPP",width = 3),
                  valueBoxOutput("progressBoxMEDERR",width = 3),
                  valueBoxOutput("approvalBoxMEDERR",width = 3)
                ),
                fluidRow(
                  column(12,
                         fluidRow(
                           box(plotOutput("distPlotUPPDash"),title = "Úlceras por presión",background = "navy", solidHeader = TRUE),
                           box(plotOutput("distPlotMEDERRDash"),title = "Errores de medicación evitables",background = "navy", solidHeader = TRUE)
                         )
                  )
                ),
                fluidRow(
                  column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
                )
                
                
        ),
        ############################URG########################################
        tabItem(tabName = "urg",
                fluidRow(
                  column(1,offset = 0,tags$img(src="hosplogoT.png")),
                  column(5,offset = 4, h2("Tiempo Medio de Urgencias"))
                  
                ),
                fluidRow(
                  column(6,
                         offset = 4,
                         box(
                           title = "Maximo de horas en Urgencias",
                           sliderInput("n", "Horas:", min=1, max = 120, value = 48)
                           #width = 3
                         )
                  )
                ),
                tabsetPanel(
                  tabPanel("Tiempo medio en urgencias",
                           fluidRow(box(plotOutput("distPlotUrg")),
                                    box(plotOutput("distHistUrg"))
                           )
                           ),
                   tabPanel("Por tipo de episodio",
                            fluidRow(
                              box(plotOutput("barplotUrg")),
                              box(plotOutput("ggplotUrg"))
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
                  column(1,offset = 0,tags$img(src="hosplogoT.png")),
                  column(1,offset = 4, h2("Caídas"))
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
                                           box(plotOutput("distRadarFalls",height = 210),width = 3),
                                           box(plotOutput("distRadarFallsSERV",height = 210),width = 3)
                                         ),
                                         fluidRow(
                                           box(plotOutput("distRadarFallsLEV",height = 210),width = 3),
                                           box(plotOutput("distRadarFallsACO",height = 210),width = 3)
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
                  column(1,offset = 0,tags$img(src="hosplogoT.png")),
                  column(5,offset = 4, h2("Úlceras por presión"))
                ),
                tabsetPanel(
                  tabPanel("Valoración de pacientes",
                            fluidRow(
                              box(plotOutput("distPlotUPP"))
                            )
                  ),
                  tabPanel("Tipo de UCC",
                            fluidRow(
                            box(plotOutput("distPlotUPP3"))
                          ) 
                    ),
                  tabPanel("Comparativa con Encuesta Nacional de prevalencia 2013",
                          fluidRow(
                          box(plotOutput("distPlotUPP4"))
                          )
                  )
                ),
                fluidRow(
                  column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
                )
        ),
        #####################MEDERR#######################################################
        tabItem(tabName = "mederr",
                fluidRow(
                  column(1,offset = 0,tags$img(src="hosplogoT.png")),
                  column(6,offset = 4, h2("Errores de medicación evitables"))
                ), 
                tabsetPanel(
                  tabPanel("Número de alertas por estancia",
                           fluidRow(
                             box(plotOutput("distPlotMEDERR")),
                             box(plotOutput("distPlotMEDERRPOS"))
                           )),
                  tabPanel("Número de alertas por prescripciones de farmacia",
                           fluidRow(
                             box(plotOutput("distPlotMEDERRORD"))
                             #box(plotOutput("distPlotMEDERRPOS"))
                           )
                  ),
                  tabPanel("Número de alertas por eventos de medicación adversa",
                           fluidRow(
                             box(plotOutput("distPlotMEDERREAM"))
                           ))
                ),
                fluidRow(
                  column(6,offset = 10,tags$img(src="cernerlogoT.png", width = "120px", height = "60px"))
                )
        )
        
      )
      
      
    )
  ))
