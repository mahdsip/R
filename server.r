library(shiny)
library(shinydashboard)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotrix)
#Prepare FALLS DATA
CAIDAS = read.csv("falls/benefits_falls.csv",header = TRUE,sep = ";")
CAIDAS$FECHA_HORA2 = as.Date(CAIDAS$FECHA_HORA, "%d%m%y")
CAIDAS$FECHA_HORA = parse_date_time(CAIDAS$FECHA_HORA, "%d%m%y %H%M")
CAIDAS$LUGAR = as.character(CAIDAS$LUGAR)
CAIDAS$CAIDA = ifelse(CAIDAS$LUGAR>0,1,0)
CAIDAS$MES = as.yearmon(CAIDAS$FECHA_HORA,"%Y-%B")
CAIDAS$HORA = format(CAIDAS$FECHA_HORA, "%H", tz="Europe/Madrid")
CAIDAS$POWERPLAN = sub(".*r/c", "", CAIDAS$POWERPLAN)
CAIDAS$POWERPLAN = ifelse(is.na(CAIDAS$POWERPLAN),"Sin powerplan",CAIDAS$POWERPLAN)
CAIDAS$UBICACION <- substr(CAIDAS$UBICACION,1,6)
CAIDAS$LUGAR[grep("Otr",CAIDAS$LUGAR)] <- "Otras"
CAIDAS = subset(CAIDAS, CAIDAS$CAIDA ==1) 
CAID <-CAIDAS
C = aggregate(CAIDAS$CAIDA,list(Mes = CAIDAS$MES),sum)
C = subset(C,C$Mes=="jun 2015")
C = C$x
#Prepare URG DATA
URG = read.csv("emr/benefits_emr.csv",header = TRUE,sep = ",")
URG4 <- URG
URG$FECHA_LLEGADA = parse_date_time(URG$FECHA_LLEGADA, "%d%m%y %H%M%S",tz="Europe/Madrid")
URG$MES = as.yearmon(URG$FECHA_LLEGADA,"%Y-%B",tz="Europe/Madrid")

URG4$FECHA_LLEGADA = parse_date_time(URG4$FECHA_LLEGADA, "%d%m%y %H%M%S",tz="Europe/Madrid")
URG4$MES = as.yearmon(URG4$FECHA_LLEGADA,"%Y-%B",tz="Europe/Madrid")

TRESHOLD = (2*24*60)
URG = subset(URG, URG$TIEMPO< TRESHOLD) 


AGG2 = aggregate(URG4$TIEMPO,list(Mes = URG4$MES,TIP = URG4$TIPO_EPISODIO),mean)
AGG2 = subset(AGG2,AGG2$TIP!="Recien Nacido")

URG = subset(URG,URG$FECHA_LLEGADA>="2014-10-01 00:00:00")
URG1 = subset(URG,URG$FECHA_LLEGADA<"2015-01-01 00:00:00" & URG$FECHA_LLEGADA>="2014-10-01 00:00:00")
URG2 = subset(URG,URG$FECHA_LLEGADA>="2015-01-01 00:00:00")
MEAN1 = 315
M = aggregate(URG2$TIEMPO,list(Mes = URG2$MES),mean)
M = subset(M,M$Mes == "jul 2015")
MEAN2 = M$x
#Prepare UPP DATA
UPP = read.csv("upp/benefits_upp.csv", header=TRUE)
UPP$FECHA_VALORACION2 = as.Date(UPP$FECHA_VALORACION, "%d%m%y")
UPP$FECHA_VALORACION = parse_date_time(UPP$FECHA_VALORACION, "%d%m%y %H%M")
UPP$MES = as.yearmon(UPP$FECHA_VALORACION,"%Y-%B")
UPP$RIESGO = ifelse(UPP$RIESGO=="","NO RIESGO","RIESGO")
#Prepare MEDERR DATA
MEDERR = read.csv("mederr/benefits_mederr.csv",header = TRUE,sep = ",")
MEDERR = subset(MEDERR,MEDERR$EPISODIO=="Hospitalizacion")
MEDERR$FECHA_ENTRADA = parse_date_time(MEDERR$FECHA_ENTRADA, "%d%m%y %H%M%S")
MEDERR$FECHA_ENTRADA2 = as.Date(MEDERR$FECHA_ENTRADA, "%d%m%y")
MEDERR$MES = as.yearmon(MEDERR$FECHA_ENTRADA,"%Y-%B")
MEDERR$POSICION= as.character(MEDERR$POSICION)
MEDERR$POSICION[grep("Méd",MEDERR$POSICION)] <- "Medico"
MEDERR$POSICION[grep("MED",MEDERR$POSICION)] <- "Medico"
MEDERR$POSICION[grep("PSI",MEDERR$POSICION)] <- "Medico"
MEDERR$POSICION[grep("FAR",MEDERR$POSICION)] <- "Farmaceútico"
MEDERR1 <- MEDERR
MEDERR2 <- MEDERR
#MEDERR$TIPO[grep("DUP",MEDERR$TIPO)] <- "DUPLICIDAD"
#MEDERR$TIPO[grep("GDRUG",MEDERR$TIPO)] <- "INCOMPATIBILIDAD"
#MEDERR$TIPO[grep("ALLERGY",MEDERR$TIPO)] <- "ALERGIA"


ESTAN1 = c(20181,20591,23036,26531,26218,25896,27565, 26310,27935,25797,26749,25410)
ESTAN2 = c("30/07/2014 00:00:00","30/08/2014 00:00:00","30/09/2014 00:00:00","30/10/2014 00:00:00","30/11/2014 00:00:00","30/12/2014 00:00:00","30/01/2015 00:00:00","28/02/2015 00:00:00","30/03/2015 00:00:00","30/04/2015 00:00:00","30/05/2015 00:00:00","30/06/2015 00:00:00")
ESTAN = data.frame(ESTAN1,ESTAN2)
ESTAN$ESTAN2 = parse_date_time(ESTAN$ESTAN2, "%d%m%y %H%M%S")
ESTAN$MES = as.yearmon(ESTAN$ESTAN2,"%Y-%B")
ESTAN$ESTAN2 <-NULL

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$progressBoxED <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      #color = "purple"
      paste0("-",format(round(100-(MEAN2*100/MEAN1),digits = 2),nsmall = 2), "%"),
      paste0("TMU de ",format(round(MEAN2,digits = 2),nsmall = 2)," Descenso de: ",format(round(315-MEAN2,digits = 2),nsmall = 2)," minutos sobre la base"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBoxED <- renderValueBox({
    valueBox(
      "-10%", "OBJETIVO: Descenso del 10% (31,5 minutos) del TMU sobre una base de 315 minutos", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )})
  
  
  output$progressBoxFalls <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      #color = "purple"
      paste0("-",format(round(100-(C*1000/24000)*100/0.64,digits = 2),nsmall = 2), "%"),
      paste0(format(round(C*1000/24000,digits = 2),nsmall = 2)," caídas por cada 1000 estancias",
             ", descenso de ",format(round(0.64-(C*1000/24000),digits = 2),nsmall = 2), " caídas por cada 1000 estancias"),
      icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBoxFalls <- renderValueBox({
    valueBox(
      "-10%", "OBJETIVO: Disminuir el número de caídas un 10% (0,58) sobre la bas de 0,64 por 1000 estancias", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )})
  
  output$progressBoxUPP <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      #color = "purple"
      paste0("-","10", "%"),
      paste0("N"," UPP sobre prevalencia semestral, descenso de ", "n", " UPP por prevalencia semestral"),
      icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBoxUPP <- renderValueBox({
    valueBox(
      "-15%",
      "OBJETIVO: Disminuir un 15% (0,74) las UPP sobre una base de 4,86% de prevalencia semestral",
      icon = icon("thumbs-up",
      lib = "glyphicon"),
      color = "yellow"
    )})
  output$progressBoxMEDERR <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      #color = "purple"
      
      paste0("-",format(round(100-(MEDALER2*100/MEDALER1),digits = 2),nsmall = 2), "%"),
      paste0("El número de alertas por estancia a descendido en ",format(round((MEDALER1-MEDALER2),digits = 2),nsmall = 2)," puntos desde Julio de 2014"),
      icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBoxMEDERR <- renderValueBox({
    valueBox(
      "N/A", "OBJETIVO: Disminución del número de errores de medicación evitables", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )}) 
  output$progressBoxMEDIV <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      #color = "purple"
      paste0("-","6,87", "%"),
      paste0("El número de pacientes con de medicación de IV a VO a descendido en","N"),
      icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBoxMEDIV <- renderValueBox({
    valueBox(
      "-50%",
      "OBJETIVO: Disminución del número de pacientes con paso de medicación de IV a VO",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )})  
  
  output$dateText2 <- renderText({
    paste("input$date2 is", as.character(input$date2))
  })
  
  output$dateText1 <- renderText({
    paste("input$date is", as.character(input$date1))
  })

  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
 ####################DASHBOARD######################################### 
  output$distPlotUrgDash <- renderPlot({
    #TRESHOLD = (input$slider*24*60)
    TRESHOLD = (2*24*60)
    URG = subset(URG, URG$TIEMPO< TRESHOLD) 
    URG = subset(URG,URG$FECHA_LLEGADA>="2014-10-01 00:00:00")
    URG1 = subset(URG,URG$FECHA_LLEGADA<"2015-01-01 00:00:00" & URG$FECHA_LLEGADA>="2014-10-01 00:00:00")
    URG2 = subset(URG,URG$FECHA_LLEGADA>="2015-01-01 00:00:00")
    MEAN1 = 315
    M = aggregate(URG2$TIEMPO,list(Mes = URG2$MES),mean)
    M = subset(M,M$Mes == "jul 2015")
    MEAN2 = M$x    
    
    plot(aggregate(URG$TIEMPO,list(Mes = URG$MES),mean),type = "l",pch = 15,col = 40)
    abline(h=mean(URG$TIEMPO),col = "green")
    abline(h=315, col = "brown")
    #abline(h=mean(URG2$TIEMPO),col = "red")
    legend(x="bottomright",c("Tiempo Medio Base","Tiempo Medio Actual"),lty = c(1,1),lwd=c(2.5,2.5),col=c("brown","green"))
  })
  
  output$distPlotFallsDash <- renderPlot({
    barplot(table(CAIDAS$MES))
  })
  
  output$distPlotUPPDash <- renderPlot({
    plot(aggregate(UPP$UPP_HOSPITAL,list(Mes = UPP$MES),sum),type = "l")
  })
  
  output$distPlotMEDERRDash <- renderPlot({
    TABLA = table(MEDERR$TIPO,MEDERR$MES)
    TABLA2 = table(MEDERR$MES)
    TABLA2 = as.data.frame(TABLA2)
    MES = as.vector(TABLA2$Var1)
    EST = as.vector(ESTAN$ESTAN1)
    ALR = as.vector(TABLA2$Freq)
    ALR = replace(ALR,ALR==84667,49627)
    ALR = replace(ALR,ALR==87366,54598)
    ALR = replace(ALR,ALR==91524,60087)
    ALR = replace(ALR,ALR==100095,65263)
    ALR = replace(ALR,ALR==74769,44047)
    ALR = replace(ALR,ALR==50867,33495)
    ALER = round(ALR/EST,digits = 4)
    MEDALER1 = as.numeric(ALER[1])
    MEDALER2 = as.numeric(ALER[12])
    
    barplot(ALER,
            main="Número de alertas por estancia",
            xlab="Mes",
            ylab="Alertas por estancia",
            col = "grey",
            names.arg = MES)
    
  })
##############################EMERGENCY#####################################3  
  
  
  output$distPlotUrg <- renderPlot({
    #TRESHOLD = (input$slider*24*60)
    TRESHOLD = (input$n*60)
    URG$FECHA_HORA2 = as.Date(URG$FECHA_LLEGADA, "%d%m%y", tz="Europe/Madrid")
    URG = subset(URG, URG$TIEMPO< TRESHOLD) 
    URG = subset(URG,URG$FECHA_HORA2>=input$dateRangeALL[1]&URG$FECHA_HORA2<=input$dateRangeALL[2])
    URG1 = subset(URG,URG$FECHA_LLEGADA<"2015-01-01 00:00:00" & URG$FECHA_LLEGADA>="2014-10-01 00:00:00")
    URG2 = subset(URG,URG$FECHA_LLEGADA>="2015-01-01 00:00:00")
    MEAN = aggregate(URG$TIEMPO,list(Mes = URG$MES),mean)
    MEAN1 = 315
    M = aggregate(URG2$TIEMPO,list(Mes = URG2$MES),mean)
    M = subset(M,M$Mes == "jul 2015")
    MEAN2 = M$x
    
    plot(aggregate(URG$TIEMPO,list(Mes = URG$MES),mean),type = "l",pch = 15,
         col = 'darkblue', 
         main="Tiempo medio de urgencias por mes",
         xlab="Meses",
         ylab="Minutos")
    abline(h=mean(URG$TIEMPO),col = "green")
    abline(h=315, col = "brown")
    #abline(h=mean(URG2$TIEMPO),col = "red")
    legend(x="bottomright",c("Tiempo Medio Base","Tiempo Medio Actual"),lty = c(1,1),lwd=c(2.5,2.5),col=c("brown","green"))
  })
  
  output$distHistUrg<- renderPlot({
    URG$FECHA_HORA2 = as.Date(URG$FECHA_LLEGADA, "%d%m%y", tz="Europe/Madrid")
    URG = subset(URG,URG$FECHA_HORA2>=input$dateRangeALL[1]&URG$FECHA_HORA2<=input$dateRangeALL[2])
    hist(URG$TIEMPO,
         col = 'darkblue', 
         breaks = 180,
         main="Histograma de tiempos de urgencia",
         xlab="Tiempos en urgencias",
         ylab="Frecuencia")
  })
  
  output$ggplotUrg<- renderPlot({
    
  
    

    
    ggplot(AGG2, aes(x = factor(AGG2$Mes), y = AGG2$x, fill=factor(AGG2$TIP))) + 
      geom_bar(stat = "identity", position=position_dodge()) +
      geom_text(aes(y=AGG2$x, ymax=AGG2$x, label=round(AGG2$x,0.01)), position= position_dodge(width=0.01), vjust=-.5, color="black") +
      scale_y_continuous("Minutos",limits=c(0,500)) + 
      scale_x_discrete("Meses") +
      scale_fill_discrete(name ="Tipo Episodio", labels=c("Hospitalizados", "Urgencias"))
  })
  
  output$barplotUrg <- renderPlot({
    
    URG4 = subset(URG4,URG4$TIPO_EPISODIO!="Recien Nacido")
    
    barplot(table(URG4$TIPO_EPISODIO,URG4$MES),
            col = c('green','darkblue','darkblue'),
            main="Número de entradas en urgencias por tipo",
            xlab="Tipo",
            ylab="Pacientes",
            legend =  rownames(table(URG4$TIPO_EPISODIO,URG4$MES)))
  })
  
##########################FALLS############################################
  
  output$distPlotFalls <- renderPlot({
    subset(CAIDAS,CAIDAS$FECHA_HORA2>=input$dateRangeALL[1]&CAIDAS$FECHA_HORA2<=input$dateRangeALL[2])
    BP<-barplot(table(CAIDAS$MES),
            col = 'darkblue', 
            border = 'white',
            main="Número de caidas por mes",
            xlab="Meses",
            ylab="Caidas")
    text(BP,c(1,1),table(CAIDAS$MES),col = "white")
  })
  
  output$distPlotFallsPW <- renderPlot({
   
    PP = table(CAIDAS$POWERPLAN)
    pie3D(PP,main = "Porcentaje de caídas con y sin PowerPlan")
    
  })
  
  output$distPlotFallsLO <- renderPlot({
    
    PP = table(CAIDAS$LUGAR)
    pie(PP,main = "Porcentaje de caídas por lugar")
    
  })
  
  output$distPlotFallsUB <- renderPlot({
    PP = table(CAIDAS$UBICACION)
    pie(PP,main = "Porcentaje de caídas por ubicación")
    
  })
  
  output$distHistFalls<- renderPlot({
    hist(table(CAIDAS$MES),
         breaks = 5, 
         col = 'darkblue', 
         border = 'white',
         ylim=c(0,6),
         main="Histograma de caídas al mes",
         xlab="Caidas al mes",
         ylab="Frecuencia")
    
    
  })
 
  output$distRadarFalls<- renderPlot({

    
    radar <- ggplot(CAID, aes(factor(CAID$HORA))) +  geom_bar(fill="darkblue",width = 1, colour = "green")
    radar + coord_polar()   
  }) 

##################UPP#######################################################  
  output$distPlotUPP <- renderPlot({
    UPP$FECHA_VALORACION2 = as.Date(UPP$FECHA_VALORACION, "%d%m%y", tz="Europe/Madrid")
    UPP$FECHA_VALORACION = parse_date_time(UPP$FECHA_VALORACION, "%d%m%y %H%M", tz="Europe/Madrid")
    UPP$MES = as.yearmon(UPP$FECHA_VALORACION,"%Y-%B")
    UPP$RIESGO = ifelse(UPP$RIESGO=="RIESGO","RIESGO","NO RIESGO")
    UPP = subset(UPP,UPP$FECHA_VALORACION2>=input$dateRangeALL[1]&UPP$FECHA_VALORACION2<=input$dateRangeALL[2])
    barplot(table(UPP$RIESGO,UPP$BRADEN),
            col = c('darkblue','green'),
            main="Número de pacientes por valoración",
            xlab="Braden",
            ylab="Pacientes",
            beside=TRUE,
            legend =  rownames(table(UPP$RIESGO,UPP$BRADEN)))
  })
  
  output$distPlotUPP2 <- renderPlot({
    UPP$FECHA_VALORACION2 = as.Date(UPP$FECHA_VALORACION, "%d%m%y", tz="Europe/Madrid")
    UPP = subset(UPP,UPP$FECHA_VALORACION2>=input$dateRangeALL[1]&UPP$FECHA_VALORACION2<=input$dateRangeALL[2])
    plot(aggregate(UPP$UPP_HOSPITAL,list(Mes = UPP$MES),sum),type = "l",
         col = 'darkblue', 
         main="Úlceras nosocomiales por mes",
      
         xlab="Meses",
         ylab="Úlceras"
         )
  })

########################MEDERR########################################
  output$distPlotMEDERR <- renderPlot({
    #MEDERR1 = subset(MEDERR,MEDERR1$FECHA_ENTRADA2>=input$dateRangeALL[1]&MEDERR1$FECHA_ENTRADA2<=input$dateRangeALL[2])
    TABLA = table(MEDERR1$TIPO,MEDERR1$MES)

    MP<-barplot(TABLA,
                col=c("grey","green","darkblue"),
                border = 'white',
                main="Número de alertas por mes",
                xlab="Mes",
                ylab="Alertas por mes",
                legend = c("ALERGIA","INCOMPATIBLE","DUPLICADA"))
    par(new=TRUE)
    plot(ESTAN$MES,ESTAN$ESTAN1,type="o",col="brown",pch=17,xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("Estancias",side=4,line=3)
    text(MP,c(1,1),TABLA,col = "white")
  })
  
  output$distPlotMEDERRPOS <- renderPlot({

    
    POSI = table(MEDERR2$POSICION)
    pie3D(POSI,main = "Porcentaje de alertas por posición")
    
  })
  
})
