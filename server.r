library(shiny)
library(shinydashboard)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotrix)
#library(plotly)

######################BENEFITS STANTDARD REFERENCES###########################

#SPANISH HEALTH MINISTRY - Time from Door to Doctor in ED (not Urgent) ################
#It has 4 levels Emergency, Very Urgent, Urgent, Less Urgent and Not Urgent 
URG_MED_1 = 120
#SPANISH HEALTH MINISTRY - Time from Registration to Admission in ED (not Urgent) ################
URG_MED_2 = 240


#############################STANCIES#############################################
ESTAN1 = c(20181,20591,23036,26531,26218,25896,27565, 26310,27935,25797,26749,25410)
ESTAN2 = c("30/07/2014 00:00:00","30/08/2014 00:00:00","30/09/2014 00:00:00","30/10/2014 00:00:00","30/11/2014 00:00:00","30/12/2014 00:00:00","30/01/2015 00:00:00","28/02/2015 00:00:00","30/03/2015 00:00:00","30/04/2015 00:00:00","30/05/2015 00:00:00","30/06/2015 00:00:00")
ESTAN = data.frame(ESTAN1,ESTAN2)
ESTAN$ESTAN2 = parse_date_time(ESTAN$ESTAN2, "%d%m%y %H%M%S", tz="UTC")
ESTAN$MES = as.yearmon(ESTAN$ESTAN2,"%Y-%B")
ESTAN$ESTAN2 <-NULL
################################BEDS########################################
CAMAS1 = c(651,664,768,856,874,835,972, 962,982,982,982,982)
CAMAS2 = c("30/07/2014 00:00:00","30/08/2014 00:00:00","30/09/2014 00:00:00","30/10/2014 00:00:00","30/11/2014 00:00:00","30/12/2014 00:00:00","30/01/2015 00:00:00","28/02/2015 00:00:00","30/03/2015 00:00:00","30/04/2015 00:00:00","30/05/2015 00:00:00","30/06/2015 00:00:00")
CAMAS = data.frame(CAMAS1,CAMAS2)
CAMAS$CAMAS2 = parse_date_time(CAMAS$CAMAS2, "%d%m%y %H%M%S", tz="UTC")
CAMAS$MES = as.yearmon(CAMAS$CAMAS2,"%Y-%B")
CAMAS$CAMAS2 <-NULL
################################INPATIENTS########################################
INP1 = c(2320,2239,2517,2883,2785,2728,2942,2890,2987,2754,2898,2787)
INP2 = c("30/07/2014 00:00:00","30/08/2014 00:00:00","30/09/2014 00:00:00","30/10/2014 00:00:00","30/11/2014 00:00:00","30/12/2014 00:00:00","30/01/2015 00:00:00","28/02/2015 00:00:00","30/03/2015 00:00:00","30/04/2015 00:00:00","30/05/2015 00:00:00","30/06/2015 00:00:00")
INP = data.frame(INP1,INP2)
INP$INP2 = parse_date_time(INP$INP2, "%d%m%y %H%M%S", tz="UTC")
INP$MES = as.yearmon(INP$INP2,"%Y-%B")
INP$INP2 <-NULL
##################################ORDERS#####################################
ORD1 = c(70455,74465,77789,91886,92042,89398,96936,94623,99873,91064,92086,94379)
ORD2 = c("30/07/2014 00:00:00","30/08/2014 00:00:00","30/09/2014 00:00:00","30/10/2014 00:00:00","30/11/2014 00:00:00","30/12/2014 00:00:00","30/01/2015 00:00:00","28/02/2015 00:00:00","30/03/2015 00:00:00","30/04/2015 00:00:00","30/05/2015 00:00:00","30/06/2015 00:00:00")
ORD = data.frame(ORD1,ORD2)
ORD$ORD2 = parse_date_time(ORD$ORD2, "%d%m%y %H%M%S", tz="UTC")
ORD$MES = as.yearmon(ORD$ORD2,"%Y-%B")
ORD$ORD2 <-NULL
##################################RAM#####################################
RAM1 = c(0,0,0,1,2,1,6,1,7,2,1,1)
RAM2 = c("30/07/2014 00:00:00","30/08/2014 00:00:00","30/09/2014 00:00:00","30/10/2014 00:00:00","30/11/2014 00:00:00","30/12/2014 00:00:00","30/01/2015 00:00:00","28/02/2015 00:00:00","30/03/2015 00:00:00","30/04/2015 00:00:00","30/05/2015 00:00:00","30/06/2015 00:00:00")
RAM = data.frame(RAM1,RAM2)
RAM$RAM2 = parse_date_time(RAM$RAM2, "%d%m%y %H%M%S", tz="UTC")
RAM$MES = as.yearmon(RAM$RAM2,"%Y-%B")
RAM$RAM2 <-NULL
###############################KPI DATA#######################################
KPI = read.csv("KPI.csv")
KPI$BASE = as.numeric(KPI$BASE)
KPI$OBJETIVO = as.numeric(KPI$OBJETIVO)

#####################Prepare URG DATA##################################################
URG= read.csv("emr/benefits_emr.csv",header = TRUE,sep = ",")

#Prepare URG DASHBOARD DATA
URG_DASH <- URG
URG_DASH$FECHA_LLEGADA = parse_date_time(URG_DASH$FECHA_LLEGADA, "%d%m%y %H%M%S", tz="UTC")
URG_DASH$MES = as.yearmon(URG_DASH$FECHA_LLEGADA,"%Y-%B")
TRESHOLD = (2*24*60)
URG_DASH = subset(URG_DASH, URG_DASH$TIEMPO< TRESHOLD) 
URG_DASH = subset(URG_DASH,URG_DASH$FECHA_LLEGADA>="2014-10-01 00:00:00")
URG_DASH1 = subset(URG_DASH,URG_DASH$FECHA_LLEGADA<"2015-01-01 00:00:00" & URG_DASH$FECHA_LLEGADA>="2014-10-01 00:00:00")
URG_DASH2 = subset(URG_DASH,URG_DASH$FECHA_LLEGADA>="2015-01-01 00:00:00")
M = aggregate(URG_DASH2$TIEMPO,list(Mes = URG_DASH2$MES),mean)
M = subset(M,M$Mes == "jun 2015")
URG_MEAN2 = M$x

#Prepare URG 1 DATA
URG_1 <- URG
URG_1$FECHA_LLEGADA = parse_date_time(URG_1$FECHA_LLEGADA, "%d%m%y %H%M%S", tz="UTC")
URG_1$MES = as.yearmon(URG_1$FECHA_LLEGADA,"%Y-%B")
URG_1 = subset(URG_1,URG_1$FECHA_LLEGADA>="2014-10-01 00:00:00")

#Prepare URG 2 DATA
URG_2 <- URG
URG_2$FECHA_LLEGADA = parse_date_time(URG_2$FECHA_LLEGADA, "%d%m%y %H%M%S", tz="UTC")
URG_2$MES = as.yearmon(URG_2$FECHA_LLEGADA,"%Y-%B")
URG_2 = subset(URG_2,URG_2$FECHA_LLEGADA>="2014-10-01 00:00:00")

#Prepare URG 3 DATA
URG_3 <- URG
URG_3$FECHA_LLEGADA = parse_date_time(URG_3$FECHA_LLEGADA, "%d%m%y %H%M%S", tz="UTC")
URG_3$MES = as.yearmon(URG_3$FECHA_LLEGADA,"%Y-%B")
URG_3 = subset(URG_3,URG_3$FECHA_LLEGADA>="2014-10-01 00:00:00")

#Prepare URG 4 DATA
URG_4 <- URG
URG_4$FECHA_LLEGADA = parse_date_time(URG_4$FECHA_LLEGADA, "%d%m%y %H%M%S", tz="UTC")
URG_4$MES = as.yearmon(URG_4$FECHA_LLEGADA,"%Y-%B")
URG_4 = subset(URG_4,URG_4$FECHA_LLEGADA>="2014-10-01 00:00:00")
URG_4$FECHA_HORA2 = as.Date(URG_4$FECHA_LLEGADA, "%d%m%y", tz="UTC")
URG_4 = subset(URG_4,URG_4$TIPO_EPISODIO!="Recien Nacido")
URG_4_AGG = aggregate(URG_4$TIEMPO,list(Mes = URG_4$MES,TIP = URG_4$TIPO_EPISODIO),mean)
URG_4_AGG = subset(URG_4_AGG,URG_4_AGG$TIP!="Recien Nacido")

##############Prepare FALLS DATA##################################################
Sys.setlocale('LC_ALL', 'spanish')
CAIDAS = read.csv("falls/benefits_falls.csv",header = TRUE,sep = ";")
CAIDAS$LUGAR = as.character(CAIDAS$LUGAR)
CAIDAS$CAIDA = ifelse(CAIDAS$LUGAR>0,1,0)
CAIDAS = subset(CAIDAS, CAIDAS$CAIDA ==1) 
CAIDAS$FECHA_HORA = parse_date_time(CAIDAS$FECHA_HORA, "%d%m%y %H%M",tz="UTC")
CAIDAS$HORA = format(CAIDAS$FECHA_HORA, "%H", tz="UTC")
CAIDAS$MES = as.yearmon(CAIDAS$FECHA_HORA,"%Y-%B")
CAIDAS$FECHA_HORA = as.Date(CAIDAS$FECHA_HORA, "%d%m%y %H%M",tz="UTC")
CAIDAS$POWERPLAN = sub(".*r/c", "", CAIDAS$POWERPLAN)
CAIDAS$POWERPLAN = ifelse(CAIDAS$POWERPLAN=="","Sin powerplan",CAIDAS$POWERPLAN)
CAIDAS$UBICACION <- substr(CAIDAS$UBICACION,1,6)
CAIDAS$LUGAR[grep("Otr",CAIDAS$LUGAR)] <- "Otras"
CAIDAS$ACTIVIDAD = factor(CAIDAS$ACTIVIDAD)
#CAIDAS$ACTIVIDAD[grep("Otr",CAIDAS$ACTIVIDAD)] <- "Otras"

#Prepare FALLS 1
CAIDAS_1 <- CAIDAS

#Prepare FALLS 2
CAIDAS_2 <- CAIDAS

#Prepare FALLS 3
CAIDAS_3 <- CAIDAS

#Prepare FALLS 4
CAIDAS_4 <- CAIDAS

#Prepare FALLS 5
CAIDAS_5 <- CAIDAS

#Prepare FALLS 6
CAIDAS_6 <- CAIDAS


FALL = aggregate(CAIDAS$CAIDA,list(Mes = CAIDAS$MES),sum)
FALL = subset(FALL,FALL$Mes=="jun 2015")
FALL = FALL$x

FALL_EST = subset(ESTAN,ESTAN$MES=="jun 2015")
FALL_EST = FALL_EST$ESTAN1

TABLACAI = table(CAIDAS$MES)
TABLACAI = as.data.frame(TABLACAI)
MES = as.vector(TABLACAI$Var1)
EST = as.vector(ESTAN$ESTAN1)
CA = as.vector(TABLACAI$Freq)
CAI = round(1000*CA/EST,digits = 4)


####################Prepare UPP DATA##############################################
UPP = read.csv("upp/benefits_upp.csv", header=TRUE)
UPP$FECHA_VALORACION = parse_date_time(UPP$FECHA_VALORACION, "%d%m%y %H%M",tz="UTC")
UPP$MES = as.yearmon(UPP$FECHA_VALORACION,"%Y-%B")
UPP$FECHA_VALORACION = as.Date(UPP$FECHA_VALORACION, "%d%m%y",tz="UTC")
UPP$RIESGO = ifelse(UPP$RIESGO=="","NO RIESGO","RIESGO")
UPP$UPP_HOSPITAL = UPP$UPP - UPP$UPP_FUERA

PAC = aggregate(UPP$UPP_HOSPITAL,list(Mes = UPP$MES),sum)
PAC$PREV = PAC$x*100/INP$INP1
PAC$x <- NULL
UPPTARGET = subset(PAC,PAC$Mes=="jun 2015")


UPP_1 <- UPP
UPP_2 <- UPP

###############Prepare MEDERR DATA###########################################
MEDERR = read.csv("mederr/benefits_mederr.csv",header = TRUE,sep = ",")
MEDERR = subset(MEDERR,MEDERR$EPISODIO=="Hospitalizacion")
MEDERR$FECHA_ENTRADA = parse_date_time(MEDERR$FECHA_ENTRADA, "%d%m%y %H%M%S", tz="UTC")
MEDERR$FECHA_ENTRADA2 = as.Date(MEDERR$FECHA_ENTRADA, "%d%m%y", tz="UTC")
MEDERR$MES = as.yearmon(MEDERR$FECHA_ENTRADA,"%Y-%B")
MEDERR$POSICION= as.character(MEDERR$POSICION)
MEDERR$POSICION[grep("Méd",MEDERR$POSICION)] <- "Médico"
MEDERR$POSICION[grep("MED",MEDERR$POSICION)] <- "Médico"
MEDERR$POSICION[grep("PSI",MEDERR$POSICION)] <- "Médico"
MEDERR$POSICION[grep("MAT",MEDERR$POSICION)] <- "Médico"
MEDERR$POSICION[grep("FAR",MEDERR$POSICION)] <- "Farmaceútico"
MEDERR1 <- MEDERR
MEDERR2 <- MEDERR

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



################## Define server logic required to draw ############################
shinyServer(function(input, output) {
  

####################DASHBOARD#########################################   
  RESULT_ACCEPT = 0.2
  
  URG_MEAN_BASE = KPI$BASE[1]
  URG_OBJ_PER = KPI$OBJETIVO[1]
  URG_OBJ = URG_MEAN_BASE -(URG_MEAN_BASE*URG_OBJ_PER/100)
  URG_OBJ_ABS = (-1)*(URG_MEAN_BASE - URG_OBJ)
  
  UPP_BASE = KPI$BASE[3]
  UPP_OBJ_PER = KPI$OBJETIVO[3]
  UPP_OBJ = UPP_BASE - (UPP_BASE*UPP_OBJ_PER/100)
  
  FALL_BASE = KPI$BASE[2]
  FALL_OBJ_PER = KPI$OBJETIVO[2]
  FALL_OBJ = FALL_BASE - (FALL_BASE*FALL_OBJ_PER/100)

  UPP_VAL = (UPP_BASE-UPPTARGET$PREV)*100/UPP_BASE
  
  
  taskData <- data.frame(
    value = c(format(round((-1*(URG_MEAN_BASE - URG_MEAN2)*100/URG_OBJ_ABS),digits = 2),nsmall = 2),
              format(round((FALL_BASE-(FALL*1000/FALL_EST))*100/(FALL_BASE-FALL_OBJ),digits = 2),nsmall = 2),
              format(round((UPP_VAL*100)/UPP_OBJ_PER,digits = 2),nsmall = 2),
              "100"
    ),
    message = c("Tiempo medio de urgencias","Caidas","UPP","Errores de medicación evitables"),
    stringsAsFactors = FALSE
  )
  
  notifData <- subset(taskData,as.numeric(taskData$value)>99) 
  
  output$taskMenu <- renderMenu({
    tasks <- apply(taskData, 1, function(row) {
      taskItem(
        color = if (as.numeric(row[["value"]]) >= 100) "green" 
                else 
                    if (as.numeric(row[["value"]])+(RESULT_ACCEPT*100) >= 100) "yellow" 
                    else "red",
        value = row[["value"]],
        paste(row[["message"]])
      )
    })
    
    dropdownMenu(type = "tasks", .list = tasks)
  })
  
  output$notifMenu <- renderMenu({
    
    notif <- apply(notifData, 1, function(row) {
      notificationItem(
        text = row[["message"]],
        icon("check"),
        status = "success"
      )
    })  
    
    dropdownMenu(type = "notifications", .list = notif, badgeStatus = "success",icon = tags$b("Miguel Huerta"))
  })
  
  
  output$legend <- renderPlot({
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="topleft",c("100% logrado","> 80% logrado","< 80% logrado","Objetivo"),box.lty=0,pch = c(15),lty = c(0),lwd=c(5),col=c("green","orange","red","purple"))
  })  
  
  ################### DASHBOARD URG###############################
  output$progressBoxED <- renderValueBox({
    
    URG_VAL = (-1)*(URG_MEAN_BASE - URG_MEAN2)
    URG_VAL_PER = (URG_VAL*100/URG_OBJ_ABS)
    URG_VAL_ABS = (URG_VAL*100/URG_MEAN_BASE)
    
    valueBox(
      paste0(format(round(URG_VAL_ABS,digits = 2),nsmall = 2), "%"),
      paste0("VALOR: ",format(round(URG_MEAN2,digits = 2),nsmall = 2)," minutos.","Variación de: ",format(round(URG_VAL,digits = 2),nsmall = 2)," minutos sobre la base"),
      icon = if ((100-(URG_MEAN2*100/URG_MEAN_BASE))+(RESULT_ACCEPT*URG_OBJ_PER) >= URG_OBJ_PER) icon("thumbs-up", lib = "glyphicon")
                else 
                  icon("thumbs-down", lib = "glyphicon"),
      color = if ((100-(URG_MEAN2*100/URG_MEAN_BASE)) >= URG_OBJ_PER) "green" 
              else 
                if ((100-(URG_MEAN2*100/URG_MEAN_BASE))+(RESULT_ACCEPT*URG_OBJ_PER) >= URG_OBJ_PER) "yellow" 
                else "red"
    )

  })
  output$approvalBoxED <- renderValueBox({
    valueBox(
      paste0("-",URG_OBJ_PER,"%"),
      paste0("OBJETIVO: ",URG_OBJ," minutos.","Variación de ",URG_OBJ_ABS," minutos sobre una base de ",URG_MEAN_BASE," minutos"), 
      icon = icon("area-chart"),
      color = "purple"
    )})
  output$Progess1 <- renderText({ 
    paste0((100-(URG_MEAN2*100/URG_MEAN_BASE))*100/URG_OBJ_PER)
  })
  
  output$distPlotUrgDash <- renderPlot({

    plot(aggregate(URG_DASH$TIEMPO,list(Mes = URG_DASH$MES),mean),type = "l",pch = 15,col = 40,ylab = "Tiempo medio")
    abline(h=mean(URG_DASH$TIEMPO),col = "blue")
    abline(h=URG_MEAN_BASE, col = "brown")
    abline(h=(URG_MEAN_BASE - (URG_MEAN_BASE*URG_OBJ_PER/100)), col = "orange")
    legend(x="bottom",horiz = TRUE, c("Base","Actual","Objetivo","Medio"),lty = c(1,1,1,1),lwd=c(2.5,2.5,2.5,2.5),col=c("brown","blue","orange","grey"))
  })
  
  ####################### DASH BOARD FALLS##############################
  output$progressBoxFalls <- renderValueBox({
    FALL_VAL = (FALL*1000/FALL_EST)
    FALL_VAL_PER = (-1)*(100-(FALL_VAL*100/FALL_BASE))
    #URG_VAL_ABS = (URG_VAL*100/URG_MEAN_BASE)
    
    valueBox(
      paste0(format(round(FALL_VAL_PER,digits = 2),nsmall = 2), "%"),
      paste0("VALOR: ",format(round(FALL_VAL,digits = 2),nsmall = 2)," caídas por cada 1000 estancias",
             ". Variación de ",format(round(FALL_VAL-FALL_BASE,digits = 2),nsmall = 2), " caídas por cada 1000 estancias sobre la base"),
      icon = if ((100-(FALL*1000/FALL_EST)*100/FALL_BASE)+(RESULT_ACCEPT*FALL_OBJ_PER) >= FALL_OBJ_PER) icon("thumbs-up", lib = "glyphicon")
      else 
        icon("thumbs-down", lib = "glyphicon"),
      color = if ((100-(FALL*1000/FALL_EST)*100/FALL_BASE) >= FALL_OBJ_PER) "green" 
      else 
        if ((100-(FALL*1000/FALL_EST)*100/FALL_BASE)+(RESULT_ACCEPT*FALL_OBJ_PER) >= FALL_OBJ_PER) "yellow" 
      else "red"      
      
    )
  })
  output$approvalBoxFalls <- renderValueBox({
    valueBox(
      paste0("-",FALL_OBJ_PER,"%"),
      paste0("OBJETIVO: ",FALL_OBJ," por cada 1000 estancias. Variación de ",format(round(FALL_OBJ-FALL_BASE,digits = 2),nsmall = 2)," sobre la base de ",FALL_BASE," caídas por 1000 estancias"),
      icon = icon("area-chart"),
      color = "purple"
    )})
  
  output$distPlotFallsDash <- renderPlot({
    
    BPF<-barplot(CAI, 
            main="Número de caídas por 1000 estancias",
            xlab="Mes",
            ylab="Caídas por 1000 estancias",
            ylim = c(0,0.7),
            col = "grey",
            names.arg = MES)
    abline(h=FALL_BASE, col = "brown")
    abline(h=FALL_OBJ, col = "orange")
    legend(x="bottom",horiz = TRUE, c("Base","Objetivo"),lty = c(1,1),lwd=c(2.5,2.5),col=c("brown","orange"))
  })
  #################DASHBOARD UPP#####################################
  
  output$progressBoxUPP <- renderValueBox({
    
    valueBox(
      paste0(format(round(-((UPP_BASE-UPPTARGET$PREV)*100/UPP_BASE),digits = 2),nsmall = 2), "%"),
      paste0("VALOR: ",format(round(UPPTARGET$PREV,digits = 2),nsmall = 2),"% de prevalencia en Junio. Variación de ", format(round(UPPTARGET$PREV-UPP_BASE,digits = 2),nsmall = 2), " puntos de prevalencia sobre la base"),
      icon = if (((UPP_BASE-UPPTARGET$PREV)*100/UPP_BASE)+(RESULT_ACCEPT*UPP_OBJ_PER) >=UPP_OBJ_PER) icon("thumbs-up", lib = "glyphicon")
      else 
        icon("thumbs-down", lib = "glyphicon"),
      color = if ((UPP_BASE-UPPTARGET$PREV)*100/UPP_BASE >= UPP_OBJ_PER) "green" 
      else 
        if (((UPP_BASE-UPPTARGET$PREV)*100/UPP_BASE)+(RESULT_ACCEPT*UPP_OBJ_PER) >= UPP_OBJ_PER) "yellow" 
      else "red"  
    )
  })
  output$approvalBoxUPP <- renderValueBox({
    valueBox(
      paste0("-",UPP_OBJ_PER,"%"),
      paste0(UPP_OBJ,"% de prevalencia de UPP. Variación de ",UPP_OBJ-UPP_BASE," puntos sobre una base de ",UPP_BASE,"% de prevalencia semestral"),
      icon = icon("area-chart"),
      color = "purple"
    )})
  
  output$distPlotUPPDash <- renderPlot({

    plot(PAC,type = "l", ylim = c(1,5))
    abline(h=UPP_BASE, col = "brown")
    abline(h=UPP_OBJ, col = "orange")
    legend(x="bottom",horiz = TRUE,c("Prevalencia Base","Prevalencia","Objetivo"),lty = c(1,1),lwd=c(2.5,2.5),col=c("brown","grey","orange"))
  })
  ############## DASHBOARD MEDERR ##################################
  MEDERR_OBJ = 0
  MEDERR_OBJ_PER = 0
  
  output$progressBoxMEDERR <- renderValueBox({
    valueBox(
      paste0("-",format(round(100-(MEDALER2*100/MEDALER1),digits = 2),nsmall = 2), "%"),
      paste0("VALOR: El número de alertas por estancia a descendido en ",format(round((MEDALER1-MEDALER2),digits = 2),nsmall = 2)," puntos desde Julio de 2014"),
      icon = if ((100-(MEDALER2*100/MEDALER1))+(RESULT_ACCEPT*MEDERR_OBJ_PER) >=MEDERR_OBJ_PER) icon("thumbs-up", lib = "glyphicon")
      else 
        icon("thumbs-down", lib = "glyphicon"),
      color = if (100-(MEDALER2*100/MEDALER1) >= MEDERR_OBJ_PER) "green" 
      else 
        if ((100-(MEDALER2*100/MEDALER1))+(RESULT_ACCEPT*MEDERR_OBJ_PER) >= MEDERR_OBJ_PER) "yellow" 
      else "red" 
    )
  })
  output$approvalBoxMEDERR <- renderValueBox({
    valueBox(
      "N/A", "OBJETIVO: Disminución del número de errores de medicación evitables", 
      icon = icon("area-chart"),
      color = "purple"
    )}) 
  
  output$distPlotMEDERRDash <- renderPlot({

    barplot(ALER,
            main="Número de alertas por estancia",
            xlab="Mes",
            ylab="Alertas por estancia",
            col = "grey",
            names.arg = MES)
    abline(h=MEDALER1, col = "brown")
    legend(x="bottom",horiz = TRUE,c("EMA Base"),lty = c(1,1),lwd=c(2.5,2.5),col=c("brown","grey","orange"))
    #abline(h=UPP_OBJ, col = "orange")
    
  })
  
############################URG#############################  
  output$distPlotUrg <- renderPlot({
    
    TRESHOLD_1 = (input$n*60)
    URG_1 = subset(URG_1, URG_1$TIEMPO< TRESHOLD_1)
    URG_1$FECHA_HORA = as.Date(URG_1$FECHA_LLEGADA, "%d%m%y", tz="UTC")
    URG_1 = subset(URG_1,URG_1$FECHA_HORA>=input$dateRangeALL[1]&URG_1$FECHA_HORA<=input$dateRangeALL[2])
    
    plot(aggregate(URG_1$TIEMPO,list(Mes = URG_1$MES),mean),type = "l",pch = 15,col = 40,ylab = "Tiempo medio", main = "Tiempos medios de urgencias")
    abline(h=mean(URG_1$TIEMPO),col = "blue")
    abline(h=URG_MEAN_BASE, col = "brown")
    abline(h=(URG_MEAN_BASE - (URG_MEAN_BASE*URG_OBJ_PER/100)), col = "orange")
    par(xpd=TRUE)
    legend(x="bottom",xpd = TRUE,horiz = TRUE, c("Base","Actual","Objetivo","Medio"),lty = c(1,1,1,1),lwd=c(2.5,2.5,2.5,2.5),col=c("brown","blue","orange","grey"))
  }) 
  
  output$distHistUrg<- renderPlot({
    
    TRESHOLD_2 = (input$n*60)
    URG_2$FECHA_HORA2 = as.Date(URG_2$FECHA_LLEGADA, "%d%m%y", tz="UTC")
    URG_2 = subset(URG_2, URG_2$TIEMPO< TRESHOLD_2)
    
    URG_2 = subset(URG_2,URG_2$FECHA_HORA2>=input$dateRangeALL[1]&URG_2$FECHA_HORA2<=input$dateRangeALL[2])
    hist(URG_2$TIEMPO,
         col = 'darkblue', 
         breaks = 180,
         main="Histograma de tiempos de urgencia",
         xlab="Tiempos en urgencias",
         ylab="Frecuencia")
  })
  
  output$barplotUrg <- renderPlot({
    
    TRESHOLD_3 = (input$n*60)
    URG_3$FECHA_HORA2 = as.Date(URG_3$FECHA_LLEGADA, "%d%m%y", tz="UTC")
    URG_3 = subset(URG_3, URG_3$TIEMPO< TRESHOLD_3)
    
    URG_3 = subset(URG_3,URG_3$TIPO_EPISODIO!="Recien Nacido")
    URG_3 = subset(URG_3,URG_3$FECHA_HORA2>=input$dateRangeALL[1]&URG_3$FECHA_HORA2<=input$dateRangeALL[2])
    barplot(table(URG_3$TIPO_EPISODIO,URG_3$MES),
            col = c('green','darkblue','darkblue'),
            main="Número de entradas en urgencias por tipo de episodio",
            xlab="Tipo",
            ylab="Pacientes")
            #legend =  rownames(table(URG_3$TIPO_EPISODIO,URG_3$MES)))
    legend(x="topright",xpd = TRUE,horiz = TRUE, c("No hospitalizados","Hospitalizados"),lty = c(1,1),lwd=c(2.5,2.5),col=c("blue","green"))
  })
  
  output$ggplotUrg<- renderPlot({
    TRESHOLD_4 = (input$n*60)
    URG_4$FECHA_HORA2 = as.Date(URG_4$FECHA_LLEGADA, "%d%m%y", tz="UTC")
    URG_4 = subset(URG_4, URG_4$TIEMPO< TRESHOLD_4)
    URG_4 = subset(URG_4,URG_4$TIPO_EPISODIO!="Recien Nacido")
    URG_4 = subset(URG_4,URG_4$FECHA_HORA2>=input$dateRangeALL[1]&URG_4$FECHA_HORA2<=input$dateRangeALL[2])
    
    URG_4_AGG = aggregate(URG_4$TIEMPO,list(Mes = URG_4$MES,TIP = URG_4$TIPO_EPISODIO),mean)
    URG_4_AGG = subset(URG_4_AGG,URG_4_AGG$TIP!="Recien Nacido")
    URG_4_AGG$Mes = factor(URG_4_AGG$Mes)
    URG_4_AGG$TIP = factor(URG_4_AGG$TIP)
    
    #URG_4_AGG1 = subset(URG_4_AGG,URG_4_AGG$TIP=="Hospitalización")
    #URG_4_AGG2 = subset(URG_4_AGG,URG_4_AGG$TIP!="Hospitalización")
    

#     p <- plot_ly(
#       x = URG_4_AGG1$Mes,
#       y = URG_4_AGG1$x,
#       name = "Hospitalizados",
#       type = "bar",
#       filename="r-docs/simple-bar"
#     )
#     p    
#     p2 <- add_trace(
#       p,
#       x = URG_4_AGG2$Mes,
#       y = URG_4_AGG2$x,
#       name = "No hospitalizados",
#       filename="r-docs/simple-bars"
#     )
#     p2
    #URG_4_AGG<<-URG_4_AGG    
    #environment = environment()
    
    #URG_4_AGG$x = factor(URG_4_AGG$x)
    #URG_4_AGG$Mes = factor(URG_4_AGG$Mes)
    #URG_4_AGG$TIP = factor(URG_4_AGG$TIP)
    ggplot(URG_4_AGG, aes(x = URG_4_AGG$Mes, y = URG_4_AGG$x, fill=URG_4_AGG$TIP), environment=environment()) + 
      geom_bar(stat = "identity", position=position_dodge()) +
      geom_text(aes(y=(URG_4_AGG$x)-10, ymax=URG_4_AGG$x, label=round(URG_4_AGG$x,0.01)), position= position_dodge(width=1), color="black") +
      scale_y_continuous("Minutos",limits=c(0,500)) + 
      scale_x_discrete("Meses") +
      geom_hline(yintercept=URG_MED_1, color = "brown")+
      geom_hline(yintercept=URG_MED_2, color = "orange")+
      ggtitle(expression(atop(bold("Tiempo medio en urgencias por tipo de episodio"), atop(italic("Referencia: Tiempo de puerta a doctor para urgencias leves(120) y de registro a ingreso (240) del Ministerio de Sanidad"), "")))) +
      #scale_fill_discrete(name ="Tipo Episodio", labels=c("Hospitalizados", "No hospitalizados"))+ theme_classic()
      scale_fill_manual(values = c("green", "blue"),
                        name= "Tipo episodio", 
                        labels=c("Hospitalizados", "No hospitalizados"),
                        guide = guide_legend(reverse = TRUE))+ theme_classic() + theme(legend.position="top")
    
    })
############################FALLS#############################  
  output$distHistFalls<- renderPlot({
  
    CAIDAS_1 = subset(CAIDAS_1,CAIDAS_1$FECHA_HORA>=input$dateRangeALL[1]&CAIDAS_1$FECHA_HORA<=input$dateRangeALL[2])
    HIST = table(CAIDAS_1$MES)
    hist(HIST,
         breaks = 5, 
         col = 'darkblue', 
         ylim=c(0,6),
         xlim = c(0,20),
         main="Histograma de caídas al mes",
         xlab="Caidas al mes",
         ylab="Frecuencia")
  })
  
  output$distPlotFalls <- renderPlot({
    CAIDAS_3 = subset(CAIDAS_3,CAIDAS_3$FECHA_HORA>=input$dateRangeALL[1]&CAIDAS_3$FECHA_HORA<=input$dateRangeALL[2])
    BP<-barplot(table(CAIDAS_3$MES),
                col = 'darkblue', 
                border = 'white',
                main="Número de caidas por mes y estancias",
                xlab="Meses",
                ylab="Caidas")
    text(BP,c(1,1),table(CAIDAS_3$MES),col = "white")
    par(new=TRUE)
    plot(ESTAN$MES,ESTAN$ESTAN1,type="o",col="brown",pch=17,xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("Estancias",side=4,line=3)
  })
  
  output$distPlotFallsPW <- renderPlot({
    CAIDAS_2 = subset(CAIDAS_2,CAIDAS_2$FECHA_HORA>=input$dateRangeALL[1]&CAIDAS_2$FECHA_HORA<=input$dateRangeALL[2])
    PP = table(CAIDAS_2$POWERPLAN)
    CAID = sum(CAIDAS_2$CAIDA)
    FRAME <- as.data.frame(PP)
    x <- FRAME$Freq
    names(x) <- FRAME$Var1
    pct <- round(x/sum(x)*100)
    pct <- paste(names(x), pct)
    pct <- paste(pct, "%", sep="")
    pie3D(PP,
          main = paste0("Caídas: ",CAID),labels = pct
          #col=c("darkblue","green","blue","darkblue","darkgreen","brown")
          )
    
  })
  
  output$distPlotFallsLO <- renderPlot({
    CAIDAS_4 = subset(CAIDAS_4,CAIDAS_4$FECHA_HORA>=input$dateRangeALL[1]&CAIDAS_4$FECHA_HORA<=input$dateRangeALL[2])
    PP = table(CAIDAS_4$LUGAR)
    FRAME <- as.data.frame(PP)
    x <- FRAME$Freq
    names(x) <- FRAME$Var1
    pct <- round(x/sum(x)*100)
    pct <- paste(names(x), pct)
    pct <- paste(pct, "%", sep="")
    pie(PP,label = pct,main = "Porcentaje de caídas por lugar",col=c("darkblue","green","blue","darkblue","darkgreen","brown"))
    
  })  
  
  output$distPlotFallsUB <- renderPlot({
    CAIDAS_5 = subset(CAIDAS_5,CAIDAS_5$FECHA_HORA>=input$dateRangeALL[1]&CAIDAS_5$FECHA_HORA<=input$dateRangeALL[2])
    PP = table(CAIDAS$UBICACION)
    FRAME <- as.data.frame(PP)
    x <- FRAME$Freq
    names(x) <- FRAME$Var1
    pct <- round(x/sum(x)*100)
    pct <- paste(names(x), pct)
    pct <- paste(pct, "%", sep="")
    pie(PP,label = pct,main = "Porcentaje de caídas por ubicación",col=c("darkblue","green","blue","darkblue","darkgreen","brown"))
    
  })
  
  output$distRadarFalls<- renderPlot({

    #CAIDAS_6 = subset(CAIDAS_6,CAIDAS_6$FECHA_HORA>=input$dateRangeALL[1]&CAIDAS_6$FECHA_HORA<=input$dateRangeALL[2])
    radar <- ggplot(CAIDAS_6, aes(factor(CAIDAS_6$HORA)), environment=environment()) +  geom_bar(fill="darkblue",width = 1, colour = "green")
    radar + xlab("Horas")+ ylab("Caidas")+ coord_polar()+ ggtitle("Total caídas")+ theme_bw()#theme_classic()

  }) 
  
  output$distRadarFallsSERV<- renderPlot({
    
    CAIDAS_61 = subset(CAIDAS_6,CAIDAS_6$ACTIVIDAD=="Ir al servicio")
    radar <- ggplot(CAIDAS_61, aes(factor(CAIDAS_61$HORA)), environment=environment()) +  geom_bar(fill="darkblue",width = 1, colour = "green")
    radar + xlab("Horas")+ ylab("Caidas")+ coord_polar()+ ggtitle("Actividad: Ir al servicio")+ theme_bw()#theme_classic()
    
  }) 
  
  output$distRadarFallsLEV<- renderPlot({
    
    CAIDAS_62 = subset(CAIDAS_6,CAIDAS_6$ACTIVIDAD=="Levantarse")
    radar <- ggplot(CAIDAS_62, aes(factor(CAIDAS_62$HORA)), environment=environment()) +  geom_bar(fill="darkblue",width = 1, colour = "green")
    radar + xlab("Horas")+ ylab("Caidas")+ggtitle("Actividad: Levantarse")+ coord_polar() + theme_bw()#theme_classic()
    
  }) 
  output$distRadarFallsACO<- renderPlot({
    
    CAIDAS_63 = subset(CAIDAS_6,CAIDAS_6$ACTIVIDAD=="Acostarse")
    radar <- ggplot(CAIDAS_63, aes(factor(CAIDAS_63$HORA)), environment=environment()) +  geom_bar(fill="darkblue",width = 1, colour = "green")
    radar + xlab("Horas") + ylab("Caidas")+ ggtitle("Actividad: Acostarse")+ coord_polar() + theme_bw()#theme_classic()
    
  }) 
  ##################UPP#######################################################  
  output$distPlotUPP <- renderPlot({
    
    UPP_1 = subset(UPP_1,UPP_1$FECHA_VALORACION>=input$dateRangeALL[1]&UPP_1$FECHA_VALORACION<=input$dateRangeALL[2])
    barplot(table(UPP_1$RIESGO,UPP_1$BRADEN),
            col = c('darkblue','green'),
            main="Número de pacientes por valoración",
            xlab="Braden",
            ylab="Pacientes",
            beside=TRUE,
            legend =  rownames(table(UPP_1$RIESGO,UPP_1$BRADEN)))
  })
  
  output$distPlotUPP2 <- renderPlot({
    UPP_2 = subset(UPP_2,UPP_2$FECHA_VALORACION>=input$dateRangeALL[1]&UPP_2$FECHA_VALORACION<=input$dateRangeALL[2])
    plot(aggregate(UPP_2$UPP_HOSPITAL,list(Mes = UPP_2$MES),sum),type = "l",
         col = 'darkblue', 
         main="Úlceras por mes",
         xlab="Meses",
         ylab="Úlceras"
    )
    par(new=TRUE)
    plot(aggregate(UPP_2$UPP_FUERA,list(Mes = UPP_2$MES),sum),type = "l",col = 'green', yaxt="n",
         ann=FALSE
    )
    legend(x="bottomright", c("Nosocomiales","Fuera del hospital","Estancias"),lty = c(1,1,1),lwd=c(1,1,1),col=c("green","blue","brown"))
    #axis(4)
    #mtext("Nosocomiales",side=4,line=3)
    par(new=TRUE)
    plot(ESTAN$MES,ESTAN$ESTAN1,type="o",col="brown",xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("Estancias",side=4,line=3)
    
  })  
  
  output$distPlotUPP4 <- renderPlot({
    
    plot(PAC,type = "l", ylim = c(1,8),main = "Prevalencia de UPP")
    abline(h=7.63, col = "red")
    abline(h=4.86, col = "brown")
    abline(h=4.13, col = "orange")
    legend(x="bottom",horiz = TRUE,c("Prev. adultos ENP","Prevalencia Base","Prevalencia","Objetivo"),lty = c(1,1),lwd=c(2.5,2.5),col=c("red","brown","grey","orange"))
  })

  output$distPlotUPP3 <- renderPlot({
    UPP_2 = subset(UPP_2,UPP_2$FECHA_VALORACION>=input$dateRangeALL[1]&UPP_2$FECHA_VALORACION<=input$dateRangeALL[2])
    UPP_21 <- aggregate(UPP_2$UPP_FUERA,list(Mes = UPP_2$MES),sum)
    UPP_22 <- aggregate(UPP_2$UPP,list(Mes = UPP_2$MES),sum)
    UPP_22$UPP <- UPP_21$x
    A <- UPP_22$UPP
    B <- UPP_22$x
    D <- UPP_22$Mes
    
    C <-rbind(A,B)
    barplot(C,beside = TRUE,names.arg = D,col=c("green","darkblue"))
    legend(x="bottom",horiz = TRUE, c("Nosocomiales","Fuera del hospital","Estancias"),lty = c(1,1,1),lwd=c(1,1,1),col=c("green","blue","brown"))
    #axis(4)
    #mtext("Nosocomiales",side=4,line=3)
    par(new=TRUE)
    plot(ESTAN$MES,ESTAN$ESTAN1,type="o",col="brown",xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("Estancias",side=4,line=3)
    
  }) 
  ########################MEDERR########################################
  output$distPlotMEDERR <- renderPlot({
    MEDERR1 = subset(MEDERR1,MEDERR1$FECHA_ENTRADA2>=input$dateRangeALL[1]&MEDERR1$FECHA_ENTRADA2<=input$dateRangeALL[2])
    TABLA = table(MEDERR1$TIPO,MEDERR1$MES)
    
    MP<-barplot(TABLA,
                col=c("grey","green","darkblue"),
                border = 'white',
                main="Número de alertas por mes",
                xlab="Mes",
                ylab="Alertas por mes"
                #legend = c("ALERGIA","INCOMPATIBLE","DUPLICADA")
                )
    legend(x="bottom",horiz = TRUE, c("Estancias","ALERG","INCOMP","DUPL"),pch = c(17,15,15,15),lty = c(1,1,1,1),lwd=c(1,1,1,1),col=c("brown","grey","green","blue"))
    par(new=TRUE)
    plot(ESTAN$MES,ESTAN$ESTAN1,type="o",col="brown",pch=17,xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("Estancias",side=4,line=3)
    text(MP,c(1,1),TABLA,col = "white")
    
  })
  
  output$distPlotMEDERRORD <- renderPlot({
    MEDERR1 = subset(MEDERR1,MEDERR1$FECHA_ENTRADA2>=input$dateRangeALL[1]&MEDERR1$FECHA_ENTRADA2<=input$dateRangeALL[2])
    TABLA = table(MEDERR1$TIPO,MEDERR1$MES)
    
    MP<-barplot(TABLA,
                col=c("grey","green","darkblue"),
                border = 'white',
                main="Número de alertas por mes y por prescripción",
                xlab="Mes",
                ylab="Alertas por mes"
                #legend = c("ALERGIA","INCOMPATIBLE","DUPLICADA")
    )
    legend(x="bottom",horiz = TRUE, c("PRESCR","ALERG","INCOMP","DUPL"),pch = c(17,15,15,15),lty = c(1,1,1,1),lwd=c(1,1,1,1),col=c("brown","grey","green","blue"))
    par(new=TRUE)
    plot(ORD$MES,ORD$ORD1,type="o",col="brown",pch=17,xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("Prescripciones",side=4,line=3)
    text(MP,c(1,1),TABLA,col = "white")
  })
  
  output$distPlotMEDERREAM <- renderPlot({
    MEDERR1 = subset(MEDERR1,MEDERR1$FECHA_ENTRADA2>=input$dateRangeALL[1]&MEDERR1$FECHA_ENTRADA2<=input$dateRangeALL[2])
    TABLA = table(MEDERR1$TIPO,MEDERR1$MES)
    
    MP<-barplot(TABLA,
                col=c("grey","green","darkblue"),
                border = 'white',
                main="Número de alertas por mes y por eventos de medicación adversa",
                xlab="Mes",
                ylab="Alertas por mes"
                #legend = c("ALERGIA","INCOMPATIBLE","DUPLICADA")
    )
    legend(x="bottom",horiz = TRUE, c("EMA","ALERG","INCOMP","DUPL"),pch = c(17,15,15,15),lty = c(1,1,1,1),lwd=c(1,1,1,1),col=c("brown","grey","green","blue"))
    par(new=TRUE)
    plot(RAM$MES,RAM$RAM1,type="o",col="brown",pch=17,xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    mtext("Prescripciones",side=4,line=3)
    text(MP,c(1,1),TABLA,col = "white")
  })
  
  output$distPlotMEDERRPOS <- renderPlot({
    MEDERR2 = subset(MEDERR2,MEDERR2$FECHA_ENTRADA2>=input$dateRangeALL[1]&MEDERR2$FECHA_ENTRADA2<=input$dateRangeALL[2])
    MEDERR2 = subset(MEDERR2,MEDERR2$POSICION=="Médico"|MEDERR2$POSICION=="Farmaceútico")
    POSI = table(MEDERR2$POSICION)
    FRAME <- as.data.frame(POSI)
    x <- FRAME$Freq
    names(x) <- FRAME$Var1
    pct <- round(x/sum(x)*100)
    pct <- paste(names(x), pct)
    pct <- paste(pct, "%", sep="")
    pie3D(POSI,main = "Porcentaje de alertas por posición",col = c("darkblue","green"),labels = pct)
    
  })
})
