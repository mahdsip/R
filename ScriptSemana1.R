#Usar variables
SquareRoot2 = sqrt(2) o SquareRoot2 <- sqrt(2)
SquareRoot2 -> Te muestra el contenido.
#se pueden realizar directamente operaciones matematicas del tipo 5*6 o sqrt() para raiz cuadrada
#listar todas las variables
ls() 
#Crear un vector combine function
Country = c ("Brazil","Spain","Italy")
LifeExpentancy = c(76,87,89)
Country[1] --> "Brazil" 
#Crear un dataFrame
CountryData = data.frame(Country,LifeExpentancy)
#Añadir nueva variable al dataFrame
CountryData$Population = c (1990000,54800900,54099988)
#Combinar dataFrames
Country = c("Canada","Australia")
LFE = c(76,65)
Pop = c(13121222,4652333)
NewCountry = data.Frame(Country,LFE,Pop)

AllCountry = rbind(CountryData,NewCountryData)

#Mostar el directorio de trabajo
getWD()
#Cambiar directorio de trabajo
setwd('C:/CURSODATA')
#Cargar un csv
USDA = read.csv("USDA.csv")

str (USDA)
#Información sobre el dataset
summary(USDA)
#Información resumen sobre el dataset con minimo,1st quadrant,mediana,media,3quadrant,maximo
USDA$Sodium
#información sobre todos loc productos con sodio
which.max(USDA$Sodium) 
#posicion dentro del vector con maximo valor de sodio
names(USDA)
#Nombre de los campos
USDA$Description[265]
#valor 265 dentro del vector Descripcion para saber el nombre de maximo valor de sodio
HightSodium = subset (USDA,Sodium >10000)
#Subconjunto con los alimentos que superan en 10000 en sodio en 100 gr
row(HighSodium) 
#Numero de filas devueltas
HightSodium$Description
#lista los valores de los alimentos con > 10000
match("CAVIAR",USDA$Description)
#Busca el elemento y te da la posicion
USDA$Sodium[4154]
#Te da el valor del sodio en el caviar
USDA$Sodium[match"CAVIAR",USDA$Description)
#Hace lo mismo
sd(USDA$Sodium,na.rm=TRUE)
#Desviacion estandar quitando nulos
mean(USDA$Sodium,na.rm=TRUE)
#media quitando nulos

plot(USDA$Protein,USDA$TotalFat)
#tabla con x e y
hist(USDA$VitaminC, xlim= c(0,100), breaks=2000)
#Histograma con frecuencias de una variable
boxplot(USDA$Sugar, main = "Boxplor of sugar level", ylab = "Sugar in labs")
#grafica que muestra minimo, maximo, media y outliers


#Añadir una variable al dataset, por ejemplo 0 o 1 si supera o no 1000 dew sodio
USDA$HightFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat,na.rm=TRUE))

table(USDA$HightSodium)
#Resumen de todos cada ocurrencia con todos lo valores posibles

table(USDA$HightSodium,USDA$HightFat)
#Cruza los datos en la tabla.

tapply(USDA$Iron,USDA$HightProtein,mean,na.rm=TRUE)
#Agrupa en un campo por la agrupacion hecha en el primer campo, esto agrupa
#por alta o baja proteina la media de hierro para cada agrupacion. 
