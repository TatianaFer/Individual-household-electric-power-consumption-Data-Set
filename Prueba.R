#################################
## Prueba técnica
#################################
# Elaborado por: Tatiana Fernández
# Día de elaboración: 18-03-2019
#################################

# # Directorios de trabajo
inPath  <- file.path("..", "input")
outPath <- file.path("..", "output")
srcPath <- file.path("..", "src")

################################################################################
# # Librerias
################################################################################
install.packages("tidyverse")
library(plyr)
library(dplyr)
library(reshape2)
library(xlsx)
library(ggplot2)
library(data.table)
library(lubridate)
library(inegiR)
library(forecast)

################################################################################
# # Lectura de las bases
################################################################################
Data <- fread(file.path(inPath, "household_power_consumption.txt"), header=TRUE, sep=";", dec= ".",
					 na.strings = "?", colClasses = c("character","character",rep("numeric",7)))  

#Data$Date <- as.Date(gsub("/", "-", Data$Date), format="%d-%m-%Y")
Data$Date <- as.Date(Data$Date, "%d/%m/%Y")
Data$Date_Time <- paste(Data$Date, Data$Time)
Data$Date_Time <- strptime(Data$Date_Time, "%d/%m/%Y %H:%M:%S")
# Como los tres subgrupos no son los unicos en el hogar es necesario saber cuanta energía gastan los otros 
Data$Sub_4 = (Data$Global_active_power * 1000 / 60) - (Data$Sub_metering_1 + Data$Sub_metering_2 + Data$Sub_metering_3)
mydata <- na.omit(Data) # Eliminar los NA

#Guardar la base limpia 
save(mydata,file="Base.RData")
# Estadísticos descriptivos
xtable(summary(mydata[, c(3:9)]))
# corelaciones
xtable(cor(mydata[, c(3:9)]))	
cor(mydata$Global_active_power,mydata$Global_intensity)	
# Gráficos iniciales 

with(mydata, hist(Global_active_power, main = "Energía activa global", 
    xlab = "kilovatios)", ylab = "Frecuencia", col = "blue"))
plot(x = mydata$Date_Time, y = mydata$Global_active_power, type = "l", 
    xlab = "", ylab = "Global Active Power (kilowatts)") 
    
plot(x = mydata$Date_Time, y = mydata$Sub_metering_1, type = "n", 
    xlab = "", ylab = "Energy sub metering")
lines(x = mydata$Date_Time, y = mydata$Sub_metering_1)
lines(x = mydata$Date_Time, y = mydata$Sub_metering_2, col = "red")
lines(x = mydata$Date_Time, y = mydata$Sub_metering_3, col = "blue")
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
    lty = 1, col = c("black", "red", "blue"))


Mes <- mydata %>% 
  group_by(year(Date), month(Date)) %>%                       
  summarise(Acti=mean(Global_active_power),
  			Reac=mean(Global_reactive_power),
  			Volt=mean(Voltage),
			Inte=mean(Global_intensity),
  			Sub1 = mean(Sub_metering_1),
            Sub2=mean(Sub_metering_2), 
            Sub3=mean(Sub_metering_3), 
            Sub4=mean(Sub_4))

Semana <- mydata %>% 
  group_by(year(Date), week(Date)) %>%                       
  summarise(Acti=mean(Global_active_power),
  			Reac=mean(Global_reactive_power),
  			Volt=mean(Voltage),
			Inte=mean(Global_intensity),
  			Sub1 = mean(Sub_metering_1),
            Sub2=mean(Sub_metering_2), 
            Sub3=mean(Sub_metering_3), 
            Sub4=mean(Sub_4))


### Serie mensual para cada año
tserie=ts(Mes,freq=12,start=c(2006,12))
str(tserie)
Fileplot <- file.path(outPath, "Series_anual.png")
png(Fileplot, width = 700, height = 700)
plot <- plot(tserie[,3:10],main="Comportamiento del consumo de energía por año", xlab = "Años", ylab = "Kilovatios promedio")
dev.off()


### Serie semanal 

tserieS=ts(Semana,freq=48,start=c(2006,12))
str(tserieS)
Fileplot <- file.path(outPath, "Series_seman.png")
png(Fileplot, width = 700, height = 700)
plot <- plot(tserieS[,3:10],main="Comportamiento del consumo de energía por año", xlab = "Años", ylab = "Kilovatios promedio")
dev.off()

# Series anuales por cada variable
tserie1 <- ts(Mes$Acti, frequency = 12, start=c(2006,12))
plot(tserie1, main = "Energía Activa", xlab = "Años", ylab = "Kilovatios promedio")
tserie2 <- ts(Mes$Reac, frequency = 12, start=c(2006,12))
plot(tserie2)
tserie3 <- ts(Mes$Volt, frequency = 12, start=c(2006,12))
plot(tserie3)
tserie4 <- ts(Mes$Inte, frequency = 12, start=c(2006,12))
plot(tserie4)
tserie5 <- ts(Mes$Sub1, frequency = 12, start=c(2006,12))
plot(tserie5)
tserie6 <- ts(Mes$Sub2, frequency = 12, start=c(2006,12))
plot(tserie6)
tseries7 <- ts(Mes$Sub3, frequency = 12, start=c(2006,12))
plot(tseries7)
tseries8 <- ts(Mes$Sub4, frequency = 12, start=c(2006,12))
plot(tseries8)


# Comparar la distribución del consumo para cada mes
BXplot1File1 <- file.path(outPath, "BX_Activa.png")
png(BXplot1File1, width = 700, height = 700)
BXplot1 <- boxplot(tserie1 ~ cycle(tserie1), main = "Energía Activa", xlab = "Meses", 
				   ylab ="Kilovatios promedio", col = c("dodgerblue1", "dodgerblue3") )
dev.off()

BXplot1File2 <- file.path(outPath, "BX_Reactiva.png")
png(BXplot1File2, width = 700, height = 700)
BXplot2 <- boxplot(tserie2 ~ cycle(tserie2), main = "Energía Reactiva", xlab = "Meses", 
				  ylab ="Kilovatios promedio",col = c("dodgerblue1", "dodgerblue3") )
dev.off()

BXplot1File3 <- file.path(outPath, "BX_Voltaje.png")
png(BXplot1File3, width = 700, height = 700)
BXplot3 <- boxplot(tserie3 ~ cycle(tserie3), main = "Voltaje", xlab = "Meses", 
				  ylab ="Voltios promedio",col = c("dodgerblue1", "dodgerblue3") )
dev.off()

BXplot1File4 <- file.path(outPath, "BX_Inten.png")
png(BXplot1File4, width = 700, height = 700)
BXplot4 <- boxplot(tserie4 ~ cycle(tserie4), main = "Intensidad de la corriente global", 
			xlab = "Meses", ylab ="Amperios promedio", col = c("dodgerblue1", "dodgerblue3") )
dev.off()

BXplot1File5 <- file.path(outPath, "BX_Cocina.png")
png(BXplot1File5, width = 700, height = 700)
BXplot5 <- boxplot(tserie5 ~ cycle(tserie5), main = "Consumo de energía eléctrica en la cocina", 
			xlab = "Meses", ylab ="Kilovatios promedio", col = c("dodgerblue1", "dodgerblue3") )
dev.off()

BXplot1File6 <- file.path(outPath, "BX_Lavanderia.png")
png(BXplot1File6, width = 700, height = 700)
BXplot6 <- boxplot(tserie6 ~ cycle(tserie6), main = "Consumo de energía eléctrica en la lavandería", 
			xlab = "Meses", ylab ="Kilovatios promedio", col = c("dodgerblue1", "dodgerblue3") )
dev.off()

BXplot1File7 <- file.path(outPath, "BX_Aire_A-Calentador.png")
png(BXplot1File7, width = 700, height = 700)
BXplot7 <- boxplot(tseries7 ~ cycle(tseries7), main = "Consumo de energía eléctrica en el aire acondicinado y el calentador", 
			xlab = "Meses", ylab ="Kilovatios promedio", col = c("dodgerblue1", "dodgerblue3") )
dev.off()

BXplot1File8 <- file.path(outPath, "BX_resto.png")
png(BXplot1File8, width = 700, height = 700)
BXplot8 <- boxplot(tseries8 ~ cycle(tseries8), main = "Consumo de energía eléctrica en el aire acondicinado y el calentador", 
			xlab = "Meses", ylab ="Kilovatios promedio", col = c("dodgerblue1", "dodgerblue3") )
dev.off()



Year <- mydata %>% 
  group_by(year(Date)) %>%                       
  summarise(Acti=mean(Global_active_power),
  			Reac=mean(Global_reactive_power),
  			Volt=mean(Voltage),
			Inte=mean(Global_intensity),
  			Sub1 = mean(Sub_metering_1),
            Sub2=mean(Sub_metering_2), 
            Sub3=mean(Sub_metering_3), 
            Sub4=mean(Sub_4))


### Serie mensual para cada año
tserieY=ts(Year,freq=12,start=c(2006,12))
str(tserieY)
Fileplot <- file.path(outPath, "Series_Año.png")
png(Fileplot, width = 700, height = 700)
plot <- plot(tserieY[,3:8],main="Comportamiento del consumo de energía por año", xlab = "Años", ylab = "Kilovatios promedio")
dev.off()


# Descomposición de la serie
 #	Serie observada = Tendencia + Efecto estacional + Residuos.
daas = decompose(tserie1)
Filedes <- file.path(outPath, "Descomp.png")
png(Filedes, width = 700, height = 700)
plot(daas, xlab='Año', main= "Descomposición de la serie de tiempo")
	# observed: Serie de tiempo original
	# Trend: Componente de tendencia estimado
	# Seasonal: Componente estacional estimado
	# Random: Componente aleatorio	
dev.off()
# Estimación de tendencia
x = log(tserie1)
dif1.x = diff(x)
plot(dif1.x)

## Split the device into four plotting regions
ResuFile <- file.path(outPath, "Resumen.png")
png(ResuFile, width = 700, height = 700)
par(mfrow = c(2,2))
plot(mydata$Global_active_power ~ mydata$DateTime, ylab = "Energía activa", xlab = "", type = "l")
plot(mydata$Voltage ~ mydata$DateTime, ylab = "Voltaje", xlab = "Tiempo", type = "l")
plot(mydata$Sub_metering_1 ~ mydata$DateTime, ylab = "Subgrupos", xlab = "", type = "l")
lines(mydata$Sub_metering_2 ~ mydata$DateTime, col = 'Red')
lines(mydata$Sub_metering_3 ~ mydata$DateTime, col = 'Blue')
legend("topright", col = c("black", "red", "blue"), legend = c("Sub1", "Sub2", "Sub3"), lwd = 1)
plot(mydata$Global_reactive_power ~ mydata$DateTime, ylab = "Energía reactiva", xlab = "Tiempo", type = "l")

dev.off()

# Ajustar un modelo para realizar predicciones 
fit <- auto.arima(tserie1)
summary(fit)
plot(fit)
pronostico<- forecast(fit,12,level=60)
ResuPron <- file.path(outPath, "Pronostico.png")
png(ResuPron, width = 700, height = 700)
plot(pronostico)
dev.off()





































day=as.Date("2017-06-14") - 0:364
value=runif(365) + seq(-140, 224)^2 / 10000
data=data.frame(day, value)
 
Data 
group_by(year(Date), week(Date))
# Split per month, one column per month
mydata %>% mutate(month = as.Date(cut(Date, breaks = "month"))) %>% 
    ggplot(aes(x=Date, y=value)) +
      geom_line() + 
      theme(
          axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),
          strip.background = element_rect(fill=alpha("slateblue",0.2)),
          strip.placement="bottom"
          ) +
      xlab("") + 
      facet_wrap(~as.Date(month), scales="free_x", nrow=1)
 
# Split per month.
data %>% mutate(month = as.Date(cut(Date, breaks = "month"))) %>% 
    ggplot(aes(x=Date, y=value, fill=as.factor(month))) +
      geom_line() + 
      geom_area() +
      theme(
          legend.position="none",
          axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),
          strip.background = element_rect(fill=alpha("slateblue",0.2)),
          strip.placement="bottom"
          ) +
      xlab("") + 
      facet_wrap(~as.Date(month), scales="free", ncol=3)
