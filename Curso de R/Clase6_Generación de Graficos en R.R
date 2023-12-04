#---------------------------------------------------------------------
#GRÁFICOS

#Para la generación de graficos vamos a emplear los conjuntos de datos que vienen por defecto
#en R en la función data()

data()

#Datos a cargar

DBO = BOD #Biochemical Oxygen Demand
summary(DBO) #Estadistica descriptiva
str(DBO) #Estructura de los datos

CO2 = CO2 #Carbon Dioxide Uptake in Grass Plants
summary(CO2) #Estadistica descriptiva
str(CO2) #Estructura de los datos

calidadAire = airquality #New York Air Quality Measurements
summary(calidadAire) #Estadistica descriptiva
str(calidadAire) #Estructura de los datos

#---------------------------------------------------------------------
#1. GRÁFICOS DE DISPERSIÓN

plot(x=DBO$Time, y=DBO$demand, type="p") #Gráfico de dispersión básico, asignación del eje x y eje y

#El tipo de gráfico tiene estas opciones: "p" punto, "l" linea, "b" linea y punto

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")")) #Asignamos el nombre del eje x y eje x

#La expresión matemática empleada para el eje y la pueden consultar en el siguiente enlace:
#https://rpubs.com/kylewbrown/math-expressions

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20)) #Vamos a ponerle límites a los ejes

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno") #Vamos asignarle un título

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7") #Vamos asignarle un subtítulo


plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16) #Vamos a cambiar el tipo de puntos

#En este enlace encuentras los tipos de puntos disponibles
#http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5) #Vamos a cambiar el tamaño de los puntos

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5) #Vamos a cambiar el tamaño de las ordenadas y abscisas

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5) #Vamos a cambiar el tamaño de las etiquetas de eje

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5) #Vamos a cambiar el tamaño del título

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5) #Vamos a cambiar el tamaño del título

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5,
     family="TT Times New Roman") #Vamos a cambiar el tipo de letra a times new roman

plot(x=DBO$Time, y=DBO$demand, type="p", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5,
     family="TT Times New Roman", col="red") #Vamos a cambiar el color de los puntos

#Mas información en:
#https://www.statmethods.net/advgraphs/parameters.html

#---------------------------------------------------------------------
#2. GRÁFICOS DE LINEAS

#Para este caso vamos a emplear el ejemplo anterior y cambiaremos el type a "l"

plot(x=DBO$Time, y=DBO$demand, type="l", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5,
     family="TT Times New Roman", col="red") #Vamos a cambiar de tipo punto a lineas


plot(x=DBO$Time, y=DBO$demand, type="l", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5,
     family="TT Times New Roman", col="red", lwd=1.5) #Vamos a cambiar el grosor de la linea

plot(x=DBO$Time, y=DBO$demand, type="l", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5,
     family="TT Times New Roman", col="red", lwd=1.5, lty=4) #Vamos a cambiar el tipo de linea

plot(x=DBO$Time, y=DBO$demand, type="b", xlab ="Tiempo (d)", ylab = expression("DBO (mg/L"*" O"[2]*")"), 
     xlim=c(0,10),ylim=c(0,20),
     main="Demanda bioquímica de oxígeno", sub="evaluada en los días 1-7",
     pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5, cex.sub=1.5,
     family="TT Times New Roman", col="red", lwd=1.5, lty=4) #Vamos a cambiar el grafico a linea y punto

#---------------------------------------------------------------------
#3. OTROS GRAFICOS DE DISPERSIÓN

plot(CO2$conc, CO2$uptake, type="p", pch=20) #Grafico de dispersión sencillo

plot(CO2$conc, CO2$uptake, type="p", pch=20, col=CO2$Treatment) #Grafico de dispersión agrupado por tipo de tratamiento

plot(CO2$conc, CO2$uptake, type="p", pch=20, col=CO2$Treatment, cex=CO2$uptake/max(CO2$uptake)) #En esta línea se maginifico el tamaño de los puntos segun capaciadad de captación (CO2 captado / maximo de CO2 captado)

plot(CO2$conc, CO2$uptake, type="p", pch=20, col=CO2$Treatment, cex=CO2$uptake/max(CO2$uptake),
     ylab=expression("CO"[2]*" capturado "* "(mg/L)"), xlab=expression("CO"[2]*" concentración "* "(mg/L)") )

#---------------------------------------------------------------------
#4. DIAGRAMAS DE CAJAS Y ALAMBRES

boxplot(Ozone~Month, data= calidadAire) #Vamos a graficar la concentración de ozono por mes
boxplot(Ozone~Month, data= calidadAire, xlab="Mes", ylab=expression("O"[3]*" ,mg/L")) #Adicionamos ejes nombres

boxplot(Ozone~Month, data= calidadAire, xlab="Mes", ylab=expression("O"[3]*" ,mg/L"), whisklty = 2) #Tipo de linea  de los alambres

boxplot(Ozone~Month, data= calidadAire, xlab="Mes", ylab=expression("O"[3]*" ,mg/L"), whisklty = 2,
        outbg = "red") #Color de los atípicos

boxplot(Ozone~Month, data= calidadAire, xlab="Mes", ylab=expression("O"[3]*" ,mg/L"), whisklty = 2,
        outbg = "red", lwd=1.1) #Grosor de lines

boxplot(Ozone~Month, data= calidadAire, xlab="Mes", ylab=expression("O"[3]*" ,mg/L"), whisklty = 2,
        outbg = "red", lwd=0.5, outpch=25) #Tipo de punto de los atípicos

boxplot(Ozone~Month, data= calidadAire, xlab="Mes", ylab=expression("O"[3]*" ,mg/L"), whisklty = 2,
        outbg = "red", lwd=0.5, outpch=25, fatten=0.5, col=rgb(0, 1, 1, alpha = 0.15)) #Color de las cajas


#---------------------------------------------------------------------
#5. CONSULTAS PARA REALIZAR GRÁFICOS SENCILLOS
#http://www.sthda.com/english/wiki/r-base-graphs

#---------------------------------------------------------------------
#6. CONSULTAS PARA REALIZAR GRÁFICOS SENCILLOS CON LA LIBRARÍA GGPLOT2
#https://rpubs.com/arvindpdmn/ggplot2-basics
