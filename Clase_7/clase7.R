# Ejercicio A
# Creacion de objetos
a <- 1
b = 2
suma = a + b
c <- suma / b
d <- 8
D <- 9
# sobreescritura
f <- 5
f <- 6
#Ejemplos de sintaxis
a <- 1
b <- 3
r <- a + b
s <- b - a
p <- s*r
q <- sqrt(p)
r
##[1]4
s
##[1]2
p
##[1]8
q
##[1]2.828427

# Vectores
nom <- c("Manuel", "Eduardo")
x <- c(1,4,3,8)
num1 <- c(1,2,3,4,5)
num2 <- 1:5
num1
##[1]1 2 3 4 5
num2
##[1]1 2 3 4 5
#concatenacion
num3 <- c(num1,num2)
num3
##[1]1 2 3 4 5 1 2 3 4 5
str(num1)
##num[1:5]1 2 3 4 5

#Listas
lista <- list("Valores de un dado",1:6)
lista[1]
lista[2]
lista[[2]][3]
lista[[2]] > 3.5
lista1 <- list("Valores de un dado", 1:6)
lista2 <- list("Valores de una moneda", c("cara", "cruz"))
lista <- c(lista1, lista2)

#Dataframes en R
df <- data.frame(nombres <- c("Claudia", "Gador", "Alba"))
edades <- c(8, 9, 12)
str(df)
df
df$nombres
df$edades
df[[1]]; df[[2]]

#funcion attach
attach(df)
nombres
edades

#datos de texto
ejemplo <- read.table("ejemplo.dat", header=TRUE)
names(ejemplo)
matriz <- matrix(c(1,2,3,4,5,6), nrow=3)
write.table(matriz, "matriz.dat", row.names=F, col.names=F)

#Datos CSV
a <- as.data.frame(matrix(seq(0.5,25,0.5), nrow=10, ncol=5))
write.csv(a, "matriz1.csv", row.names=FALSE)
write.csv2(a, "matriz2.csv", row.names=FALSE)
a1 <- read.csv("matriz1.csv")
a2 <- read.csv2("matriz2.csv")

# Manipulacion de dataframes
temp <- c(20.37, 18.56, 18.4, 21.96, 29.53, 28.16,
          36.38, 36.62, 40.03, 27.59, 22.15, 19.85)
humedad <- c(88, 86, 81, 79, 80, 78,
             71, 69, 78, 82, 85, 83)
precipitaciones <- c(72, 33.9, 37.5, 36.6, 31.0, 16.6,
                     1.2, 6.8, 36.8, 30.8, 38.5, 22.7)
mes <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", 
         "diciembre")

datos <- data.frame(mes = mes, temperatura = temp, humedad = 
                      humedad,
                    precipitaciones = precipitaciones)
names(datos) # Nombres de las variables (columnas)

# Primeras filas de nuestro conjunto de datos
head(datos)
#Output
# mes temperatura humedad precipitaciones
#1 enero 20.37 88 72.0
#2 febrero 18.56 86 33.9
#3 marzo 18.40 81 37.5
#4 abril 21.96 79 36.6
#5 mayo 29.53 80 31.0
#6 junio 28.16 78 16.6

summary(datos)
# Resumen descriptivo de los datos
#mes temperatura humedad precipitaciones
#april :1 Min. :18.40 Min. :69.0 Min. : 1.20
#august :1 1st Qu.:20.24 1st Qu.:78.0 1st Qu.:21.18
#december:1 Median :24.87 Median :80.5 Median :32.45
#february:1 Mean :26.63 Mean :80.0 Mean :30.37
#january :1 3rd Qu.:31.24 3rd Qu.:83.5 3rd Qu.:36.98



## EJERCICIO B
# ejemplo de dato de texto
dato_texto <- read.table("~/R Scripts/nombress.txt", header = TRUE)
dato_texto
# ejemplo de dato de csv
dato_csv <- read.csv("~/R Scripts/profesiones.csv", header = TRUE)
dato_csv



## EJERCICIO C
natalidad_mexico <- read.csv("~/R Scripts/Natalidad_Mexico.csv", header = TRUE)

# Agregar una nueva columna con el porcentaje de hombres sobre el total
natalidad_mexico$porcentaje_hombres <- (natalidad_mexico$Hombres / natalidad_mexico$Total) * 100

# Eliminar la columna 'X'
natalidad_mexico <- natalidad_mexico[ , !names(natalidad_mexico) %in% c("X")]

# Filtrar los datos para el año 2018
natalidad_2018 <- subset(natalidad_mexico, year == 2018)

# Ordenar los datos por el total de nacimientos de mayor a menor
natalidad_ordenada <- natalidad_mexico[order(-natalidad_mexico$Total), ]

# Ver los primeros registros después de las modificaciones
head(natalidad_ordenada)



## EJERCICIO D

# Funcion Pie
# Filtrar los datos del año 2018
natalidad_2018 <- subset(natalidad_mexico, year == 2018)

# Sumar los nacimientos de hombres y mujeres en 2018
total_hombres_mujeres <- c(sum(natalidad_2018$Hombres), sum(natalidad_2018$Mujeres))

# Crear etiquetas para el gráfico de pastel
etiquetas <- c("Hombres", "Mujeres")

# Generar gráfico de pastel
pie(total_hombres_mujeres, labels = etiquetas, main = "Distribución de nacimientos en 2018", col = c("blue", "pink"))


# Función Barplot
# Crear un gráfico de barras de nacimientos totales por estado en 2018
barplot(natalidad_2018$Total, names.arg = natalidad_2018$estado, las = 2, col = "lightblue", 
        main = "Total de nacimientos por estado en 2018", ylab = "Total de nacimientos", cex.names = 0.7)


# Función Hist
# Crear un histograma de nacimientos totales en todos los años y estados
hist(natalidad_mexico$Total, breaks = 20, col = "green", 
     main = "Distribución de nacimientos totales", xlab = "Total de nacimientos", ylab = "Frecuencia")
