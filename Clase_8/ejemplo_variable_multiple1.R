# ejemplo publicidad variable multiple
datos <- read.csv("~/R Scripts/Politecnico Malvinas - Procesamiento de Datos/Clase_8/advertising.csv")
head(datos)
pairs(datos)
cor(datos)
summary(datos)

# plantear y comparar distintos modelos para las variables
modelo1 <- lm(Sales ~ TV + Radio + Newspaper, data = datos)
summary(modelo1)

modelo2 <- lm(Sales ~ TV + Radio, data = datos)
summary(modelo2)

nuevos_datos <- data.frame(TV = c(230.1), Radio = c(37.8))

#pronostico las ventas del modelo
pronostico <- predict(modelo2, nuevos_datos)
pronostico

#creo otro modelo
modelo3 <- lm(Sales ~ TV, data = datos)
summary(modelo3)

nuevos_datos <- data.frame(TV = c(230.1))
#pronostico las ventas del modelo
pronostico <- predict(modelo3, nuevos_datos)
pronostico
