# crear un dataframe con los datos
datos <- data.frame(
  edad = c(34, 27, 65, 20, 53, 49, 42, 31, 55, 61),
  inseguridad = c(4.5, 3, 7, 3.5, 8, 5,4, 4, 5.5, 7.5)
)
datos

# Matriz de correlacion
correlation_matrix <- cor(datos) #en forma de matriz
# cor(datos)
pairs(datos) #en forma grafica
print(correlation_matrix)

# c) Modelo de regresion lineal
modelo <- lm(inseguridad ~ edad, data = datos)

# resumen
summary(modelo)

nuevos_datos <- data.frame(edad = c(42, 31))

#predecir, las ventas usando el modelo
pronostico <- predict(modelo, nuevos_datos)
pronostico

# valores predecidos con fitted
datos$inseguridadPredecida<- modelo$fitted.values
datos

# sacar residual
datos$residual <- datos$inseguridadPredecida - datos$inseguridad
datos
