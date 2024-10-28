# cargamos dataframe
datos <- data.frame(
  PHP = c(13, 13, 13, 15, 16, 15, 12, 13, 13, 13, 11, 14, 15, 15, 15),
  Algoritmos = c(15, 14, 16, 20, 18, 16, 13, 16, 15, 14, 12, 16, 17, 19, 13),
  Base_de_Datos = c(15, 13, 13, 14, 18, 17, 15, 14, 14, 13, 12, 11, 16, 14, 15),
  Programacion = c(13, 12, 14, 16, 17, 15, 11, 15, 13, 10, 10, 14, 15, 16, 10)
)
# ajustar el modelo de regresion lineal multiple
reg_model <- lm(PHP ~ Algoritmos + Base_de_Datos + Programacion, data = datos)

# ver el resumen del modelo
summary(reg_model)

# Supongamos nuevos valores para Algoritmos, Base de Datos y Programación
nuevos_datos <- data.frame(
  Algoritmos = c(17, 18, 19),
  Base_de_Datos = c(15, 16, 17),
  Programacion = c(14, 15, 16)
)

# Predecir las notas de PHP
prediccion <- predict(reg_model, nuevos_datos)
print(prediccion)


