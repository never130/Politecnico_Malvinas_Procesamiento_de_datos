## EJERCICIO C
# Elegir un dataset y aplicar modelos de regresión lineal
# El dataset viene por defecto en R. Se puede exportar mediante archivo CSV
write.csv(mtcars, file = "mtcars.csv",  row.names = FALSE)

data(mtcars)
head(mtcars)
cor(mtcars)

# aplicacion del modelo
regMod <- lm(mpg ~ hp + disp + wt, data=mtcars)
summary(regMod)

# Nuevos valores para predecir
nuevos_datos <- data.frame(hp = c(100, 150), 
                           disp = c(200, 300), 
                           wt = c(2.5, 3.0))
# Prediccion
predicciones <- predict(regMod, nuevos_datos)
print(predicciones)


## EJERCICIO D
#  Crear modelos de regresión múltiple con distintas combinaciones de variables
# Modelo 1: mpg ~ wt + hp
regMod1 <- lm(mpg ~ wt + hp, data=mtcars)
summary(regMod1)

# prediccion Modelo 1
nuevos_datos1 <- data.frame(wt = 3.0, hp = 150)
prediccion1 <- predict(regMod1, nuevos_datos1)
print(prediccion1)

# Modelo 2: mpg ~  wt
regMod2 <- lm(mpg ~ wt, data=mtcars)
summary(regMod2)

# prediccion Modelo 2
nuevos_datos2 <- data.frame(wt = 3.0)
prediccion2 <- predict(regMod2, nuevos_datos2)
print(prediccion2)
