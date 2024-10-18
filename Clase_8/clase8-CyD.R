## EJERCICIO C
# Elegir un dataset y aplicar modelos de regresión lineal
# El dataset viene por defecto en R. Se puede exportar mediante archivo CSV
write.csv(mtcars, file = "mtcars.csv", row.names = FALSE)

data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl) # Convertir a factor
regMod <- lm(mpg ~ ., data=mtcars)
summary(regMod)


## EJERCICIO D
#  Crear modelos de regresión múltiple con distintas combinaciones de variables
# Modelo 1: mpg ~ disp + hp
regMod1 <- lm(mpg ~ disp + hp, data=mtcars)
summary(regMod1)

# Gráfico de residuos vs valores predichos
plot(regMod1$fitted.values, regMod1$residuals,
     xlab = "Valores predichos (mpg)", 
     ylab = "Residuos", 
     main = "Modelo 1: mpg ~ disp + hp")
abline(h = 0, col = "red", lty = 2)

# Modelo 2: mpg ~ disp + hp + wt
regMod2 <- lm(mpg ~ disp + hp + wt, data=mtcars)
summary(regMod2)

# Gráfico de residuos vs valores predichos
plot(regMod2$fitted.values, regMod2$residuals,
     xlab = "Valores predichos (mpg)", 
     ylab = "Residuos", 
     main = "Modelo 2: mpg ~ disp + hp + wt")
abline(h = 0, col = "red", lty = 2)
