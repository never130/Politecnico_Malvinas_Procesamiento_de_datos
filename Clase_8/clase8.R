# EJERCICIO A
# Regresión lineal simple en R

# Funcion lm:
#lm(formula, data, subset, weights, na.action,
#   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#   singular.ok = TRUE, constrasts = NULL, offset)

# Ejemplo de aplicacion 1
edad <- c(46,20,52,30,57,25,28,36,57,44,24,31,52,23,60,48,34,51,40,34)
peso <- c(84,73,65,70,76,69,63,72,79,75,27,89,65,57,59,69,60,79,75,82)
grasa <-
  c(354,190,405,263,451,302,288,325,402,365,209,290,346,254,395,434,220
    ,374,308,220)
datos <- data.frame(edad,peso,grasa)

# Salida del grafico generado
pairs(datos)

# Calculo de coeficiente de correlacion lineal
cor(datos)
##      edad      peso     grasa
## edad 1.0000000 0.2705334 0.8741798
## peso 0.2705334 1.0000000 0.3009460
## grasa 0.8741798 0.3009460 1.0000000

# Funcion lm
reg <- lm(grasa~edad, edad=datos)
reg
## Call:
## lm(formula = grasa ~ edad, data = datos)
## 
## Coefficients:
## (Intercept) edad 
## 107.633 5.356

summary(reg)
## Call:
## lm(formula = grasa ~ edad, data = datos)
## 
## Residuals:
## Min 1Q Median 3Q Max 
## -69.75 -25.37 -2.68 23.51 69.25 
## 
## Coefficients:
## Estimate Std. Error t value Pr(>|t|) 
## (Intercept) 107.6328 29.1057 3.698 0.00165 ** 
## edad 5.3565 0.7013 7.638 4.71e-07 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 38.94 on 18 degrees of freedom
## Multiple R-squared: 0.7642, Adjusted R-squared: 0.7511 
## F-statistic: 58.33 on 1 and 18 DF, p-value: 4.712e-07

# Visualizacion con la recta de regresión ajustada
plot(edad,grasa,xlab="Edad",ylab="Grasa en la sangre")
abline(reg, col="blue")

# Calculo de predicciones predict()
pred_edades <- data.frame(edad=seq(20,60))
predict(reg,pred_edades)

# Prediccion a mayor rango
pred_edad <- data.frame(edad=c(10,80))
predict(reg,pred_edad)


## EJEMPLO DE APLICACION 2
salario<- c(11, 10, 8, 5, 9, 7, 3, 11, 8, 7)
ausencias<- c(18, 17, 29, 36, 11, 28, 35, 14, 20, 32)
df <-data.frame(salario,ausencias)
df

# Aplicacion de test de Shapiro Wilk
shapiro.test( df$salario )

# Aplicacion del modelo de regresion lineal
modeloR <- lm(ausencias ~ salario,
              data = df) # (x, y)
summary(modeloR) # para ver el resumen del modelo
###Salida:
#  Call:
#  lm(formula = ausencias ~ salario, data = df)
#Residuals:
#  Min 1Q Median 3Q Max 
#-9.6757 -2.9368 0.3243 3.3353 5.3022 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|) 
#(Intercept) 47.8744 5.4120 8.846 2.1e-05 ***
#  salario -3.0221 0.6549 -4.615 0.00172 ** 
#
#  Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 5.026 on 8 degrees of freedom
#Multiple R-squared: 0.7269, Adjusted R-squared: 0.6928 
#F-statistic: 21.3 on 1 and 8 DF, p-value: 0.001722

# Representacion de la recta del modelo de regresion
plot( df$salario, 
      df$ausencias, 
      xlim = c( 0, 14 ),
      ylim = c( 0, 70 ), 
      xlab = "Salario", 
      ylab = "Ausencias" )
abline( modeloR, col = "#e74c3c" ) # añadir la recta


# ANALISIS DE LOS MODELOS DE REGRESION LINEAL Y MULTIPLE
## Ejemplo de aplicacion, estudio de mercado sobre el precio de los pisos.
precio <- c(250, 130, 165, 310, 320, 400, 200, 80, 69, 179, 120, 223, 300, 
            198, 165, 69, 73, 123, 356, 183)
superficie <- c(120, 80, 100, 180, 190, 250, 99, 90, 60, 100, 110, 120, 180, 
                130, 90, 50, 60, 70, 120, 130)
antiguedad <-c(15, 20, 30, 15, 12, 40, 30, 27, 14, 20, 22, 25, 21, 33, 5, 12, 
               6, 10, 28, 30)
pisos <- data.frame(precio, superficie, antiguedad)

# Dibujar el diagrama de dispersión 
plot(pisos$precio,pisos$superficie)

# Modelo de regresion lineal simple (precio ~ superficie)
reg_lin <- lm(precio ~ superficie, data = pisos)
summary(reg_lin)
# Resultado: precio = -7.813 + 1.747 * superficie

# Modelo de regresion lineal simple (precio ~ antiguedad)
reg_lin1 <- lm(precio ~ antiguedad, data = pisos)
summary(reg_lin1)
# Resultado: precio = 110.423 + 4.107 * antiguedad

# Modelo de regresion lineal múltiple (precio ~ superficie + antiguedad)
reg_mult <- lm(precio ~ superficie + antiguedad, data = pisos)
summary(reg_mult)
# Resultado: precio = -6.82133 + 1.75516 * superficie – 0.09239 * antiguedad


## EJERCICIO B: Ejemplos de aplicacion
# Modelo nulo y modelo completo
modelo_nulo <- lm(precio ~ 1, data=pisos)  # Modelo sin predictores
modelo_completo <- lm(precio ~ ., data=pisos)  # Modelo con todos los predictores

#1 Selección hacia adelante (forward)
stepAIC_forward <- step(modelo_nulo, direction="forward", scope=formula(modelo_completo))
summary(stepAIC_forward)

#2 Selección Backward (Selección secuencial hacia atrás):
stepAIC_backward <- step(reg_mult, direction="backward")
summary(stepAIC_backward)

#3 Selección Central (Selección central):
install.packages("leaps")
library(leaps)
regCentral <- regsubsets(precio ~ ., data=pisos, nvmax=4)
summary(regCentral)

#4 Selección por Regresión Lineal Stepwise (Selección paso a paso):
install.packages("MASS")
library(MASS)
stepwiseReg <- stepAIC(reg_mult, direction="both")

#5 Selección por Penalización (Lasso, Ridge, Elastic Net):
install.packages("glmnet")
library(glmnet)
x <- as.matrix(pisos[, -which(names(pisos) == "precio")])
y <- pisos$precio

# Aplicar Ridge Regression (alpha=0)
ridgeMod <- glmnet(x, y, alpha=0)
# Graficar el modelo de Ridge
plot(ridgeMod)


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

# Modelo 2: mpg ~ disp + hp + wt
regMod2 <- lm(mpg ~ disp + hp + wt, data=mtcars)
summary(regMod2)
