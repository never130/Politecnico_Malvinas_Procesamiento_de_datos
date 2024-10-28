#a. Crear un data frame con los datos de la tabla
datos <- data.frame(
  Mes = c("Enero", "Febrero", "Marzo"),
  Ingresos = c(45000, 33400, 6450),
  Gastos = c(41500, 35400, 6300),
  Impuestos = c(51200, 35600, 7100)
)
str(datos)

#b. Añadir una nueva fila con los siguientes datos de Abril
datos <- rbind(datos, data.frame(Mes = "Abril", Ingresos = 49700, Gastos = 36300, Impuestos = 6850))
print(datos$Mes)

#c Cambiar los ingresos de marzo por 50400
datos$Ingresos[3] <- 50400

#d. Crear una nueva columna con los beneficios de cada mes
datos$Beneficios <- datos$Ingresos - datos$Gastos - datos$Impuestos
datos

#e. Crear una nueva columna con el factor Balance con dos posibles
#categorías: positivo si ha habido beneficios y negativo si ha habido perdida 
datos$Balance <- ifelse(datos$Beneficios > 0, "positivo", "negativo")
datos  

#f. Filtrar el conjunto de datos para quedarse con los nombres de los meses y 
#los beneficios de los meses con balance positivo. 
subset(datos, Balance == "positivo", select = c(Mes, Beneficios))

#g. Calcular el promedio de los balances positivos y negativos
promedio_positivo <- mean(datos$Beneficios[datos$Balance == "positivo"])
print(promedio_positivo)

promedio_negativo <- mean(datos$Beneficios[datos$Balance == "negativo"])
print(promedio_negativo)


