# A
# Cargamos el dataset protesta
load("~/R Scripts/Politecnico Malvinas - Procesamiento de Datos/Clase_9/protesta.Rdata")

# previsualizamos el conjunto de datos
summary(protesta)
head(protesta)

# Creamos el modelo logistico
modelo <- glm(prot ~ coi.pol, binomial, protesta)
summary(modelo)

# Opcional: Calculo de log odds
intercepto <- -2.07201
coef_coi_pol <- 0.81655
# calcular log-odds
log_odds <- intercepto + coef_coi_pol
# convertir log odds a probabilidad
probabilidad <- 1 / (1 + exp(-log_odds))
probabilidad


# B
coima_si<- data.frame(coi.pol = c("Sí"))
coima_si
coima_no <- data.frame(coi.pol =c("No"))
coima_no
modelo

predict(modelo, coima_si, type="response")
predict(modelo, coima_no, type = "response")


# C
modelo2 <- glm(prot ~ int.pol + ciudad + coi.pol,
               binomial, protesta)
summary(modelo2)


# D
extorsion1 <- data.frame(coi.pol = c("Sí"),
                         int.pol = c(4),
                         ciudad=c("grande"))
predict(modelo2, extorsion1, type = "response")

extorsion2 <- data.frame(coi.pol = c("Sí"),
                         int.pol = c(1),
                         ciudad = c("pequeña"))
predict(modelo2, extorsion2, type = "response")
