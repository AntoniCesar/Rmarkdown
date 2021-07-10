suma <- c(1:1000)
sum(suma)


i <- NULL; aux <- NULL; r <- NULL
for (i in 0:10) {
  aux <- 2^i; r <- c(r, aux)}

dfclase <- data.frame(grupo, nota)
dfclase

load("ei1012-1516-la-s1-datos.Rdata")
length(dfclase$grupo)
1 + 2

which(dfclase$grupo == "A")
which(dfclase$nota > 7)

sort(dfclase$nota)
sort(dfclase$nota,decreasing = TRUE)
which.max(dfclase$nota)


grupoc <- dplyr::filter(dfclase, dfclase$grupo == "C")
grupoc
length(grupoc$grupo)


aprobados <- dplyr::filter(dfclase, dfclase$nota > 10.5)
aprobados
length(aprobados$nota)
which(dfclase$nota > 10.5)
length(aprobados)


menor4.9 <- dplyr::filter(dfclase, dfclase$nota <= 4.9)
(length(menor4.9$nota)/length(dfclase$nota))*100


mayor4.9 <- dplyr::filter(dfclase, dfclase$nota >= 4.9)
(length(mayor4.9$nota)/length(dfclase$nota))*100


boxplot(`nota` ~ `grupo`, dfclase, col = palette(rainbow(2)))


# EJERCICIO 05

#Cuánto alumnos del grupo B aprobaron

dplyr::filter(aprobados, aprobados$grupo == "B")

MAX <- max(dfclase$nota)



#Ejercicio 9 con dataframe
concentracion <- data.frame(conc)

max(concentracion$conc)

ppm40 <- dplyr::filter(concentracion, concentracion$conc > 40)
length(ppm40$conc)

mean(concentracion$conc)

ppmcrec <- sort(concentracion$conc)
ppmcrec
head(ppmcrec, 10)





#Ejercicio 9 con vector

#a .  ¿Cuál ha sido la concentración máxima?
max(conc)

# b . ¿En cuántos de los muestreos se ha superado la concentración de 40.0 ppm?
mayor40 <- conc[conc > 40]
length(mayor40)

# c . ¿Cuál ha sido la concentración media del día?
mean(conc)

# d . ¿Cuáles fueron las 10 mediciones más bajas del día?
ascendentecon <- sort(conc)
ascendentecon[1:10]

# Si la primera medida fue a las 00:00. ¿A qué hora del día se alcanzó 
# la concentración máxima?



## Estilo Tidyverse 

#  %>% nos permite evitar la anidacion



