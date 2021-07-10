6 EJERCICIO
================

***1.Calcula los valores numéricos aproximados de***

``` r
#a.
(0.3*0.15)/((0.3*0.15)+(0.2*0.8)+(0.5*0.12)) 
```

    ## [1] 0.1698113

***2.Realizar las siguientes sumas***

a . 1 + 2 + 3 + ……. + 1000

``` r
suma01 <- c(0:1000)
sum(suma01)
```

    ## [1] 500500

b . 1 + 2 + 4 + 8 + 16 + ….. + 1024

``` r
i <- NULL; aux <- NULL; r <- NULL
for (i in 0:10) {
  aux <- 2^i; r <- c(r, aux)}
sum(r)
```

    ## [1] 2047

***3.El vector grupo representa el grupo al que pertenece una serie de
alumnos***

a . ¿Cuántos elementos tiene?

*primero cargamos el archivo*

``` r
load("ei1012-1516-la-s1-datos.Rdata")
```

*convertimos a data frame*

``` r
dfclase <- data.frame(grupo, nota)
head(dfclase)
```

    ##   grupo nota
    ## 1     B  4.9
    ## 2     A  5.4
    ## 3     E  5.2
    ## 4     D  6.8
    ## 5     B  5.0
    ## 6     D  6.2

*obeservamos la cantidad de elementos que tiene con length*

``` r
length(dfclase$grupo)
```

    ## [1] 192

b . ¿En que posiciones del vector está la letra “A”?

*Obervamos la posición con la función “which”*

``` r
which(dfclase$grupo == "A")
```

    ##  [1]   2   8  17  21  28  84 101 108 111 115 123 136 190 192

***4. El vector nota representa la nota de un examen de los alumnos que
están en los grupos del vector grupo.***

a . ¿Cuanto suman todas las notas?

``` r
sum(dfclase$nota)
```

    ## [1] 962

b . ¿Cual es la media aritmética de todas las notas?

``` r
mean(dfclase$nota)
```

    ## [1] 5.010417

c . ¿En qué posiciones están las notas mayores de 7.0?

``` r
which(dfclase$nota > 7)
```

    ## [1]  81 103 120 151

d . Visualiza las notas ordenadas de mayor a menor

``` r
Notdec <- sort(dfclase$nota,decreasing = TRUE)
head(Notdec)
```

    ## [1] 7.7 7.5 7.4 7.2 7.0 6.9

e . ¿En qué posición está la nota máxima?

``` r
which.max(dfclase$nota)
```

    ## [1] 120

***5. A partir de los vectores grupo y nota definidos.***

a . Suma las notas de los 10 primeros alumnos del vector

``` r
notas10p <- dfclase$nota[1:10]
sum(notas10p)
```

    ## [1] 51.8

b . ¿Cuántos alumnos hay del grupo C?

``` r
grupoc <- dplyr::filter(dfclase, dfclase$grupo == "C")
length(grupoc$grupo)
```

    ## [1] 39

c . ¿Cuántos alumnos han aprobado?

``` r
aprobados <- dplyr::filter(dfclase, dfclase$nota >= 5.5)
length(aprobados$nota)
```

    ## [1] 73

d . ¿Cuántos alumnos del grupo B han aprobado?

``` r
GrupoB <- dplyr::filter(aprobados, aprobados$grupo == "B")
length(GrupoB$nota)
```

    ## [1] 9

e . ¿Qué porcentaje de alumnos del grupo C han aprobado?

``` r
AprobadosC <- dplyr::filter(aprobados, aprobados$grupo == "C")
(length(AprobadosC$nota)/length(dfclase$grupo))*100
```

    ## [1] 10.41667

f . ¿De qué grupos son la máxima y mínima notas de toda la muestra?

``` r
max(dfclase$nota)
```

    ## [1] 7.7

g . Nota media de los alumnos de grupo A y B, juntos, considerando sólo
a los que han aprobado.

``` r
aprobadosAB <- dplyr::filter(aprobados, aprobados$grupo == "A" | 
                               aprobados$grupo == "B")
mean(aprobadosAB$nota)
```

    ## [1] 6.109091

***6. Calcula el percentil 66 de las notas de todos los alumnos, y
también de los alumnos del grupo C.***

1.  Percentil de todos los alumnos

``` r
percentil <- quantile(dfclase$nota, 66/100)
percentil
```

    ## 66% 
    ## 5.5

2.  Percentil de todos los alumnos del grupo C

``` r
per66C <- quantile(grupoc$nota, 66/100)
per66C
```

    ##   66% 
    ## 5.808

***7. Un alumno tiene una nota de 4.9. ¿Qué porcentaje, del total de
alumnos, tiene una nota menor o igual que la suya? ¿Y qué porcentaje
tiene una nota mayor o igual que la suya?***

a . porcentaje de alummnos que tiene nota menor o igual a 4.9

``` r
menor4.9 <- dplyr::filter(dfclase, dfclase$nota <= 4.9)
(length(menor4.9$nota)/length(dfclase$nota))*100
```

    ## [1] 46.875

b . porcentaje de alummnos que tiene nota mayor o igual a 4.9

``` r
mayor4.9 <- dplyr::filter(dfclase, dfclase$nota >= 4.9)
(length(mayor4.9$nota)/length(dfclase$nota))*100
```

    ## [1] 56.25

***8. Realiza el gráfico de diagramas de caja de las notas de cada
grupo, para poder comparar el nivel de cada uno de ellos.***

``` r
boxplot(`nota` ~ `grupo`, dfclase, col = palette(rainbow(2)))
```

![](Readme_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

***9. Si la variable conc recoge la concentración de plomo (en ppm) en
el aire de cierta zona durante un día completo***

a . ¿Cuál ha sido la concentración máxima?

``` r
max(conc)
```

    ## [1] 47.34

b . ¿En cuántos de los muestreos se ha superado la concentración de 40.0
ppm?

``` r
mayor40 <- conc[conc > 40]
length(mayor40)
```

    ## [1] 61

c . ¿Cuál ha sido la concentración media del día?

``` r
mean(conc)
```

    ## [1] 24.07229

d . ¿Cuáles fueron las 10 mediciones más bajas del día?

``` r
ascendentecon <- sort(conc)
ascendentecon[1:10]
```

    ##  [1] 0.93 1.07 1.77 2.03 2.58 2.73 2.75 2.88 2.88 2.91

### EJERCICIOS PARTE 2

***2.1. Graficar los puntos
(1,1),(2,4),(3,6),(4,8),(5,25),(6,36),(7,49),(8,61),(9,81),(10,100) en
un plano utilizando RStudio***

***2.2 Ingresar la matriz A en RStudio***

``` r
(A <- matrix(c(1:4, seq(2,8,by = 2),seq(3,12,by = 3)), nrow = 4, ncol = 3))
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    2    3
    ## [2,]    2    4    6
    ## [3,]    3    6    9
    ## [4,]    4    8   12

***2.4 Crea una función que cree una matriz nula ingresando las
dimensiones***

``` r
matriz_nula <- function(filas,columnas){ 
  matrix(0 , nrow = filas ,ncol = columnas) 
}
matriz_nula(2,3)
```

    ##      [,1] [,2] [,3]
    ## [1,]    0    0    0
    ## [2,]    0    0    0

``` r
matriz_nula(5,5)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    0    0    0    0    0
    ## [2,]    0    0    0    0    0
    ## [3,]    0    0    0    0    0
    ## [4,]    0    0    0    0    0
    ## [5,]    0    0    0    0    0

``` r
matriz_nula(2,6)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6]
    ## [1,]    0    0    0    0    0    0
    ## [2,]    0    0    0    0    0    0

***2.6 Obtener la matriz transpuesta de A (ejercicio 2)***

``` r
(Transpuesta_A <- t(A))
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    2    4    6    8
    ## [3,]    3    6    9   12

***2.8 Crea una función para calcular P^6***

``` r
# MATRIZ P
(P<-matrix(c(1,-2,1,2,4,0,3,-2,1), ncol=3, nrow=3))
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    2    3
    ## [2,]   -2    4   -2
    ## [3,]    1    0    1

``` r
 P%*%P%*%P%*%P%*%P%*%P
```

    ##       [,1]  [,2]  [,3]
    ## [1,] -1792    24 -2824
    ## [2,]  -464 -2416 -1344
    ## [3,]  -648   440  -912

``` r
# Función
 # n es la potencia
x <- function(P,n){
  if(dim(P)[2]==dim(P)[1]){
    aux <- P
    j <- 1
    repeat{
      P <- P%*%aux
      j <- j+1
      if(j==n)(break)
    }
    P
  }
}

#Consulta
x(P,6)
```

    ##       [,1]  [,2]  [,3]
    ## [1,] -1792    24 -2824
    ## [2,]  -464 -2416 -1344
    ## [3,]  -648   440  -912

***12. Considere β =((Xt.X)^-1).Xt.Y*** ***Determine la matriz β***

``` r
(X <- matrix(c(rep(1,5),1,-1,0,1,2),nrow = 5,ncol = 2))
```

    ##      [,1] [,2]
    ## [1,]    1    1
    ## [2,]    1   -1
    ## [3,]    1    0
    ## [4,]    1    1
    ## [5,]    1    2

``` r
(Y <- matrix(c(0,0,1,1,3),nrow = 5,ncol = 1))
```

    ##      [,1]
    ## [1,]    0
    ## [2,]    0
    ## [3,]    1
    ## [4,]    1
    ## [5,]    3

``` r
# Trasnpuesta de la matriz * matriz
 trasn_x.x <- t(X)%*%X

# Inversa de la multipicación de la matriz y su trasnpuesta
 (Inversa <- solve(trasn_x.x))
```

    ##            [,1]       [,2]
    ## [1,]  0.2692308 -0.1153846
    ## [2,] -0.1153846  0.1923077

``` r
# Trasnpuesta de X con el resultado de la inversa
  (rp <- Inversa%*%t(X))
```

    ##            [,1]       [,2]       [,3]       [,4]       [,5]
    ## [1,] 0.15384615  0.3846154  0.2692308 0.15384615 0.03846154
    ## [2,] 0.07692308 -0.3076923 -0.1153846 0.07692308 0.26923077

``` r
# Resultado Parcial por la matriz y
  # Resultado de la matriz β
  (β <- rp%*%Y)
```

    ##           [,1]
    ## [1,] 0.5384615
    ## [2,] 0.7692308

### EJERCICIOS PARTE 3
