# ORGANIZACIÓN PANAMERICANA DE LA SALUD (OPS)
# UNIDAD TÉCNICA DE VIGILANCIA, PREPARACIÓN Y RESPUESTA A EMERGENCIAS Y DESASTRES
# CURSO INTRODUCCIÓN AL USO DE R EN EMERGENCIAS DE SALUD 
# MÓDULO 1 - INTRODUCCIÓN A R
# CLASE 4 - CONCEPTOS FUNDAMENTALES PARA COMENZAR A PROGRAMAR EN R

# 1. Para seguir la clase ####
2 + 2

# 2. Operadores ####

# Operadores aritméticos
# Suma
2 + 3

# Sustracción
2 - 3

# Multiplicación 
2 * 3

# División
30 / 5

# Exponencial
2^3


# Operadores relacionales (de comparación)
# Igual a 
"A" == "a"

# Diferente de
2 != 0

# Mayor que            
4 > 2

# Menor que
4 < 2

# Mayor o igual
6 >= 4

# Menor o igual
4 <= 6

# Operadores lógicos
# Y 
2<3 & 2>1 
2<3 & 2<1 

# O 
2<3 | 2<1 

# Negación
1 == 1
!(1 == 1)

# 3. Objetos ####

# Crear un objeto con operador <-
a <- 2 + 2  

# Ejecutar el objeto creado
a

# Asignar una cadena de caracteres y ejecutar el objeto
b <- "Quiero aprender a usar R"
b <- Quiero aprender a usar R

b <- "Estoy aprendiendo a usar R"
b

# Realizar operaciones con objetos
c <- a + 5
texto1 = "Buenos"
texto2 = "días"
texto3 <- texto1 + texto2

# Cómo nombrar un objeto
casos_confirmados <- 569
casos.notificados <- 1058
casosDescartados  <- 67

# Cómo no podemos nombrar objetos
5 <- 50
5 <- "cinco"
NA <- 2+2
TRUE <- 2+2
FALSE <- 2+2

# Para eliminar un objeto
rm(a)

# Para eliminar más de un objeto
rm(b, c, texto1)


# 4. Funciones ####

# Funcione c() para concatenar
vector <- c(10,12,15,18,14,12,16,18,17,19,18,15,16,14,18)
vector

# Funciones estadísticas
# Suma
sum(vector)
10 + 12 + 15 + 18 + 14 + 12 + 16 + 18 + 17 + 19 + 18 + 15 + 16 + 14 + 18

# Número de elementos
length(vector) 

# Mínimo
min(vector)

# Máximo
max(vector)

# Intervalo
range(vector)

# Media
mean(vector)  

# Varianza
var(vector) 

# Desviación estándar
sd(vector) 

# Cuantiles / Percentiles
quantile(c(10,12,15,18,14,12,16,18,17,19,18,15,16,14,18), 
         probs = c(0.1, 0.3, 0.5, 0.8))
quantile(vector, 
         probs = c(0.1, 0.3, 0.5, 0.8))

# Funciones de redondeo
# Redondear a dos decimales
round(10.258, digits = 2)  

# Entero hacia arriba
ceiling(10.258) 

# Entero hacia abajo
floor(10.258)   

# 5. Paquetes ####

# Instalar un paquete de CRAN
install.packages("pacman")

# Instalación de múltiples paquetes:
install.packages('lubridate', 'tidyverse', dependencies = TRUE) 

# Instalar paquetes de GitHub
# Instalar el paquete geobr
remotes::install_github('ipeaGIT/geobr', subdir = "r-package")

# Cargar paquetes
# Recuerde cargar los paquetes

install_github('ipeaGIT/geobr', subdir = "r-package")

# instalar paquetes de github IranMx2
install.packages("remotes")
library("remotes")
install.packages("devtools")
library(devtools)
install.packages("githubinstall")
library(githubinstall)

install_github("qinwf/awesome-R")

# Instalar y cargar con pacman
pacman::p_load(rio, ggspatial)

# Comparar con pasos secuenciales
install.packages("rio")
install.packages("ggspatial")
library(rio)
library(ggspatial)

# Actualizar paquetes
update.packages(ask = FALSE)

# 6. Clases ####

# Numérico
peso <- 63.5
peso
class(peso)

altura <- 182
altura
class(altura)

# Entero
var_num <- 42
var_int <- 42L

class(var_num)
class(var_int)

# Carácter
imc <- c("bajo peso", "sobrepeso", 'obesidad')
imc
class(imc)

imc <- c("< 18,5", "18,5 - 24,9", "≥ 30")
imc
class(imc)

# Fecha
# Fecha del sistema
hoje <- today()
hoje
class(hoje)
as.numeric(hoje)

# Factor
# Como factor, sin especificar los niveles
faixet <- factor(c("21 a 40", "0 a 20", "41 a 60", "61 ou mais"))
faixet
class(faixet)

# Como carácter
faixet <- c("61 ou mais", "0 a 20", "21 a 40", "41 a 60")
faixet
class(faixet)

# Como factor, definiendo niveles
faixet <- factor(faixet, 
                 levels = c("61 ou mais", "41 a 60", "21 a 40", "0 a 20"),
                 labels = c("61 ou mais", "41 a 60", "21 a 40", "0 a 20"))
faixet

# Lógico
fumo <- c(TRUE, FALSE, F, T)
fumo
class(fumo)

# Cuidado con la coerción
a <- c(1, -3)
class(a)

a <- c(1, -3, "a", TRUE)
class(a)