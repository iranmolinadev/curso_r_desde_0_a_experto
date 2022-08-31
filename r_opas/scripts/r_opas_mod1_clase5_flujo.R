# ORGANIZACIÓN PANAMERICANA DE LA SALUD (OPS)
# UNIDAD TÉCNICA DE VIGILANCIA, PREPARACIÓN Y RESPUESTA A EMERGENCIAS Y DESASTRES
# CURSO INTRODUCCIÓN AL USO DE R EN EMERGENCIAS DE SALUD 
# MÓDULO 1 - INTRODUCCIÓN A R
# CLASE 5 - FLUJO DE TRABAJO: DIRECTORIO, ENTORNO DE PROYECTO Y TIDYDATA

# 1. Para seguir la clase ####

# Instalar/Cargar paquetes
pacman::p_load(pacman,    # Instalar y cargar paquetes
               tidyverse) # Manejo y tratamiento de datos

# 2. Directorio de trabajo ####

# Directorio de trabajo actual
getwd() 

# Cambiar el directorio 
setwd("C:\\Users\\laisr\\OneDrive\\Documentos\\OPAS\\CursoR\\r_opas") 
setwd("C:/Users/laisr/OneDrive/Documentos/OPAS/CursoR/r_opas")

# 6. Piping (%>%) ####
 
# Crear un vector
vetor <- c(10,12,15,18,14,12,16,18,17,19,18,15,16,14,18)

# Calcular la media y redondear
vetor %>% 
  mean() %>% 
  round(digits=1)

# Crear un objeto con el valor de la media
media  <- mean(vetor)
media

# Crear un objeto con la media redondeada
media2 <- round(media, digits=1)
media2
