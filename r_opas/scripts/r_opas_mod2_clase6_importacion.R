# ORGANIZACIÓN PANAMERICANA DE LA SALUD (OPS)
# UNIDAD TÉCNICA DE VIGILANCIA, PREPARACIÓN Y RESPUESTA A EMERGENCIAS Y DESASTRES
# CURSO INTRODUCCIÓN AL USO DE R EN EMERGENCIAS DE SALUD 
# MÓDULO 2 - INTRODUCCIÓN A R
# CLASE 6 - IMPORTACIÓN Y EXPORTACIÓN DE ARCHIVOS A R

# 1. Para seguir la clase ####

# Instalar/Cargar paquetes
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap

# 2. Importación de datos con el paquete rio ####

# El nombre "rio" es una referencia a Input y Output en R "R I/O".
?rio

# Importar de un archivo

# Importar Notificaciones de Síndrome Gripal en eSUS, 2022 (Descarga: 17/06/2022) 
sg  <- import("datos/sg_2022.csv") 

# Importar Notificaciones de Síndrome Respiratorio Agudo Grave 
  # en SIVEP-Gripe, 2020-2022 (Descarga: 17/06/2022) 
srag  <- import("datos/srag_20_22.xlsx")

# Importar estimacíón de la población de los municipios de un estado brasileño, 
  #2020 e 2021 (Fuente de datos: Ministerio de Salud)
pop_21 <- import("datos/pop_20_21.xls", 
                    which ="pop_ma_2021")
pop_20 <- import("datos/pop_20_21.xls")

# Importar de Google Sheets 
google <- import("https://docs.google.com/spreadsheets/d/1VZgTTA7cNP9TLo8WeH9Wp7g5POdUkYaN49AhNeWA2ys/edit#gid=0")
      
# 4. Importar datos del RedCAP ####
simp <- redcap_read(redcap_uri="https://redcap.saude.gov.br/api/", 
                       token="", 
                       raw_or_label="label")$data
    
# 6. Exportación de datos con el paquete rio ####

# Exportar en .rds las notificaciones del síndrome gripal en eSUS, 2022 (Download: 17/06/2022) 
export(sg, "datos/sg.rds")

# Exportar en .rds las notificaciones del Síndrome Respiratorio Agudo Severo en 
  #SIVEP-Gripe, 2020-2022 (Descarga: 17/06/2022) 
# Primeiro e segundo argumento
export(srag, "datos/srag.rds")