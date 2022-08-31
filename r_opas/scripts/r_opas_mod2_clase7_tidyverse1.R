# ORGANIZACIÓN PANAMERICANA DE LA SALUD (OPS)
# UNIDAD TÉCNICA DE VIGILANCIA, PREPARACIÓN Y RESPUESTA A EMERGENCIAS Y DESASTRES
# CURSO INTRODUCCIÓN AL USO DE R EN EMERGENCIAS DE SALUD 
# MÓDULO 2 - TRATAMENTO DE DADOS E CARACTERÍSTICAS RELACIONADAS ÀS PESSOAS
# CLASE 7 - GRAMÁTICA TIDYVERSE PARA EL ANÁLISIS DE DATOS:  
#           PREPARACIÓN DE LAS BASES DE DATOS PARA ANÁLISIS 
#           DE SITUACIÓN DE SALUD (PARTE 1)

# 1. Para seguir la clase ####

# Instalar/Cargar paquetes
pacman::p_load(pacman,    # Instalar y cargar paquetes
               rio,       # Importación y exportación de datos 
               tidyverse) # Manejo y tratamiento de datos

# Importar notificaciones del síndrome gripal (GS) en el eSUS realizada por un estado brasileño, 2022 (Descargar: 17/06/2022)
sg  <- import("dados/sg_2022.csv") 

# Importar Notificaciones de Síndrome Respiratorio Agudo Severo (SRAS) en el SIVEP-Gripe realizadas por un estado brasileño, 2020-2022 (Descargar: 17/06/2022) 
srag  <- import("dados/srag_20_22.xlsx")

# Importar estimaciones de población municipal, 2020-2021 (Fuente: Ministerio de Salud) 
pop_21 <- import("dados/pop_20_21.xls", 
                    which ="pop_2021")
pop_20 <- import("dados/pop_20_21.xls")


# 2. Estructura general de la base y de las variables ####

# Vamos a explorar nuestra base de datos

  # ¿Cuántas observaciones y variables identificas en los objetos creados?
  # sg :
  # srag : 

# Para visualizar la base de datos
View(sg)
View(srag)
  
  # ¿Consiguió ver todas las variables de srag?

# Nombres de las variables
names (sg)
names(srag)

# Primeras observaciones
head(sg)
head(sg, 4)

# Últimas observaciones
tail(srag)
tail(srag,2)

# Estructura general (clases de variables y primeras observaciones)
# ¡MUY USADO!
glimpse(sg)
glimpse(srag)

# 3. Análisis exploratorio de datos ####

# Vamos a explorar nuestra variables
# Objetivo: Identificar la necesidad de transformación


# class() o glimpse() para la clase de la variable

class(srag$dt_notific)

class(sg$datanotificacao)

# Transformaciones necesarias: transformación para fecha

                            
# La función table() para la tabla de frecuencias exploratoria
# Uso del signo de dólar para la lista descendente de variables

table(srag$dt_notific)

table(srag$dt_notific, useNA = "always")


# count() para la tabla de frecuencia exploratoria

# Estado de la notificación
srag %>% 
  count(sg_uf_not)


# Fecha de notificación
sg %>% 
  count(datanotificacao)

# Transformaciones: Transformación para la fecha 
# Excluir los registros de 2020 y 2021

# Sexo
sg %>% 
  count(sexo)

class(sg$sexo)

# Transformación: Transformar Indefinido en Ignorado

# Resultado do teste
sg %>% 
  count(resultadoteste)

# Transformación: Convertir el blanco en ignorado

# Evolución del caso
sg %>% 
  count(evolucaocaso)

class(sg$evolucaocaso)

# Transformación: Combinar el blanco y el ignorado

# Edad
sg %>% 
  count(idade)

class(sg$idade)

# summary() para generar medidas de síntesis

summary(sg$idade)
  
# Transformaciones: Transformar >117 en ignorado
  # Crear rangos de edad


# 4. Seleccionar columnas: select() ####

# Revisar la base de datos
glimpse(srag)


# Abrir o banco de dados para identificar a posição das colunas  
View(srag)


# Seleccionar la base de datos por posición
srag_ejemplo <- srag %>% 
  select(1,2,3,4)

glimpse(srag_ejemplo)
head(srag_ejemplo)

# Por el nombre
srag_ejemplo <- srag %>% 
  select(cs_sexo, cs_gestant, puerpera)

head(srag_ejemplo)

# Por intervalo
srag_ejemplo <- srag %>% 
  select(1:4)

srag_ejemplo <- srag %>% 
  select(febre:outro_sin)

# Con más de un criterio
srag_ejemplo <- srag %>% 
  select(1:4, febre:outro_sin)

# Eliminar el dataframe utilizado para el ejemplo
rm(srag_ejemplo)

# Se puede utilizar para reordenar las variables en el dataframe
# Primero veamos los nombres de las variables 
glimpse(sg)

# Seleccionar las columnas de interés y reordenar sus posiciones en la base de datos sg
sg_ejemplo <- sg %>% 
  select(sexo, idade, datainiciosintomas,  evolucaocaso, 
         classificacaofinal, municipioibge, resultadoteste,
         sintomas) 
         
# ¿Vemos cómo se ven las columnas en la base de datos sg?
glimpse(sg_ejemplo)

# Selección de variables con el operador "-" y la función c()
srag_ejemplo <- srag %>%
  select(-lote_ref)

glimpse(srag_ejemplo)

# Eliminar más de una variable de la base de datos
srag_ejemplo <- srag %>%
  select(-lote_ref, -fab_covref)

glimpse(srag_ejemplo)

# Eliminar variables por intervalo
srag_ejemplo <- srag %>%
  select(-c(vacina_cov:lote_ref))

glimpse(srag_ejemplo)


# 5. Renombrar variables: rename() ####

# Primero recordemos los nombres de las variables en la base sg y conozcamos su estructura
glimpse(sg)

# Para renombrar una variable
# Por ejemplo: Cambiemos las iniciales de los datos a mayúsculas en sg
sg_ejemplo <- sg %>%    
  rename(DATAINICIOSINTOMAS = datainiciosintomas) 

# Para renombrar más de una variable
# Por ejemplo: Hagamos que todas las variables estén en mayúsculas
sg <- sg %>%    
  rename(DATANOTIFICACAO = datanotificacao,
         DATAINICIOSINTOMAS = datainiciosintomas,
         SEXO = sexo, 
         IDADE = idade,
         EVOLUCAOCASO = evolucaocaso,
         CLASSIFICACAOFINAL = classificacaofinal,
         MUNICIPIOIBGE = municipioibge,
         RESULTADOTESTE = resultadoteste,
         SINTOMAS = sintomas) 

# Utilice comillas simples cuando haya espacios o acentos especiales en el nombre de la variable
# Por ejemplo: Comprobemos la base de población municipal en 2020
glimpse(pop_20)

pop_20 <- pop_20 %>%    
  rename(MUNICIPIO = `Município de residência`,
         POP_2020 = pop_2020) 

glimpse(pop_20)

# Por ejemplo: Transformemos la base de 2021

glimpse(pop_21)

pop_21 <- pop_21 %>%    
  rename(MUNICIPIO = `Município de residência`,
         POP_2021 = pop_2021)

glimpse(pop_21)


# 6. Exportación de bases transformadas ####
export(srag, "dados/srag_aula8.rds")
export(sg, "dados/sg_aula8.rds")
export(pop_20, "dados/pop_20.rds")
export(pop_21, "dados/pop_21.rds")

