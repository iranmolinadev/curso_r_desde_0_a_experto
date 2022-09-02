`# ORGANIZACIÓN PANAMERICANA DE LA SALUD (OPS)
# UNIDAD TÉCNICA DE VIGILANCIA, PREPARACIÓN Y RESPUESTA A EMERGENCIAS Y DESASTRES
# CURSO PRINCIPIANTE DE R APLICADO A EMERGENCIAS DE SALUD
# MÓDULO 2 - TRATAMIENTO DE DATOS Y CARACTERÍSTICAS RELACIONADAS CON LAS PERSONAS
# CLASE 8 - PREPARACIÓN DE LAS BASES DE DATOS PARA ANÁLISIS DE SITUACIÓN DE SALUD 2

# 1. Para seguir la clase ####

# Instalar/Cargar paquetes

pacman::p_load(pacman,      # Instalar y cargar paquetes
               rio,         # Importación y exportación de datos
               tidyverse,   # Manejo y tratamiento de datos     
               lubridate,   # Manejo de variables de tipo fecha
               abjutils)    # Limpieza de texto

# Importar notificaciones del síndrome gripal (GS) en el sg realizada por un estado brasileño, 2022 (Descargar: 17/06/2022)
sg  <- import("datos/sg_2022.csv") 

# Importar Notificaciones de Síndrome Respiratorio Agudo Severo (SRAS) en el SIVEP-Gripe realizadas por un estado brasileño, 2020-2022 (Descargar: 17/06/2022) 
srag  <- import("datos/srag_20_22.xlsx")

# Importar estimaciones de población municipal, 2020-2021 (Fuente: Ministerio de Salud) 
pop_21 <- import("datos/pop_20_21.xls", 
                 which ="pop_ma_2021")
pop_20 <- import("datos/pop_20_21.xls")

# Seleccíon SG
sg <- sg %>% 
  select(datanotificacao,sexo, idade, datainiciosintomas,  evolucaocaso, 
         classificacaofinal, municipioibge, resultadoteste,
         sintomas) 

# Seleccíon SRAG
srag <- srag %>% 
  select(dt_notific, dt_sin_pri,cs_sexo, dt_nasc, nu_idade_n, cs_raca, 
         sg_uf, uti, classi_fin, pcr_sars2, cs_escol_n, criterio,
         evolucao, dt_interna, id_regiona, co_mun_res, co_regiona)

# Renombrar pop
pop_20 <- pop_20 %>%    
  rename(MUNICIPIO = `Município de residência`,
         POP_2020 = pop_2020) 

pop_21 <- pop_21 %>%    
  rename(MUNICIPIO = `Município de residência`,
         POP_2021 = pop_2021)

# Renombrar sg
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

# Renombrar srag
srag <- srag %>%    
  rename(DT_NOTIFIC = dt_notific,
         DT_SIN_PRI = dt_sin_pri,
         CO_MUN_RES = co_mun_res,
         ID_REGIONA = id_regiona,
         CO_REGIONA = co_regiona,
         CS_SEXO = cs_sexo, 
         DT_NASC  = dt_nasc,
         NU_IDADE_N = nu_idade_n,
         CS_RACA = cs_raca,
         SG_UF = sg_uf,
         UTI = uti, 
         CLASSI_FIN = classi_fin,
         PCR_SARS2 = pcr_sars2,
         CS_ESCOL_N = cs_escol_n,
         EVOLUCAO = evolucao,
         DT_INTERNA = dt_interna,
         CRITERIO = criterio)

# Estructura general de las bases
glimpse(sg)
glimpse(srag)
glimpse(pop_20)
glimpse(pop_21)

# 2. Crear y transformar columnas: mutate() ####
# Crear una columna con base en otra variable de la base
sg_exemplo <- sg %>% 
  mutate(datanotificacao = DATANOTIFICACAO)

glimpse(sg_exemplo)

# Eliminar el objeto
rm(sg_exemplo)

# La función mutate permite múltiples argumentos
sg <- sg %>% 
  mutate(datanotificacao    = as_date(DATANOTIFICACAO),
         datainiciosintomas = as_date(DATAINICIOSINTOMAS))

glimpse(sg)

# 2.1 - Transformaciones de variables de tipo fecha ####
# Día-Mes-Año (Day-Month-Year)

srag <- srag %>% 
  mutate(dt_notific  = dmy(DT_NOTIFIC),
         dt_sin_pri  = dmy(DT_SIN_PRI),
         dt_nasc     = dmy(DT_NASC),
         dt_interna  = dmy(DT_INTERNA))

# glimpse() con variables seleccionadas para observar el resultado
srag %>% 
  select(DT_NOTIFIC, dt_notific,
         DT_SIN_PRI, dt_sin_pri,
         DT_NASC, dt_nasc) %>% 
  glimpse()

# Mes-Día-Año (Month-Day-Year)

mdy("11/03/2020")
mdy("Mar 11 20")

# Año-Día-Mes (Year-Day-Month)
ydm("2020 11 03")
ydm("2020 11 Março")

# Año-Mes-Día (Year-Month-Day)
ymd("20200311")

# Solo el mes
sg <- sg %>% 
  mutate(mes_notific = month(datanotificacao))

sg %>% 
  count(mes_notific)

# Solo el año 
srag <- srag %>% 
  mutate(ano_notific = year(dt_notific))

srag %>% 
  count(ano_notific)

# Año epidemiológico
epiyear("2022/01/01")
epiyear("2021/12/31")

# Semana epidemiológica 
epiweek("2021/12/31")
epiweek("2022/01/01")

# Semana epidemiológica 
sg %>% 
  mutate(se_notific  = epiweek(datanotificacao))%>% 
  count(se_notific)

# Año epidemiológico
srag %>% 
  count(epiyear(dt_notific))

# Fecha de hoy

today()
epiweek(today())

# Fecha y hora de ahora
date()
now()

# Fechas y Operadores aritméticos
sg <- sg %>% 
  mutate(delay_not = datanotificacao - datainiciosintomas) 

# Tabulación y clase de la variable delay_not para observar el resultado
sg %>% 
  count(delay_not)

class(sg$delay_not)

# Conteo de tiempo (numérico, en meses)
sg %>% 
  mutate(delay_not = 
           time_length(interval(datainiciosintomas,
                                datanotificacao),
                       "months"),
         delay_not = round(delay_not, 0)) 


# Conteo de tiempo (numérico, en días)
sg <- sg %>% 
  mutate(delay_not = 
           time_length(interval(datainiciosintomas, 
                                datanotificacao),
                       "days")) 

# Tabulación y clase de la variable delay_not para observar el resultado
sg %>% 
  count(delay_not) %>% 
  rename(DIA = delay_not,
         NUMERO = n)

class(sg$delay_not)


# Método strptime 
format(as_date("2020-03-11"), "%d/%m/%Y")
format(as_date("2020-03-11"), "%d %B %Y")

# 2.2 - Transformación de variables numéricas ####
# Números enteros
srag <- srag %>% 
  mutate(idade = as.integer(NU_IDADE_N))

# Números com decimales
srag <- srag %>% 
  mutate(idade2 = as.numeric(NU_IDADE_N))

# glimpse() con variables seleccionadas para observar el resultado
srag %>% 
  select(NU_IDADE_N, idade, idade2) %>% 
  glimpse()

srag <- srag %>% 
  select(-idade2)

# 2.3 - Transformación de variables de tipo factor ####

# Crear factor y cambiar las etiquetas
srag <- srag %>% 
  mutate(sexo = factor(CS_SEXO, 
                       levels = c("F", "M", "I"),
                       labels = c("Feminino", "Masculino", "Ignorado")))

srag %>% 
  count(CS_SEXO)

srag %>% 
  count(sexo)

# Factores en orden inverso
srag %>% 
  count(fct_infreq(sexo))

# 2.4 - Transformación de variables de tipo texto ####

# Transformar en carácter
caractere <- 12345
class(caractere)
caractere <- as.character(caractere)
class(caractere)

# Recortar textos según la posición
texto <- "Epidemiología"
class(texto)

str_sub(string = texto, 
        end = 3, 
        start = 1)

str_sub("Epidemiologia", 1, 3)

# Separar código y nombre del municipio
glimpse(pop_20)

pop_20 <- pop_20 %>% 
  mutate(cod_ibge       = str_sub(MUNICIPIO,1,6)) %>% 
  mutate(nome_municipio = str_sub(MUNICIPIO,start=8))

glimpse(pop_20)

# Atención a los números en str_sub()
pop_20 <- pop_20 %>% 
  mutate(cod_ibge = as.integer(cod_ibge))  

# Posición en orden inverso
str_sub(texto, -5, -1)

# Espacios son caracteres
str_sub("Vigilancia epidemiológica en emergencias de salud", -20)

# Encontrar partes de texto
str_detect(sg$SINTOMAS, 
           pattern = "tosse",
           u_case = FALSE) 

str_detect(sg$SINTOMAS, 
           pattern = "Tosse") 


# Identificar caracteres al principio de un texto
str_starts(srag$CO_MUN_RES, "21")

# Tabular en cuántos casos de srag el municipio de residencia comienza con "21"
table(str_starts(srag$CO_MUN_RES, "21"))

# Tabular cuántas veces aparece la palabra diarrea en la variable signos y síntomas
table(str_detect(sg$SINTOMAS, 
                 pattern = "Tosse"))

# Cambiar un texto a mayúsculas
sg <- sg %>% 
  mutate(sintomas=str_to_upper(SINTOMAS))

glimpse(sg %>% 
          select(SINTOMAS, sintomas))

# Convierte en minúsculas
str_to_lower(" Vigilância    epidemiológica    em    emergências    sanitárias")

# Elimina espacios al principio y al final
str_trim(" Vigilância    epidemiológica    em    emergências    sanitárias ", 
         side = "both")

# Elimina espacios excesivos
str_squish(" Vigilância    epidemiológica    em    emergências    sanitárias ")

# Elimina acentos
abjutils::rm_accent("Vigilância epidemiológica em emergências sanitárias")

# Elimina caracteres que no son letras y los sustituye por espacios. 
str_replace_all("Epidemi0l0gi@", "[^[:alpha:]]", " ")

# Tabular cuántos registros tuvieron Secreción Nasal o Tos
table(str_detect(sg$SINTOMAS, 
                 pattern = "Coriza|Tosse"))

# Strings dinámicos con str_glue()
str_glue("{nrow(sg)} casos de SG com início dos sintomas \n até {format(max(sg$datainiciosintomas), '%d %b %Y')}.")

# 3. Separar y unir variables: separate() e unite() ####
# Separar textos, con base en un estándar
sg <- sg %>% 
  separate(SINTOMAS,
           sep = ",",
           into = c("sintoma1", "sintoma2", "sintoma3", "sintoma4", 
                    "sintoma5", "sintoma6", "sintoma7", "sintoma8", 
                    "sintoma9"))

# Primeros registros (head) para comprobar transformación
sg %>% 
  select(sintomas, sintoma1, sintoma3) %>% 
  head()

# Unir textos en una sola columna
sg <- sg %>% 
  unite(
    col = "SINTOMAS",         
    c("sintoma1", "sintoma2", "sintoma3", 
      "sintoma4", "sintoma5", "sintoma6",
      "sintoma7", "sintoma8", "sintoma9"), 
    sep = ", ",                  
    remove = TRUE,                
    #na.rm = TRUE                    
  )

class(sg$SINTOMAS)

# view() para comprobar transformación
sg %>% 
  select(sintomas, SINTOMAS) %>% 
  view()


# 4. (Re) categorizar variáveis: case_when() ####

# Frecuencia de la variable UTI
srag %>% 
  count(UTI)

# Crear variable uti con labels 
srag <- srag %>%
  mutate(uti = case_when(UTI == "1.0" ~ "Unidade de terapia intensiva", 
                         UTI == "2.0" ~ "Outras internações",
                         TRUE     ~ "Ignorado"))

# Frecuencia de la nueva variable uti
srag %>% 
  count(uti)

names(srag)

# Frecuencia de la variable CLASSIFICACAOFINAL
sg %>% 
  count(CLASSIFICACAOFINAL)

# Crear variable classificacaofinal y criterio_class
sg <- sg %>% 
  mutate(classificacaofinal = 
           case_when(CLASSIFICACAOFINAL   == "Confirmado Clínico Epidemiológico"  | 
                       CLASSIFICACAOFINAL == "Confirmado Clínico Imagem" |
                       CLASSIFICACAOFINAL == "Confirmado Laboratorial"|
                       CLASSIFICACAOFINAL == "Confirmado por Critério Clínico" 
                     ~ "Confirmado",
                     CLASSIFICACAOFINAL   == "Descartado" ~ CLASSIFICACAOFINAL,
                     TRUE ~ "Ignorado")) %>% 
  mutate(criterio_class = 
           case_when(CLASSIFICACAOFINAL == "Confirmado Clínico Epidemiológico" 
                     ~ "Clínico Epidemiológico",
                     CLASSIFICACAOFINAL == "Confirmado Clínico Imagem" 
                     ~ "Clínico Imagem",
                     CLASSIFICACAOFINAL == "Confirmado Laboratorial"
                     ~ "Laboratorial",
                     CLASSIFICACAOFINAL == "Confirmado por Critério Clínico" 
                     ~ "Clínico",
                     TRUE ~ "")) 

# Frecuencia de las nuevas variables classificacaofinal y criterio_class
sg %>% 
  count(classificacaofinal)

sg %>% 
  count(criterio_class)

# ifelse() para transformaciones con dos condiciones
sg <- sg %>% 
  mutate(tosse = ifelse(str_detect(sintomas, "TOSSE"), 
                        "Sim", "Não"))

# Frecuencia de la nueva variable tos
sg %>%
  count(tosse)

# 5. Filtrar líneas: filter() ####
# Filtrar casos confirmados
sg %>%
  filter(classificacaofinal == "Confirmado") %>% 
  count(criterio_class)

# Filtrar niños y adolescentes
srag %>%
  filter(NU_IDADE_N < 18) %>% 
  count()

edad_nino <- srag %>% 
  filter(NU_IDADE_N < 18)

summary(edad_nino$NU_IDADE_N)

# Transformaciones en las bases de datos del curso ####

# mutate() para SG
sg <- sg %>% 
  mutate(datanotificacao = ymd(DATANOTIFICACAO)) %>% 
  #mutate(datanotificacao = ymd(as.character(DATANOTIFICACAO))) %>% 
  mutate(mes_notific     = month(datanotificacao)) %>% 
  mutate(se_notific      = epiweek(datanotificacao)) %>% 
  mutate(sexo            = case_when(SEXO == "Indefinido" ~ "Ignorado",
                                     TRUE ~ SEXO)) %>% 
  mutate(datainiciosintomas = ymd(DATAINICIOSINTOMAS)) %>% 
  mutate(mes_ini_sint       = month(datainiciosintomas)) %>% 
  mutate(ano_ini_sint       = year(datainiciosintomas)) %>% 
  mutate(se_ini_sint        = epiweek(datainiciosintomas)) %>% 
  mutate(evolucaocaso       = case_when(EVOLUCAOCASO == "Em_tratamento_domiciliar" ~  
                                          "Alta Hospitalar",
                                        EVOLUCAOCASO == "Internado" ~
                                          "Ignorado",
                                        EVOLUCAOCASO == "Internado_em_UTI" ~
                                          "Ignorado",
                                        EVOLUCAOCASO == "" ~
                                          "Ignorado",
                                        TRUE ~ EVOLUCAOCASO)) %>% 
  mutate(evolucaocaso       = factor(evolucaocaso,
                                     levels = c("Cura", "Alta Hospitalar", "Óbito", "Ignorado", "Cancelado"))) %>% 
  mutate(classificacaofinal = case_when(CLASSIFICACAOFINAL == "Confirmado Clínico Epidemiológico"  | 
                                          CLASSIFICACAOFINAL == "Confirmado Clínico Imagem" |
                                          CLASSIFICACAOFINAL == "Confirmado Laboratorial"|
                                          CLASSIFICACAOFINAL == "Confirmado por Critério Clínico" ~ "Confirmado",
                                        CLASSIFICACAOFINAL =="Descartado" ~ "Descartado",
                                        TRUE ~ "Ignorado")) %>% 
  mutate(classificacaofinal       = factor(classificacaofinal,
                                           levels = c("Confirmado", "Descartado", "Ignorado"))) %>% 
  mutate(criterio_class = case_when(CLASSIFICACAOFINAL == "Confirmado Clínico Epidemiológico" ~ 
                                      "Clínico Epidemiológico",
                                    CLASSIFICACAOFINAL == "Confirmado Clínico Imagem" ~ 
                                      "Clínico Imagem",
                                    CLASSIFICACAOFINAL == "Confirmado Laboratorial"~ 
                                      "Laboratorial",
                                    CLASSIFICACAOFINAL == "Confirmado por Critério Clínico" ~
                                      "Clínico",
                                    TRUE ~ "")) %>% 
  mutate(municipioibge = str_sub(MUNICIPIOIBGE, 1, 6)) %>% 
  mutate(municipioibge = as.numeric(municipioibge)) %>% 
  separate(SINTOMAS,
           sep = ",",
           into = c("sintoma1", "sintoma2", "sintoma3", "sintoma4", "sintoma5", "sintoma6",
                    "sintoma7", "sintoma8", "sintoma9"))
glimpse(sg)

# mutate() para SRAG
srag <- srag %>% 
  mutate(dt_notific  = dmy(DT_NOTIFIC),
         se_notific  = epiweek(dt_notific),
         mes_notific = month(dt_notific))  %>% 
  mutate(dt_sin_pri  = dmy(DT_SIN_PRI),
         se_sin_pri  = epiweek(dt_sin_pri),
         mes_sin_pri = month(dt_sin_pri)) %>% 
  mutate(cs_sexo     = case_when(CS_SEXO == "M" ~ "Masculino",
                                 CS_SEXO == "F" ~ "Feminino",
                                 TRUE ~ "Ignorado")) %>% 
  mutate(dt_nasc     = dmy(DT_NASC)) %>% 
  mutate(nu_idade_n  = as.numeric(NU_IDADE_N)) %>% 
  mutate(cs_raca     = case_when(CS_RACA == "1.0" ~ "Branca",
                                 CS_RACA == "2.0" ~ "Preta",
                                 CS_RACA == "3.0" ~ "Amarela",
                                 CS_RACA == "4.0" ~ "Parda",
                                 CS_RACA == "5.0" ~ "Indígena",
                                 TRUE ~ "Ignorado")) %>% 
  mutate(uti = case_when(UTI == "1.0" ~ "Unidade de terapia intensiva", 
                         UTI == "2.0" ~ "Outras internações",
                         TRUE         ~ "Ignorado")) %>% 
  mutate(cs_escol_n = case_when(CS_ESCOL_N == "0.0" ~ "Sem escolaridade/ Analfabeto",
                                CS_ESCOL_N == "1.0" ~ "Fundamental 1º ciclo (1ª a 5ª série)",
                                CS_ESCOL_N == "2.0" ~ "Fundamental 2º ciclo (6ª a 9ª série)",
                                CS_ESCOL_N == "3.0" ~ "Médio (1º ao 3º ano)",
                                CS_ESCOL_N == "4.0" ~ "Superior",
                                CS_ESCOL_N == "5.0" ~ "Não se aplica",
                                TRUE ~ "Ignorado")) %>% 
  mutate(cs_escol_n = factor(cs_escol_n,
                             levels = c("Sem escolaridade/ Analfabeto",
                                        "Fundamental 1º ciclo (1ª a 5ª série)",
                                        "Fundamental 2º ciclo (6ª a 9ª série)",
                                        "Médio (1º ao 3º ano)",
                                        "Superior",
                                        "Não se aplica",
                                        "Ignorado"))) %>% 
  mutate(criterio = case_when(CRITERIO == "1.0" ~ "Laboratorial",
                              CRITERIO == "2.0" ~ "Clínico Epidemiológico",
                              CRITERIO == "3.0" ~ "Clínico",
                              CRITERIO == "4.0" ~ "Clínico Imagem",
                              TRUE ~ "Ignorado")) %>% 
  mutate(evolucao = case_when(EVOLUCAO == "1.0" ~ "Cura",
                              EVOLUCAO == "2.0" ~ "Óbito",
                              EVOLUCAO == "3.0" ~ "Óbito por outras causas",
                              TRUE ~ "Ignorado")) %>% 
  mutate(dt_interna = dmy(DT_INTERNA)) %>% 
  mutate(co_mun_res = str_sub(CO_MUN_RES,1,6),
         co_mun_res = as.numeric(co_mun_res)) %>% 
  mutate(co_regiona = str_sub(CO_REGIONA,1,4),
         co_regiona = as.numeric(co_regiona))

# mutate() para estimaciones de población 
pop_20 <- pop_20 %>% 
  mutate(cod_ibge       = str_sub(MUNICIPIO,1,6),
         cod_ibge       = as.numeric(cod_ibge)) %>% 
  mutate(nome_municipio = str_sub(MUNICIPIO,start=8))

pop_21 <- pop_21 %>% 
  mutate(cod_ibge       = str_sub(MUNICIPIO,1,6),
         cod_ibge       = as.numeric(cod_ibge)) %>% 
  mutate(nome_municipio = str_sub(MUNICIPIO,8))

# Veamos las transformaciones
glimpse(srag)
glimpse(sg)
glimpse(pop_20)
glimpse(pop_21)

# filter() para SG
sg <- sg %>% 
  filter(datanotificacao > "2022-01-01" &
           datanotificacao < "2022-07-01") %>% 
  filter(evolucaocaso != "Cancelado") %>% 
  filter(str_sub(municipioibge,1,2)=="21") 

# filter() para SRAG
srag <- srag %>% 
  filter(dt_notific < "2022-07-01" &
           dt_notific > "2020-03-01") %>% 
  filter(dt_notific < "2022-07-01" &
           dt_notific > "2020-03-01") %>% 
  filter(SG_UF == "MA") %>% 
  filter(CLASSI_FIN == "5.0" | 
           PCR_SARS2 == "1.0") %>% 
  filter(str_sub(co_mun_res,1,2)=="21") 

# Exportar basis transformadas ####
export(srag, "datos/srag_tratada.rds")
export(sg, "datos/sg_tratada.rds")
export(pop_20, "datos/pop_20_aula9.rds")
export(pop_21, "datos/pop_21_aula9.rds")

# ¡¡NO SUFRAS SOLO CUANDO EL CONTROL SE BLOQUEA!! ¡¡PEDIR AYUDA!!