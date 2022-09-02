# ORGANIZACIÓN PANAMERICANA DE LA SALUD (OPS) - OFICINA DE BRASIL
# UNIDAD TÉCNICA DE VIGILANCIA, PREPARACIÓN Y RESPUESTA A EMERGENCIAS Y DESASTRES
# CURSO PRINCIPIANTE DE R APLICADO A EMERGENCIAS DE SALUD
# MÓDULO 2 - TRATAMIENTO DE DATOS Y CARACTERÍSTICAS RELACIONADAS CON LAS PERSONAS
# CLASE 9 - TABULACIÓN DE DATOS

# 1. Para acompanhar a aula ####
# Instalar/Carregar pacotes
pacman::p_load(rio, 
               tidyverse, 
               lubridate)

# Importar Notificaciones de síndrome gripal en el eSUS, 2022 (Descarga: 17/06/2022)
sg <- import("datos/sg_tratada.rds") 

# Importar Notificaciones de síndrome respiratorio agudo grave en el SIVEP-Gripe,
  # 2020-2022 (Descarga: 17/06/2022) 
srag <- import("datos/srag_tratada.rds")

# Importar estimaciones de población municipales, Ministerio de Salud,
  # 2020-2022 (Descarga: 17/06/2022) 
pop_21 <- import("datos/pop_21_aula9.rds")
pop_20 <- import("datos/pop_20_aula9.rds")


# 3. Agrupar datos: group_by()  ####
# Agrupar los casos de SRAG por raza / color de piel
srag_exemplo <- srag %>% 
  group_by(cs_raca) 

# Agrupar los casos de SRAG por sexo y tipo de cama
srag_sex_uti <- srag %>% 
  group_by(cs_sexo) %>% 
  count(uti) 

# Observar el objeto creado en la consola
srag_sex_uti 

# Crear el mismo objeto sin group_by()
srag_sex_uti2 <- srag %>% 
  count(cs_sexo, uti) 

# Observar el objeto creado en la consola
srag_sex_uti2 

# Crear el objeto pop_exemplo con el conteo de municipios
pop_exemplo <- pop_20 %>% 
  count(POP_2020) 

# Observar el objeto creado en la consola
head(pop_exemplo)

# 4. Resumir datos en tablas: summarise() ####
# Crear el objeto srag_sex_uti con summarise()
srag_sex_uti3 <- srag %>% 
  group_by(cs_sexo, uti) %>% 
  summarise(n = n())

# Observar el objeto creado en la consola
srag_sex_uti3

# Al igual que otras funciones, summarise() puede tener más de un argumento
srag_evol_idade <- srag %>% 
  group_by(evolucao) %>% 
  summarise(n     = n(),                        # Conteo
            idade_min   = min(nu_idade_n),      # Mínima
            idade_media = mean(nu_idade_n),     # Media
            idade_max   = max(nu_idade_n),      # Máxima
            idade_mediana = median(nu_idade_n)) # Mediana

srag_evol_idade

# Sem agrupamento: resultados para toda a base
srag_idade_tot <- srag %>% 
  # group_by(evolucao) %>% 
  summarise(n     = n(),                        # Contagem
            idade_min   = min(nu_idade_n),      # Mínima
            idade_media = mean(nu_idade_n),     # Mediana
            idade_max   = max(nu_idade_n),      # Máxima
            idade_mediana = median(nu_idade_n)) # Mediana

srag_idade_tot

# ¡Cuidado con la agrupación! ¿Qué observa usted en el cálculo de la proporción a continuación?
srag_raca <- srag %>% 
  group_by(cs_raca) %>% 
  summarise(n     = n(), 
            prop = (n/sum(n))*100) 

srag_raca

# En varios casos es mejor usar el mutate() 
srag_raca <- srag %>% 
  group_by(cs_raca) %>% 
  summarise(n     = n())%>% 
  mutate(prop = (n/sum(n))*100)

srag_raca

# round() para redondear
srag_raca <- srag %>% 
  group_by(cs_raca) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) 
srag_raca


# 5. Ordenar los datos: arrange()  ####
# Ascendente
srag_raca <- srag %>% 
  group_by(cs_raca) %>% 
  summarise(n=n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1))  %>% 
  arrange(prop)

srag_raca

# Descendente
srag_raca <- srag %>% 
  group_by(cs_raca) %>% 
  summarise(n=n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1))  %>% 
  arrange(desc(prop))

srag_raca


# Transformaciones de factor para mover los "Ignorados" al final
srag_raca <- srag %>% 
  mutate(cs_raca = fct_infreq(cs_raca),
         cs_raca = fct_relevel(cs_raca, 
                               "Ignorado", 
                               after = Inf)) %>%
  group_by(cs_raca) %>% 
  summarise(n = n()) %>%
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1))

srag_raca

# Eliminar objetos temporales
rm(srag_exemplo, srag_sex_uti, srag_sex_uti2, srag_sex_uti3,
   srag_idade_tot, srag_evol_idade, pop_exemplo, srag_raca)

# SRAG por COVID-19: Sexo 2020
srag_sexo_20 <- srag %>% 
  filter(year(dt_sin_pri) == 2020) %>% 
  group_by(cs_sexo) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  arrange(desc(prop))

srag_sexo_20

# Unir n y % en una sola variable
srag_sexo_20 <- srag_sexo_20 %>% 
  mutate(n_prop20 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

srag_sexo_20

# SRAG por COVID-19: Sexo 2021
srag_sexo_21 <- srag %>% 
  filter(year(dt_sin_pri) == 2021) %>% 
  group_by(cs_sexo) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  arrange(desc(prop))%>% 
  mutate(n_prop21 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

# SRAG por COVID-19: Sexo 2022
srag_sexo_22 <- srag %>% 
  filter(year(dt_sin_pri) == 2022) %>% 
  group_by(cs_sexo) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  arrange(desc(prop))%>% 
  mutate(n_prop22 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

# SRAG por COVID-19: Nivel de educación 2020
srag_esc_20 <- srag %>% 
  filter(year(dt_sin_pri) == 2020) %>% 
  group_by(cs_escol_n) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1))%>% 
  mutate(n_prop20 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

srag_esc_20

# SRAG por COVID-19: Nivel de educación 2021
srag_esc_21 <- srag %>% 
  filter(year(dt_sin_pri) == 2021) %>% 
  group_by(cs_escol_n) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  mutate(n_prop21 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

# SRAG por COVID-19: Nivel de educación 2022
srag_esc_22 <- srag %>% 
  filter(year(dt_sin_pri) == 2022) %>% 
  group_by(cs_escol_n) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  mutate(n_prop22 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

# SRAG por COVID-19: Raza / Color 2020
srag_raca_20 <- srag %>% 
  filter(year(dt_sin_pri) == 2020) %>% 
  mutate(cs_raca = fct_infreq(cs_raca),
         cs_raca = fct_relevel(cs_raca, 
                               "Ignorado", 
                               after = Inf)) %>%
  group_by(cs_raca) %>% 
  summarise(n = n()) %>%
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  mutate(n_prop20 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

# SRAG por COVID-19: Raza / Color 2021
srag_raca_21 <- srag %>% 
  filter(year(dt_sin_pri) == 2021) %>% 
  mutate(cs_raca = fct_infreq(cs_raca),
         cs_raca = fct_relevel(cs_raca, 
                               "Ignorado", 
                               after = Inf)) %>%
  group_by(cs_raca) %>% 
  summarise(n = n()) %>%
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  mutate(n_prop21 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

# SRAG por COVID-19: Raza / Color 2022
srag_raca_22 <- srag %>% 
  filter(year(dt_sin_pri) == 2022) %>% 
  mutate(cs_raca = fct_infreq(cs_raca),
         cs_raca = fct_relevel(cs_raca, 
                               "Ignorado", 
                               after = Inf)) %>%
  group_by(cs_raca) %>% 
  summarise(n = n()) %>%
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) %>% 
  mutate(n_prop22 = str_c(n, " (", prop, ")")) %>% 
  select(-n, -prop)

# Notificaciones de SG: Clasificación final
sg_class <- sg %>% 
  group_by(classificacaofinal) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) 

# Notificaciones de SG: Evolución
sg_evolucao <- sg %>% 
  group_by(evolucaocaso) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1)) 

# Atención a los denominadores ####
#Ej.: Criterio de clasificación
sg_crit_class <- sg %>%
  filter(criterio_class!= "") %>% 
  group_by(criterio_class) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100,
         prop = round(prop,1))%>% 
  arrange(desc(prop)) 

# Exportación de cada objeto individualmente
rio::export(srag_sexo_20, "resultados/sexo_srag20.xlsx")
rio::export(srag_sexo_21, "resultados/sexo_srag21.xlsx")
rio::export(srag_sexo_22, "resultados/sexo_srag22.xlsx")

# list() exportar todos los objetos en diferentes pestañas
# SRAG
export(list(sexo20 = srag_sexo_20, 
            sexo21 = srag_sexo_21,
            sexo22 = srag_sexo_22,
            esc20 = srag_esc_20,
            esc21 = srag_esc_21,
            esc22 = srag_esc_22,
            raca20 = srag_raca_20,
            raca21 = srag_raca_21,
            raca22 = srag_raca_22), 
       "resultados/tabelas_srag_aula9.xlsx")

# SG
export(list(evolucao = sg_evolucao,
            class = sg_class, 
            crit_class = sg_crit_class), 
       "resultados/tabelas_sg_aula9.xlsx") 


# 6. Combinar data.frames con bind() ####
# Cambiar el nombre de la variable en los tres data.frames de frecuencia de SG
sg_evolucao <- sg_evolucao %>% 
  rename(var = evolucaocaso)

sg_class <- sg_class %>% 
  rename(var = classificacaofinal)

sg_crit_class <- sg_crit_class %>% 
  rename(var = criterio_class)

# Combinar data.frames para crear la Tabla 1
tb1 <- bind_rows(sg_evolucao,
                   sg_class,
                   sg_crit_class)

tb1

# data.frame() para crear objetos de tipo dataframe
df_exemplo <- data.frame(var = c("Masculino", "Feminino"), 
                         n = c(50,50), 
                         prop = c("50%", "50%"))

# Vamos utilizar essa ideia para inserir o nome das variáveis na nossa tabela
df_exemplo <- data.frame(var = "Sexo", 
                         n = NA, 
                         prop = NA)

df_exemplo

# Ahora insertaremos dentro del comando bind_rows()
tb1 <- bind_rows(
  data.frame(var = "Evolução do caso", n = NA, prop = NA),
  sg_evolucao,
  data.frame(var = "Classificação Final", n = NA, prop = NA),
  sg_class,
  data.frame(var = "Critério de Classificação (n=12.347)", n = NA, prop = NA),
  sg_crit_class)

tb1

# 7. Combinar data.frames con join() ####
# Población: combinar las estimaciones de población de 2020 y 2021
pop <- pop_20 %>% 
  left_join(pop_21, by="MUNICIPIO")

pop

# SRAG: combinar las frecuencias anuales de la variable sexo
tb2_sexo <- 
  full_join(srag_sexo_20,
            srag_sexo_21,
            by = "cs_sexo") %>% 
  full_join(srag_sexo_22, 
            by = "cs_sexo") %>% 
  rename(var = cs_sexo)

tb2_sexo

# SRAG: combinar las frecuencias anuales de la variable raza / color de piel
tb2_raca <- 
  full_join(srag_raca_20,
            srag_raca_21,
            by = "cs_raca") %>% 
  full_join(srag_raca_22, 
            by = "cs_raca")%>% 
  rename(var = cs_raca)

# SRAG: combinar las frecuencias anuales de la variable nivel de educación
tb2_esc <- 
  full_join(srag_esc_20,
            srag_esc_21,
            srag_esc_22, 
            by = "cs_escol_n") %>% 
  full_join(srag_esc_22, 
            by = "cs_escol_n")%>% 
  rename(var = cs_escol_n)

tb2_total <- srag %>% 
  
# Crear objeto de la Tabla 2
tb2 <- 
  bind_rows(data.frame(var = "Sexo", n_prop20 = "", 
                       n_prop21 = "", n_prop22 = ""),
            tb2_sexo,
            data.frame(var = "Raça / Cor da pele", n_prop20 = "", 
                       n_prop21 = "", n_prop22 = ""),
            tb2_raca,
            data.frame(var = "Escolaridade", n_prop20 = "", 
                       n_prop21 = "", n_prop22 = ""),
            tb2_esc)

tb2

#Cambiar el nombre de las columnas de la Tabla 2
tb2 <- tb2 %>% 
  rename("Características" = var,
         "2020 n(%)" = n_prop20,
         "2021 n(%)" = n_prop21,
         "2022 n(%)" = n_prop22)
tb2

# Eliminar los objetos temporales que se crearon
rm(df_exemplo, 
   srag_esc_20, srag_esc_21, srag_esc_22,
   srag_raca_20, srag_raca_21, srag_raca_22, 
   srag_sexo_20, srag_sexo_21, srag_sexo_22,
   tb2_esc, tb2_raca, tb2_sexo,
   sg_class, sg_crit_class, sg_evolucao)

# Exportar las tablas unificadas 
export(list(tb1 = tb1,
             tb2 = tb2), 
       "resultados/tabelas1_2.xlsx")

# SAIBA MAIS! Para confecção e diagramação de tabelaS no R, explore as funcionalidades 
# de pacotes como descr, htmlTable, summarytools, flextable, formattable, mmtable2 e Rmarkdown
