# Para iniciar conexión con GitHub

usethis::create_github_token() # redirige a github donde eligiras nombre especifico del token

# copia el token para después ingresarlo con gitcreds_set()

gitcreds::gitcreds_set()

### Mi token
#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

# Configurar usuario de gitHub
# que abre el archivo .gitconfig
usethis::edit_git_config() 

# inicializar el repositorio de Git
usethis::use_git() #

# conectar tu repositorio local de Git con los servidores de GitHub
usethis::use_github()

install.packages("devtools")
library(devtools)
install_github("twitter/AnomalyDetection")

install.packages("githubinstall")
library(githubinstall)
githubinstall("AnomalyDetection", force = TRUE)

githubinstall("rstudio/rmarkdown", force = TRUE)


#https://github.com/uhub/awesome-r 
# Este es un repositorio de librerias para Rstudio

