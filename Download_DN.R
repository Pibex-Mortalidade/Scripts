# Carregando os pacotes necessários.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, RCurl)

# Criando a pasta que receberá os arquivos.
if(!dir.exists("datasus")) dir.create("datasus/")
setwd("./datasus")

### Identificando os arquivos no site para o download
FTP <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"

# Para fazer o download de uma UF especifica defina dentro do subset senao remova o pipe( %>% ) após o unlist.
filenames = getURL(FTP, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>% 
  strsplit("\r*\n") %>%
  unlist() %>%
  str_subset("DORN")

### Efetuando o download dos arquivos.
walk2(paste0(FTP,filenames),
      filenames,
      ~download.file(.x, destfile = .y, mode="wb")
)

# Para ter acesso aos dados SINASC deve-se fazer o ajuste pelo ftp indicado abaixo e no subset alterar para "DN":
# "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"

