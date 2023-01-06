

setwd("C:/Users/eddus/Documents/UnB/Laboratorio1-Shiny/dados_SIM_SESDF/SES_DODF")

library(stringr)
library(data.table)
library(dplyr)
library(Amelia)


################################################################################

library(foreign)

SSDF_Obitos2016 <- foreign::read.dbf(file = "DO2016_oficial.DBF")

View(SSDF_Obitos2000)

SSDF_Obitos2017 <- foreign::read.dbf(file = "DO2017_Id.DBF")

View(SSDF_Obitos2000)

SSDF_Obitos2020 <- foreign::read.dbf(file = "DOI2020_Id.DBF")

View(SSDF_Obitos2000)




################################################################################

filesSESDF <- list.files(pattern = "SSDF")

dadosSESDF <- list(data.frame(),data.frame(),data.frame(),data.frame(),data.frame(),
                data.frame(),data.frame(),data.frame(),data.frame(),data.frame(),
                data.frame(),data.frame(),data.frame(),data.frame(),data.frame(),
                data.frame(),data.frame(),data.frame(),data.frame(),data.frame())

namesSESDF<- list(character(),character(),character(),character(),character(),
               character(),character(),character(),character(),character(),
               character(),character(),character(),character(),character(),
               character(),character(),character(),character(),character())

for (i in 1:length(dadosSESDF)) {
  
  dadosSESDF[[i]] <- fread(file = filesSESDF[i])
  dadosSESDF[[i]] <- data.frame( dadosSESDF[[i]])
  namesSESDF[[i]] <- colnames(dadosSESDF[[i]])
  
}


all_names <- character()

for (i in 1:length(namesSESDF)) {
  
  all_names <- append(all_names, namesSESDF[[i]], after = length(all_names))
  
}


unique_names <- unique(tolower(all_names))

length(unique_names)

col_SESDF <- data.frame(rep(0, 127))
col_SESDF$unique_names <- unique_names
colnames(col_SESDF) <- c("quantidade", "unique_names")

# Computando quantas vezes cada variavel aparece nos 20 arquivos

for (i in 1:length(dadosSESDF)) {
  
  for (j in 1:length(dadosSESDF[[i]])) {
    
    for (k in 1:length(col_SESDF$unique_names)) {
      
      if(col_SESDF$unique_names[k] == tolower(colnames(dadosSESDF[[i]][j]))){
        
        col_SESDF$quantidade[k] <- col_SESDF$quantidade[k] + 1
      }
    }
  }
}

# Criando colunas NA para deixar todos os arquivos com as mesmas 127 variaveis e na mesma ordem

for (i in 1:length(dadosSESDF)) {
  
  for (j in 1:length(dadosSESDF[[i]])) {
    
    for (k in 1:length(unique_names)) {    
  
      if ((unique_names[k] %in% tolower(colnames(dadosSESDF[[i]]))) == FALSE) {
        
        dadosSESDF[[i]][,unique_names[k]] <- c(rep(NA, nrow(dadosSESDF[[i]])))
      } 
    }
  }
  
  dadosSESDF[[i]] <- dadosSESDF[[i]][ , order(names(dadosSESDF[[i]]))]
  colnames(dadosSESDF[[i]]) <- tolower(colnames(dadosSESDF[[i]]))
}

# Consolidando as bases em uma unica usando como chave o nome da coluna/variavel

SESDFconsolidado <- data.frame(rbind(dadosSESDF[[1]], dadosSESDF[[2]], dadosSESDF[[3]], 
                                     dadosSESDF[[4]], dadosSESDF[[5]], dadosSESDF[[6]],
                                     dadosSESDF[[7]], dadosSESDF[[8]], dadosSESDF[[9]],
                                     dadosSESDF[[10]], dadosSESDF[[11]], dadosSESDF[[12]],
                                     dadosSESDF[[13]], dadosSESDF[[14]], dadosSESDF[[15]],
                                     dadosSESDF[[16]], dadosSESDF[[17]], dadosSESDF[[18]],
                                     dadosSESDF[[19]], dadosSESDF[[20]]))


dim(SESDFconsolidado)
str(SESDFconsolidado)
View(SESDFconsolidado)

#gerando arquivo csv

write.csv(SESDFconsolidado, file = "SESDFconsolidado.csv", row.names = FALSE)

######################################################################
# Filtrando os dados para os residentes do DF (codmunres inicia com 53)

dados <- fread("SESDFconsolidado.csv")

dados <- data.frame(dados)

dim(dados)

subset(dados, substr(dados$codmunres, 1, 2) == 53)$codmunres %>% table()

dados <- subset(dados, substr(dados$codmunres, 1, 2) == 53)

dim(dados)

write.csv(dados, file = "SESDFconsolidado_filtered.csv", row.names = FALSE)


########################################################################
# Ajustando a variavel codbaires no ano de 2010 (buscando os codigos em outros anos usando a variavel baires)

dados <- fread("SESDFconsolidado_filtered.csv")

dados <- data.frame(dados)

#Variavel ano do obito
dados$ano_obito <- str_sub(as.character(dados$dtobito),-4,-1)

str(dados)

dim(dados)

View(dados$baires)

View(dados[which(dados["ano_obito"] == 2010), c("baires","codbaires")])
baires2010 <- dados[which(dados["ano_obito"] == 2010), c("baires","codbaires")]
View(baires2010)

logic_vec_codbaires <- is.na(as.numeric(baires2010$codbaires)) 
logic_vec_codbaires

baires2010_NA <- baires2010[logic_vec_codbaires,]
View(baires2010_NA)

baires2010_NA_grouped <- baires2010_NA %>%
                        select(baires, codbaires) %>%
                        group_by(baires, codbaires) %>%
                        summarise("quantidade" = n()) 
View(baires2010_NA_grouped)

baires2010_faltantes <- data.frame(as.character(paste(baires2010_NA_grouped$baires, baires2010_NA_grouped$codbaires, sep = " ")))
colnames(baires2010_faltantes) <- "baires"
baires2010_faltantes$baires <- toupper(baires2010_faltantes$baires)
View(baires2010_faltantes)

baires_demais <- dados[which(dados["ano_obito"] != 2010 &
                               dados["ano_obito"] != 2020 &
                               is.na(dados["baires"]) == FALSE &
                               is.na(dados["codbaires"]) == FALSE &
                               dados["baires"] != "" &
                               dados["baires"] != "AGUARDE" &
                               dados["baires"] != "Aguarde..."), c("baires", "codbaires")]



baires_demais$baires <- toupper(baires_demais$baires)

baires_demais_grouped <- baires_demais %>%
                          group_by(baires, codbaires) %>%
                          group_keys()

baires_demais_grouped$baires <- toupper(baires_demais_grouped$baires)

View(baires_demais_grouped)

baires2010_faltantes2 <- left_join(baires2010_faltantes, baires_demais_grouped, by = "baires")

# write.csv(baires2010_faltantes2, file = "codbaires_faltantes.txt", sep = ",")

### Nesse ponto foi efetuado um procedimento manual para complementar os dados no arquivo "codbaires_faltantes.txt" ###

baires_codigos <- read.csv("codbaires_faltantes.txt", sep = ",", header = TRUE)
View(baires_codigos)

nrow(dados[which(dados[,"ano_obito"] == 2010),])

rows_NA <- which(is.na(as.numeric(dados[dados[,"ano_obito"] == 2010,"codbaires"])) == TRUE)


length(rows_NA)


for (i in rows_NA) {
  
  for (j in 1:nrow(baires_codigos)) {
    
    if(toupper(as.character(paste(dados[which(dados[,"ano_obito"] == 2010),"baires"][i], 
                                  dados[which(dados[,"ano_obito"] == 2010),"codbaires"][i], sep = " "))) == toupper(baires_codigos$baires[j])){
      
      dados[which(dados[,"ano_obito"] == 2010),"codbaires"][i] <- baires_codigos$codbaires[j]
      
    }
  }
  print(i)
}

write.csv(dados, file = "SESDFconsolidado_filtered.csv", row.names = FALSE)

##################################################################################################

dados <- fread("SESDFconsolidado_filtered.csv")

dataset <- data.frame(dados)

sum(is.na(dataset$codbaires))

##categorizaçao das RAS
dataset$codbaires <- as.numeric(dataset$codbaires)
dataset$RA=NA
dataset$RA[dataset$codbaires==5|dataset$codbaires==6|dataset$codbaires==299|
             dataset$codbaires==1| dataset$codbaires==4|dataset$codbaires==77|
             dataset$codbaires==1020|dataset$codbaires==1050|dataset$codbaires==1999]= 'Asa Sul' #OK


dataset$RA[dataset$codbaires==14|dataset$codbaires==15|dataset$codbaires==16|
             dataset$codbaires==20|dataset$codbaires==399|dataset$codbaires==1010|
             dataset$codbaires==1030|dataset$codbaires==1040|dataset$codbaires==1060]='Asa Norte' #OK

dataset$RA[dataset$codbaires==21|dataset$codbaires==22|dataset$codbaires==23|dataset$codbaires==24|
             dataset$codbaires==25|dataset$codbaires==26|dataset$codbaires==78|dataset$codbaires==116|
             dataset$codbaires==117|dataset$codbaires==27|dataset$codbaires==28|dataset$codbaires==29|
             dataset$codbaires==699|dataset$codbaires==2010|dataset$codbaires==2020|
             dataset$codbaires==2030|dataset$codbaires==2040|dataset$codbaires==2050|
             dataset$codbaires==2060|dataset$codbaires==2070|dataset$codbaires==2080|
             dataset$codbaires==2999]='Gama' #OK

dataset$RA[dataset$codbaires==30|dataset$codbaires==31|dataset$codbaires==32|dataset$codbaires==33|
             dataset$codbaires==34|dataset$codbaires==35|dataset$codbaires==36|dataset$codbaires==39|
             dataset$codbaires==119|dataset$codbaires==3010|dataset$codbaires==3020|
             dataset$codbaires==3030|dataset$codbaires==3999]='Taguatinga' #OK

dataset$RA[dataset$codbaires==52|dataset$codbaires==53|dataset$codbaires==54|dataset$codbaires==55|
             dataset$codbaires==499|dataset$codbaires==1001|dataset$codbaires==4010|
             dataset$codbaires==4020|dataset$codbaires==4030|dataset$codbaires==4999]='Brazlândia' #OK

dataset$RA[dataset$codbaires==56|dataset$codbaires==57|dataset$codbaires==109|
             dataset$codbaires==5010|dataset$codbaires==5020|dataset$codbaires==5030|
             dataset$codbaires==5040|dataset$codbaires==5999]='Sobradinho' #OK

dataset$RA[dataset$codbaires==63|dataset$codbaires==64|dataset$codbaires==80|dataset$codbaires==125|
             dataset$codbaires==126|dataset$codbaires==65|dataset$codbaires==66|dataset$codbaires==67|
             dataset$codbaires==68|dataset$codbaires==71|dataset$codbaires==85|dataset$codbaires==122|
             dataset$codbaires==123|dataset$codbaires==219|dataset$codbaires==6010|
             dataset$codbaires==6020|dataset$codbaires==6030|dataset$codbaires==6040|
             dataset$codbaires==6050|dataset$codbaires==6060|dataset$codbaires==6999]='Planaltina' #OK


dataset$RA[dataset$codbaires==18|dataset$codbaires==112|dataset$codbaires==69|dataset$codbaires==70|
             dataset$codbaires==72| dataset$codbaires==199|dataset$codbaires==7010|
             dataset$codbaires==7020|dataset$codbaires==7030|dataset$codbaires==7040|
             dataset$codbaires==7050|dataset$codbaires==7999]='Paranoá' #OK

dataset$RA[dataset$codbaires==2|dataset$codbaires==8|dataset$codbaires==8010|
             dataset$codbaires==8020|dataset$codbaires==8030|dataset$codbaires==8099]="Núcleo Bandeirante" #OK

dataset$RA[dataset$codbaires==42|dataset$codbaires==43|dataset$codbaires==44|dataset$codbaires==45|
             dataset$codbaires==46|dataset$codbaires==47|dataset$codbaires==48|dataset$codbaires==49|
             dataset$codbaires==68|dataset$codbaires==71|dataset$codbaires==85|dataset$codbaires==122|
             dataset$codbaires==50|dataset$codbaires==41| dataset$codbaires==51|dataset$codbaires==11|dataset$codbaires==40|
             dataset$codbaires==599| dataset$codbaires==531| dataset$codbaires==3513 |
             dataset$codbaires==236520| dataset$codbaires==685320|dataset$codbaires==9010|
             dataset$codbaires==9020|dataset$codbaires==9030|dataset$codbaires==9040|
             dataset$codbaires==9050|dataset$codbaires==9060|dataset$codbaires==9070|
             dataset$codbaires==9080|dataset$codbaires==9090|dataset$codbaires==9100|
             dataset$codbaires==9110|dataset$codbaires==9120|dataset$codbaires==9130|
             dataset$codbaires==9999]='Ceilândia' #OK

dataset$RA[dataset$codbaires==73|dataset$codbaires==74|dataset$codbaires==75|
             dataset$codbaires==76|dataset$codbaires==799|dataset$codbaires==10010|
             dataset$codbaires==10020|dataset$codbaires==10030|dataset$codbaires==10040|
             dataset$codbaires==10050|dataset$codbaires==10060|dataset$codbaires==10070|
             dataset$codbaires==10999]='Guará' #OK

dataset$RA[dataset$codbaires==12|dataset$codbaires==17|dataset$codbaires==139|
             dataset$codbaires==11010|dataset$codbaires==11020|dataset$codbaires==11999]='Cruzeiro' #OK

dataset$RA[dataset$codbaires==37|dataset$codbaires==38|dataset$codbaires==100|
             dataset$codbaires==101|dataset$codbaires==129|dataset$codbaires==156741|
             dataset$codbaires==12010|dataset$codbaires==12020|dataset$codbaires==12030|
             dataset$codbaires==12999]='Samambaia' #OK

dataset$RA[dataset$codbaires==83|dataset$codbaires==90|dataset$codbaires==169|
             dataset$codbaires==13010|dataset$codbaires==13020|dataset$codbaires==13030|
             dataset$codbaires==13040|dataset$codbaires==13050|dataset$codbaires==13060|
             dataset$codbaires==13999]='Santa Maria' #OK

dataset$RA[dataset$codbaires==82|dataset$codbaires==9|dataset$codbaires==10|dataset$codbaires==149|
             dataset$codbaires==14010|dataset$codbaires==14020|dataset$codbaires==14030|
             dataset$codbaires==14040|dataset$codbaires==14050|dataset$codbaires==14999]='São Sebastião' #OK

dataset$RA[dataset$codbaires==84|dataset$codbaires==81|dataset$codbaires==159|
             dataset$codbaires==15010|dataset$codbaires==15020|dataset$codbaires==15030|
             dataset$codbaires==15999]='Recanto das Emas' # OK

dataset$RA[dataset$codbaires==87|dataset$codbaires==102|dataset$codbaires==16010|
             dataset$codbaires==16020|dataset$codbaires==16030|dataset$codbaires==16999]='Lago Sul' #OK

dataset$RA[dataset$codbaires==89|dataset$codbaires==17010|dataset$codbaires==17020|
             dataset$codbaires==17999]='Riacho Fundo' #OK

dataset$RA[dataset$codbaires==88|dataset$codbaires==104|dataset$codbaires==18010|
             dataset$codbaires==18020|dataset$codbaires==18030|dataset$codbaires==18040|
             dataset$codbaires==18999]='Lago Norte' #OK

dataset$RA[dataset$codbaires==86|dataset$codbaires==19010]='Candangolândia' #OK

dataset$RA[dataset$codbaires==108|dataset$codbaires==115|dataset$codbaires==20010|
             dataset$codbaires==20020|dataset$codbaires==20030|dataset$codbaires==20040|
             dataset$codbaires==20050|dataset$codbaires==20999]='Águas Claras' #OK

dataset$RA[dataset$codbaires==92|dataset$codbaires==21010|dataset$codbaires==21020|
             dataset$codbaires==21030|dataset$codbaires==21999]='Riacho Fundo II' #OK

dataset$RA[dataset$codbaires==105|dataset$codbaires==106|dataset$codbaires==22010|
             dataset$codbaires==22020|dataset$codbaires==22999]='Sudoeste/Octogonal' #OK

dataset$RA[dataset$codbaires==19| dataset$codbaires==522|dataset$codbaires==23010]='Varjão' #OK

dataset$RA[dataset$codbaires==107|dataset$codbaires==127|dataset$codbaires==24010|
             dataset$codbaires==24020|dataset$codbaires==24030|dataset$codbaires==24040|
             dataset$codbaires==24999]='Park Way' #OK

dataset$RA[dataset$codbaires==93|dataset$codbaires==25010|dataset$codbaires==25020|
             dataset$codbaires==25030|dataset$codbaires==25999]='SCIA' #OK

dataset$RA[dataset$codbaires==58|dataset$codbaires==26010|dataset$codbaires==26020|
             dataset$codbaires==26030|dataset$codbaires==26999]='Sobradinho II' #OK

dataset$RA[dataset$codbaires==103|dataset$codbaires==121|dataset$codbaires==27010|
             dataset$codbaires==27020|dataset$codbaires==27030|dataset$codbaires==27040|
             dataset$codbaires==27999]="Jardim Botânico" #OK

dataset$RA[dataset$codbaires==113|dataset$codbaires==28010|dataset$codbaires==28020|
             dataset$codbaires==28030|dataset$codbaires==28999]='Itapoã' #OK

dataset$RA[dataset$codbaires==111|dataset$codbaires==120|dataset$codbaires==128|
             dataset$codbaires==29010]='SIA' #OK

dataset$RA[dataset$codbaires==110|dataset$codbaires==124|dataset$codbaires==118|
             dataset$codbaires==30010|dataset$codbaires==30020|dataset$codbaires==30030|
             dataset$codbaires==30040|dataset$codbaires==30999]='Vicente Pires' #OK

dataset$RA[dataset$codbaires==59|dataset$codbaires==60|dataset$codbaires==61|
             dataset$codbaires==62|dataset$codbaires==130|dataset$codbaires==31010|
             dataset$codbaires==31020|dataset$codbaires==31999]='Fercal' #OK

##categorizaçao de acordo com a ped

dataset$grupo = NA
dataset$grupo[(dataset$RA %in% c("Brasilia", 'Plano Piloto', 'Asa Norte', 'Asa Sul', 'Jardim Botânico', 'Lago Norte', 'Lago Sul', 'Park Way', 'Sudoeste/Octogonal'))] = 'grupo1'
dataset$grupo[(dataset$RA %in% c('Águas Claras', 'Candangolândia', 'Cruzeiro', 'Gama', 'Guará', 'Núcleo Bandeirante', 'Sobradinho',
                                 'Sobradinho II', 'Taguatinga', 'Vicente Pires'))] = 'grupo2'
dataset$grupo[(dataset$RA %in% c('Brazlândia', 'Ceilândia', 'Planaltina', 'Riacho Fundo', 'Riacho Fundo II', 'SIA', 'Samambaia', 'Santa Maria',
                                 'São Sebastião'))] = 'grupo3'
dataset$grupo[(dataset$RA %in% c('Fercal', 'Itapoã', 'Paranoá', 'Recanto das Emas', 'SCIA', 'Varjão'))] = 'grupo4'


## legenda:
#grupo_um  =   alta renda
# grupo dois = Media alta renda
# grupo tres =  Media-Baixa renda
#grupo quatro = Baixa renda

sum(is.na(dataset['grupo']))
table(dataset$grupo)

# Selecao das variaveis de interesse

variaveis <- c("numerodo", "dtobito", "sexo", "esc", "estciv",
               "idade","dtnasc", "codbaires","racacor", "grupo", "RA", "assistmed",
               "necropsia", "linhaa", "linhab", "linhac", "linhad", "linhaii",
               "causabas","ano_obito")

dataset_reduzido <- dataset[,variaveis]

write.csv(dataset_reduzido, file = "dataset_consolidado_reduzido.csv", row.names = FALSE)

#########################################################################################################

dataset_reduzido <- read.csv2("dataset_consolidado_reduzido.csv", sep = ",")

dim(dataset_reduzido)
str(dataset_reduzido)


##########################################################################################################

?str_detect

# Funcao para codificar a variavel CAUSABAS por capitulo da CID10
categ_cap_cid10 <- function(x) {
  if (str_detect(x, "A|B")) {
    return("I")
  } else if (str_detect(x, "C|(D0)|(D1)|(D2)|(D3)|(D4)")) {
    return("II")
  } else if (str_detect(x, "(D5)|(D6)|(D7)|(D8)")) {
    return("III")
  } else if (str_detect(x, "E")) {
    return("IV")
  } else if (str_detect(x, "F")) {
    return("V")
  } else if (str_detect(x, "G")) {
    return("VI")
  } else if (str_detect(x, "(H0)|(H1)|(H2)|(H3)|(H4)|(H5)")) {
    return("VII")
  } else if (str_detect(x, "(H6)|(H7)|(H8)|(H9)")) {
    return("VIII")
  } else if (str_detect(x, "I")) {
    return("IX")
  } else if (str_detect(x, "J")) {
    return("X")
  } else if (str_detect(x, "K")) {
    return("XI")
  } else if (str_detect(x, "L")) {
    return("XII")
  } else if (str_detect(x, "M")) {
    return("XIII")
  } else if (str_detect(x, "N")) {
    return("XIV")
  } else if (str_detect(x, "O")) {
    return("XV")
  } else if (str_detect(x, "P")) {
    return("XVI")
  } else if (str_detect(x, "Q")) {
    return("XVII")
  } else if (str_detect(x, "R")) {
    return("XVIII")
  } else if (str_detect(x, "S|T")) {
    return("XIX")
  } else if (str_detect(x, "V|W|X|Y")) {
    return("XX")
  } else if (str_detect(x, "Z")) {
    return("XXI")
  } else {
    return("**")
  }
}

dataset_reduzido$capcid10 <- NA

str(dataset_reduzido$causabas)

dataset_reduzido$causabas <- sub("[[:punct:]]", "", dataset_reduzido$causabas)

# Aplicando a funcao para criar uma nova variavel
dataset_reduzido$capcid10 <- sapply(dataset_reduzido$causabas, categ_cap_cid10)

dataset_reduzido$capcid10nomes <- dataset_reduzido$capcid10

dataset_reduzido_copia <- dataset_reduzido


# Adicionando os labels dos capitulos
dataset_reduzido$capcid10nomes <- factor(dataset_reduzido$capcid10nomes, 
                             levels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
                                        "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX",
                                        "XX", "XXI", "**"), 
                             labels = c("Algumas doenças infecciosas e parasitárias",
                                        "Neoplasmas (tumores)",
                                        "Doenças do sangue e dos árgãos hematopoéticos e alguns transtornos imunitários",
                                        "Doenças endócrinas, nutricionais e metabólicas",
                                        "Transtornos mentais e comportamentais",
                                        "doenças do sistema nervoso",
                                        "doenças do olho e anexos",
                                        "doenças do ouvido e da apófise mastóide",
                                        "doenças do aparelho circulatório",
                                        "doenças do aparelho respiratório",
                                        "doenças do aparelho digestivo",
                                        "Doenças da pele e do tecido subcutâneo",
                                        "Doenças do sistema osteomuscular e do tecido conjuntivo",
                                        "Doenças do aparelho geniturinário",
                                        "Gravidez, parto e puerpério",
                                        "Algumas afecções originadas no período perinatal",
                                        "Malformações congênitas, deformidades e anomalias cromossômicas",
                                        "Sintomas, sinais e achados anormais de exames clínicos e de laboratório, não classificados em outra parte",
                                        "Lesões, envenenamentos e algumas outras consequências de causas externas",
                                        "Causas externas de morbidade e de mortalidade",
                                        "Fatores que influenciam o estado de saúde e o contato com os serviços de saúde",
                                        "CID 10: Revisão não disponível ou não preenchido ou inválido"))


write.csv(dataset_reduzido, file = "dataset_consolidado_reduzido.csv", row.names = FALSE)




cid10causas <- readxl::read_xls("cid10causas.xls")

View(cid10causas)
str(cid10causas)
dim(cid10causas)


#dataset_reduzido$causabas <- sub("[[:punct:]]", "", dataset_reduzido$causabas)


dataset_reduzido$causabasnomes <- factor(dataset_reduzido$causabas, 
                                         levels = cid10causas$Codigo, 
                                         labels = cid10causas$DESCRICAO)



# Excluindo algumas variaveis que nao serao 

variaveis_exc <- c("assistmed","necropsia","linhaa","linhab","linhac","linhad","linhaii")

dataset_reduzido[,variaveis_exc] <- NULL

write.csv(dataset_reduzido, file = "dataset_consolidado_reduzido_backup3004.csv", row.names = FALSE)

write.csv(dataset_reduzido, file = "dataset_consolidado_reduzido.csv", row.names = FALSE)

###################################################################################################################
# Tratando e adicionando label aas demais variaveis

dataset_reduzido <- fread("dataset_consolidado_reduzido.csv")

dataset_reduzido <- data.frame(dataset_reduzido)

dim(dataset_reduzido)

dataset_reduzido[which(dataset_reduzido[,"sexo"] == 1), "sexo"] <- "M"
dataset_reduzido[which(dataset_reduzido[,"sexo"] == 2), "sexo"] <- "F"
dataset_reduzido[which(dataset_reduzido[,"sexo"] == 0), "sexo"] <- "I"

dataset_reduzido$sexo <- factor(dataset_reduzido$sexo, 
                                         levels = c("M", "F", "I"), 
                                         labels = c("Masculino",
                                                    "Feminino",
                                                    "Ignorado"))
table(dataset_reduzido$sexo)

dataset_reduzido[which(dataset_reduzido[,"esc"] == 0), "esc"] <- 9
dataset_reduzido[which(dataset_reduzido[,"esc"] == 8), "esc"] <- 9

dataset_reduzido$esc <- factor(dataset_reduzido$esc, 
                                levels = c(1, 2, 3, 4, 5, 9), 
                                labels = c("Nenhuma",
                                           "de 1 a 3 anos",
                                           "de 4 a 7 anos",
                                           "de 8 a 11 anos",
                                           "12 anos e mais",
                                           "ignorado"))

table(dataset_reduzido$esc)

dataset_reduzido$estciv <- factor(dataset_reduzido$estciv, 
                               levels = c(1, 2, 3, 4, 5, 9), 
                               labels = c("Solteiro",
                                          "Casado",
                                          "Viuvo",
                                          "Separado judicialmente/divorciado",
                                          "Uniao estavel",
                                          "ignorado"))

table(dataset_reduzido$estciv)

dataset_reduzido$racacor <- factor(dataset_reduzido$racacor, 
                                  levels = c(1, 2, 3, 4, 5), 
                                  labels = c("Branca",
                                             "Preta",
                                             "Amarela",
                                             "Parda",
                                             "Indigena"))

table(dataset_reduzido$racacor)

dataset_reduzido$grupo <- factor(dataset_reduzido$grupo,
                                 levels = c("grupo1", "grupo2", "grupo3", "grupo4"),
                                 labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4"))

table(dataset_reduzido$grupo)

library(stringr)

dataset_reduzido$dtnasc <- as.Date(paste(str_sub(string = as.character(dataset_reduzido$dtnasc), start = -4, end = -1),
                                         str_sub(string = as.character(dataset_reduzido$dtnasc), start = -6, end = -5),
                                         str_sub(string = as.character(dataset_reduzido$dtnasc), start = 1, end = -7),
                                         sep = "-"))

dataset_reduzido$dtobito <- as.Date(paste(str_sub(string = as.character(dataset_reduzido$dtobito), start = -4, end = -1),
                                          str_sub(string = as.character(dataset_reduzido$dtobito), start = -6, end = -5),
                                          str_sub(string = as.character(dataset_reduzido$dtobito), start = 1, end = -7),
                                          sep = "-"))

dataset_reduzido$idade_anos <- round(as.numeric((dataset_reduzido$dtobito-dataset_reduzido$dtnasc))/365,2)

write.csv(dataset_reduzido, file = "dataset_consolidado_reduzido.csv", row.names = FALSE)

#####################################################################################################################

# Categorizacao da idade

dataset_reduzido <- fread("dataset_consolidado_reduzido.csv")

dataset_reduzido <- data.frame(dataset_reduzido)

dataset_reduzido$idade_cat <- NA

dataset_reduzido$idade_cat <- cut(dataset_reduzido$idade_anos,
                                  breaks=c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,
                                           65,70,75,80,85,200),
                                  labels=c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                           "40-44","45-49","50-54","55-59","60-64","65-69",
                                           "70-74","75-79","80-84","85+"),
                                  include.lowest = TRUE, right = FALSE, ordered_result = TRUE)

View(dataset_reduzido[,c("idade_anos", "idade_cat")])

write.csv(dataset_reduzido, file = "dataset_consolidado_reduzido.csv", row.names = FALSE)

#####################################################################################################################

setwd("C:/Users/eddus/Documents/UnB/Laboratorio1-Shiny/dados_SIM_SESDF/SES_DODF")

library(stringr)
library(data.table)
library(dplyr)
library(DT)
library(Amelia)


dataset_reduzido <- fread("dataset_consolidado_reduzido.csv")

dataset_reduzido <- data.frame(dataset_reduzido)

str(dataset_reduzido)
dim(dataset_reduzido)
View(dataset_reduzido)

# Analise Exploratoria

# Dados NA/Missing

missmap(dataset_reduzido, 
        main = "Dados NA", 
        col = c("yellow", "black"), 
        legend = TRUE)


dados_missing <- data.frame("variavel" = names(dataset_reduzido),
                            "quantidade_NA" = NA)

j = 1

for (i in 1:length(names(dataset_reduzido))){
  dados_missing$quantidade_NA[j] <- sum(is.na(dataset_reduzido[i]))
  dados_missing$proporcao_NA[j] <- (sum(is.na(dataset_reduzido[i]) / nrow(dataset_reduzido)))*100
  j <- j  +1
}

View(dados_missing)
View(dataset_reduzido)

#######################################################################################
# Proporcao de dados NA em relacao ao proprio ano

dados_missing_prop_ano <- data.frame("variavel" = names(dataset_reduzido),
                                 "Geral"=NA, "2000" = NA, "2001" = NA, "2002" = NA,
                                 "2003" = NA, "2004" = NA, "2005" = NA, "2006" = NA,
                                 "2007" = NA, "2008" = NA, "2009" = NA, "2010" = NA,
                                 "2011" = NA, "2012" = NA, "2013" = NA, "2014" = NA,
                                 "2015" = NA, "2016" = NA, "2017" = NA, "2018" = NA,
                                 "2019" = NA, "2020" = NA)

colnames(dados_missing_prop_ano) <- c("variavel","Geral", unique(dataset_reduzido$ano_obito))

for(i in colnames(dados_missing_prop_ano)[-1]){
  
  for (j in names(dataset_reduzido)){
    
    for(k in dados_missing_prop_ano$variavel){
      
      if(j == k){
        
        if(i == "Geral"){
          
          dados_missing_prop_ano[,i][which(dados_missing_prop_ano$variavel == k, arr.ind = TRUE)] <- 
            round((sum(is.na(dataset_reduzido[j])) / nrow(dataset_reduzido))*100,3)
          
        } else {
          
          dados_missing_prop_ano[,i][which(dados_missing_prop_ano$variavel == k, arr.ind = TRUE)] <- 
            round((sum(is.na(dataset_reduzido[,j][which(dataset_reduzido["ano_obito"] == i)])) / 
                     nrow(dataset_reduzido[dataset_reduzido$ano_obito == i,]))*100,3)
          
        }
      }
    }
  }
}

View(dados_missing_prop_ano)

#####################################################################################################

# Dados Rebeca

dados_rebeca <- read.csv("dados_ssdf_Rebeca.csv")
dados_rebeca$X <- NULL

dim(dados_rebeca)
View(dados_rebeca)

table(dados_rebeca$ano)

dados_missing_rebeca<- data.frame("variavel" = names(dados_rebeca),
                                     "Geral"=NA, "2000" = NA, "2001" = NA, "2002" = NA,
                                     "2003" = NA, "2004" = NA, "2005" = NA, "2006" = NA,
                                     "2007" = NA, "2008" = NA, "2009" = NA, "2010" = NA,
                                     "2011" = NA, "2012" = NA, "2013" = NA, "2014" = NA,
                                     "2015" = NA, "2016" = NA, "2017" = NA)

colnames(dados_missing_rebeca) <- c("variavel","Geral", "2000", "2001", "2002", "2003", "2004",
                                    "2005", "2006", "2007", "2008", "2009", "2010", "2011",
                                    "2012", "2013", "2014", "2015", "2016", "2017")

for(i in colnames(dados_missing_rebeca)[-1]){
  
  for (j in names(dados_rebeca)){
    
    for(k in dados_missing_rebeca$variavel){
      
      if(j == k){
        
        if(i == "Geral"){
          
          dados_missing_rebeca[,i][which(dados_missing_rebeca$variavel == k, arr.ind = TRUE)] <- 
            round((sum(is.na(dados_rebeca[j])) / nrow(dados_rebeca))*100,3)
          
        } else {
          
          dados_missing_rebeca[,i][which(dados_missing_rebeca$variavel == k, arr.ind = TRUE)] <- 
            round((sum(is.na(dados_rebeca[,j][which(dados_rebeca["ano"] == i)])) / 
                     nrow(dados_rebeca[dados_rebeca$ano == i,]))*100,3)
          
        }
      }
    }
  }
}

View(dados_missing_rebeca)

######################################################################################

setwd("C:/Users/eddus/Documents/UnB/Laboratorio1-Shiny/dados_SIM_SESDF/SES_DODF")

library(data.table)
library(dplyr)


dataset_reduzido <- fread("dataset_consolidado_reduzido.csv")

dataset_reduzido <- data.frame(dataset_reduzido)

dataset_reduzido$sexo <- as.factor(dataset_reduzido$sexo)
dataset_reduzido$esc <- as.factor(dataset_reduzido$esc)
dataset_reduzido$estciv <- as.factor(dataset_reduzido$estciv)
dataset_reduzido$racacor <- as.factor(dataset_reduzido$racacor)
dataset_reduzido$grupo <- as.factor(dataset_reduzido$grupo)
dataset_reduzido$RA <- as.factor(dataset_reduzido$RA)
dataset_reduzido$ano_obito <- as.factor(dataset_reduzido$ano_obito)
dataset_reduzido$capcid10 <- as.factor(dataset_reduzido$capcid10)
dataset_reduzido$capcid10nomes <- as.factor(dataset_reduzido$capcid10nomes)
dataset_reduzido$idade_cat <- as.factor(dataset_reduzido$idade_cat)

str(dataset_reduzido)
dim(dataset_reduzido)
View(dataset_reduzido)


# Variaveis categoricas

colnames(dataset_reduzido)

table(dataset_reduzido$sexo)

table(dataset_reduzido$esc)

table(dataset_reduzido$estciv)

table(dataset_reduzido$racacor)

table(dataset_reduzido$grupo)

table(dataset_reduzido$RA)

table(dataset_reduzido$ano_obito)

table(dataset_reduzido$capcid10nomes)

table(dataset_reduzido$capcid10)

table(dataset_reduzido$idade_cat)

#########################################################################


library(shiny)

shinyApp(
  ui = fluidPage(
    tabsetPanel(
      tabPanel("Map", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
                 mainPanel(
                   htmlOutput("Attacks")
                 )
               )
      ),
      tabPanel("plot", fluid = TRUE,
               sidebarLayout(
                 sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
                 mainPanel(fluidRow(
                   column(7,  plotlyOutput("")),
                   column(5, plotlyOutput(""))   
                 )
                 )
               )
      )
    )
  ), 
  server = function(input, output) {
    
  }
)

