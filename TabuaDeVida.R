if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(tidyverse, reshape2)

#### TAXA DE MORTALIDADE ####

# Importante verificar se as PROJEÇOES são centradas no meio do ano.

# primeira parte vou ler as projeçoes que fiz download no site do ibge.
Proj_M <-
  readxl::read_xls(
    "projecoes_2018_populacao_idade_simples_2010_2060_20201209.xls",
    range = c("A5:J97"),
    sheet = "DF"
  )
Proj_M$SEXO <- "M" #criei uma coluna SEXO

Proj_F <-
  readxl::read_xls(
    "projecoes_2018_populacao_idade_simples_2010_2060_20201209.xls",
    range = c("A101:J193"),
    sheet = "DF"
  )
Proj_F$SEXO <- "F" #criei uma coluna SEXO

Projecoes <- rbind(Proj_F,Proj_M) #Juntando os arquivos 
Projecoes$UF <- "DF" # Criei uma coluna de UF
Projecoes <- Projecoes %>% filter(IDADE!='TOTAL') # Nos dados veio uma linha que nao queria entao exclui ela
Projecoes <- melt(Projecoes,id.vars = c('IDADE','UF','SEXO')) # "derreti" meus dados para deixar em melhor formato

######## ######## ########### ########

# Esta tabela que estou carregando é fruto das outras aulas, ou seja, a QTD de obito.
## data.frame com colunas "UF"   "SEXO"     "IDADE"  "NUMERADOR".

load("Tabela_Numerador.rdata")

Projecoes <- Projecoes %>% filter(variable==2015) # estou escolhendo o ano do meu denominador.
names(Projecoes) <- c('IDADE','UF','SEXO','ANO','DENOMINADOR') # renomeando variaveis
Projecoes$IDADE <- as.numeric(Projecoes$IDADE) #transformando a idade em formato numerico

# Como os 90 anos no meu banco estava '90+' estes resultados ficaram como NA
# após a tranformção acima entao mandei receber 90.
Projecoes$IDADE[is.na(Projecoes$IDADE)]<-90  

#estou tranformando todas as idade maiores que 90 para 90, pois assim determino meu limite superior.
Tabela_Numerador$IDADE[Tabela_Numerador$IDADE>=90] <-90 

# Agora irei agregar estas idades e ordenar ( selecionar apenas 3 anos para uma media movel)
# esta parte é opcional dependendo do estudo em questão entao talvez tenha que fazer alterações.

Tabela_Numerador <- Tabela_Numerador %>% filter(ANO %in% c(2014:2016), UF=="DF") %>%
  group_by(UF,SEXO,ANO,IDADE) %>% summarise(QTD=sum(QTD)) %>% arrange(IDADE)

# efetuar uma média movel para suavizar o numerador
Tabela_Numerador <- Tabela_Numerador %>% group_by(UF,SEXO,IDADE) %>%
  summarise(NUMERADOR=round(sum(QTD/3),0))

# criar um arquivo unindo numerador denomidor
Taxa_mortalidade<-left_join(Projecoes,Tabela_Numerador)

# vou criar um vector para criar as classe da idade
# o plano agora vai ser efetuar transformaçoes para unir os dados desagregados, vou usar o "cut".

GG <- seq(0,95,5)
GG[21] <- 1
GG <- sort(GG)

Taxa_mortalidade$CLASSE <- cut(Taxa_mortalidade$IDADE, GG, right = F) # right= F é necessario para [ )
Taxa_mortalidade$CLASSE <- as.character(Taxa_mortalidade$CLASSE)
Taxa_mortalidade$CLASSE[Taxa_mortalidade$IDADE>=90] <- '90+'
Taxa_mortalidade$CLASSE[Taxa_mortalidade$CLASSE=='[1,5)'] <- '[01,5)' # coloquei o zero senao depois ele tira da ordem 
Taxa_mortalidade$CLASSE[Taxa_mortalidade$CLASSE=='[5,10)'] <- '[05,10)'

# agregando os dados por classe
Taxa_mortalidade <- Taxa_mortalidade %>% group_by(CLASSE,UF,SEXO,ANO) %>%
  summarise(DENOMINADOR=sum(DENOMINADOR),NUMERADOR=sum(NUMERADOR))

# criando o NMX ou seja a taxa.
Taxa_mortalidade <- Taxa_mortalidade %>% mutate(NMX=round(NUMERADOR/DENOMINADOR,7))

######## ######## ########### ########
###### CRIAR O FATOR DE SEPARAÇÃO NKX ######
### Média dos tempos de vida no intervalo
# meu arquivo load tem estas variaveis:
# "CLASSE" "SEXO"   "UF"   "nkx"
# calculei a parte e é simples, olhar slide no aprender...

load("MediaTempoMedio.rdata")


########################## 
TABUA_DE_VIDA <- left_join(Taxa_mortalidade, MediaTempoMedio) %>% arrange(SEXO, CLASSE)
TABUA_DE_VIDA$Tamanho <- rep(c(1,4,rep(5,18)),2)

names(TABUA_DE_VIDA)[7:8] <- c('M(X)','NKX')

TABUA_DE_VIDA$NKX <- round(as.numeric(TABUA_DE_VIDA$NKX),3)

TABUA_DE_VIDA$"Q(X)"<-round((TABUA_DE_VIDA$Tamanho*TABUA_DE_VIDA$`M(X)`) /(1+((TABUA_DE_VIDA$Tamanho- TABUA_DE_VIDA$NKX )*TABUA_DE_VIDA$`M(X)`)),7)

TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=='90+',"Q(X)"] <- 1
TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="[0,1)","l(X)"]<-100000


TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="[0,1)","Q(X)"]<- TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="[0,1)","M(X)"]
TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="[0,1)","d(X)"]<-10^5*TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="[0,1)","Q(X)"]


h=0
for(j in 1:540){
  for (i in (2+h):(20*j)) {
    TABUA_DE_VIDA[i,"l(X)"]<-TABUA_DE_VIDA[i-1,"l(X)"]-TABUA_DE_VIDA[i-1,"d(X)"]
    TABUA_DE_VIDA[i,"d(X)"]<-TABUA_DE_VIDA[i,"l(X)"]*TABUA_DE_VIDA[i,"Q(X)"]
  }
  
  for (i in (h+1):(20*j-1)) {
    TABUA_DE_VIDA[i,"L(X)"]<-  (TABUA_DE_VIDA[i, "Tamanho"] * TABUA_DE_VIDA[i+1,"l(X)"])  +
      (TABUA_DE_VIDA[i,"d(X)"]*TABUA_DE_VIDA[i,"NKX"])
  }
  
  
  h <- h+20
}

###############  fazer isso apenas se o seu LX for desconhecido     ###############
### Outra forma é ultilizar os valores das nações unidas 
y <- TABUA_DE_VIDA$`L(X)`[TABUA_DE_VIDA$CLASSE%in%c("[20,25)","[25,30)","[30,35)","[35,40)","[40,45)",
                                                    "[45,50)","[5,10)", "[50,55)","[55,60)","[60,65)",
                                                    "[65,70)" ,"[70,75)", "[75,80)", "[80,85)", "[85,90)")]
x <- TABUA_DE_VIDA$`l(X)`[TABUA_DE_VIDA$CLASSE%in%c(c("[20,25)","[25,30)","[30,35)","[35,40)","[40,45)",
                                                      "[45,50)","[5,10)", "[50,55)","[55,60)","[60,65)",
                                                      "[65,70)" ,"[70,75)", "[75,80)", "[80,85)", "[85,90)"))]
modelo <- lm(y/x~x)
AB <- modelo$coefficients
###############           ###################

TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="90+","L(X)"]<- (AB[1]+(AB[2]*TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="90+","l(X)"]))*
  TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="90+","l(X)"]


TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="90+","T(X)"]<-TABUA_DE_VIDA[TABUA_DE_VIDA$CLASSE=="90+","L(X)"]
h=0
for(j in 1:540){
  for (i in (j*20-1):(1+h)) {
    TABUA_DE_VIDA[i,"T(X)"]<-TABUA_DE_VIDA[i+1,"T(X)"]+TABUA_DE_VIDA[i,"L(X)"] }
  
  h <- h+20
}

TABUA_DE_VIDA <- TABUA_DE_VIDA %>% mutate(`e(x)`=round(`T(X)`/`l(X)`,2))

########################### exportando ############

library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, "FEMININO");addWorksheet(wb, "MASCULINO")
names(TABUA_DE_VIDA)[11]<-"lx"
writeDataTable(wb,"FEMININO", TABUA_DE_VIDA[TABUA_DE_VIDA$SEXO== "F",])
writeDataTable(wb,"MASCULINO", TABUA_DE_VIDA[TABUA_DE_VIDA$SEXO== "M",])

saveWorkbook(wb, file="TABUA_DE_VIDA.xlsx", overwrite = T)

