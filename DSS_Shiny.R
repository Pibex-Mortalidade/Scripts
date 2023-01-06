library(data.table)
library(plotly)
library(highcharter)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(wordcloud)
library(shinythemes)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinybusy)
library(shinycssloaders)
library(shinyWidgets)
library(rintrojs)
library(ECharts2Shiny)
library(geobr)
library(sf)
library(leaflet)
library(tidyverse)
library(readxl)
library(wesanderson)
library(here)

setwd("C:/Users/Cesar Augusto Galvao/Documents/UnB/EXT - Mortalidade/Dashboard-Final")

getwd()

#dataset_reduzido <-read_csv("dataset_consolidado_reduzido.csv", locale(encoding = "WINDOWS-1252"),
#                            col_names=T, col_types = cols(.default = "c"))

dataset_reduzido <- fread("dataset_consolidado_reduzido.csv")


dataset_reduzido <- data.frame(dataset_reduzido)

dataset_reduzido$sexo <- factor(dataset_reduzido$sexo, levels = c("Feminino",
                                                                  "Masculino",
                                                                  "Ignorado"),
                                ordered = TRUE)

dataset_reduzido$esc <- factor(dataset_reduzido$esc, levels = c("Nenhuma",
                                                                "de 1 a 3 anos",
                                                                "de 4 a 7 anos",
                                                                "de 8 a 11 anos",
                                                                "12 anos e mais",
                                                                "ignorado"),
                               labels = c("Nenhuma",
                                          "1-3 anos",
                                          "4-7 anos",
                                          "8-11 anos",
                                          "12+ anos",
                                          "ignorado"),
                               ordered = TRUE)

dataset_reduzido$estciv <- factor(dataset_reduzido$estciv, levels = c("Solteiro",
                                                                      "Uniao estavel",
                                                                      "Casado",
                                                                      "Separado judicialmente/divorciado",
                                                                      "Viuvo",
                                                                      "ignorado"),
                                  labels = c("Solteiro",
                                             "Uniao est.",
                                             "Casado",
                                             "Sep/div",
                                             "Viuvo",
                                             "ignorado"))

dataset_reduzido$racacor <- factor(dataset_reduzido$racacor, levels = c("Parda",
                                                                        "Branca",
                                                                        "Preta",
                                                                        "Amarela",
                                                                        "Indigena"))

dataset_reduzido$grupo <- as.factor(dataset_reduzido$grupo)

dataset_reduzido$RA <- as.factor(dataset_reduzido$RA)

dataset_reduzido$capcid10nomes <- as.factor(dataset_reduzido$capcid10nomes)

dataset_reduzido$idade_cat <- factor(dataset_reduzido$idade_cat, ordered = TRUE,
                                     levels = c("<1","1-4","5-9","10-14","15-19",
                                                "20-24","25-29","30-34","35-39",
                                                "40-44","45-49","50-54","55-59",
                                                "60-64","65-69","70-74","75-79",
                                                "80-84","85+"))

dataset_reduzido$palavras <- factor(dataset_reduzido$capcid10nomes,
                                    levels = c("Algumas afecções originadas no período perinatal",
                                               "Algumas doenças infecciosas e parasitárias",
                                               "Causas externas de morbidade e de mortalidade",
                                               "CID 10: Revisão não disponível ou não preenchido ou inválido",
                                               "Doenças da pele e do tecido subcutâneo",
                                               "doenças do aparelho circulatório",
                                               "doenças do aparelho digestivo",
                                               "Doenças do aparelho geniturinário",
                                               "doenças do aparelho respiratório",
                                               "doenças do olho e anexos",
                                               "doenças do ouvido e da apófise mastóide",
                                               "Doenças do sangue e dos árgãos hematopoéticos e alguns transtornos imunitários",
                                               "doenças do sistema nervoso",
                                               "Doenças do sistema osteomuscular e do tecido conjuntivo",
                                               "Doenças endócrinas, nutricionais e metabólicas",
                                               "Gravidez, parto e puerpério",
                                               "Lesões, envenenamentos e algumas outras consequências de causas externas",
                                               "Malformações congênitas, deformidades e anomalias cromossômicas",
                                               "Neoplasmas (tumores)",
                                               "Sintomas, sinais e achados anormais de exames clínicos e de laboratório, não classificados em outra parte",
                                               "Transtornos mentais e comportamentais"),
                                    labels = c("Perinatal",
                                               "Infecciosas",
                                               "Externas",
                                               "Indisponivel",
                                               "Cutaneos",
                                               "Circulatorio",
                                               "Digestivo",
                                               "Geniturinario",
                                               "Respiratorio",
                                               "Olhos",
                                               "Ouvidos",
                                               "Sangue",
                                               "Nervoso",
                                               "Osteomuscular",
                                               "Endocrinas",
                                               "Gravidez",
                                               "Lesoes",
                                               "Malformacoes",
                                               "Neoplasmas",
                                               "Outros",
                                               "Mentais"))

dataset_reduzido$causabasnomes <- gsub("não especificado", replacement = "", dataset_reduzido$causabasnomes)

dataset_reduzido$causabasnomes <- gsub("não especificada", replacement = "", dataset_reduzido$causabasnomes)


##################################################################################################

decada<-c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
          "2020")



pal <- colorFactor(
  palette =c("#FF3333","#6699FF","green"),
    levels  =c("Feminino", "Masculino", "Ignorado"))

dados_resumido<-dataset_reduzido%>%
  mutate(RA=as.character(RA))%>%
  mutate(RA=ifelse(grepl("^Asa", RA), "Plano Piloto", RA))%>%
  filter(!is.na(idade_cat)&!is.na(RA)&sexo!="Ignorado"&ano_obito %in% decada)%>%
  #mutate(idade_cat=as.character(idade_cat))%>%
  #mutate(idade_cat=ifelse(idade_cat=="85-89"|idade_cat=="90+", "85+", idade_cat))%>%
  group_by(ano_obito,sexo, RA,idade_cat)%>%
  dplyr::summarize(N=n())%>%
  rename(Ano=ano_obito,
         Sexo=sexo,
         Idade=idade_cat)%>%
  mutate(Ano=as.character(Ano),
         Sexo=as.character(Sexo),
         Idade=as.character(Idade))

########################################################################################
#Preenchendo com 0 onde nao houve mortalidade

#anos_vec <- dados_resumido$Ano %>% unique()

#sexo_vec <- dados_resumido$Sexo %>% unique()

#RA_vec <- dados_resumido$RA %>% unique()

#Idade_vec <- dados_resumido$Idade %>% unique()

#xyz <- 0

#for (i in anos_vec) {
#  for (j in sexo_vec) {
#    for (k in RA_vec) {
#      for (l in Idade_vec) {
#        if(is.na(as.numeric(dados_resumido[dados_resumido$Ano == i &
#                                           dados_resumido$Sexo == j &
#                                           dados_resumido$RA == k &
#                                           dados_resumido$Idade == l, "N"]))){
#          
#          dados_resumido <- rbind(dados_resumido, 
#                                  data.frame("Ano" = i, "Sexo" = j, "RA" = k, "Idade" = l, "N" = 0))
#                             
#        }
#        xyz <- xyz + 1
#        print(xyz)
#      }
#    }
#  }
#}

##########################################################################################

sheets<-excel_sheets("Estruturas-Etarias-por-RA-2010-2020.xlsx")
tot<-c("Total...2", "Total...5", "Total...8", "Total...11", "Total...14",
       "Total...17", "Total...20", "Total...23", "Total...26", "Total...29",
       "Total...32")
tabelao<-data.frame(matrix(NA, 1,5))
names(tabelao)<-c("Idade", "Sexo", "Ano", "População", "RA")
lista<-list()

for(sheet in sheets){
  pop_ra<-read_excel("Estruturas-Etarias-por-RA-2010-2020.xlsx", sheet=sheet, skip=4)%>%
    slice(1:34)%>%
    select(!(tot))
  names(pop_ra)<-c("Idade", "Masculino 2010","Feminino 2010",
                   "Masculino 2011", "Feminino 2011",
                   "Masculino 2012", "Feminino 2012",
                   "Masculino 2013", "Feminino 2013",
                   "Masculino 2014","Feminino 2014",
                   "Masculino 2015","Feminino 2015",
                   "Masculino 2016","Feminino 2016",
                   "Masculino 2017", "Feminino 2017",
                   "Masculino 2018", "Feminino 2018",
                   "Masculino 2019", "Feminino 2019",
                   "Masculino 2020","Feminino 2020")
  
  pop_ra_longo<-pop_ra%>%
    pivot_longer(!Idade, names_to = "Ano", values_to = "População")%>%
    separate(Ano, c("Sexo", "Ano"),sep=" " )%>%
    mutate(RA=sheet)
  
  tabelao<-full_join(tabelao,pop_ra_longo)
}

UQ<-c("1","2", "3", "4")
CN<-c("5","6", "7", "8", "9")
DQ<-c("10", "11", "12", "13", "14")
QD<-c("15", "16", "17", "18", "19")
# fazendo as categorias das projecoes ficarem iguais aas dos dados
tab_pop<-tabelao%>%
  mutate(Idade=gsub(" a ", "-", Idade))%>%
  filter(!is.na(Idade)&RA!="DF")%>%
  mutate(Idade=ifelse(Idade %in% UQ, "1-4", 
                      ifelse(Idade %in% CN, "5-9",
                             ifelse(Idade %in% DQ,"10-14",
                                    ifelse(Idade %in% QD, "15-19",Idade)))),
         `População`=as.double(`População`),
         RA=ifelse(RA=="Sudoeste_Octogonal", "Sudoeste/Octogonal", RA))%>%
  relocate(Ano, Sexo, RA, Idade, `População`)%>%
  mutate(Idade=as.character(Idade))

#Correcao no label da idade 0
tab_pop <- tab_pop %>%
  mutate(Idade = ifelse(Idade == "0","<1", Idade))


#juntando proje??o populacional com obitos para o calculo dos indicadores
uniao<-left_join(dados_resumido,tab_pop)



converte_unidade<-read_municipality() 

#lendo os shapefiles
my_spdf <- read_sf( dsn = ".",
                    layer="Regioes_Administrativas")%>%
  rename(RA=ra)%>%
  dplyr::mutate(RA=gsub("Í","I", RA),
                RA=gsub("Ú","U", RA),
                RA=gsub("Á","A", RA),
                RA=gsub("Ã","A", RA),
                RA=gsub("Â","A", RA),
                RA=gsub("Õ","O", RA))

#calculando a mortalidade
data_mort<-uniao%>%
  mutate(RA=toupper(RA))%>%
  filter(!is.na(RA))%>%
  mutate(RA=gsub("Í","I", RA),
         RA=gsub("Ú","U", RA),
         RA=gsub("Á","A", RA),
         RA=gsub("Ã","A", RA),
         RA=gsub("Â","A", RA),
         RA=gsub("Õ","O", RA),
         RA=gsub("ASA SUL","PLANO PILOTO", RA),
         RA=gsub("ASA NORTE","PLANO PILOTO", RA))%>%
  mutate(mort=round(N/`População`*1000, 1))


dado_com_poligono_aux<-dplyr::left_join(my_spdf, data_mort, by ="RA")

dado_com_poligono_conv<-st_transform(dado_com_poligono_aux,
                                crs = st_crs(converte_unidade))

dado_com_poligono<-dado_com_poligono_conv%>%
  rename(idade_cat=Idade,
         sexo=Sexo,
         ano_obito=Ano)%>%
  mutate(idade_cat=factor(idade_cat),
         sexo=factor(sexo))

mapa_RA32_33 <- dado_com_poligono[dado_com_poligono$RA %in% c("PÔR DO SOL", "ARNIQUEIRA"),] %>%
  select(RA, geometry)

########################################################################################################
# Dashboard ############################################################################################
########################################################################################################

################################################################################
#Header



header <- dashboardHeader(
  title = "Mortalidade no Distrito Federal",
  titleWidth = 450
)

################################################################################
#Sidebar

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    menuItem(
      "Ano do Óbito",
      icon = icon("calendar"),
      noUiSliderInput(
        inputId = "ano_obito",
        label = "",
        value = c(min(dataset_reduzido$ano_obito, na.rm = TRUE), max(dataset_reduzido$ano_obito, na.rm = TRUE)),
        min = min(dataset_reduzido$ano_obito, na.rm = TRUE),
        max = max(dataset_reduzido$ano_obito, na.rm = TRUE),
        step = 1L,
        orientation = "vertical",
        width = "500px",
        height = "400px",
        margin = NULL,
        format = wNumbFormat(decimals = 0),
        color = "lightblue"
        #sep = "",
        #ticks = FALSE
      )
    ),
    br(),
#    menuItem(
#      "Sexo",
#      tabName = "sexo",
#      icon = icon("venus-mars"),
#      checkboxGroupInput(
#        inputId = "sexo",
#        label = "",
#        choices = c("Masculino","Feminino"),
#        selected = c("Masculino","Feminino"),
#        inline = TRUE
#      )
#    ),
#    br(),
#   menuItem(
#      "Faixa Etária",
#      tabName = "idade_cat",
#      icon = icon("address-card"),
#      checkboxGroupButtons(
#        inputId = "all_idade_cat",
#        label = "",
#        choices = "Todos/Nenhum",
#        size = "sm",
#        selected = "Todos/Nenhum"
#      ),
#      checkboxGroupInput(
#        inputId = "idade_cat",
#        label = "",
#        choices = levels(dataset_reduzido$idade_cat),
#        selected = levels(dataset_reduzido$idade_cat),
#        inline = FALSE
#      )
#    ),
#    br(),
#   menuItem(
#      "Escolaridade",
#      tabName = "esc",
#      icon = icon("graduation-cap"),
#      checkboxGroupButtons(
#        inputId = "all_esc",
#        label = "",
#        choices = "Todos/Nenhum",
#        size = "sm",
#        selected = "Todos/Nenhum"
#      ),
#      checkboxGroupInput(
#        inputId = "esc",
#        label = "",
#        choices = levels(dataset_reduzido$esc),
#        selected = levels(dataset_reduzido$esc),
#        inline = FALSE
#      )
#    ),
#    br(),
#    menuItem(
#      "Estado Civil",
#      tabName = "estciv",
#      icon = icon("scroll"),
#      checkboxGroupButtons(
#        inputId = "all_estciv",
#        label = "",
#        choices = "Todos/Nenhum",
#        size = "sm",
#        selected = "Todos/Nenhum"
#      ),
#      checkboxGroupInput(
#        inputId = "estciv",
#        label = "",
#        choices = levels(dataset_reduzido$estciv),
#        selected = levels(dataset_reduzido$estciv),
#        inline = FALSE
#      )
#    ),
#    br(),
#    menuItem(
#      "Raça/cor",
#      tabName = "racacor",
#      icon = icon("user"),
#      checkboxGroupButtons(
#        inputId = "all_racacor",
#        label = "",
#        choices = "Todos/Nenhum",
#        size = "sm",
#        selected = "Todos/Nenhum"
#      ),
#      checkboxGroupInput(
#        inputId = "racacor",
#        label = "",
#        choices = levels(dataset_reduzido$racacor),
#        selected = levels(dataset_reduzido$racacor),
#        inline = FALSE
#      )
#    ),
#    br(),
#    menuItem(
#      "Grupo PED",
#      tabName = "grupo",
#      icon = icon("hand-holding-usd"),
#      checkboxGroupButtons(
#        inputId = "all_grupo",
#        label = "",
#        choices = "Todos/Nenhum",
#        size = "sm",
#        selected = "Todos/Nenhum"
#      ),
#      checkboxGroupInput(
#        inputId = "grupo",
#        label = "",
#        choices = levels(dataset_reduzido$grupo),
#        selected = levels(dataset_reduzido$grupo),
#        inline = FALSE
#      )
#    ),
#    br(),
    menuItem(
      "Região Administrativa",
      tabName = "RA",
      inline = TRUE,
      icon = icon("map-marked-alt"),
      checkboxGroupButtons(
        inputId = "all_RA",
        label = "",
        choices = "Todos/Nenhum",
        size = "sm",
        selected = "Todos/Nenhum"
      ),
      fluidRow(
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "grupo1_RA",
                 label = "",
                 choices = "Grupo 1",
                 size = "sm",
                 width = '60%',
                 selected = "Grupo 1"
               )),
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "grupo2_RA",
                 label = "",
                 choices = "Grupo 2",
                 size = "sm",
                 width = '60%',
                 selected = "Grupo 2"
               )),
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "grupo3_RA",
                 label = "",
                 choices = "Grupo 3",
                 size = "sm",
                 width = '60%',
                 selected = "Grupo 3"
               )),
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "grupo4_RA",
                 label = "",
                 choices = "Grupo 4",
                 size = "sm",
                 width = '400px',
                 selected = "Grupo 4"
               ))
      ),
      checkboxGroupInput(
        inputId = "RA",
        label = "",
        choices = levels(dataset_reduzido$RA),
        selected = levels(dataset_reduzido$RA),
        inline = FALSE
      )
  ),
    br(),
    menuItem(
      "Filtros para o mapa",
      tabName = "mapa",
      icon = icon("globe-americas"),
      sliderInput(inputId = "ano_mapa",
                  label = "Ano do Obito",
                  min = 2010,
                  max = 2020,
                  value = 2020,
                  sep = "",
                  pre = "",
                  post = "",
                  ticks = FALSE),
      checkboxGroupInput(
        inputId = "sexo",
        label = "Sexo",
        choices = c("Masculino","Feminino"),
        selected = c("Masculino","Feminino"),
        inline = TRUE
      ),
      checkboxGroupButtons(
        inputId = "all_idade_cat",
        label = "Faixa Etaria",
        choices = "Todos/Nenhum",
        size = "sm",
        selected = "Todos/Nenhum"
      ),
      fluidRow(
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "idade_cat1",
                 label = "",
                 choices = "Infancia",
                 size = "sm",
                 width = '60%',
                 selected = "Infancia"
               )),
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "idade_cat2",
                 label = "",
                 choices = "Jovens",
                 size = "sm",
                 width = '60%',
                 selected = "Jovens"
               )),
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "idade_cat3",
                 label = "",
                 choices = "Adultos",
                 size = "sm",
                 width = '60%',
                 selected = "Adultos"
               )),
        column(2, offset = 0, 
               checkboxGroupButtons(
                 inputId = "idade_cat4",
                 label = "",
                 choices = "Idosos",
                 size = "sm",
                 width = '60%',
                 selected = "Idosos"
               ))
      ),
      checkboxGroupInput(
        inputId = "idade_cat",
        label = "",
        choices = levels(dataset_reduzido$idade_cat),
        selected = levels(dataset_reduzido$idade_cat),
        inline = FALSE
      ),
      br()
    ),
#    menuItem(
#      "Causa da Morte (CID 10)",
#      tabName = "capcid10nomes",
#      icon = icon("skull-crossbones"),
#      checkboxGroupButtons(
#        inputId = "all_capcid10nomes",
#        label = "",
#        choices = "Todos/Nenhum",
#        size = "sm",
#        selected = "Todos/Nenhum"
#      ),
#      checkboxGroupInput(
#        inputId = "capcid10nomes",
#        label = "",
#        choices = levels(dataset_reduzido$capcid10nomes),
#        selected = levels(dataset_reduzido$capcid10nomes),
#        inline = FALSE
#     )
#    ),
    div(
      id = "botao_atualizar",
      bsButton(inputId = "botao_atualizar",
               label = "ATUALIZAR", 
               icon = icon("redo"), 
               style = "primary",
               width = 270,
               size = "larger")
    )
  )
)
################################################################################
# Body

body <- dashboardBody(
  tabBox(
    width = 1000,
    height = 900,
    title = "Mortalidade no Distrito Federal",
    
    tabPanel(title = "Perfil da Mortalidade",
             fluidRow(
               column(
                 width = 12,
                 valueBoxOutput("mortes_total",
                                width = 4)%>% withSpinner(type=6),
                 valueBoxOutput("mortes_total2",
                                width = 4)%>% withSpinner(type=6),
                 valueBoxOutput("mortes_total3",
                                width = 4)%>% withSpinner(type=6)),
               column(
                 width = 8,
                 box(#title = "Por Sexo",
                     solidHeader = TRUE,
                    width = 6,
                    collapsible = TRUE,
                    plotlyOutput(outputId = "sexo_output")),
                 box(#title = "Por Raça/cor",
                     solidHeader = TRUE,
                     width = 6,
                     collapsible = TRUE,
                     plotlyOutput(outputId = "racacor_output")),
                 box(#title = "Por Estado Civil",
                     solidHeader = TRUE,
                     width = 6,
                     collapsible = TRUE,
                     plotlyOutput(outputId = "estciv_output")),
                 box(#title = "Por Escolaridade",
                     solidHeader = TRUE,
                     width = 6,
                     collapsible = TRUE,
                     plotlyOutput(outputId = "esc_output"))
               ),
               column(
                 width = 4,
                 box(#title = "Por Faixa Etaria",
                     solidHeader = TRUE,
                     width = 12,
                     height = 900,
                     collapsible = TRUE,
                     plotlyOutput(outputId = "idade_cat_output"))
                 
                 )
             )
             ),
    tabPanel(title = "Taxas",
             #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
             column(width = 12,
                    box(
                      width = NULL,
                      height = 800,
                      title = "",
                      leafletOutput(outputId = "mapa",
                                    height = "780px")
                    )
             )
    ),
    tabPanel(title = "Principais Causas",
             fluidRow(
               column(
                 width = 4,
                 box(#title = "Causas (Capitulo) - Mulheres",
                   solidHeader = TRUE,
                   width = 12,
                   #height = 800,
                   collapsible = TRUE,
                   plotOutput(outputId = "wordcloud_fem_output",
                              width = "435px", height="435px")),
                 box(#title = "Causas (Capitulo) - Homens",
                     solidHeader = TRUE,
                     width = 12,
                     #height = 800,
                     collapsible = TRUE,
                     plotOutput(outputId = "wordcloud_masc_output",
                                width = "435px", height="435px"))
               ),
               column(
                 width = 4,
                 box(#title = "Causas Especificas - Mulheres",
                     solidHeader = TRUE,
                     width = 12,
                     collapsible = TRUE,
                     plotlyOutput(outputId = "causabas_fem_output",
                                  width = "435px", height="435px"),
                 ),
                 box(#title = "Causas Especificas - Homens",
                     solidHeader = TRUE,
                     width = 12,
                     collapsible = TRUE,
                     plotlyOutput(outputId = "causabas_masc_output",
                                  width = "435px", height="435px")
                     )
                ),
               column(
                 width = 4,
                 box(#title = "Principais Causas (Capitulo)",
                   solidHeader = TRUE,
                   width = 12,
                   #height = 860,
                   collapsible = TRUE,
                   plotlyOutput(outputId = "capcid10_output",
                                width = "435px", height="950px"))
                 
               )
             )
    )
  )
)


################################################################################
# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header, sidebar, body)

################################################################################
# Server


server <- function(input, output, session) {
  
  valuebox_mortes <- reactive({
    input$botao_atualizar
    isolate({
        valueBox(paste0(nrow(dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                         ano_obito <= input$ano_obito[2],
                                                         #grupo %in% input$grupo,
                                                         RA %in% input$RA))), 
                 subtitle = tags$p("Total de Mortes", style = "font-size: 240%;"),
                 icon = icon("skull-crossbones"), color = "light-blue")
    })
  })
  
  output$mortes_total <- renderValueBox({
    valuebox_mortes()
  })
  
  valuebox_mortes2 <- reactive({
    input$botao_atualizar
    
    isolate({
      
      dataset_reduzido %>% 
        filter(ano_obito >= input$ano_obito[1],
               ano_obito <= input$ano_obito[2],
               #grupo %in% input$grupo,
               RA %in% input$RA) %>% 
        select(capcid10nomes) %>%
        group_by(capcid10nomes) %>%
        summarise("quantidade" = n()) %>%
        filter(quantidade == max(quantidade)) -> causa_mais
      
      valueBox(paste0(causa_mais[1,2], " (", round((causa_mais[1,2]/(nrow(dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                                                                      ano_obito <= input$ano_obito[2],
                                                                                                      #grupo %in% input$grupo,
                                                                                                      RA %in% input$RA))))*100,2),"%)"),
               subtitle = tags$p(paste("Principal Causa (Capítulo):/n ", causa_mais$capcid10nomes), style = "font-size: 180%;"),
               icon = icon("skull"), color = "light-blue")
    })
  })
  
  output$mortes_total2 <- renderValueBox({
    valuebox_mortes2()
  })
  
  valuebox_mortes3 <- reactive({
    input$botao_atualizar
    
    isolate({
      
      dataset_reduzido %>% 
        filter(ano_obito >= input$ano_obito[1],
               ano_obito <= input$ano_obito[2],
               #grupo %in% input$grupo,
               RA %in% input$RA) %>% 
        select(causabasnomes) %>%
        group_by(causabasnomes) %>%
        summarise("quantidade" = n()) %>%
        filter(quantidade == max(quantidade)) -> causa_mais2
      
      valueBox(paste0(causa_mais2[1,2], " (", round((causa_mais2[1,2]/(nrow(dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                                                                       ano_obito <= input$ano_obito[2],
                                                                                                       #grupo %in% input$grupo,
                                                                                                       RA %in% input$RA))))*100,2),"%)"), 
               subtitle = tags$p(paste("Principal Causa especifica: ", causa_mais2$causabasnomes), style = "font-size: 180%;"),
               icon = icon("disease"), color = "light-blue")
    })
  })
  
  output$mortes_total3 <- renderValueBox({
    valuebox_mortes3()
  })
  
    
    

###############################################################################################################
# Graficos
  

  grafico_sexo_output <- reactive({
    input$botao_atualizar
    isolate({
      
      sexo_grafico <- plot_ly(data = dataset_reduzido %>% 
                      filter(ano_obito >= input$ano_obito[1],
                             ano_obito <= input$ano_obito[2],
                             RA %in% input$RA,
                             #grupo %in% input$grupo,
                             sexo != "Ignorado",
                             is.na(sexo) == FALSE) %>%
                      select(sexo),
                      labels = ~factor(sexo),
                      type = "pie",
                      alpha = 0.5,
                      opacity = 0.7,
                      alpha_stroke = 0.5,
                      marker = list(colors= ~pal(factor(sexo)),
                                    line = list(color = '#330000', width = 3),
                                    direction = -1),
                      insidetextfont = list(color = '#FFFFFF'),
                      showlegend = TRUE) %>% layout(title = 'Sexo', font = 2)
            
      
      #sexo_grafico <- ggplot(data = dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
      #                                                          ano_obito <= input$ano_obito[2],
      #                                                          RA %in% input$RA,
      #                                                          grupo %in% input$grupo,
      #                                                          sexo != "Ignorado")) +
      #  geom_bar(aes(x = sexo, fill = sexo, text = paste("Sexo: ", sexo)), show.legend = FALSE,position = "identity", 
      #           color = 'black', na.rm = TRUE, alpha = 0.5, width = 0.5) +
      #  theme_gray() + 
      #  theme(legend.position='none') +
      #  xlab("Sexo") +
      #  ylab("Quantidade") +
      #  theme(plot.title = element_text(hjust = 0.5),
      #        plot.subtitle = element_text(hjust = 0.5),
      #        plot.caption = element_text(hjust = 0),
      #        axis.title.y = element_blank())+
      #  scale_x_discrete(na.translate = FALSE)+
      #  scale_fill_brewer(palette = "Set1")
      
      #ggplotly(sexo_grafico, tooltip = c("count","text"))
    })
  })
  
  output$sexo_output <-renderPlotly({
    grafico_sexo_output()
  })
  
  grafico_racacor_output <- reactive({
    input$botao_atualizar
    isolate({
      racacor_grafico <- ggplot(data = dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                                   ano_obito <= input$ano_obito[2],
                                                                   RA %in% input$RA,
                                                                   #grupo %in% input$grupo,
                                                                   sexo != "Ignorado"), aes(x = reorder(racacor, 
                                                                                                            racacor,
                                                                                                            function(x) -length(x)),
                                                                                                fill = sexo,
                                                                                                text = paste("Sexo: ",sexo, 
                                                                                                             "<br>",
                                                                                                             "Raça/cor :", as.factor(racacor)))) +
        geom_bar(position = "dodge", show.legend = TRUE, color = 'black', 
                 na.rm = TRUE, alpha = 0.5, width = 0.8) +
        theme_gray() + 
        theme(legend.position='none') +
        ggtitle("Raça/cor")+
        #xlab("Raça/Cor") +
        #ylab("Quantidade") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        scale_x_discrete(na.translate = FALSE)+
        scale_fill_brewer( palette = "Set1")
      
      ggplotly(racacor_grafico, tooltip = c("count","text"))
    })
  })
  
  output$racacor_output <-renderPlotly({
    grafico_racacor_output()
  })
  
  grafico_estciv_output <- reactive({
    input$botao_atualizar
    isolate({
      estciv_grafico <- ggplot(data = dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                                  ano_obito <= input$ano_obito[2],
                                                                  RA %in% input$RA,
                                                                  #grupo %in% input$grupo,
                                                                  estciv != "ignorado",
                                                                  sexo != "Ignorado"), aes(x = reorder(estciv, estciv,
                                                                                                         function(x) -length(x)),
                                                                                             fill = sexo,
                                                                                             text = paste("Sexo: ",sexo, 
                                                                                                          "<br>",
                                                                                                          "Estado Civil :", as.factor(estciv)))) +
        geom_bar(position = "dodge", show.legend = TRUE, color = 'black', 
                 na.rm = TRUE, alpha = 0.5, width = 0.8) +
        theme_gray() + 
        ggtitle("Estado Civil")+
        #xlab("Estado Civil") +
        #ylab("Quantidade") +
        theme(legend.position='none') +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        scale_x_discrete(na.translate = FALSE)+
        scale_fill_brewer(palette = "Set1")
      
      ggplotly(estciv_grafico, tooltip = c("count","text"))
    })
  })
  
  output$estciv_output <-renderPlotly({
    grafico_estciv_output()
  })
  
  grafico_esc_output <- reactive({
    input$botao_atualizar
    isolate({
      esc_grafico <- ggplot(data = dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                               ano_obito <= input$ano_obito[2],
                                                               RA %in% input$RA,
                                                               #grupo %in% input$grupo,
                                                               esc != "ignorado",
                                                               sexo != "Ignorado"), aes(x = esc, fill = sexo,
                                                                                       text = paste("Sexo: ",sexo, 
                                                                                                    "<br>",
                                                                                                    "Escolaridade :", as.factor(esc)))) +
        geom_bar(position = "dodge", show.legend = TRUE, color = 'black', 
                 na.rm = TRUE, alpha = 0.5, width = 0.8) +
        theme_gray() + 
        theme(legend.position='none') +
        ggtitle("Escolaridade") +
        #xlab("Escolaridade") +
        #ylab("Quantidade") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        scale_x_discrete(na.translate = FALSE) +
        scale_fill_brewer(palette = "Set1")
      
      ggplotly(esc_grafico, tooltip = c("count","text"))
    })
  })
  
  output$esc_output <-renderPlotly({
    grafico_esc_output()
  })

  grafico_idade_cat_output <- reactive({
    input$botao_atualizar
    isolate({
      idade_cat_grafico <- ggplot(data = dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                                     ano_obito <= input$ano_obito[2],
                                                                     RA %in% input$RA,
                                                                     #grupo %in% input$grupo,
                                                                     is.na(idade_cat) == FALSE,
                                                                     sexo != "Ignorado")) +
        geom_bar(aes(y = idade_cat, fill = sexo,
                     text = paste("Sexo: ",sexo, 
                                  "<br>",
                                  "Faixa Etaria :", as.factor(idade_cat))), position = "dodge", show.legend = TRUE, color = 'black', 
                 na.rm = TRUE, alpha = 0.5) +
        theme_gray() + 
        theme(legend.position='none') +
        ggtitle("Faixa Etaria") +
        ylab("") +
        xlab("") +
        scale_fill_brewer(direction=-1,palette = "Set1")
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
      
      ggplotly(idade_cat_grafico,
               height = 860, tooltip = c("count","text"))
    })
  })
  
  output$idade_cat_output <-renderPlotly({
    grafico_idade_cat_output()
  })
  
##########################################################################################################################
  
  output$mapa <- renderLeaflet({
    
    pal <- colorQuantile("OrRd", NULL, n = 4)
    
    dado_mapa<-dado_com_poligono%>%
      filter(ano_obito == input$ano_mapa,
             sexo %in% input$sexo,
             is.na(RA) == FALSE,
             is.na(População) == FALSE,
             is.na(N) == FALSE,
             is.na(idade_cat) == FALSE,
             idade_cat %in% input$idade_cat) %>% 
      select(RA, População, N, geometry) %>%
      group_by(RA) %>%
      summarise(N = sum(N),
                pop = sum(População),
                taxa = (round(sum(N)/sum(População)*1000, 3)))
    
    dado_com_poligono$População
    
    leaflet() %>% 
      addProviderTiles("CartoDB.PositronNoLabels")%>%
      setView(-47.9, -15.8, zoom=10)%>%
      addPolygons(data = dado_mapa,
                  fillColor=~pal(taxa),
                  color="grey",
                  weight =2,
                  opacity = 0.8,
                  fillOpacity = 0.9,
                  label = ~paste0(dado_mapa$RA,
                                  " - Taxa de Mortalidade: ",round(dado_mapa$taxa,1), 
                                  " por mil habitantes"))%>%
      addPolygons(data = mapa_RA32_33,
                  fillColor= "gray",
                  color="grey",
                  weight =2,
                  opacity = 0.8,
                  fillOpacity = 0.9,
                  label = ~paste0(mapa_RA32_33$RA,
                                  " - Dados não disponíveis"))%>%
      addLegend("bottomright", pal = pal, values = round(dado_mapa$taxa,1),
                title = "Taxas de Mortalidade (Quartis)",
                opacity = 1)
  })
  
  
################################################################################################################################
# Graficos Painel 3
  
  grafico_wordcloud_fem_output <- reactive({
    input$botao_atualizar
    isolate({
      
      dataset_reduzido %>%
        select(palavras, ano_obito,RA,sexo) %>%
        filter(ano_obito >= input$ano_obito[1],
               ano_obito <= input$ano_obito[2],
               RA %in% input$RA,
               sexo == "Feminino",
               palavras != "Indisponivel") -> nuvem
      
      word.corpus <- Corpus(VectorSource(nuvem$palavras))  #Corpus
      
      word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
      word.freq <- sort(rowSums(word.counts), decreasing = TRUE)
      head(word.freq) 
      
      layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
      par(mar=rep(0, 4))
      plot.new()
      text(x=0.5, y=0.82, "Causas (Capitulo) - Mulheres", cex = 1.6, font = 1)
      
      wordcloud(words = names(word.freq), 
                freq = word.freq, 
                scale = c(4.5,1),
                max.words = 20, 
                random.order = FALSE, 
                color = wes_palette("Darjeeling1"),
                main = "Causas (Capitulos) - Mulheres")
      
    })
  })
  
  output$wordcloud_fem_output <-renderPlot({
    grafico_wordcloud_fem_output()
  })
  
  
  
  grafico_wordcloud_masc_output <- reactive({
    input$botao_atualizar
    isolate({
      
      dataset_reduzido %>%
        select(palavras, ano_obito,RA,sexo) %>%
        filter(ano_obito >= input$ano_obito[1],
               ano_obito <= input$ano_obito[2],
               RA %in% input$RA,
               sexo == "Masculino",
               palavras != "Indisponivel") -> nuvem
      
      word.corpus <- Corpus(VectorSource(nuvem$palavras))  #Corpus
      
      word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
      word.freq <- sort(rowSums(word.counts), decreasing = TRUE)
      head(word.freq) 
      
      layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
      par(mar=rep(0, 4))
      plot.new()
      text(x=0.5, y=0.82, "Causas (Capitulo) - Homens", cex = 1.6, font = 1)
      
      wordcloud(words = names(word.freq), 
                freq = word.freq, 
                scale = c(4.5,1),
                max.words = 20, 
                random.order = FALSE, 
                color = wes_palette("Darjeeling1"))
      
    })
  })
  
  output$wordcloud_masc_output <-renderPlot({
    grafico_wordcloud_masc_output()
  })
  
  
  
  #########################################################
  
  grafico_causabas_masc_output <- reactive({
    input$botao_atualizar
    isolate({
      
      causabas_masc <- dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                   ano_obito <= input$ano_obito[2],
                                                   RA %in% input$RA,
                                                   sexo == "Masculino",
                                                   is.na(causabas) == FALSE,
                                                   is.na(causabasnomes) == FALSE) %>%
                          group_by(causabas, causabasnomes) %>%
                          summarise(count = n()) %>%
                          ungroup() %>%
                          top_n(n = 8, wt = count) %>%
                          arrange(desc(count))
      
      ggplot(data = causabas_masc) +
        geom_bar(aes(y = count, 
                     x = sort(causabas),
                     fill = sort(causabas, decreasing = TRUE),
                     text = paste("Causa :", as.factor(causabasnomes))), 
                 stat = "identity",
                 color = "black",
                 alpha = 0.5) +
        scale_fill_brewer(palette = "Blues") +
        theme_gray() + 
        ggtitle(label = "Causas Especificas - Homens") +
        theme(legend.position='none') +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        scale_x_discrete(na.translate = FALSE) -> causabas_masc_grafico
      
      ggplotly(causabas_masc_grafico,
               tooltip = c("count","text"))
      
    })
  })
  
  output$causabas_masc_output <-renderPlotly({
    grafico_causabas_masc_output()
  })
  
  
  
  ########################################################
  
  grafico_causabas_fem_output <- reactive({
    input$botao_atualizar
    isolate({
      
      causabas_fem <- dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                  ano_obito <= input$ano_obito[2],
                                                  RA %in% input$RA,
                                                  sexo == "Feminino",
                                                  is.na(causabas) == FALSE,
                                                  is.na(causabasnomes) == FALSE) %>%
                        group_by(causabas, causabasnomes) %>%
                        summarise(count = n()) %>%
                        ungroup() %>%
                        top_n(n = 8, wt = count) %>%
                        arrange(desc(count))
      
     ggplot(data = causabas_fem) +
      geom_bar(aes(y = count, 
                   x = sort(causabas),
                   fill = sort(causabas, decreasing = TRUE),
                   text = paste("Causa :", as.factor(causabasnomes))), 
               stat = "identity",
               color = "black",
               alpha = 0.5) +
      scale_fill_brewer(palette = "Reds") +
      theme_gray() + 
      ggtitle(label = "Causas Especificas - Mulheres") +
      theme(legend.position='none') +
       theme(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust = 0),
             axis.title.y = element_blank(),
             axis.title.x = element_blank())+
      scale_x_discrete(na.translate = FALSE) -> causabas_fem_grafico
    
    ggplotly(causabas_fem_grafico,
             tooltip = c("count","text"))
      
  
    })
  })
  
  output$causabas_fem_output <-renderPlotly({
    grafico_causabas_fem_output()
  })
  
  
  #########################################################
  
  grafico_capcid10_output <- reactive({
    input$botao_atualizar
    isolate({
      capcid10_grafico <- ggplot(data = dataset_reduzido %>% filter(ano_obito >= input$ano_obito[1],
                                                                    ano_obito <= input$ano_obito[2],
                                                                    RA %in% input$RA,
                                                                    #grupo %in% input$grupo,
                                                                    capcid10 != "**",
                                                                    sexo != "Ignorado"), aes(y = reorder(capcid10, capcid10,
                                                                                                         function(x) length(x)),
                                                                                             fill = sexo)) +
        geom_bar(aes(y = reorder(capcid10, capcid10,
                                 function(x) length(x)),
                     fill = sexo,
                     text = paste("Sexo: ",sexo, 
                                  "<br>",
                                  "Causa :", as.factor(capcid10nomes))), 
                 position = "dodge", show.legend = TRUE, color = 'black', 
                 na.rm = TRUE, alpha = 0.5, width = 0.8) +
        theme_gray() + 
        theme(legend.position='none') +
        scale_fill_brewer(palette = "Set1") +
        ggtitle("Causas - Capitulo CID10") +
        #ylab("Causas - Capitulo CID10") +
        #xlab("Quantidade") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0),
              axis.title.y = element_blank(),
              axis.title.x = element_blank())+
        scale_x_discrete(na.translate = FALSE)
      
      ggplotly(capcid10_grafico,
               height = 930,
               tooltip = c("count","text"))
    })
  })
  
  output$capcid10_output <-renderPlotly({
    grafico_capcid10_output()
  })
  
  
  ####################################################################################################################################
  # Botoes marcar/desmarcar
  
  # Marcar/desmarcar Primeira Infancia
  observe({
    
    x <- input$idade_cat1
    
    x <- c("<1","1-4")
    
    updateCheckboxGroupInput(
      session,
      "idade_cat",
      label = NULL, 
      choices = levels(dataset_reduzido$idade_cat),
      selected = x
    )
  })
  
  # Marcar/desmarcar Jovens
  observe({
    
    x <- input$idade_cat2
    
    x <- c("5-9","10-14","15-19")
    
    updateCheckboxGroupInput(
      session,
      "idade_cat",
      label = NULL, 
      choices = levels(dataset_reduzido$idade_cat),
      selected = x
    )
  })
  
  # Marcar/desmarcar Adultos
  observe({
    
    x <- input$idade_cat3
    
    x <- c("20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59")
    
    updateCheckboxGroupInput(
      session,
      "idade_cat",
      label = NULL, 
      choices = levels(dataset_reduzido$idade_cat),
      selected = x
    )
  })
  
  # Marcar/desmarcar Idosos
  observe({
    
    x <- input$idade_cat4
    
    x <- c("60-64","65-69","70-74","75-79","80-84","85+")
    
    updateCheckboxGroupInput(
      session,
      "idade_cat",
      label = NULL, 
      choices = levels(dataset_reduzido$idade_cat),
      selected = x
    )
  })
  
  
  # Marcar/desmarcar todas as Faixas Etárias
  observe({
    x <- input$all_idade_cat
    if (!is.null(x)) {
      x <- levels(dataset_reduzido$idade_cat)
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "idade_cat",
      label = NULL, 
      choices = levels(dataset_reduzido$idade_cat),
      selected = x
    )
  })
  
################################################################################
    
  # Marcar/desmarcar RAs do Grupo 1
  observe({
    
    x <- input$grupo1_RA

    x <- c("Brasilia", 'Plano Piloto', 'Asa Norte', 'Asa Sul', 'Jardim Botânico', 'Lago Norte', 'Lago Sul', 'Park Way', 'Sudoeste/Octogonal')

    updateCheckboxGroupInput(
      session,
      "RA",
      label = NULL, 
      choices = levels(dataset_reduzido$RA),
      selected = x
    )
  })
  
  # Marcar/desmarcar RAs do Grupo 2
  observe({
    
    x <- input$grupo2_RA

    x <- c('Águas Claras', 'Candangolândia', 'Cruzeiro', 'Gama', 'Guará', 'Núcleo Bandeirante', 'Sobradinho',
           'Sobradinho II', 'Taguatinga', 'Vicente Pires')
    
    updateCheckboxGroupInput(
      session,
      "RA",
      label = NULL, 
      choices = levels(dataset_reduzido$RA),
      selected = x
    )
  })
  
  # Marcar/desmarcar RAs do Grupo 3
  observe({
    x <- input$grupo3_RA

    x <- c('Brazlândia', 'Ceilândia', 'Planaltina', 'Riacho Fundo', 'Riacho Fundo II', 'SIA', 'Samambaia', 'Santa Maria',
             'São Sebastião')

    updateCheckboxGroupInput(
      session,
      "RA",
      label = NULL, 
      choices = levels(dataset_reduzido$RA),
      selected = x
    )
  })
  
  # Marcar/desmarcar RAs do Grupo 4
  observe({
    
    x <- input$grupo4_RA

    x <- c('Fercal', 'Itapoã', 'Paranoá', 'Recanto das Emas', 'SCIA', 'Varjão')

    updateCheckboxGroupInput(
      session,
      "RA",
      label = NULL, 
      choices = levels(dataset_reduzido$RA),
      selected = x
    )
  })

################################################################################  

  # Marcar/desmarcar todas as RAs
  observe({
    x <- input$all_RA
    if (!is.null(x)) {
      x <- levels(dataset_reduzido$RA)
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "RA",
      label = NULL, 
      choices = levels(dataset_reduzido$RA),
      selected = x
    )
  })
  
  # Marcar/desmarcar todas as Estado Civil
  observe({
    x <- input$all_estciv
    if (!is.null(x)) {
      x <- levels(dataset_reduzido$estciv)
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "estciv",
      label = NULL, 
      choices = levels(dataset_reduzido$estciv),
      selected = x
    )
  })
  
  # Marcar/desmarcar todas as Escolaridade
  observe({
    x <- input$all_esc
    if (!is.null(x)) {
      x <- levels(dataset_reduzido$esc)
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "esc",
      label = NULL, 
      choices = levels(dataset_reduzido$esc),
      selected = x
    )
  })
  
  # Marcar/desmarcar todas as Raca/cor
  observe({
    x <- input$all_racacor
    if (!is.null(x)) {
      x <- levels(dataset_reduzido$racacor)
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "racacor",
      label = NULL, 
      choices = levels(dataset_reduzido$racacor),
      selected = x
    )
  })
  
  # Marcar/desmarcar todas as Estado Civil
  observe({
    x <- input$all_grupo
    if (!is.null(x)) {
      x <- levels(dataset_reduzido$grupo)
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "grupo",
      label = NULL, 
      choices = levels(dataset_reduzido$grupo),
      selected = x
    )
  })
  
  # Marcar/desmarcar todas as causas de mortes (capcid10)
  observe({
    x <- input$all_capcid10nomes
    if (!is.null(x)) {
      x <- levels(dataset_reduzido$capcid10nomes)
    }
    else {
      x <- character(0) 
    }
    
    updateCheckboxGroupInput(
      session,
      "capcid10nomes",
      label = NULL, 
      choices = levels(dataset_reduzido$capcid10nomes),
      selected = x
    )
  })
  
}

shinyApp(ui, server)

