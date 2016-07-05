library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)

ui <- dashboardPage(
  dashboardHeader(title = "Distribuições Contínuas de Probabilidade", titleWidth = 400),
  dashboardSidebar(width = 290,
    selectInput("dist", "Distribuição:", choices = c(   
      "Selecione uma distribuição"       = "null",
                       Exponencial       = "exp",
                       Weibull           = "wei",
                       Normal            = "norm",
                       Beta              = "beta",
                       Gama              = "gama",
                       "t de Student"    = "t.stu",
                       "Qui-Quadrado"    = "qq",
                       Cauchy            = "cchy",
                       F                 = "f"),
                               selected  = NULL),
#### sidebar exponencial ####
conditionalPanel(condition = "input.dist == 'exp'",
 numericInput("lambda",
     HTML("&lambda; :"),
     value = 5),
     textOutput("alertaexp"),
     bsAlert("alertexp"),
 sliderInput("xmaxexp", 
     "Valor máximo para o eixo X:",
     min = 1,
     max = 1000,
     value = 3),
 checkboxInput("multexp", "Multiplas funções", FALSE),
conditionalPanel(condition = "input.multexp == true",
 fluidRow(column(numericInput("lambda2",HTML("&lambda;<sub>2</sub>:"), value = 0, width = 100),width = 4), 
          column(numericInput("lambda3",HTML("&lambda;<sub>3</sub>:"), value = 0, width = 100),width = 4),
          column(numericInput("lambda4",HTML("&lambda;<sub>4</sub>:"), value = 0, width = 100),width = 4))),
 checkboxInput("acum", "Calcular acumulada", FALSE),
conditionalPanel(condition = "input.acum == true",
 fluidRow( column(numericInput("pini", "Ponto Inicial:", value = 0, width = 150),width = 6), 
           column(numericInput("pfim", "Ponto Final:", value = 1, width = 150),width = 6))),
 checkboxInput("histexp", "Gerador de Amostra", FALSE),
conditionalPanel(condition = "input.histexp == true",
 numericInput("nexp",
 "Tamanho da Amostra:",
  value = 20))),
#### sidebar weibull ##################
                   conditionalPanel(condition = "input.dist == 'wei'",
                                    fluidRow( column(numericInput("alfawei",HTML("&alpha; (Escala):"), value = 1, width = 100),width = 6), 
                                              column(numericInput("betawei",HTML("&beta; (Forma):"), value = 3, width = 100),width = 6)),
                                    textOutput("alertaweialfa"),
                                    bsAlert("alertalfawei"),
                                    textOutput("alertaweibeta"),
                                    bsAlert("alertbetawei"),
                                    sliderInput("xmaxwei", 
                                                "Valor máximo para o eixo X:",
                                                min = 1,
                                                max = 1000,
                                                value = 3),
                                    checkboxInput("multwei", HTML("Multiplas funções"), FALSE),
                                    conditionalPanel(condition = "input.multwei == true",
                                                     fluidRow( column(numericInput("alfawei2",HTML("&alpha;<sub>2</sub> (Escala):"), value = 0, width = 100),width = 6), 
                                                               column(numericInput("betawei2",HTML("&beta;<sub>2</sub> (Forma):"), value = 0, width = 100),width = 6)),
                                                     fluidRow( column(numericInput("alfawei3",HTML("&alpha;<sub>3</sub> (Escala):"), value = 0, width = 100),width = 6), 
                                                               column(numericInput("betawei3",HTML("&beta;<sub>3</sub> (Forma):"), value = 0, width = 100),width = 6)),
                                                     fluidRow( column(numericInput("alfawei4",HTML("&alpha;<sub>4</sub> (Escala):"), value = 0, width = 100),width = 6), 
                                                               column(numericInput("betawei4",HTML("&beta;<sub>4</sub> (Forma):"), value = 0, width = 100),width = 6))
                                    ),
                                    checkboxInput("acumwei", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumwei == true",
                                                     fluidRow( column(numericInput("piniwei","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                               column(numericInput("pfimwei","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histwei", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histwei == true",
                                                     numericInput("nwei",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar normal ##################
                   conditionalPanel(condition = "input.dist == 'norm'",
                                    fluidRow( column(numericInput("munorm",HTML("&mu; (Média):"), value = 0, width = 100),width = 5), 
                                              column(numericInput("sdnorm",HTML("&sigma; (Desvio Padrão):"), value = 1, width = 150),width = 7)),
                                    textOutput("alertamunorm"),
                                    bsAlert("alertnormmu"),
                                    textOutput("alertasdnorm"),
                                    bsAlert("alertnormsd"),
                                    sliderInput("xminnorm", 
                                                "Valor mínimo para o eixo X:",
                                                min = -500,
                                                max =  500,
                                                value = -3),
                                    textOutput("alertaeixonorm"),
                                    bsAlert("alertnormeixo"),
                                    sliderInput("xmaxnorm", 
                                                "Valor máximo para o eixo X:",
                                                min = -500,
                                                max =  500,
                                                value = 3),
                                    checkboxInput("multnorm", HTML("Multiplas funções"), FALSE),
                                    conditionalPanel(condition = "input.multnorm == true",
                                                     fluidRow( column(numericInput("munorm2",HTML("&mu;<sub>2</sub> (Média):"), value = 0, width = 100),width = 5), 
                                                               column(numericInput("sdnorm2",HTML("&sigma;<sub>2</sub> (Desvio Padrão):"), value = 0, width = 150),width = 7)),
                                                     fluidRow( column(numericInput("munorm3",HTML("&mu;<sub>3</sub> (Média):"), value = 0, width = 100),width = 5), 
                                                               column(numericInput("sdnorm3",HTML("&sigma;<sub>3</sub> (Desvio Padrão):"), value = 0, width = 150),width = 7)),
                                                     fluidRow( column(numericInput("munorm4",HTML("&mu;<sub>4</sub> (Média):"), value = 0, width = 100),width = 5), 
                                                               column(numericInput("sdnorm4",HTML("&sigma;<sub>4</sub> (Desvio Padrão):"), value = 0, width = 150),width = 7))),
                                    checkboxInput("acumnorm", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumnorm == true",
                                                     fluidRow( column(numericInput("pininorm","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                               column(numericInput("pfimnorm","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histnorm", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histnorm == true",
                                                     numericInput("nnorm",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar beta ##################
                   conditionalPanel(condition = "input.dist == 'beta'",
                                    fluidRow( column(numericInput("alfabeta",HTML("&alpha;:"), value = 3, width = 100),width = 6), 
                                              column(numericInput("betabeta",HTML("&beta;:"), value = 3, width = 100),width = 6)),
                                    textOutput("alertabetaalfa"),
                                    bsAlert("alertalfabeta"),
                                    textOutput("alertbetabeta"),
                                    bsAlert("alertbbeta"),
                                    sliderInput("xmaxbeta", 
                                                "Valor máximo para o eixo X:",
                                                min = 1,
                                                max = 5,
                                                value = 1),
                                    checkboxInput("multbeta", HTML("Multiplas funções"), FALSE),
                                    conditionalPanel(condition = "input.multbeta == true",
                                                     fluidRow( column(numericInput("alfabeta2",HTML("&alpha;<sub>2</sub>:"), value = 0, width = 100),width = 6), 
                                                               column(numericInput("betabeta2",HTML("&beta;<sub>2</sub>:"), value = 0, width = 100),width = 6)),
                                                     fluidRow( column(numericInput("alfabeta3",HTML("&alpha;<sub>3</sub>:"), value = 0, width = 100),width = 6), 
                                                               column(numericInput("betabeta3",HTML("&beta;<sub>3</sub>:"), value = 0, width = 100),width = 6)),
                                                     fluidRow( column(numericInput("alfabeta4",HTML("&alpha;<sub>4</sub>:"), value = 0, width = 100),width = 6), 
                                                               column(numericInput("betabeta4",HTML("&beta;<sub>4</sub>:"), value = 0, width = 100),width = 6))),
                                    checkboxInput("acumbeta", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumbeta == true",
                                                     fluidRow( column(numericInput("pinibeta","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                               column(numericInput("pfimbeta","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histbeta", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histbeta == true",
                                                     numericInput("nbeta",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar gama ##################
                   conditionalPanel(condition = "input.dist == 'gama'",
                                    fluidRow(column(numericInput("alfagama",HTML("&alpha; (Forma):"), value = 3, width = 100),width = 6), 
                                             column(numericInput("betagama",HTML("&beta; (Escala):"), value = 3, width = 100),width = 6)),
                                    textOutput("alertagamaalfa"),
                                    bsAlert("alertalfagama"),
                                    textOutput("alertagamabeta"),
                                    bsAlert("alertbetagama"),
                                    sliderInput("xmaxgama", 
                                                "Valor máximo para o eixo X:",
                                                min = 0,
                                                max = 50,
                                                value = 3),
                                    checkboxInput("multgama", HTML("Multiplas funções"), FALSE),
                                    conditionalPanel(condition = "input.multgama == true",
                                                     fluidRow(column(numericInput("alfagama2",HTML("&alpha;<sub>2</sub> (Forma):"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("betagama2",HTML("&beta;<sub>2</sub> (Escala):"), value = 0, width = 100),width = 6)),
                                                     fluidRow(column(numericInput("alfagama3",HTML("&alpha;<sub>3</sub> (Forma):"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("betagama3",HTML("&beta;<sub>3</sub> (Escala):"), value = 0, width = 100),width = 6)),
                                                     fluidRow(column(numericInput("alfagama4",HTML("&alpha;<sub>4</sub> (Forma):"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("betagama4",HTML("&beta;<sub>4</sub> (Escala):"), value = 0, width = 100),width = 6))),
                                    checkboxInput("acumgama", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumgama == true",
                                                     fluidRow(column(numericInput("pinigama","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                              column(numericInput("pfimgama","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histgama", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histgama == true",
                                                     numericInput("ngama",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar t.student ##################
                   conditionalPanel(condition = "input.dist == 't.stu'",
                                    numericInput("df",
                                                 "v (Graus de Liberdade):",
                                                 value = 10),
                                    textOutput("alertadft"),
                                    bsAlert("alerttdf"),
                                    sliderInput("xmint",
                                                "Valor mínimo para o eixo X:",
                                                min = -50,
                                                max =   -1,
                                                value = -3),
                                    sliderInput("xmaxt", 
                                                "Valor máximo para o eixo X:",
                                                min =   1,
                                                max =  50,
                                                value = 3),
                                    checkboxInput("multt", "Multiplas funções", FALSE),
                                    conditionalPanel(condition = "input.multt == true",
                                                     fluidRow(column(numericInput("df2",HTML("v<sub>2</sub>:"), value = 0, width = 100),width = 4), 
                                                              column(numericInput("df3",HTML("v<sub>3</sub>:"), value = 0, width = 100),width = 4),
                                                              column(numericInput("df4",HTML("v<sub>4</sub>:"), value = 0, width = 100),width = 4))),
                                    checkboxInput("acumt", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumt == true",
                                                     fluidRow(column(numericInput("pinit","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                              column(numericInput("pfimt","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histt", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histt == true",
                                                     numericInput("nt",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar quiquadrado ##################
                   conditionalPanel(condition = "input.dist == 'qq'",
                                    numericInput("dfqq",
                                                 "v (Graus de Liberdade):",
                                                 value = 10),
                                    textOutput("alertadfqq"),
                                    bsAlert("alertqqdf"),
                                    sliderInput("xmaxqq", 
                                                "Valor máximo para o eixo X:",
                                                min =   5,
                                                max =  150,
                                                value = 8),
                                    checkboxInput("multqq", "Multiplas funções", FALSE),
                                    conditionalPanel(condition = "input.multqq == true",
                                                     fluidRow(column(numericInput("dfqq2",HTML("v<sub>2</sub>:"), value = 0, width = 100),width = 4), 
                                                              column(numericInput("dfqq3",HTML("v<sub>3</sub>:"), value = 0, width = 100),width = 4),
                                                              column(numericInput("dfqq4",HTML("v<sub>4</sub>:"), value = 0, width = 100),width = 4))),
                                    checkboxInput("acumqq", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumqq == true",
                                                     fluidRow(column(numericInput("piniqq","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                              column(numericInput("pfimqq","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histqq", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histqq == true",
                                                     numericInput("nqq",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar cauchy ####
                   conditionalPanel(condition = "input.dist == 'cchy'",
                                    fluidRow(column(numericInput("alfacchy",HTML("&alpha; (Escala):"), value = 3, width = 100),width = 6), 
                                             column(numericInput("betacchy",HTML("&beta; (Locação):"), value = 3, width = 100),width = 6)),
                                    textOutput("alertaalfacchy"),
                                    bsAlert("alertcchyalfa"),
                                    textOutput("alertabetacchy"),
                                    bsAlert("alertcchybeta"),
                                    sliderInput("xmincchy", 
                                                "Valor mínimo para o eixo X:",
                                                min = -500,
                                                max =  499,
                                                value = -1),
                                    textOutput("alertaeixocchy"),
                                    bsAlert("alertcchyeixo"),
                                    sliderInput("xmaxcchy", 
                                                "Valor máximo para o eixo X:",
                                                min = -499,
                                                max = 500,
                                                value = 1),
                                    checkboxInput("multcchy", HTML("Multiplas funções"), FALSE),
                                    conditionalPanel(condition = "input.multcchy == true",
                                                     fluidRow(column(numericInput("alfacchy2",HTML("&alpha;<sub>2</sub> (Escala):"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("betacchy2",HTML("&beta;<sub>2</sub> (Locação):"), value = 0, width = 100),width = 6)),
                                                     fluidRow(column(numericInput("alfacchy3",HTML("&alpha;<sub>3</sub> (Escala):"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("betacchy3",HTML("&beta;<sub>3</sub> (Locação):"), value = 0, width = 100),width = 6)),
                                                     fluidRow(column(numericInput("alfacchy4",HTML("&alpha;<sub>4</sub> (Escala):"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("betacchy4",HTML("&beta;<sub>4</sub> (Locação):"), value = 0, width = 100),width = 6))),
                                    checkboxInput("acumcchy", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumcchy == true",
                                                     fluidRow(column(numericInput("pinicchy","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                              column(numericInput("pfimcchy","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histcchy", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histcchy == true",
                                                     numericInput("ncchy",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar f ####
                   conditionalPanel(condition = "input.dist == 'f'",
                                    numericInput("dffn1",
                                                 HTML("m (Graus de liberdade no denominador):"),
                                                 value = 10),
                                    textOutput("alertamf"),
                                    bsAlert("alertfdfm"),
                                    numericInput("dffm1",
                                                 HTML("n (Graus de liberdade no numerador):"),
                                                 value = 10),
                                    textOutput("alertanf"),
                                    bsAlert("alertfdfn"),
                                    sliderInput("xmaxf", 
                                                "Valor máximo para o eixo X:",
                                                min =   5,
                                                max =  150,
                                                value = 8),
                                    checkboxInput("multf", "Multiplas funções", FALSE),
                                    conditionalPanel(condition = "input.multf == true",
                                                     fluidRow(column(numericInput("dffn2",HTML("m<sub>2</sub>:"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("dffm2",HTML("n<sub>2</sub>:"), value = 0, width = 100),width = 6)),
                                                     fluidRow(column(numericInput("dffn3",HTML("m<sub>3</sub>:"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("dffm3",HTML("n<sub>3</sub>:"), value = 0, width = 100),width = 6)),
                                                     fluidRow(column(numericInput("dffn4",HTML("m<sub>4</sub>:"), value = 0, width = 100),width = 6), 
                                                              column(numericInput("dffm4",HTML("n<sub>4</sub>:"), value = 0, width = 100),width = 6))),
                                    checkboxInput("acumf", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumf == true",
                                                     fluidRow(column(numericInput("pinif","Ponto Inicial:", value = 0, width = 150),width = 6), 
                                                              column(numericInput("pfimf","Ponto Final:", value = 1, width = 150),width = 6))),
                                    checkboxInput("histf", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histf == true",
                                                     numericInput("nf",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20))),
#### sidebar logistica ####
                   conditionalPanel(condition = "input.dist == 'logist'",
                                    numericInput("mulogist",
                                                 HTML("&mu; :"),
                                                 value = 3),
                                    numericInput("sdlogist",
                                                 HTML("&sigma; :"),
                                                 value = 3),
                                    sliderInput("xminlogist", 
                                                "Valor mínimo para o eixo X:",
                                                min = -50,
                                                max =  50,
                                                value = -3),
                                    sliderInput("xmaxlogist", 
                                                "Valor máximo para o eixo X:",
                                                min = -50,
                                                max =  50,
                                                value = 3),
                                    checkboxInput("multlogist", HTML("Multiplas funções"), FALSE),
                                    conditionalPanel(condition = "input.multlogist == true",
                                                     numericInput("mulogist2",
                                                                  HTML("&mu;<sub>2</sub> (vermelha):"),
                                                                  value = 0),
                                                     numericInput("sdlogist2",
                                                                  HTML("&sigma;<sub>2</sub> (vermelha):"),
                                                                  value = 0),
                                                     numericInput("mulogist3",
                                                                  HTML("&mu;<sub>3</sub> (azul):"),
                                                                  value = 0),
                                                     numericInput("sdlogist3",
                                                                  HTML("&sigma;<sub>3</sub> (azul):"),
                                                                  value = 0),
                                                     numericInput("mulogist4",
                                                                  HTML("&mu;<sub>4</sub> (verde):"),
                                                                  value = 0),
                                                     numericInput("sdlogist4",
                                                                  HTML("&sigma;<sub>4</sub> (verde):"),
                                                                  value = 0)),
                                    checkboxInput("acumlogist", "Calcular acumulada", FALSE),
                                    conditionalPanel(condition = "input.acumlogist == true",
                                                     numericInput("pinilogist",
                                                                  "Ponto Inicial:",
                                                                  value = 1),
                                                     numericInput("pfimlogist",
                                                                  "Ponto final:",
                                                                  value = 2)),
                                    checkboxInput("histlogist", "Gerador de Amostra", FALSE),
                                    conditionalPanel(condition = "input.histlogist == true",
                                                     numericInput("nlogist",
                                                                  "Tamanho da Amostra:",
                                                                  value = 20)))
  ),

###################################### main ####
dashboardBody(conditionalPanel(condition = "input.dist != 'null'",
#### medias e variancias ####
conditionalPanel(condition = "input.dist == 'exp'",
    textOutput("mvexp"),
    textOutput("acuexp")),
conditionalPanel(condition = "input.dist == 'wei'",
    textOutput("mvwei"),
    textOutput("acumuwei")),
conditionalPanel(condition = "input.dist == 'norm'",
    textOutput("mvnorm"),
    textOutput("acunorm")),
conditionalPanel(condition = "input.dist == 'beta'",
    textOutput("mvbeta"),
    textOutput("acubeta")),
conditionalPanel(condition = "input.dist == 'gama'",
    textOutput("mvgama"),
    textOutput("acugama")),
conditionalPanel(condition = "input.dist == 't.stu'",
    textOutput("mvt"),
    textOutput("acut")),
conditionalPanel(condition = "input.dist == 'qq'",
    textOutput("mvqq"),
    textOutput("acuqq")),
conditionalPanel(condition = "input.dist == 'cchy'",
    textOutput("mvcchy"),
    textOutput("acucchy")),
conditionalPanel(condition = "input.dist == 'f'",
    textOutput("mvf"),
    textOutput("acuf")),
##################### tab intro #####
tabsetPanel(
   tabPanel("Introdução",
#### exp ####
conditionalPanel(condition = "input.dist == 'exp'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição exponencial é um caso particular da distribuição gama, ela é
        importante para a área de confiabilidade (análise de sobrevivência),
        ela se ajusta a uma variável aleatória contínua que pode assumir qualquer 
        valor não-negativo, normalmente utilizada para modelar o tempo até o evento desejado."),
   uiOutput("tabexp")),
#### wei ####
conditionalPanel(condition = "input.dist == 'wei'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição de Weibull é uma das mais usadas na teoria da confiabilidade,
        se assemelha à distribuição exponencial, porém possui um parâmetro a mais. Uma
        variável que segue uma distribuição de weibull assume qualquer valor não-negativo."),
   uiOutput("tabwei")),
#### normal ####
conditionalPanel(condition = "input.dist == 'norm'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição normal também é chamada de Gaussiana e tem grandes usabilidades
        , entre elas, a inferência estatística. Utilizando o teorema central do limite
        é possível o cálculo da probabilidade de outras distribuições por meio de 
        aproximação caso o número de observações seja satisfatoriamente grande. Uma
        variável que segue essa distribuição assume qualquer valor dentro dos 
        números reais."),
   uiOutput("tabnorm")),
#### beta ####
conditionalPanel(condition = "input.dist == 'beta'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição beta é normalmente utilizada para modelar variáveis que sejam uma 
        proporção, ou definidas no intervalo entre 0 e 1, já que é o intervalo em que
        a distribuição está definida. Ela possui dois parâmetros e ambos são de forma."),
   uiOutput("tabbeta")),
#### gama ####
conditionalPanel(condition = "input.dist == 'gama'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição gama é bem geral, algumas distribuições são casos particulares dela,
        como a exponencial e a qui-quadrada. Ela pode assumir valores não-negativos e é muito
        utilizada para a análise de tempo de vida de produtos."),
   uiOutput("tabgama")),
#### t stu ####
conditionalPanel(condition = "input.dist == 't.stu'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição t de student se assemelha bastante a distribuição normal, porém 
        ela possui apenas um parâmetro, que é a quantidade de graus de liberdade e quanto
        maior ela for mais proximo a distribuição estará de uma distribuição normal.
        Ela também é utilizada como estatística de teste para testes de hipóteses 
        e construção de intervalos de confiança."),
   uiOutput("tabt")),
#### quiquadrado ####
conditionalPanel(condition = "input.dist == 'qq'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição qui-quadrada é um caso particular da distribuição gama, caso seja
        somado um número 'n' de distribuições normais padrão ao quadrado será obtido uma distribuição
        qui-quadrada com 'n' graus de liberdade. Essa distribuição assume valores não-negativos
        e pode ser utlizada para algumas finalidades como: inferencia estatística;
        teste de hipóteses e construção de intervalos de confiança. "),
   uiOutput("tabqq")),
#### cchy ####
conditionalPanel(condition = "input.dist == 'cchy'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição de Cauchy tem a particularidade de não possuir média e variância. Ela
        possui várias contribuições para a área da física e da matemática, como ser uma das
        soluções para a equação de Laplace. Essa distribuição pode assumir qualquer valor
        dos números reais."),
uiOutput("tabcchy")),
#### f ####
conditionalPanel(condition = "input.dist == 'f'",
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        A distribuição F também é conhecida como distribuição F de Snedecor e distribuição de
        Fisher, ela é utilizada na inferência estatística para a análise de variância. Uma 
        variável que segue distribuição F assume valores não-negativos."),
   uiOutput("tabf"))),
                                   
##################### densidades #####
tabPanel(HTML("&fnof;(x)"),
#### exp ####                     
conditionalPanel(condition = "input.dist == 'exp'",
   withMathJax(),
   HTML("$$f(x) = \\lambda e^{- \\lambda x},~ 0 \\leq x < \\infty, ~ \\lambda > 0$$"),
   plotOutput("denexp"),
   downloadButton("denexpdown", label = "Exportar")),
#### wei ####
conditionalPanel(condition = "input.dist == 'wei'",
   HTML("$$f(x) = \\beta \\alpha  * \\alpha x^{\\beta -1} e^{-\\alpha x^{\\beta}},~ x \\geq 0 , ~ \\beta > 0, ~ \\alpha > 0 $$"),                 
   plotOutput("denwei"),
   downloadButton("denweidown", label = "Exportar")),
#### norm ####
conditionalPanel(condition = "input.dist == 'norm'",
   HTML("$$f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}}e^\\frac{-(x-\\mu)^2}{2\\sigma^2},
        ~ -\\infty < x < \\infty,~ \\sigma > 0$$"),                 
   plotOutput("dennorm"),
   downloadButton("dennormdown", label = "Exportar")),
#### beta ####
conditionalPanel(condition = "input.dist == 'beta'",
   HTML("$$f(x) = \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}
        x^{\\alpha - 1 }(1-x)^{\\beta - 1},~ 0 \\leq x \\leq 1,~ \\alpha > 0,~ \\beta > 0 $$"),                 
   plotOutput("denbeta"),
   downloadButton("denbetadown", label = "Exportar")),
#### gama ####
conditionalPanel(condition = "input.dist == 'gama'",
   HTML("$$f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)}x^{\\alpha -1}e^{-\\beta x},
        ~ 0 \\leq x < \\infty,~ \\alpha > 0,~ \\beta > 0$$"),
   plotOutput("dengama"),
   downloadButton("dengamadown", label = "Exportar")),
#### t stu ####
conditionalPanel(condition = "input.dist == 't.stu'",
   HTML("$$f(x) = \\frac{\\Gamma((v+1)/2)}{\\Gamma(v/2)\\sqrt{v\\pi}}
        (1+ \\frac{x^2}{v})^{- \\frac{v+1}{2}},
        ~ -\\infty \\leq x < \\infty,~ \\mbox{v inteiro e positivo}$$"),                 
   plotOutput("dent"),
   downloadButton("dentdown", label = "Exportar")),
#### quiquadrado ####
conditionalPanel(condition = "input.dist == 'qq'",
   HTML("$$f(x) = \\frac{2^{-v/2}}{\\Gamma(v/2)}x^{(v/2)-1}e^{-x/2},
        ~ 0 \\leq x < \\infty,~ v > 0 $$"),                 
   plotOutput("denqq"),
   downloadButton("denqqdown", label = "Exportar")),
#### cchy ####
conditionalPanel(condition = "input.dist == 'cchy'",
   HTML("$$f(x) = \\frac{1}{\\pi\\alpha[1+(\\frac{x-\\beta}{\\alpha})^2]},
        ~ -\\infty \\leq x < \\infty,~ \\alpha > 0 $$"),                 
   plotOutput("dencchy"),
   downloadButton("dencchydown", label = "Exportar")),
#### f ####
conditionalPanel(condition = "input.dist == 'f'",
   HTML("$$f(x) = \\frac{\\Gamma(\\frac{m+n}{2})(\\frac{m}{n})^{m/2} x^{\\frac{m}{2}-1}}
        {\\Gamma(\\frac{m}{2})\\Gamma(\\frac{n}{2})[(\\frac{m}{n})x+1]^{\\frac{m+n}{2}}},
        ~ 0 \\leq x < \\infty,~ \\mbox{m e n inteiros e positivos}  $$"),                 
   plotOutput("denf"),
   downloadButton("denfdown", label = "Exportar"))),
##################### acumuladas ####
tabPanel(HTML("F(X)"),
#### exp ####                     
conditionalPanel(condition = "input.dist == 'exp'",
   HTML("$$F(X)= 1- e^{- \\lambda x}$$"),
   plotOutput("acumuexp"),        
   downloadButton("acumuexpdown", label = "Exportar")),
#### wei ####
conditionalPanel(condition = "input.dist == 'wei'",
   HTML("$$F(X)= 1 - e^{-\\alpha x^{\\beta}}$$"),                 
   plotOutput("acuwei"),
   downloadButton("acuweidown", label = "Exportar")),
#### normal ####
conditionalPanel(condition = "input.dist == 'norm'",
   HTML("$$F(X) = \\frac{1}{2} [1 + erf(x/\\sqrt{2})],~ \\mbox{erf é a funcão erro} $$"),                 
   plotOutput("acumunorm"),
   downloadButton("acumunormdown", label = "Exportar")),
#### beta ####
conditionalPanel(condition = "input.dist == 'beta'",
   HTML("$$F(X) = b_x(\\alpha,\\beta) = \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} 
        \\int_{0}^{x}t^{\\alpha - 1}(1 - t)^{\\beta-1}dt  $$"),
   plotOutput("acumubeta"),
   downloadButton("acumubetadown", label = "Exportar")),
#### gama ####
conditionalPanel(condition = "input.dist == 'gama'",
   HTML("$$F(X) = \\frac{\\gamma(\\alpha,\\beta x)}{\\Gamma(\\alpha)},
        ~ \\gamma ~ \\mbox{é a função gama incompleta}  $$"),
   plotOutput("acumugama"),
   downloadButton("acumugamadown", label = "Exportar")),
#### t stu ####
conditionalPanel(condition = "input.dist == 't.stu'",
   HTML("$$F(X) = \\frac{1}{2} + x \\Gamma(\\frac{v+1}{2})* 
        \\frac{_2F_1(\\frac{1}{2},\\frac{v+1}{2},\\frac{3}{2},
        -\\frac{x^{2}}{v})}{\\sqrt{\\pi v}\\Gamma(v/2)},
        ~  _2F_1 \\mbox{é a função hipergeométrica}$$"),
   plotOutput("acumut"),
   downloadButton("acumutdown", label = "Exportar")),
#### quiquadrado ####
conditionalPanel(condition = "input.dist == 'qq'",
   HTML("$$F(X) = \\frac{\\gamma(\\frac{v}{2},\\frac{x}{2})}{\\Gamma(\\frac{v}{2})},
        ~ \\gamma ~ \\mbox{é a função gama incompleta}$$"),
   plotOutput("acumuqq"),
   downloadButton("acumuqqdown", label = "Exportar")),
#### cchy ####
conditionalPanel(condition = "input.dist == 'cchy'",
   HTML("$$F(X) = \\frac{1}{2} + \\frac{1}{\\pi} arctan(\\frac{x - \\beta}{\\alpha})$$"),                 
   plotOutput("acumucchy"),
   downloadButton("acumucchydown", label = "Exportar")),
#### f ####
conditionalPanel(condition = "input.dist == 'f'",
   HTML("$$F(X) = b_{\\frac{m x}{m x + n}}(\\frac{m}{2},\\frac{n}{2}),
        ~ b ~ \\mbox{é a função beta incompleta}  $$"),                 
   plotOutput("acumuf"),
   downloadButton("acumufdown", label = "Exportar"))),
##################### sobrevivencias ####
tabPanel(HTML("S(X)"),
#### exp ####
conditionalPanel(condition = "input.dist == 'exp'",
   HTML("$$S(X)=e^{- \\lambda x}$$"),
   plotOutput("survexp"),
   downloadButton("survexpdown", label = "Exportar")),
#### wei ####
conditionalPanel(condition = "input.dist == 'wei'",
   HTML("$$S(X) = e^{-\\alpha x^{\\beta}}$$"), 
   plotOutput("survwei"),
   downloadButton("survweidown", label = "Exportar")),
#### norm ####
conditionalPanel(condition = "input.dist == 'norm'",
   HTML("$$S(X) = 1 - \\frac{1}{2} [1 + erf(x/\\sqrt{2})],~ \\mbox{erf é a funcão erro}$$"),                 
   plotOutput("survnorm"),
   downloadButton("survnormdown", label = "Exportar")),
#### beta ####
conditionalPanel(condition = "input.dist == 'beta'",
   HTML("$$S(X) = 1- b_x(\\alpha,\\beta) = 1 - \\frac{\\Gamma(\\alpha + \\beta)}
        {\\Gamma(\\alpha)\\Gamma(\\beta)} \\int_{0}^{x}t^{\\alpha - 1}(1 - t)^{\\beta-1}dt  $$"),                 
   plotOutput("survbeta"),
   downloadButton("survbetadown", label = "Exportar")),
#### gama ####
conditionalPanel(condition = "input.dist == 'gama'",
   HTML("$$S(X) = 1 - \\frac{\\gamma(\\alpha,\\beta x)}{\\Gamma(\\alpha)},
        ~ \\gamma ~ \\mbox{é a função gama incompleta}$$"),
   plotOutput("survgama"),
   downloadButton("survgamadown", label = "Exportar")),
#### t stu ####
conditionalPanel(condition = "input.dist == 't.stu'",
   HTML("$$S(X) =  \\frac{1}{2} - x \\Gamma(\\frac{v+1}{2})* 
        \\frac{_2F_1(\\frac{1}{2},\\frac{v+1}{2},\\frac{3}{2},
        -\\frac{x^{2}}{v})}{\\sqrt{\\pi v}\\Gamma(v/2)},
        ~  _2F_1 \\mbox{é a função hipergeométrica}$$"),                 
   plotOutput("survt"),
   downloadButton("survtdown", label = "Exportar")),
#### quiquadrado ####
conditionalPanel(condition = "input.dist == 'qq'",
   HTML("$$S(X) = 1 - \\frac{\\gamma(\\frac{v}{2},\\frac{x}{2})}{\\Gamma(\\frac{v}{2})},
        ~ \\gamma ~ \\mbox{é a função gama incompleta}$$"),                 
   plotOutput("survqq"),
   downloadButton("survqqdown", label = "Exportar")),
#### cchy ####
conditionalPanel(condition = "input.dist == 'cchy'",
   HTML("$$S(X) = \\frac{1}{2} - \\frac{1}{\\pi} arctan(\\frac{x - \\beta}{\\alpha})$$"),
   plotOutput("survcchy"),
   downloadButton("survcchydown", label = "Exportar")),
#### f ####
conditionalPanel(condition = "input.dist == 'f'",
   HTML("$$S(X) = 1 - b_{\\frac{m x}{m x + n}}(\\frac{m}{2},\\frac{n}{2}),
        ~ b ~ \\mbox{é a função beta incompleta}  $$"),
   plotOutput("survf"),
   downloadButton("survfdown", label = "Exportar"))),
##################### gerador de amostra ####
tabPanel(HTML("Gerador de Amostras"),
#### exp ####
conditionalPanel(condition = "input.dist == 'exp'",
   plotOutput("gerexp"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra será gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gerexpdown", label = "Exportar")),
#### wei ####
conditionalPanel(condition = "input.dist == 'wei'",
   plotOutput("gerwei"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gerweidown", label = "Exportar")),
#### norm ####
conditionalPanel(condition = "input.dist == 'norm'",
   plotOutput("gernorm"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gernormdown", label = "Exportar")),
#### beta ####
conditionalPanel(condition = "input.dist == 'beta'",
   plotOutput("gerbeta"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gerbetadown", label = "Exportar")),
#### gama ####
conditionalPanel(condition = "input.dist == 'gama'",
   plotOutput("gergama"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gergamadown", label = "Exportar")),
#### t stu ####
conditionalPanel(condition = "input.dist == 't.stu'",
   plotOutput("gert"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gertdown", label = "Exportar")),
#### qq ####
conditionalPanel(condition = "input.dist == 'qq'",
   plotOutput("gerqq"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gerqqdown", label = "Exportar")),
#### cchy ####
conditionalPanel(condition = "input.dist == 'cchy'",
   plotOutput("gercchy"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gercchydown", label = "Exportar")),
#### f ####
conditionalPanel(condition = "input.dist == 'f'",
   plotOutput("gerf"),
   HTML("Atenção: O gráfico exportado não será o mesmo pois a amostra é gerada novamente"),
   HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
   downloadButton("gerfdown", label = "Exportar"))
                                            
                                            
                                            
)))))
################################################## server ####
server <- function(input, output, session) {
  #### alertas exp ####
  output$alertaexp <- renderText({
    lambda <-(length.out = input$lambda)
    if(lambda <= 0 || is.na(lambda) == TRUE){
      createAlert(session, "alertexp", "exampleAlert", title = "Atenção!",
                  content = HTML("O valor de &lambda; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "exampleAlert")
      return(" ")}
  })
  output$alertaexp2 <- renderText({
    lambda <-(length.out = input$lambda2)
    if(lambda <= 0 || is.na(lambda) == TRUE){
      createAlert(session, "alertexp2", "exampleAlert2", title = "Atenção!",
                  content = HTML("O valor de &lambda; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "exampleAlert2")
      return(" ")}
  })
  output$alertaexp3 <- renderText({
    lambda <-(length.out = input$lambda3)
    if(lambda <= 0 || is.na(lambda) == TRUE){
      createAlert(session, "alertexp3", "exampleAlert3", title = "Atenção!",
                  content = HTML("O valor de &lambda; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "exampleAlert3")
      return(" ")}
  })
  output$alertaexp4 <- renderText({
    lambda <-(length.out = input$lambda4)
    if(lambda <= 0 || is.na(lambda) == TRUE){
      createAlert(session, "alertexp4", "exampleAlert4", title = "Atenção!",
                  content = HTML("O valor de &lambda; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "exampleAlert4")
      return(" ")}
  })
  #### alertas weibull ####  
  output$alertaweialfa <- renderText({
    alfa <-(length.out = input$alfawei)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfawei", "Alertweia", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweia")}
  })
  output$alertaweibeta <- renderText({
    beta <-(length.out = input$betawei)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbetawei", "Alertweib", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweib")}
  })
  output$alertaweialfa2 <- renderText({
    alfa <-(length.out = input$alfawei2)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfawei2", "Alertweia2", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweia2")}
  })
  output$alertaweibeta2 <- renderText({
    beta <-(length.out = input$betawei2)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbetawei2", "Alertweib2", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweib2")}
  })
  output$alertaweialfa3 <- renderText({
    alfa <-(length.out = input$alfawei3)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfawei3", "Alertweia3", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweia3")}
  })
  output$alertaweibeta3 <- renderText({
    beta <-(length.out = input$betawei3)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbetawei3", "Alertweib3", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweib3")}
  })
  output$alertaweialfa4 <- renderText({
    alfa <-(length.out = input$alfawei4)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfawei4", "Alertweia4", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweia4")}
  })
  output$alertaweibeta4 <- renderText({
    beta <-(length.out = input$betawei4)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbetawei4", "Alertweib4", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertweib4")}
  })
  #### alertas normal ####
  output$alertamunorm <- renderText({
    mu <-(length.out = input$munorm)
    if(is.na(mu) == TRUE){
      createAlert(session, "alertnormmu", "Alertnmu", title = "Atenção!",
                  content = HTML("O valor de &mu; deve ser válido"), append = FALSE)
    }else{closeAlert(session, "Alertnmu")}
  })
  output$alertasdnorm <- renderText({
    sd <-(length.out = input$sdnorm)
    if(sd <= 0 || is.na(sd) == TRUE){
      createAlert(session, "alertnormsd", "Alertnsd", title = "Atenção!",
                  content = HTML("O valor de &sigma; deve ser válido e positivo"), append = FALSE)
    }else{closeAlert(session, "Alertnsd")}
  })
  output$alertamunorm2 <- renderText({
    mu <-(length.out = input$munorm2)
    if(is.na(mu) == TRUE){
      createAlert(session, "alertnormmu2", "Alertnmu2", title = "Atenção!",
                  content = HTML("O valor de &mu; deve ser válido"), append = FALSE)
    }else{closeAlert(session, "Alertnmu2")}
  })
  output$alertasdnorm2 <- renderText({
    sd <-(length.out = input$sdnorm2)
    if(sd <= 0 || is.na(sd) == TRUE){
      createAlert(session, "alertnormsd2", "Alertnsd2", title = "Atenção!",
                  content = HTML("O valor de &sigma; deve ser válido e positivo"), append = FALSE)
    }else{closeAlert(session, "Alertnsd2")}
  })
  output$alertamunorm3 <- renderText({
    mu <-(length.out = input$munorm3)
    if(is.na(mu) == TRUE){
      createAlert(session, "alertnormmu3", "Alertnmu3", title = "Atenção!",
                  content = HTML("O valor de &mu; deve ser válido"), append = FALSE)
    }else{closeAlert(session, "Alertnmu3")}
  })
  output$alertasdnorm3 <- renderText({
    sd <-(length.out = input$sdnorm3)
    if(sd <= 0 || is.na(sd) == TRUE){
      createAlert(session, "alertnormsd3", "Alertnsd3", title = "Atenção!",
                  content = HTML("O valor de &sigma; deve ser válido e positivo"), append = FALSE)
    }else{closeAlert(session, "Alertnsd3")}
  })
  output$alertamunorm4 <- renderText({
    mu <-(length.out = input$munorm4)
    if(is.na(mu) == TRUE){
      createAlert(session, "alertnormmu4", "Alertnmu4", title = "Atenção!",
                  content = HTML("O valor de &mu; deve ser válido"), append = FALSE)
    }else{closeAlert(session, "Alertnmu4")}
  })
  output$alertasdnorm4 <- renderText({
    sd <-(length.out = input$sdnorm4)
    if(sd <= 0 || is.na(sd) == TRUE){
      createAlert(session, "alertnormsd4", "Alertnsd4", title = "Atenção!",
                  content = HTML("O valor de &sigma; deve ser válido e positivo"), append = FALSE)
    }else{closeAlert(session, "Alertnsd4")}
  })
  output$alertaeixonorm <- renderText({
    xmin <-(length.out = input$xminnorm)
    xmax <-(length.out = input$xmaxnorm)
    if(xmin >= xmax){
      createAlert(session, "alertnormeixo", "Alertneixo", title = "Atenção!",
                  content = HTML("O valor mínimo para o eixo X deve ser menor que o máximo"), append = FALSE)
    }else{closeAlert(session, "Alertneixo")}
  })
  #### alertas beta ####
  output$alertabetaalfa <- renderText({
    alfa <-(length.out = input$alfabeta)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfabeta", "Alertbetaa", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetaa")}
  })
  output$alertbetabeta <- renderText({
    beta <-(length.out = input$betabeta)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbbeta", "Alertbetab", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetab")}
  })
  output$alertabetaalfa2 <- renderText({
    alfa <-(length.out = input$alfabeta2)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfabeta2", "Alertbetaa2", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetaa2")}
  })
  output$alertbetabeta2 <- renderText({
    beta <-(length.out = input$betabeta2)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbbeta2", "Alertbetab2", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetab2")}
  })
  output$alertabetaalfa3 <- renderText({
    alfa <-(length.out = input$alfabeta3)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfabeta3", "Alertbetaa3", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetaa3")}
  })
  output$alertbetabeta3 <- renderText({
    beta <-(length.out = input$betabeta3)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbbeta3", "Alertbetab3", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetab3")}
  })
  output$alertabetaalfa4 <- renderText({
    alfa <-(length.out = input$alfabeta4)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfabeta4", "Alertbetaa4", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetaa4")}
  })
  output$alertbetabeta4 <- renderText({
    beta <-(length.out = input$betabeta4)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbbeta4", "Alertbetab4", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbetab4")}
  })
  #### alertas gama ####
  output$alertagamaalfa <- renderText({
    alfa <-(length.out = input$alfagama)
    if(alfa <= 0 || is.na(alfa) == TRUE){
      createAlert(session, "alertalfagama", "Alertagama", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertagama")}
  })
  output$alertagamabeta <- renderText({
    beta <-(length.out = input$betagama)
    if(beta <= 0 || is.na(beta) == TRUE){
      createAlert(session, "alertbetagama", "Alertbgama", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido e maior que 0"), append = FALSE)
    }else{closeAlert(session, "Alertbgama")}
  })  
  
  #### alertas t ####
  output$alertadft <- renderText({
    df <-(length.out = input$df)
    if(is.integer(df) == FALSE || df <= 0 ){
      createAlert(session, "alerttdf", "Alertstudf", title = "Atenção!",
                  content = HTML("O valor de v deve ser positivo e inteiro"), append = FALSE)
    }else{closeAlert(session, "Alertstudf")}
  })
  #### alertas qq ####
  output$alertadfqq <- renderText({
    df <-(length.out = input$dfqq)
    if(df <= 0 || is.na(df) == TRUE){
      createAlert(session, "alertqqdf", "Alertqqdf", title = "Atenção!",
                  content = HTML("O valor de v deve ser válido e positivo"), append = FALSE)
    }else{closeAlert(session, "Alertqqdf")}
  })
  #### alertas cchy ####
  output$alertabetacchy <- renderText({
    mu <-(length.out = input$betacchy)
    if(is.na(mu) == TRUE){
      createAlert(session, "alertcchybeta", "Alertbcchy", title = "Atenção!",
                  content = HTML("O valor de &beta; deve ser válido"), append = FALSE)
    }else{closeAlert(session, "Alertbcchy")}
  })
  output$alertaalfacchy <- renderText({
    sd <-(length.out = input$alfacchy)
    if(sd <= 0 || is.na(sd) == TRUE){
      createAlert(session, "alertcchyalfa", "Alertacchy", title = "Atenção!",
                  content = HTML("O valor de &alpha; deve ser válido e positivo"), append = FALSE)
    }else{closeAlert(session, "Alertacchy")}
  })
  
  output$alertaeixocchy <- renderText({
    xmin <-(length.out = input$xmincchy)
    xmax <-(length.out = input$xmaxcchy)
    if(xmin >= xmax){
      createAlert(session, "alertcchyeixo", "Alertcceixo", title = "Atenção!",
                  content = HTML("O valor mínimo para o eixo X deve ser menor que o máximo"), append = FALSE)
    }else{closeAlert(session, "Alertcceixo")}
  })
  #### alertas f ####
  output$alertamf <- renderText({
    df <-(length.out = input$dffn1)
    if(is.integer(df) == FALSE || df <= 0 ){
      createAlert(session, "alertfdfm", "Alertfmdf", title = "Atenção!",
                  content = HTML("O valor de m deve ser positivo e inteiro"), append = FALSE)
    }else{closeAlert(session, "Alertfmdf")}
  })
  output$alertanf <- renderText({
    df <-(length.out = input$dffm1)
    if(is.integer(df) == FALSE || df <= 0 ){
      createAlert(session, "alertfdfn", "Alertfndf", title = "Atenção!",
                  content = HTML("O valor de n deve ser positivo e inteiro"), append = FALSE)
    }else{closeAlert(session, "Alertfndf")}
  })
  #### tabelas ####  
  output$tabexp <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large{f(x) = \\lambda e^{- \\lambda x}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large{F(X) = 1- e^{- \\lambda x}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large{S(X) = e^{- \\lambda x}} \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large{E(X) = \\frac{1}{\\lambda}} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large{Var(X) = \\frac{1}{\\lambda^2}} \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabwei <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large{f(x) = \\beta \\alpha  * \\alpha x^{\\beta -1} e^{-\\alpha x^{\\beta}}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large{F(X)= 1 - e^{-\\alpha x^{\\beta}}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large{S(X) = e^{-\\alpha x^{\\beta}}} \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large{E(X) = \\frac{1}{\\alpha}\\Gamma(1+\\frac{1}{\\beta})} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large{Var(X) = \\frac{1}{\\alpha^2} \\Gamma(1+\\frac{2}{\\beta}) - (\\Gamma(1 + \\frac{1}{\\beta}))^2}   \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabnorm <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large{f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}}e^\\frac{-(x-\\mu)^2}{2\\sigma^2}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large{F(X)= \\frac{1}{2} [1 + erf(x/\\sqrt{2})]} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large{S(X) = 1 - \\frac{1}{2} [1 + erf(x/\\sqrt{2})]} \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large{E(X) = \\mu} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large{Var(X) = \\sigma^2}   \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabbeta <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large{f(x) = \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}
           x^{\\alpha - 1 }(1-x)^{\\beta - 1}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large{F(X)= \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} \\int_{0}^{x}t^{\\alpha - 1}(1 - t)^{\\beta-1}dt} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large{S(X) = 1 - \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} \\int_{0}^{x}t^{\\alpha - 1}(1 - t)^{\\beta-1}dt} \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large{E(X) = \\frac{\\alpha}{\\alpha + \\beta}} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large{Var(X) = \\frac{\\alpha \\beta}{(\\alpha + \\beta)^2 (\\alpha + \\beta + 1)}}   \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabgama <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large{f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)}x^{\\alpha -1}e^{-\\beta x}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large{F(X)= \\frac{\\gamma(\\alpha,\\beta x)}{\\Gamma(\\alpha)}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large{S(X) = 1 - \\frac{\\gamma(\\alpha,\\beta x)}{\\Gamma(\\alpha)}} \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large{E(X) = \\frac{\\beta}{\\alpha}} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large{Var(X) = \\frac{\\beta}{\\alpha^2}}   \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabt <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large{f(x) = \\frac{\\Gamma((v+1)/2)}{\\Gamma(v/2)\\sqrt{v\\pi}}
           (1+ \\frac{x^2}{v})^{- \\frac{v+1}{2}}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large{F(X)= \\frac{1}{2} + x \\Gamma(\\frac{v+1}{2})* 
           \\frac{_2F_1(\\frac{1}{2},\\frac{v+1}{2},\\frac{3}{2},-\\frac{x^{2}}{v})}{\\sqrt{\\pi v}\\Gamma(v/2)}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large{S(X) = \\frac{1}{2} - x \\Gamma(\\frac{v+1}{2})* 
           \\frac{_2F_1(\\frac{1}{2},\\frac{v+1}{2},\\frac{3}{2},-\\frac{x^{2}}{v})}{\\sqrt{\\pi v}\\Gamma(v/2)}} \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large{E(X) =}  \\begin{cases}
           \\mbox{0, se v > 1} \\\\
           \\mbox{Indefinida, se v = 0}
           \\end{cases} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large{Var(X) =} \\begin{cases}
           \\mbox{Indefinida, se v = 1} \\\\
           \\mbox{Infinita, se v = 2} \\\\
           \\frac{v}{v-2} \\mbox{ se v >2}
           \\end{cases}   \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabqq <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large f(x) = \\frac{2^{-v/2}}{\\Gamma(v/2)}x^{(v/2)-1}e^{-x/2} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large F(X)= \\frac{\\gamma(\\frac{v}{2},\\frac{x}{2})}{\\Gamma(\\frac{v}{2})} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large S(X) = 1 - \\frac{\\gamma(\\frac{v}{2},\\frac{x}{2})}{\\Gamma(\\frac{v}{2})} \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large E(X) =  v \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large Var(X) = 2v   \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabcchy <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large f(x) = \\frac{1}{\\pi\\alpha[1+(\\frac{x-\\beta}{\\alpha})^2]} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large F(X)= \\frac{1}{2} + \\frac{1}{\\pi} arctan(\\frac{x - \\beta}{\\alpha}) \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large S(X) = \\frac{1}{2} - \\frac{1}{\\pi} arctan(\\frac{x - \\beta}{\\alpha}) \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large E(X) =  \\mbox{Indefinida} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large Var(X) = \\mbox{Indefinida}   \\)</td>
           </tr>
           </table>
           </center>       "))})
  output$tabf <- renderUI({
    withMathJax(  
      HTML("<center>
           <table>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Densidade de Probabilidade: &nbsp;&nbsp;&nbsp; </td>
           <td> \\( \\large f(x) = \\frac{\\Gamma(\\frac{m+n}{2})(\\frac{m}{n})^{m/2} x^{\\frac{m}{2}-1}}
           {\\Gamma(\\frac{m}{2})\\Gamma(\\frac{n}{2})[(\\frac{m}{n})x+1]^{\\frac{m+n}{2}}} \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função Distribuição Acumulada:</td>
           <td> \\( \\large F(X)= I_{\\frac{m x}{m x + n}}(\\frac{m}{2},\\frac{n}{2}) \\) </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Função de Sobrevivência:</td>
           <td> \\( \\large S(X) = 1 - I_{\\frac{m x}{m x + n}}(\\frac{m}{2},\\frac{n}{2}) \\)  </td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Esperança:</td>
           <td>\\( \\large E(X) =  \\begin{cases}
           \\mbox{Indefinida se n < 2} \\\\
           \\frac{n}{n-2} \\mbox{, se } n \\geq 2
           \\end{cases} \\)</td>
           </tr>
           <tr>
           <td> &nbsp;&nbsp;&nbsp; </td>
           <td> &nbsp;&nbsp;&nbsp; </td>
           </tr>
           <tr>
           <td>Variância:</td>
           <td>\\( \\large Var(X) = \\begin{cases}
           \\mbox{Indefinida se n < 4} \\\\
           \\frac{2n^2(m + n -2)}{m(n-2)^2(m - 4)} \\mbox{, se } n \\geq 2
           \\end{cases}  \\)</td>
           </tr>
           </table>
           </center>       "))})
  
  #### media variancia acumulada wei ###############
  output$mvwei <- renderText({c("Média: ", round((1/input$alfawei)*gamma(1+(1/input$betawei)), 2),
                                c("Variância: ", 
                                  round((1/input$alfawei)^2 * (gamma(1+(2/input$betawei)) - gamma(1+(1/input$betawei))^2) ,4)))})
  output$acumuwei <- renderText({c("Probabilidade Acumulada: ", 
                                   round(pweibull(input$pfimwei, scale = (1/(input$alfawei)), shape = input$betawei)- 
                                           pweibull(input$piniwei, scale = (1/(input$alfawei)), shape = input$betawei),4))})
  #### media variancia acumulada exp ###############
  output$mvexp <- renderText({c("Média: ", round(1/input$lambda, 2),
                                c("Variância: ", round(1/(input$lambda)^2 ,4)))})
  output$acuexp <- renderText({c("Probabilidade Acumulada: ", 
                                 round(pexp(input$pfim,rate = input$lambda)-
                                         pexp(input$pini,rate = input$lambda),4))})
  #### media variancia norm ####
  output$mvnorm <- renderText({c("Média: ", round(input$munorm, 2),
                                 c("Variância: ", round(input$sdnorm^2 ,4)))})
  output$acunorm <- renderText({c("Probabilidade Acumulada: ", 
                                  round(pnorm(input$pfimnorm,mean = input$munorm, sd = input$sdnorm)-
                                          pnorm(input$pininorm,mean = input$munorm, sd = input$sdnorm),4))})
  
  #### media variancia beta ####
  output$mvbeta <- renderText({c("Média: ", round(input$alfabeta/(input$alfabeta + input$betabeta), 2),
                                 c("Variância: ", round((input$alfabeta * input$betabeta)/
                                                          ((input$alfabeta + input$betabeta)^2 * (input$alfabeta+input$betabeta+1)) ,4)))})
  output$acubeta <- renderText({c("Probabilidade Acumulada: ", 
                                  round(pbeta(input$pfimbeta, shape1 = input$alfabeta, shape2 = input$betabeta)-
                                          pbeta(input$pinibeta, shape1 = input$alfabeta, shape2 = input$betabeta),4))})
  #### media variancia gama ####
  output$mvgama <- renderText({c("Média: ", round(input$betagama/input$alfagama, 2),
                                 c("Variância: ", round(input$betagama/input$alfagama^2, 4)))})
  output$acugama <- renderText({c("Probabilidade Acumulada: ", 
                                  round(pgamma(input$pfimgama, scale = (1/input$alfagama), shape = input$betagama)-
                                          pgamma(input$pinigama, scale = (1/input$alfagama), shape = input$betagama), 4))})
  #### media variancia t.tsu ####
  output$mvt <- renderText({c("Média: ", 
                              if(input$df > 1){"0"},
                              if(input$df == 1){"Indefinida"},
                              c("Variância: ",
                                if(input$df > 2){round(input$df/(input$df-2),4)},
                                if(input$df <= 2 & input$df > 1){"Infinito"},
                                if(input$df == 1){"Indefinida"}))})
  
  
  output$acut <- renderText({c("Probabilidade Acumulada: ", 
                               round(pt(input$pfimt, df = input$df)-
                                       pt(input$pinit, df = input$df), 4))})
  
  #### media variancia qq ####
  output$mvqq <- renderText({c("Média: ", round(input$dfqq, 2),
                               c("Variância: ", round(2* input$dfqq, 4)))})
  output$acuqq <- renderText({c("Probabilidade Acumulada: ", 
                                round(pchisq(input$pfimqq, df = input$dfqq)-
                                        pchisq(input$piniqq, df = input$dfqq), 4))})
  #### media variancia cchy ####
  output$mvcchy <- renderText({c("Média: ", "Indefinida",
                                 c("Variância: ", "Indefinida"))})
  
  
  output$acucchy <- renderText({c("Probabilidade Acumulada: ", 
                                  round(pcauchy(input$pfimcchy,location = input$betacchy, scale = input$alfacchy)-
                                          pcauchy(input$pinicchy,location = input$betacchy, scale = input$alfacchy), 4))})
  #### media variancia F ####
  output$mvf <- renderText({c("Média: ", 
                              if(input$dffm1 < 2){"Indefinida"},
                              if(input$dffm1 >= 2){round(input$dffm1/(input$dffm1-2),2)},
                              c("Variância: ",
                                if(input$dffm1 < 4){"Indefinida"},
                                if(input$dffm1 >= 4){round((2*input$dffm1^2*(input$dffn1+input$dffm1-2))/
                                                             (input$dffn1*(input$dffm1-2)^2*(input$dffn1-4)),4)})
  )})
  
  
  output$acuf <- renderText({c("Probabilidade Acumulada: ", 
                               round(pf(input$pfimf, df1 = input$dffn1, df2 = input$dffm1)-
                                       pf(input$pinif, df1 = input$dffn1, df2 = input$dffm1), 4))})
  #### densidade exp##############
  output$denexp <- renderPlot(width = 550, height = 320,{
    lambda  <- (length.out = input$lambda)
    lambda2 <- (length.out = input$lambda2)
    lambda3 <- (length.out = input$lambda3)
    lambda4 <- (length.out = input$lambda4)
    xmax <-(length.out = input$xmaxexp)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = dexp,args = list(rate = lambda), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(lambda2 > 0 ){
      base <- (base +
                 stat_function(fun = dexp,args = list(rate = lambda2), geom = "line",size=1,lty = 2, colour = "red"))
      print(base)}
    
    if(lambda3 > 0 ){
      base <- (base +
                 stat_function(fun = dexp,args = list(rate = lambda3), geom = "line",size=1,lty = 4, colour = "blue"))
      print(base)}
    
    if(lambda4 > 0 ){
      base <- (base +
                 stat_function(fun = dexp,args = list(rate = lambda4), geom = "line",size=1,lty = 6, colour = "green"))
      print(base)}})
  #### download densidade exp ######################
  output$denexpdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      lambda  <- (length.out = input$lambda)
      lambda2 <- (length.out = input$lambda2)
      lambda3 <- (length.out = input$lambda3)
      lambda4 <- (length.out = input$lambda4)
      xmax <-(length.out = input$xmaxexp)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = dexp,args = list(rate = lambda), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(lambda2 > 0 ){
        base <- (base +
                   stat_function(fun = dexp,args = list(rate = lambda2), geom = "line",size=1,
                                 lty = 2, color = "red"))}
      
      if(lambda3 > 0 ){
        base <- (base +
                   stat_function(fun = dexp,args = list(rate = lambda3), geom = "line",size=1,
                                 lty = 4, color = "blue"))}
      
      if(lambda4 > 0 ){
        base <- (base +
                   stat_function(fun = dexp,args = list(rate = lambda4), geom = "line",size=1,
                                 lty = 6, color = "green"))}
      print(base)
      dev.off()})  
  #### acumulada exponencial ################
  output$acumuexp <-  renderPlot(width = 550, height = 320,{
    lambda  <- (length.out = input$lambda)
    lambda2 <- (length.out = input$lambda2)
    lambda3 <- (length.out = input$lambda3)
    lambda4 <- (length.out = input$lambda4)
    xmax    <- (length.out = input$xmaxexp)
    
    
    base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = pexp,args = list(rate = lambda), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 xlab("Quantil da distribuição")+   
                 theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                 theme_bw())
    print(base)
    if(lambda2 > 0 ){
      base <- (base +
                 stat_function(fun = pexp,args = list(rate = lambda2), geom = "line",size=1, lty=2, colour= "red"))
      print(base)}
    
    if(lambda3 > 0 ){
      base <- (base +
                 stat_function(fun = pexp,args = list(rate = lambda3), geom = "line",size=1, lty=4, colour= "blue"))
      print(base)}
    
    if(lambda4 > 0 ){
      base <- (base +
                 stat_function(fun = pexp,args = list(rate = lambda4), geom = "line",size=1, lty=6, colour= "green"))
      print(base)}})
  
  #### download acumulada exp ###################
  
  output$acumuexpdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      lambda  <- (length.out = input$lambda)
      lambda2 <- (length.out = input$lambda2)
      lambda3 <- (length.out = input$lambda3)
      lambda4 <- (length.out = input$lambda4)
      xmax    <- (length.out = input$xmaxexp)
      
      
      base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                   stat_function(fun = pexp,args = list(rate = lambda), geom = "line",size=1)+
                   ylab("Função Distribuição Acumulada")+
                   xlab("Quantil da distribuição")+   
                   theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                   theme_bw())
      if(lambda2 > 0 ){
        base <- (base +
                   stat_function(fun = pexp,args = list(rate = lambda2), geom = "line",size=1, lty=2, colour= "red"))}
      
      if(lambda3 > 0 ){
        base <- (base +
                   stat_function(fun = pexp,args = list(rate = lambda3), geom = "line",size=1, lty=4, colour= "blue"))}
      
      if(lambda4 > 0 ){
        base <- (base +
                   stat_function(fun = pexp,args = list(rate = lambda4), geom = "line",size=1, lty=6, colour= "green"))}
      print(base)
      dev.off()
    })
  #### sobrevivencia exp #################### 
  
  output$survexp <- renderPlot(width = 550, height = 320,{
    lambda  <- (length.out = input$lambda)
    lambda2 <- (length.out = input$lambda2)
    lambda3 <- (length.out = input$lambda3)
    lambda4 <- (length.out = input$lambda4)
    xmax    <- (length.out = input$xmaxexp)
    
    
    base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun =function (x){ pexp(x,lambda,lower.tail=FALSE)}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+   
                 theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                 theme_bw())
    print(base)
    if(lambda2 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pexp(x,lambda2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      print(base)}
    
    if(lambda3 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pexp(x,lambda3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      print(base)}
    
    if(lambda4 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pexp(x,lambda4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      print(base)}})
  
  #### sobrevivencia exp download ###################
  
  output$survexpdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      lambda  <- (length.out = input$lambda)
      lambda2 <- (length.out = input$lambda2)
      lambda3 <- (length.out = input$lambda3)
      lambda4 <- (length.out = input$lambda4)
      xmax    <- (length.out = input$xmaxexp)
      
      
      base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                   stat_function(fun = function (x){ pexp(x,lambda,lower.tail=FALSE)}, geom = "line",size=1)+
                   ylab("Função de Sobrevivência")+
                   xlab("Quantil da distribuição")+   
                   theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                   theme_bw())
      if(lambda2 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pexp(x,lambda2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))}
      
      if(lambda3 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pexp(x,lambda3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))}
      
      if(lambda4 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pexp(x,lambda4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))}
      print(base)
      dev.off()
    })
  
  #### gerador amostra exp #################### 
  output$gerexp <- renderPlot(width = 600, height = 400,{
    lambda <-(length.out = input$lambda)
    nexp <-(length.out = input$nexp)
    hist(rexp(nexp,rate=lambda),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dexp(x,rate=lambda),add = TRUE,lwd=2,col="blue")
    
  })
  
  #### gerador amostra exp download #########  
  output$gerexpdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      lambda <-(length.out = input$lambda)
      nexp <-(length.out = input$nexp)
      hist(rexp(nexp,rate=lambda),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dexp(x,rate=lambda),add = TRUE,lwd=2,col="blue")
      dev.off()
    })
  
  
  #### densidade weibull ############
  output$denwei <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfawei)
    beta <- (length.out = input$betawei)
    alfa2 <- (length.out = input$alfawei2)
    beta2 <- (length.out = input$betawei2)
    alfa3 <- (length.out = input$alfawei3)
    beta3 <- (length.out = input$betawei3)
    alfa4 <- (length.out = input$alfawei4)
    beta4 <- (length.out = input$betawei4)
    xmax <-(length.out = input$xmaxwei)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = dweibull,args = list(scale = (1/alfa), shape = beta), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun = dweibull,args = list(scale = (1/alfa2), shape = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun = dweibull,args = list(scale = (1/alfa3), shape = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun = dweibull,args = list(scale = (1/alfa4), shape = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })
  #### doenload densidade weibull ####
  output$denweidown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfawei)
      beta <- (length.out = input$betawei)
      alfa2 <- (length.out = input$alfawei2)
      beta2 <- (length.out = input$betawei2)
      alfa3 <- (length.out = input$alfawei3)
      beta3 <- (length.out = input$betawei3)
      alfa4 <- (length.out = input$alfawei4)
      beta4 <- (length.out = input$betawei4)
      xmax <-(length.out = input$xmaxwei)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = dweibull,args = list(scale = (1/alfa), shape = beta), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun = dweibull,args = list(scale = (1/alfa2), shape = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun = dweibull,args = list(scale = (1/alfa3), shape = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))
      }
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun = dweibull,args = list(scale = (1/alfa4), shape = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))
      }
      print(base)
      dev.off()})
  #### acumulada weibull ####
  output$acuwei <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfawei)
    beta <- (length.out = input$betawei)
    alfa2 <- (length.out = input$alfawei2)
    beta2 <- (length.out = input$betawei2)
    alfa3 <- (length.out = input$alfawei3)
    beta3 <- (length.out = input$betawei3)
    alfa4 <- (length.out = input$alfawei4)
    beta4 <- (length.out = input$betawei4)
    xmax <-(length.out = input$xmaxwei)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = pweibull,args = list(scale = (1/alfa), shape = beta), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               xlab("Quantil da distribuição")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun = pweibull,args = list(scale = (1/alfa2), shape = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun = pweibull,args = list(scale = (1/alfa3), shape = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun = pweibull,args = list(scale = (1/alfa4), shape = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })
  #### download acu weibull ####
  output$acuweidown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfawei)
      beta <- (length.out = input$betawei)
      alfa2 <- (length.out = input$alfawei2)
      beta2 <- (length.out = input$betawei2)
      alfa3 <- (length.out = input$alfawei3)
      beta3 <- (length.out = input$betawei3)
      alfa4 <- (length.out = input$alfawei4)
      beta4 <- (length.out = input$betawei4)
      xmax <-(length.out = input$xmaxwei)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = pweibull,args = list(scale = (1/alfa), shape = beta), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 xlab("Quantil da distribuição")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun = pweibull,args = list(scale = (1/alfa2), shape = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun = pweibull,args = list(scale = (1/alfa3), shape = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))
      }
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun = pweibull,args = list(scale = (1/alfa4), shape = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))
      }
      print(base)
      dev.off()})
  #### sobrevivencia weibull ####
  output$survwei <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfawei)
    beta <- (length.out = input$betawei)
    alfa2 <- (length.out = input$alfawei2)
    beta2 <- (length.out = input$betawei2)
    alfa3 <- (length.out = input$alfawei3)
    beta3 <- (length.out = input$betawei3)
    alfa4 <- (length.out = input$alfawei4)
    beta4 <- (length.out = input$betawei4)
    xmax <-(length.out = input$xmaxwei)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun =function (x) {pweibull(x,scale = (1/alfa), shape = beta, lower.tail = FALSE )}, geom = "line",size=1)+
               ylab("Função de Sobrevivência")+
               xlab("Quantil da distribuição")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pweibull(x,scale = (1/alfa2), shape = beta2, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pweibull(x,scale = (1/alfa3), shape = beta3, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "blue", lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pweibull(x,scale = (1/alfa4), shape = beta4, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "green", lty=6))
      print(base)}
  })  
  
  
  #### download sob weibull ####
  output$survweidown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfawei)
      beta <- (length.out = input$betawei)
      alfa2 <- (length.out = input$alfawei2)
      beta2 <- (length.out = input$betawei2)
      alfa3 <- (length.out = input$alfawei3)
      beta3 <- (length.out = input$betawei3)
      alfa4 <- (length.out = input$alfawei4)
      beta4 <- (length.out = input$betawei4)
      xmax <-(length.out = input$xmaxwei)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun =function (x) {pweibull(x,scale = (1/alfa), shape = beta, lower.tail = FALSE )}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pweibull(x,scale = (1/alfa2), shape = beta2, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "red", lty=2))
      }
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pweibull(x,scale = (1/alfa3), shape = beta3, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "blue", lty=4))
      }
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pweibull(x,scale = (1/alfa4), shape = beta4, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "green", lty=6))
      }
      print(base)
      dev.off()})
  #### gerador weibull ####
  output$gerwei <- renderPlot(width = 600, height = 400,{
    alfa <- (length.out = input$alfawei)
    beta <- (length.out = input$betawei)
    nwei <-(length.out = input$nwei)
    hist(rweibull(nwei,scale = (1/alfa), shape = beta),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dweibull(x,scale = (1/alfa), shape = beta),add = TRUE,lwd=2,col="blue")
    
  })  
  #### gerador weibull download ####
  output$gerweidown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfawei)
      beta <- (length.out = input$betawei)
      nwei <-(length.out = input$nwei)
      hist(rweibull(nwei,scale = (1/alfa), shape = beta),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dweibull(x,scale = (1/alfa), shape = beta),add = TRUE,lwd=2,col="blue")
      dev.off()
    })
  #### densidade normal ####
  output$dennorm <- renderPlot(width = 550, height = 320,{
    mu   <- (length.out = input$munorm)
    sd   <- (length.out = input$sdnorm)
    mu2  <- (length.out = input$munorm2)
    sd2  <- (length.out = input$sdnorm2)
    mu3  <- (length.out = input$munorm3)
    sd3  <- (length.out = input$sdnorm3)
    mu4  <- (length.out = input$munorm4)
    sd4  <- (length.out = input$sdnorm4)
    xmin <- (length.out = input$xminnorm)
    xmax <- (length.out = input$xmaxnorm)
    base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
               stat_function(fun = dnorm,args = list(mean = mu, sd = sd), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(sd2 > 0){
      base <- (base +
                 stat_function(fun = dnorm,args = list(mean = mu2, sd = sd2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(sd3 > 0){
      base <- (base +
                 stat_function(fun = dnorm,args = list(mean = mu3, sd = sd3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(sd4 > 0){
      base <- (base +
                 stat_function(fun = dnorm,args = list(mean = mu4, sd = sd4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })
  #### down densidade normal ####
  output$dennormdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      mu   <- (length.out = input$munorm)
      sd   <- (length.out = input$sdnorm)
      mu2  <- (length.out = input$munorm2)
      sd2  <- (length.out = input$sdnorm2)
      mu3  <- (length.out = input$munorm3)
      sd3  <- (length.out = input$sdnorm3)
      mu4  <- (length.out = input$munorm4)
      sd4  <- (length.out = input$sdnorm4)
      xmin <- (length.out = input$xminnorm)
      xmax <- (length.out = input$xmaxnorm)
      base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun = dnorm,args = list(mean = mu, sd = sd), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(sd2 > 0){
        base <- (base +
                   stat_function(fun = dnorm,args = list(mean = mu2, sd = sd2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(sd3 > 0){
        base <- (base +
                   stat_function(fun = dnorm,args = list(mean = mu3, sd = sd3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(sd4 > 0){
        base <- (base +
                   stat_function(fun = dnorm,args = list(mean = mu4, sd = sd4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()})  
  #### acumulada normal ####
  output$acumunorm <- renderPlot(width = 550, height = 320,{
    mu   <- (length.out = input$munorm)
    sd   <- (length.out = input$sdnorm)
    mu2  <- (length.out = input$munorm2)
    sd2  <- (length.out = input$sdnorm2)
    mu3  <- (length.out = input$munorm3)
    sd3  <- (length.out = input$sdnorm3)
    mu4  <- (length.out = input$munorm4)
    sd4  <- (length.out = input$sdnorm4)
    xmin <- (length.out = input$xminnorm)
    xmax <- (length.out = input$xmaxnorm)
    base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
               stat_function(fun = pnorm,args = list(mean = mu, sd = sd), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(sd2 > 0){
      base <- (base +
                 stat_function(fun = pnorm,args = list(mean = mu2, sd = sd2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(sd3 > 0){
      base <- (base +
                 stat_function(fun = pnorm,args = list(mean = mu3, sd = sd3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(sd4 > 0){
      base <- (base +
                 stat_function(fun = pnorm,args = list(mean = mu4, sd = sd4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })  
  #### acumulada norm download ####
  output$acumunormdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      mu   <- (length.out = input$munorm)
      sd   <- (length.out = input$sdnorm)
      mu2  <- (length.out = input$munorm2)
      sd2  <- (length.out = input$sdnorm2)
      mu3  <- (length.out = input$munorm3)
      sd3  <- (length.out = input$sdnorm3)
      mu4  <- (length.out = input$munorm4)
      sd4  <- (length.out = input$sdnorm4)
      xmin <- (length.out = input$xminnorm)
      xmax <- (length.out = input$xmaxnorm)
      base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun = pnorm,args = list(mean = mu, sd = sd), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(sd2 > 0){
        base <- (base +
                   stat_function(fun = pnorm,args = list(mean = mu2, sd = sd2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(sd3 > 0){
        base <- (base +
                   stat_function(fun = pnorm,args = list(mean = mu3, sd = sd3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(sd4 > 0){
        base <- (base +
                   stat_function(fun = pnorm,args = list(mean = mu4, sd = sd4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()})  
  #### normal sobrevivencia ####
  output$survnorm <- renderPlot(width = 550, height = 320,{
    mu   <- (length.out = input$munorm)
    sd   <- (length.out = input$sdnorm)
    mu2  <- (length.out = input$munorm2)
    sd2  <- (length.out = input$sdnorm2)
    mu3  <- (length.out = input$munorm3)
    sd3  <- (length.out = input$sdnorm3)
    mu4  <- (length.out = input$munorm4)
    sd4  <- (length.out = input$sdnorm4)
    xmin <- (length.out = input$xminnorm)
    xmax <- (length.out = input$xmaxnorm)
    base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
               stat_function(fun =function (x) {pnorm(x,mean = mu,sd = sd, lower.tail = FALSE )}, geom = "line",size=1)+
               ylab("Função de Sobrevivência")+
               xlab("Quantil da distribuição")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(sd2 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pnorm(x,mean = mu2,sd = sd2, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(sd3 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pnorm(x,mean = mu3,sd = sd3, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "blue", lty=4))
      print(base)}
    if(sd4 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pnorm(x,mean = mu4,sd = sd4, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "green", lty=6))
      print(base)}
  })
  #### surv normal download ####
  output$survnormdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      mu   <- (length.out = input$munorm)
      sd   <- (length.out = input$sdnorm)
      mu2  <- (length.out = input$munorm2)
      sd2  <- (length.out = input$sdnorm2)
      mu3  <- (length.out = input$munorm3)
      sd3  <- (length.out = input$sdnorm3)
      mu4  <- (length.out = input$munorm4)
      sd4  <- (length.out = input$sdnorm4)
      xmin <- (length.out = input$xminnorm)
      xmax <- (length.out = input$xmaxnorm)
      base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun =function (x) {pnorm(x,mean = mu,sd = sd, lower.tail = FALSE )}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(sd2 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pnorm(x,mean = mu2,sd = sd2, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "red", lty=2))
      }
      if(sd3 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pnorm(x,mean = mu3,sd = sd3, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "blue", lty=4))
      }
      if(sd4 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pnorm(x,mean = mu4,sd = sd4, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "green", lty=6))
      }
      print(base)
      dev.off()})  
  #### gerador norm ####
  output$gernorm <- renderPlot(width = 600, height = 400,{
    mu <- (length.out = input$munorm)
    sd <- (length.out = input$sdnorm)
    nnorm <-(length.out = input$nnorm)
    hist(rnorm(nnorm,mean = mu, sd = sd),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dnorm(x,mean = mu, sd = sd),add = TRUE,lwd=2,col="blue")
    
  })  
  #### gerador norm download ####
  output$gernormdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      mu <- (length.out = input$munorm)
      sd <- (length.out = input$sdnorm)
      nnorm <-(length.out = input$nnorm)
      hist(rnorm(nnorm,mean = mu, sd = sd),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dnorm(x,mean = mu, sd = sd),add = TRUE,lwd=2,col="blue")
      dev.off()
    })  
  #### densidade beta ####
  output$denbeta <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfabeta)
    beta <- (length.out = input$betabeta)
    alfa2 <- (length.out = input$alfabeta2)
    beta2 <- (length.out = input$betabeta2)
    alfa3 <- (length.out = input$alfabeta3)
    beta3 <- (length.out = input$betabeta3)
    alfa4 <- (length.out = input$alfabeta4)
    beta4 <- (length.out = input$betabeta4)
    xmax <-(length.out = input$xmaxbeta)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = dbeta,args = list(shape1 = alfa, shape2 = beta), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun = dbeta,args = list(shape1 = alfa2, shape2 = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun = dbeta,args = list(shape1 = alfa3, shape2 = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun = dbeta,args = list(shape1 = alfa4, shape2 = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })
  #### denbeta dowload ####
  output$denbetadown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfabeta)
      beta <- (length.out = input$betabeta)
      alfa2 <- (length.out = input$alfabeta2)
      beta2 <- (length.out = input$betabeta2)
      alfa3 <- (length.out = input$alfabeta3)
      beta3 <- (length.out = input$betabeta3)
      alfa4 <- (length.out = input$alfabeta4)
      beta4 <- (length.out = input$betabeta4)
      xmax <-(length.out = input$xmaxbeta)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = dbeta,args = list(shape1 = alfa, shape2 = beta), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun = dbeta,args = list(shape1 = alfa2, shape2 = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun = dbeta,args = list(shape1 = alfa3, shape2 = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun = dbeta,args = list(shape1 = alfa4, shape2 = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()})  
  #### acumulada beta ####
  output$acumubeta <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfabeta)
    beta <- (length.out = input$betabeta)
    alfa2 <- (length.out = input$alfabeta2)
    beta2 <- (length.out = input$betabeta2)
    alfa3 <- (length.out = input$alfabeta3)
    beta3 <- (length.out = input$betabeta3)
    alfa4 <- (length.out = input$alfabeta4)
    beta4 <- (length.out = input$betabeta4)
    xmax <-(length.out = input$xmaxbeta)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = pbeta,args = list(shape1 = alfa, shape2 = beta), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun = pbeta,args = list(shape1 = alfa2, shape2 = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun = pbeta,args = list(shape1 = alfa3, shape2 = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun = pbeta,args = list(shape1 = alfa4, shape2 = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })  
  #### acumu beta down ####
  output$acumubetadown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfabeta)
      beta <- (length.out = input$betabeta)
      alfa2 <- (length.out = input$alfabeta2)
      beta2 <- (length.out = input$betabeta2)
      alfa3 <- (length.out = input$alfabeta3)
      beta3 <- (length.out = input$betabeta3)
      alfa4 <- (length.out = input$alfabeta4)
      beta4 <- (length.out = input$betabeta4)
      xmax <-(length.out = input$xmaxbeta)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = pbeta,args = list(shape1 = alfa, shape2 = beta), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun = pbeta,args = list(shape1 = alfa2, shape2 = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun = pbeta,args = list(shape1 = alfa3, shape2 = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun = pbeta,args = list(shape1 = alfa4, shape2 = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()})    
  #### surv beta ####
  output$survbeta <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfabeta)
    beta <- (length.out = input$betabeta)
    alfa2 <- (length.out = input$alfabeta2)
    beta2 <- (length.out = input$betabeta2)
    alfa3 <- (length.out = input$alfabeta3)
    beta3 <- (length.out = input$betabeta3)
    alfa4 <- (length.out = input$alfabeta4)
    beta4 <- (length.out = input$betabeta4)
    xmax <-(length.out = input$xmaxbeta)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun =function (x) {pbeta(x,shape1 = alfa, shape2 = beta, lower.tail = FALSE )}, geom = "line",size=1)+
               ylab("Função de Sobrevivência")+
               xlab("Quantil da distribuição")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pbeta(x,shape1 = alfa2, shape2 = beta2, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pbeta(x,shape1 = alfa3, shape2 = beta3, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "blue", lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pbeta(x,shape1 = alfa4, shape2 = beta4, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "green", lty=6))
      print(base)}
  })    
  
  #### surv beta down ####
  output$survbetadown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfabeta)
      beta <- (length.out = input$betabeta)
      alfa2 <- (length.out = input$alfabeta2)
      beta2 <- (length.out = input$betabeta2)
      alfa3 <- (length.out = input$alfabeta3)
      beta3 <- (length.out = input$betabeta3)
      alfa4 <- (length.out = input$alfabeta4)
      beta4 <- (length.out = input$betabeta4)
      xmax <-(length.out = input$xmaxbeta)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun =function (x) {pbeta(x,shape1 = alfa, shape2 = beta, lower.tail = FALSE )}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pbeta(x,shape1 = alfa2, shape2 = beta2, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pbeta(x,shape1 = alfa3, shape2 = beta3, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "blue", lty=4))}
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pbeta(x,shape1 = alfa4, shape2 = beta4, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "green", lty=6))}
      print(base)
      dev.off()})  
  #### gerador beta ####
  output$gerbeta <- renderPlot(width = 600, height = 400,{
    alfa <- (length.out = input$alfabeta)
    beta <- (length.out = input$betabeta)
    nbeta <-(length.out = input$nbeta)
    hist(rbeta(nbeta,shape1 = alfa, shape2 = beta),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dbeta(x,shape1 = alfa, shape2 = beta),add = TRUE,lwd=2,col="blue")
    
  })  
  #### gerador beta down ####
  output$gerbetadown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfabeta)
      beta <- (length.out = input$betabeta)
      nbeta <-(length.out = input$nbeta)
      hist(rbeta(nbeta,shape1 = alfa, shape2 = beta),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dbeta(x,shape1 = alfa, shape2 = beta),add = TRUE,lwd=2,col="blue")
      dev.off()
    })  
  #### densidade gama ####
  output$dengama <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfagama)
    beta <- (length.out = input$betagama)
    alfa2 <- (length.out = input$alfagama2)
    beta2 <- (length.out = input$betagama2)
    alfa3 <- (length.out = input$alfagama3)
    beta3 <- (length.out = input$betagama3)
    alfa4 <- (length.out = input$alfagama4)
    beta4 <- (length.out = input$betagama4)
    xmax <-(length.out = input$xmaxgama)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = dgamma,args = list(scale = 1/alfa, shape = beta), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun = dgamma,args = list(scale = 1/alfa2, shape = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun = dgamma,args = list(scale = 1/alfa3, shape = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun = dgamma,args = list(scale = 1/alfa4, shape = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })
  #### den gama down ####
  output$dengamadown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfagama)
      beta <- (length.out = input$betagama)
      alfa2 <- (length.out = input$alfagama2)
      beta2 <- (length.out = input$betagama2)
      alfa3 <- (length.out = input$alfagama3)
      beta3 <- (length.out = input$betagama3)
      alfa4 <- (length.out = input$alfagama4)
      beta4 <- (length.out = input$betagama4)
      xmax <-(length.out = input$xmaxgama)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = dgamma,args = list(scale = 1/alfa, shape = beta), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun = dgamma,args = list(scale = 1/alfa2, shape = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun = dgamma,args = list(scale = 1/alfa3, shape = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun = dgamma,args = list(scale = 1/alfa4, shape = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()})  
  #### acumu gama ####
  output$acumugama <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfagama)
    beta <- (length.out = input$betagama)
    alfa2 <- (length.out = input$alfagama2)
    beta2 <- (length.out = input$betagama2)
    alfa3 <- (length.out = input$alfagama3)
    beta3 <- (length.out = input$betagama3)
    alfa4 <- (length.out = input$alfagama4)
    beta4 <- (length.out = input$betagama4)
    xmax <-(length.out = input$xmaxgama)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = pgamma,args = list(scale = 1/alfa, shape = beta), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun = pgamma,args = list(scale = 1/alfa2, shape = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun = pgamma,args = list(scale = 1/alfa3, shape = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun = pgamma,args = list(scale = 1/alfa4, shape = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })
  #### acumu gama down ####  
  output$acumugamadown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfagama)
      beta <- (length.out = input$betagama)
      alfa2 <- (length.out = input$alfagama2)
      beta2 <- (length.out = input$betagama2)
      alfa3 <- (length.out = input$alfagama3)
      beta3 <- (length.out = input$betagama3)
      alfa4 <- (length.out = input$alfagama4)
      beta4 <- (length.out = input$betagama4)
      xmax <-(length.out = input$xmaxgama)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = pgamma,args = list(scale = 1/alfa, shape = beta), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun = pgamma,args = list(scale = 1/alfa2, shape = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun = pgamma,args = list(scale = 1/alfa3, shape = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun = pgamma,args = list(scale = 1/alfa4, shape = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()})
  #### surv gama ####
  output$survgama <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfagama)
    beta <- (length.out = input$betagama)
    alfa2 <- (length.out = input$alfagama2)
    beta2 <- (length.out = input$betagama2)
    alfa3 <- (length.out = input$alfagama3)
    beta3 <- (length.out = input$betagama3)
    alfa4 <- (length.out = input$alfagama4)
    beta4 <- (length.out = input$betagama4)
    xmax <-(length.out = input$xmaxgama)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun =function (x) {pgamma(x,scale = 1/alfa, shape = beta, lower.tail = FALSE )}, geom = "line",size=1)+
               ylab("Função de Sobrevivência")+
               xlab("Quantil da distribuição")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0 & beta2 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pgamma(x,scale = 1/alfa2, shape = beta2, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0 & beta3 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pgamma(x,scale = 1/alfa3, shape = beta3, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "blue", lty=4))
      print(base)}
    if(alfa4 > 0 & beta4 > 0){
      base <- (base +
                 stat_function(fun =function (x) {pgamma(x,scale = 1/alfa4, shape = beta4, lower.tail = FALSE )}
                               , geom = "line",size=1, colour = "green", lty=6))
      print(base)}
  })      
  #### survgamadown ####
  output$survgamadown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfagama)
      beta <- (length.out = input$betagama)
      alfa2 <- (length.out = input$alfagama2)
      beta2 <- (length.out = input$betagama2)
      alfa3 <- (length.out = input$alfagama3)
      beta3 <- (length.out = input$betagama3)
      alfa4 <- (length.out = input$alfagama4)
      beta4 <- (length.out = input$betagama4)
      xmax <-(length.out = input$xmaxgama)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun =function (x) {pgamma(x,scale = 1/alfa, shape = beta, lower.tail = FALSE )}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0 & beta2 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pgamma(x,scale = 1/alfa2, shape = beta2, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0 & beta3 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pgamma(x,scale = 1/alfa3, shape = beta3, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "blue", lty=4))}
      if(alfa4 > 0 & beta4 > 0){
        base <- (base +
                   stat_function(fun =function (x) {pgamma(x,scale = 1/alfa4, shape = beta4, lower.tail = FALSE )}
                                 , geom = "line",size=1, colour = "green", lty=6))}
      print(base)
      dev.off()})
  #### gerador gama ####
  output$gergama <- renderPlot(width = 600, height = 400,{
    alfa <- (length.out = input$alfagama)
    beta <- (length.out = input$betagama)
    ngama <-(length.out = input$ngama)
    hist(rgamma(ngama,scale = 1/alfa, shape = beta),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dgamma(x,scale = 1/alfa, shape = beta),add = TRUE,lwd=2,col="blue")
    
  }) 
  #### gergamadown ####
  output$gergamadown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfagama)
      beta <- (length.out = input$betagama)
      ngama <-(length.out = input$ngama)
      hist(rgamma(ngama,scale = 1/alfa, shape = beta),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dgamma(x,scale = 1/alfa, shape = beta),add = TRUE,lwd=2,col="blue")
      dev.off()
    })  
  #### densidade t ####
  output$dent <- renderPlot(width = 550, height = 320,{
    df  <- (length.out = input$df)
    df2 <- (length.out = input$df2)
    df3 <- (length.out = input$df3)
    df4 <- (length.out = input$df4)
    xmin <-(length.out = input$xmint)
    xmax <-(length.out = input$xmaxt)
    base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
               stat_function(fun = dt,args = list(df = df), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(df2 > 0 ){
      base <- (base +
                 stat_function(fun = dt,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      print(base)}
    
    if(df3 > 0 ){
      base <- (base +
                 stat_function(fun = dt,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      print(base)}
    
    if(df4 > 0 ){
      base <- (base +
                 stat_function(fun = dt,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      print(base)}})
  
  #### dent download ####
  output$dentdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df  <- (length.out = input$df)
      df2 <- (length.out = input$df2)
      df3 <- (length.out = input$df3)
      df4 <- (length.out = input$df4)
      xmin <-(length.out = input$xmint)
      xmax <-(length.out = input$xmaxt)
      base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun = dt,args = list(df = df), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(df2 > 0 ){
        base <- (base +
                   stat_function(fun = dt,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      }
      
      if(df3 > 0 ){
        base <- (base +
                   stat_function(fun = dt,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      }
      
      if(df4 > 0 ){
        base <- (base +
                   stat_function(fun = dt,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      }
      print(base)
      dev.off()})
  
  #### acumut ####
  output$acumut <- renderPlot(width = 550, height = 320,{
    df  <- (length.out = input$df)
    df2 <- (length.out = input$df2)
    df3 <- (length.out = input$df3)
    df4 <- (length.out = input$df4)
    xmin <-(length.out = input$xmint)
    xmax <-(length.out = input$xmaxt)
    base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
               stat_function(fun = pt,args = list(df = df), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(df2 > 0 ){
      base <- (base +
                 stat_function(fun = pt,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      print(base)}
    
    if(df3 > 0 ){
      base <- (base +
                 stat_function(fun = pt,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      print(base)}
    
    if(df4 > 0 ){
      base <- (base +
                 stat_function(fun = pt,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      print(base)}})
  #### acumutdown ####
  output$acumutdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df  <- (length.out = input$df)
      df2 <- (length.out = input$df2)
      df3 <- (length.out = input$df3)
      df4 <- (length.out = input$df4)
      xmin <-(length.out = input$xmint)
      xmax <-(length.out = input$xmaxt)
      base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun = pt,args = list(df = df), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(df2 > 0 ){
        base <- (base +
                   stat_function(fun = pt,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      }
      
      if(df3 > 0 ){
        base <- (base +
                   stat_function(fun = pt,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      }
      
      if(df4 > 0 ){
        base <- (base +
                   stat_function(fun = pt,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      }
      print(base)
      dev.off()})  
  #### survt ####
  output$survt <- renderPlot(width = 550, height = 320,{
    df  <- (length.out = input$df)
    df2 <- (length.out = input$df2)
    df3 <- (length.out = input$df3)
    df4 <- (length.out = input$df4)
    xmin <-(length.out = input$xmint)
    xmax <-(length.out = input$xmaxt)
    
    base <-   (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun =function (x){ pt(x,df,lower.tail=FALSE)}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+   
                 theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                 theme_bw())
    print(base)
    if(df2 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pt(x,df2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      print(base)}
    
    if(df3 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pt(x,df3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      print(base)}
    
    if(df4 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pt(x,df4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      print(base)}})
  
  #### survtdown ####
  output$survtdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df  <- (length.out = input$df)
      df2 <- (length.out = input$df2)
      df3 <- (length.out = input$df3)
      df4 <- (length.out = input$df4)
      xmin <-(length.out = input$xmint)
      xmax <-(length.out = input$xmaxt)
      base <-   (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                   stat_function(fun =function (x){ pt(x,df,lower.tail=FALSE)}, geom = "line",size=1)+
                   ylab("Função de Sobrevivência")+
                   xlab("Quantil da distribuição")+   
                   theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                   theme_bw())
      if(df2 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pt(x,df2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      }
      
      if(df3 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pt(x,df3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      }
      
      if(df4 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pt(x,df4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      }
      print(base)
      dev.off()}) 
  #### gerador t ####
  output$gert <- renderPlot(width = 600, height = 400,{
    df <- (length.out = input$df)
    nt <-(length.out = input$nt)
    hist(rt(nt,df= df),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dt(x,df= df),add = TRUE,lwd=2,col="blue")
    
  }) 
  #### gertdown ####
  output$gertdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df <- (length.out = input$df)
      nt <-(length.out = input$nt)
      hist(rt(nt,df= df),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dt(x,df= df),add = TRUE,lwd=2,col="blue")
      dev.off()
    })    
  #### denqq ####
  output$denqq <- renderPlot(width = 550, height = 320,{
    df  <- (length.out = input$dfqq)
    df2 <- (length.out = input$dfqq2)
    df3 <- (length.out = input$dfqq3)
    df4 <- (length.out = input$dfqq4)
    xmax <-(length.out = input$xmaxqq)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = dchisq,args = list(df = df), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(df2 > 0 ){
      base <- (base +
                 stat_function(fun = dchisq,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      print(base)}
    
    if(df3 > 0 ){
      base <- (base +
                 stat_function(fun = dchisq,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      print(base)}
    
    if(df4 > 0 ){
      base <- (base +
                 stat_function(fun = dchisq,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      print(base)}})
  #### denqqdown ####
  output$denqqdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df  <- (length.out = input$dfqq)
      df2 <- (length.out = input$dfqq2)
      df3 <- (length.out = input$dfqq3)
      df4 <- (length.out = input$dfqq4)
      xmax <-(length.out = input$xmaxqq)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = dchisq,args = list(df = df), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(df2 > 0 ){
        base <- (base +
                   stat_function(fun = dchisq,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      }
      
      if(df3 > 0 ){
        base <- (base +
                   stat_function(fun = dchisq,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      }
      
      if(df4 > 0 ){
        base <- (base +
                   stat_function(fun = dchisq,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      }
      print(base)
      dev.off()})
  #### acumuqq ####
  output$acumuqq <- renderPlot(width = 550, height = 320,{
    df  <- (length.out = input$dfqq)
    df2 <- (length.out = input$dfqq2)
    df3 <- (length.out = input$dfqq3)
    df4 <- (length.out = input$dfqq4)
    xmax <-(length.out = input$xmaxqq)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = pchisq,args = list(df = df), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(df2 > 0 ){
      base <- (base +
                 stat_function(fun = pchisq,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      print(base)}
    
    if(df3 > 0 ){
      base <- (base +
                 stat_function(fun = pchisq,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      print(base)}
    
    if(df4 > 0 ){
      base <- (base +
                 stat_function(fun = pchisq,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      print(base)}})
  #### acumuqqdown ####
  output$acumuqqdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df  <- (length.out = input$dfqq)
      df2 <- (length.out = input$dfqq2)
      df3 <- (length.out = input$dfqq3)
      df4 <- (length.out = input$dfqq4)
      xmax <-(length.out = input$xmaxqq)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = pchisq,args = list(df = df), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(df2 > 0 ){
        base <- (base +
                   stat_function(fun = pchisq,args = list(df = df2), geom = "line",size=1,lty = 2, colour = "red"))
      }
      
      if(df3 > 0 ){
        base <- (base +
                   stat_function(fun = pchisq,args = list(df = df3), geom = "line",size=1,lty = 4, colour = "blue"))
      }
      
      if(df4 > 0 ){
        base <- (base +
                   stat_function(fun = pchisq,args = list(df = df4), geom = "line",size=1,lty = 6, colour = "green"))
      }
      print(base)
      dev.off()})    
  
  #### survqq ####
  output$survqq <- renderPlot(width = 550, height = 320,{
    df  <- (length.out = input$dfqq)
    df2 <- (length.out = input$dfqq2)
    df3 <- (length.out = input$dfqq3)
    df4 <- (length.out = input$dfqq4)
    xmax <-(length.out = input$xmaxqq)
    
    base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun =function (x){ pchisq(x,df,lower.tail=FALSE)}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+   
                 theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                 theme_bw())
    print(base)
    if(df2 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pchisq(x,df2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      print(base)}
    
    if(df3 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pchisq(x,df3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      print(base)}
    
    if(df4 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pchisq(x,df4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      print(base)}})
  
  #### survqqdown ####
  output$survqqdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df  <- (length.out = input$dfqq)
      df2 <- (length.out = input$dfqq2)
      df3 <- (length.out = input$dfqq3)
      df4 <- (length.out = input$dfqq4)
      xmax <-(length.out = input$xmaxqq)
      base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                   stat_function(fun =function (x){ pchisq(x,df,lower.tail=FALSE)}, geom = "line",size=1)+
                   ylab("Função de Sobrevivência")+
                   xlab("Quantil da distribuição")+   
                   theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                   theme_bw())
      if(df2 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pchisq(x,df2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      }
      
      if(df3 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pchisq(x,df3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      }
      
      if(df4 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pchisq(x,df4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      }
      print(base)
      dev.off()}) 
  #### gerador qq ####
  output$gerqq <- renderPlot(width = 600, height = 400,{
    df <- (length.out = input$dfqq)
    nqq <-(length.out = input$nqq)
    hist(rchisq(nqq,df= df),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dchisq(x,df= df),add = TRUE,lwd=2,col="blue")
    
  }) 
  #### gerqqdown ####
  output$gerqqdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      df <- (length.out = input$dfqq)
      nqq <-(length.out = input$nqq)
      hist(rchisq(nqq,df= df),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dchisq(x,df= df),add = TRUE,lwd=2,col="blue")
      dev.off()
    })      
  #### dencchy ####
  output$dencchy <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfacchy)
    beta <- (length.out = input$betacchy)
    alfa2 <- (length.out = input$alfacchy2)
    beta2 <- (length.out = input$betacchy2)
    alfa3 <- (length.out = input$alfacchy3)
    beta3 <- (length.out = input$betacchy3)
    alfa4 <- (length.out = input$alfacchy4)
    beta4 <- (length.out = input$betacchy4)
    xmax <-(length.out = input$xmaxcchy)
    xmin <-(length.out = input$xmincchy)
    base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
               stat_function(fun = dcauchy,args = list(scale = alfa, location = beta), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0){
      base <- (base +
                 stat_function(fun = dcauchy,args = list(scale = alfa2, location = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0){
      base <- (base +
                 stat_function(fun = dcauchy,args = list(scale = alfa3, location = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0){
      base <- (base +
                 stat_function(fun = dcauchy,args = list(scale = alfa4, location = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })  
  #### dencchydown ####
  output$dencchydown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfacchy)
      beta <- (length.out = input$betacchy)
      alfa2 <- (length.out = input$alfacchy2)
      beta2 <- (length.out = input$betacchy2)
      alfa3 <- (length.out = input$alfacchy3)
      beta3 <- (length.out = input$betacchy3)
      alfa4 <- (length.out = input$alfacchy4)
      beta4 <- (length.out = input$betacchy4)
      xmax <-(length.out = input$xmaxcchy)
      xmin <-(length.out = input$xmincchy)
      base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun = dcauchy,args = list(scale = alfa, location = beta), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0){
        base <- (base +
                   stat_function(fun = dcauchy,args = list(scale = alfa2, location = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0){
        base <- (base +
                   stat_function(fun = dcauchy,args = list(scale = alfa3, location = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(alfa4 > 0){
        base <- (base +
                   stat_function(fun = dcauchy,args = list(scale = alfa4, location = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()
    })    
  #### acumucchy ####
  output$acumucchy <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfacchy)
    beta <- (length.out = input$betacchy)
    alfa2 <- (length.out = input$alfacchy2)
    beta2 <- (length.out = input$betacchy2)
    alfa3 <- (length.out = input$alfacchy3)
    beta3 <- (length.out = input$betacchy3)
    alfa4 <- (length.out = input$alfacchy4)
    beta4 <- (length.out = input$betacchy4)
    xmax <-(length.out = input$xmaxcchy)
    xmin <-(length.out = input$xmincchy)
    base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
               stat_function(fun = pcauchy,args = list(scale = alfa, location = beta), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(alfa2 > 0){
      base <- (base +
                 stat_function(fun = pcauchy,args = list(scale = alfa2, location = beta2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(alfa3 > 0){
      base <- (base +
                 stat_function(fun = pcauchy,args = list(scale = alfa3, location = beta3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(alfa4 > 0){
      base <- (base +
                 stat_function(fun = pcauchy,args = list(scale = alfa4, location = beta4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })  
  #### acumucchydown ####
  output$acumucchydown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfacchy)
      beta <- (length.out = input$betacchy)
      alfa2 <- (length.out = input$alfacchy2)
      beta2 <- (length.out = input$betacchy2)
      alfa3 <- (length.out = input$alfacchy3)
      beta3 <- (length.out = input$betacchy3)
      alfa4 <- (length.out = input$alfacchy4)
      beta4 <- (length.out = input$betacchy4)
      xmax <-(length.out = input$xmaxcchy)
      xmin <-(length.out = input$xmincchy)
      base <- (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun = pcauchy,args = list(scale = alfa, location = beta), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(alfa2 > 0){
        base <- (base +
                   stat_function(fun = pcauchy,args = list(scale = alfa2, location = beta2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(alfa3 > 0){
        base <- (base +
                   stat_function(fun = pcauchy,args = list(scale = alfa3, location = beta3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(alfa4 > 0){
        base <- (base +
                   stat_function(fun = pcauchy,args = list(scale = alfa4, location = beta4)
                                 , geom = "line",size=1, colour = "green",lty=6))}
      print(base)
      dev.off()
    })    
  #### survcchy ####
  output$survcchy <- renderPlot(width = 550, height = 320,{
    alfa <- (length.out = input$alfacchy)
    beta <- (length.out = input$betacchy)
    alfa2 <- (length.out = input$alfacchy2)
    beta2 <- (length.out = input$betacchy2)
    alfa3 <- (length.out = input$alfacchy3)
    beta3 <- (length.out = input$betacchy3)
    alfa4 <- (length.out = input$alfacchy4)
    beta4 <- (length.out = input$betacchy4)
    xmax <-(length.out = input$xmaxcchy)
    xmin <-(length.out = input$xmincchy)
    base <-   (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                 stat_function(fun =function (x){ pcauchy(x,location = beta, scale = alfa,lower.tail=FALSE)}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+   
                 theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                 theme_bw())
    print(base)
    if(alfa2 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pcauchy(x,location = beta2, scale = alfa2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      print(base)}
    
    if(alfa3 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pcauchy(x,location = beta3, scale = alfa3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      print(base)}
    
    if(alfa4 > 0 ){
      base <- (base +
                 stat_function(fun = function (x){ pcauchy(x,location = beta4, scale = alfa4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      print(base)}})
  #### survdowncchy ####
  output$survcchydown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfacchy)
      beta <- (length.out = input$betacchy)
      alfa2 <- (length.out = input$alfacchy2)
      beta2 <- (length.out = input$betacchy2)
      alfa3 <- (length.out = input$alfacchy3)
      beta3 <- (length.out = input$betacchy3)
      alfa4 <- (length.out = input$alfacchy4)
      beta4 <- (length.out = input$betacchy4)
      xmax <-(length.out = input$xmaxcchy)
      xmin <-(length.out = input$xmincchy)
      base <-   (ggplot(data.frame(x = c(xmin, xmax)), aes(x)) +
                   stat_function(fun =function (x){ pcauchy(x,location = beta, scale = alfa,lower.tail=FALSE)}, geom = "line",size=1)+
                   ylab("Função de Sobrevivência")+
                   xlab("Quantil da distribuição")+   
                   theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                   theme_bw())
      if(alfa2 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pcauchy(x,location = beta2, scale = alfa2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      }
      
      if(alfa3 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pcauchy(x,location = beta3, scale = alfa3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      }
      
      if(alfa4 > 0 ){
        base <- (base +
                   stat_function(fun = function (x){ pcauchy(x,location = beta4, scale = alfa4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      }
      print(base)
      dev.off()
    })
  #### gerador cchy ####
  output$gercchy <- renderPlot(width = 600, height = 400,{
    alfa <- (length.out = input$alfacchy)
    beta <- (length.out = input$betacchy)
    ncchy <-(length.out = input$ncchy)
    hist(rcauchy(ncchy,location = beta, scale = alfa),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(dcauchy(x,location = beta, scale = alfa),add = TRUE,lwd=2,col="blue")
    
  }) 
  #### gercchydown ####
  output$gercchydown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      alfa <- (length.out = input$alfacchy)
      beta <- (length.out = input$betacchy)
      ncchy <-(length.out = input$ncchy)
      hist(rcauchy(ncchy,location = beta, scale = alfa),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(dcauchy(x,location = beta, scale = alfa),add = TRUE,lwd=2,col="blue")
      dev.off()
    })      
  #### denf ####
  output$denf <- renderPlot(width = 550, height = 320,{
    n1 <- (length.out = input$dffn1)
    m1 <- (length.out = input$dffm1)
    n2 <- (length.out = input$dffn2)
    m2 <- (length.out = input$dffm2)
    n3 <- (length.out = input$dffn3)
    m3 <- (length.out = input$dffm3)
    n4 <- (length.out = input$dffn4)
    m4 <- (length.out = input$dffm4)
    xmax <-(length.out = input$xmaxf)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = df,args = list(df1 = n1, df2 = m1), geom = "line",size=1)+
               ylab("Função Densidade de Probabilidade")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(n2 > 0 & m2 > 0){
      base <- (base +
                 stat_function(fun = df,args = list(df1 = n2, df2 = m2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(n3 > 0 & m3 > 0){
      base <- (base +
                 stat_function(fun = df,args = list(df1 = n3, df2 = m3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(n4 > 0 & m4 > 0){
      base <- (base +
                 stat_function(fun = df,args = list(df1 = n4, df2 = m4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })  
  #### denfdown ####
  output$denfdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      n1 <- (length.out = input$dffn1)
      m1 <- (length.out = input$dffm1)
      n2 <- (length.out = input$dffn2)
      m2 <- (length.out = input$dffm2)
      n3 <- (length.out = input$dffn3)
      m3 <- (length.out = input$dffm3)
      n4 <- (length.out = input$dffn4)
      m4 <- (length.out = input$dffm4)
      xmax <-(length.out = input$xmaxf)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = df,args = list(df1 = n1, df2 = m1), geom = "line",size=1)+
                 ylab("Função Densidade de Probabilidade")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(n2 > 0 & m2 > 0){
        base <- (base +
                   stat_function(fun = df,args = list(df1 = n2, df2 = m2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(n3 > 0 & m3 > 0){
        base <- (base +
                   stat_function(fun = df,args = list(df1 = n3, df2 = m3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(n4 > 0 & m4 > 0){
        base <- (base +
                   stat_function(fun = df,args = list(df1 = n4, df2 = m4)
                                 , geom = "line",size=1, colour = "green",lty=6))
      }
      print(base)
      dev.off()})  
  #### acumuf ####
  output$acumuf <- renderPlot(width = 550, height = 320,{
    n1 <- (length.out = input$dffn1)
    m1 <- (length.out = input$dffm1)
    n2 <- (length.out = input$dffn2)
    m2 <- (length.out = input$dffm2)
    n3 <- (length.out = input$dffn3)
    m3 <- (length.out = input$dffm3)
    n4 <- (length.out = input$dffn4)
    m4 <- (length.out = input$dffm4)
    xmax <-(length.out = input$xmaxf)
    base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
               stat_function(fun = pf,args = list(df1 = n1, df2 = m1), geom = "line",size=1)+
               ylab("Função Distribuição Acumulada")+
               theme(axis.title.y=element_text(size=14))+
               theme_bw())
    print(base)
    if(n2 > 0 & m2 > 0){
      base <- (base +
                 stat_function(fun = pf,args = list(df1 = n2, df2 = m2)
                               , geom = "line",size=1, colour = "red", lty=2))
      print(base)}
    if(n3 > 0 & m3 > 0){
      base <- (base +
                 stat_function(fun = pf,args = list(df1 = n3, df2 = m3)
                               , geom = "line",size=1, colour = "blue",lty=4))
      print(base)}
    if(n4 > 0 & m4 > 0){
      base <- (base +
                 stat_function(fun = pf,args = list(df1 = n4, df2 = m4)
                               , geom = "line",size=1, colour = "green",lty=6))
      print(base)}
  })  
  #### acumufdown ####
  output$acumufdown <-  downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      n1 <- (length.out = input$dffn1)
      m1 <- (length.out = input$dffm1)
      n2 <- (length.out = input$dffn2)
      m2 <- (length.out = input$dffm2)
      n3 <- (length.out = input$dffn3)
      m3 <- (length.out = input$dffm3)
      n4 <- (length.out = input$dffn4)
      m4 <- (length.out = input$dffm4)
      xmax <-(length.out = input$xmaxf)
      base <- (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun = pf,args = list(df1 = n1, df2 = m1), geom = "line",size=1)+
                 ylab("Função Distribuição Acumulada")+
                 theme(axis.title.y=element_text(size=14))+
                 theme_bw())
      if(n2 > 0 & m2 > 0){
        base <- (base +
                   stat_function(fun = pf,args = list(df1 = n2, df2 = m2)
                                 , geom = "line",size=1, colour = "red", lty=2))}
      if(n3 > 0 & m3 > 0){
        base <- (base +
                   stat_function(fun = pf,args = list(df1 = n3, df2 = m3)
                                 , geom = "line",size=1, colour = "blue",lty=4))}
      if(n4 > 0 & m4 > 0){
        base <- (base +
                   stat_function(fun = pf,args = list(df1 = n4, df2 = m4)
                                 , geom = "line",size=1, colour = "green",lty=6))
      }
      print(base)
      dev.off()})    
  #### survf ####
  output$survf <- renderPlot(width = 550, height = 320,{
    n1 <- (length.out = input$dffn1)
    m1 <- (length.out = input$dffm1)
    n2 <- (length.out = input$dffn2)
    m2 <- (length.out = input$dffm2)
    n3 <- (length.out = input$dffn3)
    m3 <- (length.out = input$dffm3)
    n4 <- (length.out = input$dffn4)
    m4 <- (length.out = input$dffm4)
    xmax <-(length.out = input$xmaxf)
    base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                 stat_function(fun =function (x){ pf(x,df1 = n1, df2 = m1,lower.tail=FALSE)}, geom = "line",size=1)+
                 ylab("Função de Sobrevivência")+
                 xlab("Quantil da distribuição")+   
                 theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                 theme_bw())
    print(base)
    if(n2 > 0 & m2 > 0){
      base <- (base +
                 stat_function(fun = function (x){ pf(x,df1 = n2, df2 = m2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      print(base)}
    
    if(n3 > 0 & m3 > 0){
      base <- (base +
                 stat_function(fun = function (x){ pf(x,df1 = n3, df2 = m3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      print(base)}
    
    if(n4 > 0 & m4 > 0){
      base <- (base +
                 stat_function(fun = function (x){ pf(x,df1 = n4, df2 = m4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      print(base)}})
  #### survdownf ####
  output$survfdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      n1 <- (length.out = input$dffn1)
      m1 <- (length.out = input$dffm1)
      n2 <- (length.out = input$dffn2)
      m2 <- (length.out = input$dffm2)
      n3 <- (length.out = input$dffn3)
      m3 <- (length.out = input$dffm3)
      n4 <- (length.out = input$dffn4)
      m4 <- (length.out = input$dffm4)
      xmax <-(length.out = input$xmaxf)
      base <-   (ggplot(data.frame(x = c(0, xmax)), aes(x)) +
                   stat_function(fun =function (x){ pf(x,df1 = n1, df2 = m1,lower.tail=FALSE)}, geom = "line",size=1)+
                   ylab("Função de Sobrevivência")+
                   xlab("Quantil da distribuição")+   
                   theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14))+
                   theme_bw())
      if(n2 > 0 & m2 > 0){
        base <- (base +
                   stat_function(fun = function (x){ pf(x,df1 = n2, df2 = m2,lower.tail=FALSE)}, geom = "line",size=1, lty=2, colour= "red"))
      }
      if(n3 > 0 & m3 > 0){
        base <- (base +
                   stat_function(fun = function (x){ pf(x,df1 = n3, df2 = m3,lower.tail=FALSE)}, geom = "line",size=1, lty=4, colour= "blue"))
      }
      if(n4 > 0 & m4 > 0){
        base <- (base +
                   stat_function(fun = function (x){ pf(x,df1 = n4, df2 = m4,lower.tail=FALSE)}, geom = "line",size=1, lty=6, colour= "green"))
      }
      print(base)
      dev.off()
    })
  
  #### gerador f ####
  output$gerf <- renderPlot(width = 600, height = 400,{
    n1 <- (length.out = input$dffn1)
    m1 <- (length.out = input$dffm1)
    nf <-(length.out = input$nf)
    hist(rf(nf,df1 = n1, df2 = m1),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
    curve(df(x,df1 = n1, df2 = m1),add = TRUE,lwd=2,col="blue")
    
  }) 
  #### gerfdown ####
  output$gerfdown <- downloadHandler(
    filename = function(){
      paste("plot", "pdf", sep = ".")
    },
    content = function(file){
      pdf(file)
      n1 <- (length.out = input$dffn1)
      m1 <- (length.out = input$dffm1)
      nf <-(length.out = input$nf)
      hist(rf(nf,df1 = n1, df2 = m1),density = 20,prob=TRUE,ylab = "Densidade",xlab = "Valor da variável",main = "")
      curve(df(x,df1 = n1, df2 = m1),add = TRUE,lwd=2,col="blue")
      dev.off()
    })      
  
}

shinyApp(ui, server)