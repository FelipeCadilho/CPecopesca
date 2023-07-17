#' Pacote para executar o changepoint por distância
#'
#' @param mudaEixoY To customize the y-axis, enter the value 1, otherwise 0 (Default).
#' @param penalidade Choose from the penalties 'None', 'SIC', 'BIC', 'MBIC', 'AIC', 'Hannan-Quinn', 'Asymptotic', 'Manual', and 'CROPS'. If 'Manual' is specified, the manual penalty will be included in the parameter 'pen.value'. If 'Asymptotic' is specified, the theoretical type I error is contained in the parameter 'pen.value'. If 'CROPS' is specified, the penalty range is contained in the parameter 'pen.value'; note that this is a vector of length 2 containing the minimum and maximum penalty value. Note that 'CROPS' can only be used if the method is 'PELT'. The predefined penalties listed COUNT the changepoint as a parameter, postfix a 0 for example 'SIC0' to NOT count the changepoint as a parameter.
#' @param pen.valor The theoretical type I error, for example, 0.05 when using the Asymptotic penalty. A vector of length 2 (min, max) if using the CROPS penalty. The penalty value when using the manual penalty option - it can be a numeric value or a text providing the formula to be used. The available variables are n = length of the original data, null = null probability, alt = alternative probability, tau = proposed change point, diffparam = difference in the number of alternative and null parameters.
#' @param metodo Choose between 'AMOC', 'PELT', 'SegNeigh', or 'BinSeg'.
#' @param teste.stat The assumed test statistic/distribution of the data. Currently, only 'Normal' and 'CUSUM' are supported.
#' @param tamSegmMin Positive integer providing the minimum length of the segment (number of observations between changes), the default is the minimum allowed by theory.
#'
#' @return
#' @export
#'
#' @examples
CPecopesca <- function(penalidade="Asymptotic",
                       pen.valor=0.05,
                       metodo="PELT",
                       teste.stat="Normal",
                       tamSegmMin=1,
                       un="(µm)"){
  #initial notice
  cat("\nConfigurou o local do seu arquivo de dados como
      \ndiretório de trabalho do ambiente R antes de executar este pacote? S/N\n")
  configuracao <<- toupper(readLines(n=1))

  if(configuracao == "S"||configuracao == "Y"){

    #packages needed for this package to work
    cat("\nEssa é a primeira execução após instalação deste pacote? S/N\n")
    pacote = toupper(readLines(n=1))

    if(pacote == "S"|| pacote == "Y"){

      #install and call a package that reads excel sheets
      install.packages("readxl")
      library("readxl")

      #install and call data control package
      install.packages("dplyr")
      library("dplyr")

      #install and call frequency change search package
      install.packages("changepoint")
      library("changepoint")

      #install and call string manipulation package
      install.packages("stringr")
      library("stringr")

    }else{
      #call package that reads spreadsheet in excel
      library("readxl")

      #calls data control package
      library("dplyr")

      #call frequency change seek packet
      library("changepoint")

      #install and call string manipulation package
      library("stringr")
    }

    #definição dos parâmetros da função do changepoint
    cat("\nDeseja configurar os parametros da função changepoint? S/N\n")
    configuracaoParametros <- toupper(readLines(n=1))

    if(!is.null(configuracaoParametros)&&configuracaoParametros=="S"||configuracaoParametros=="Y"){
      cat("\nPenalidade:\n")
      penalidade <- readLines(n=1)
      cat("\nValor de penalidade:\n")
      pen.valor <- scan(n=1)
      cat("\nMétodo:\n")
      metodo <- readLines(n=1)
      cat("\nTeste estatístico:\n")
      teste.stat <- readLines(n=1)
      cat("\nTamanho do segmento mínimo (número de observações entre as mudanças):\n")
      tamSegmMin <- scan(n=1)
    }

    #definition of the unit of measure
    cat("\nQual unidade de medida da distância?\n")
    un = paste("(",readLines(n=1),")",sep="")

    #chart language setting
    cat("\nQual idioma do gráfico:\n1) Inglês\n2) Português\n")
    idioma = scan(n=1)
    if(isTRUE(idioma) && idioma==2){
      #legendaTitulo <- "Amostra:"
      legendaX <- paste("Distância do núcleo à borda",un)
    }else{
      #legendaTitulo <- "Sample:"
      legendaX <- paste("Distance from core to edge",un)
    }

################################################################################
## START OF DATA ENTRY
################################################################################
    #data file name
    cat("\nDigite o nome do arquivo de dados com a extensão.(E.g. nome_dados.xlsx)\n")
    nome_dados <<- readLines(n=1)

    #data file extension
    cat("\nInforme o número referente a extensão do arquivo de dados:\n1-xls ou xlsx (Padrão)\n2-csv\n3-txt\n4-Dataframe\n")
    tipo_dados = scan(n=1)
    if(tipo_dados == 1){
      dado <- "xlsx"
      cat("\nInforme o nome da planilha que deseja usar. (Por padrão é a primeira)\n")
      planilha <<- readLines(n=1)
    }else if(tipo_dados == 2){
      dado <- "csv"
    }else if(tipo_dados == 3){
      dado <- "txt"
    }else if(tipo_dados == 4){
      dado <- "dataframe"
    }else if(is.null(tipo_dados)){
      dado <- "xlsx"
      cat("\nInforme o nome da planilha que deseja usar. (Por padrão é a primeira)\n")
      planilha <<- readLines(n=1)
    }
    if(dado =="txt"||dado =="csv"){
        cat("\nQual caracter foi utilizado para separação das colunas no arquivo?\n")
        separador = readLines(n=1)
    }else{
      separador = ","
    }

    dados <<- data.frame()
    if(dado == "xlsx"){
      #cria dataframe a partir dos dados da planilha
      if(is.null(planilha)){
        dados <<- read_excel(nome_dados, check.names=F, header = TRUE, encoding = "UTF-8")
      }else{
        dados <<- read_excel(nome_dados, sheet = planilha, check.names=F, header = TRUE, encoding = "UTF-8")
      }
    }else if(dado == "dataframe"){
      #cria dataframe a partir dos dados do dataframe
      dados <<- nome_dados
    }else if(dado == "csv"){
      if(is.null(separador)){
        dados <<- read.csv(file = nome_dados, check.names=F, header = TRUE, encoding = "UTF-8")
      }else{
        dados <<- read.csv(file = nome_dados, header = TRUE, check.names=F, sep = separador, encoding = "UTF-8")
      }
    }else if(dado == "txt"){
      if(is.null(separador)){
          cat("\nNecessário informar separador de arquivos em txt.\n")
      }else{
        dados <<- read.delim2(nome_dados, sep=separador,check.names=F, header = TRUE, encoding = "UTF-8")
      }
    }
################################################################################
## END OF DATA ENTRY
################################################################################

    #sample identification
    cat("\nInforme a identificação da amostra.\n")
    amostra <<- readLines(n=1)

    #y-axis question
    cat("\nDeseja alterar os eixos y de cada gráfico? S/N\n")
    eixo = toupper(readLines(n=1))
    if(eixo=="S"||eixo=="Y"){
      mudaEixoY <- 1
    }else{
      mudaEixoY <- 0
    }

    #amount of elements
    elementos <<- length(dados)

    #get the sample id
    #amostra <<- paste(legendaTitulo,amostra)

    #creates dataframe that will allocate the results
    resultados <<- data.frame()
    localiza <<- data.frame()
    media <<- data.frame()
    resultados[1,1] <<- "Posição do ponto de mudança"
    resultados[2,1] <<- "Valor do elemento no ponto de mudança"
    resultados[3,1] <<- "Médias entre as mudanças"
    resultados[4,1] <<- "Valor da distância no ponto de mudança"
    colnames(resultados) <<- c("DESCRIÇÃO")

    #names for comparison
    dfnomes <<- data.frame()
    dfnomes[1,1] <<- "dist"

################################################################################
## REPEAT START
################################################################################
    #repetition to generate the graphs by element
    for(a in 2:elementos){

      #get the name of the current element
      nome <<- colnames(dados[a])

      #point change identification function
      mudanca <<-cpt.mean(as.numeric(unlist(dados[,a])),
                          penalty=penalidade,
                          pen.value=pen.valor,
                          method=metodo,
                          test.stat=teste.stat,
                          class=TRUE,
                          param.estimates=TRUE,
                          minseglen=tamSegmMin)

      #distance column start row
      limiteDistanciaMin <<- 1

      #number of change points
      limitePontos <<- length(mudanca@cpts)

      #names for comparison
      tt <<- a-1
      dfnomes[1,tt] <<- paste(tt,": ",nome,sep="")
      colnames(dfnomes)[tt] <<- c(dfnomes[1,tt])

      #allocation of results
      for(o in 1:limitePontos){
        resultados[1,a] <<- paste(resultados[1,a],mudanca@cpts[o])
        localiza[o,tt] <<- mudanca@cpts[o]
        resultados[2,a] <<- paste(resultados[1,a],dados[mudanca@cpts[o],a])
        resultados[3,a] <<- paste(resultados[1,a],mudanca@param.est$mean[o])
        media[o,tt] <<- mudanca@param.est$mean[o]
        resultados[4,a] <<- paste(resultados[1,a],dados[mudanca@cpts[o],1])
      }

      #current element name
      colnames(resultados)[a] <<- c(nome)

      if(mudaEixoY==1){
          #determining the lower limit of the y-axis
          cat("\nInforme o limite inferior do eixo y para o elemento",nome,":\n")
          yMin <- scan(n=1)

          #determining the upper limit of the y-axis
          cat("\nInforme o limite superior do eixo y",nome,":\n")
          yMax <- scan(n=1)

          #generation of the graph that will later receive the change lines
          plot(x = dados[,1], y = dados[,a], type = "l",
               xlab = paste(legendaX),
               ylab = nome,
               ylim = c(yMin,yMax))
          title(main = amostra,
                xlab = paste(legendaX),
                ylab = nome)
      }else{
        #generation of the graph that will later receive the change lines
        plot(x = dados[,1], y = dados[,a], type = "l",
             xlab = paste(legendaX),
             ylab = nome)
        title(main = amostra,
              xlab = paste(legendaX),
              ylab = nome)
      }
      #repetition by the number of points of changes
      for(i in 1:limitePontos){

          #line control variable
          k = 1

          #starting the dataframe that will add the change lines
          linhas <<- data.frame()

          #repetition to the point of change
          for(j in limiteDistanciaMin:mudanca@cpts[i]){

              #assign distance value
              linhas[k,1] <<- dados[j,1]

              #assign element value
              linhas[k,2] <<- mudanca@param.est$mean[i]

              #line control variable
              k = k+1
          }

          #generated change line display
          lines(x = linhas[,1], y = linhas[,2], col = "red", lwd = 3)

          #object remover containing line
          rm(linhas, envir = .GlobalEnv)

          #raising the start line for the next cycle
          limiteDistanciaMin <<- mudanca@cpts[i]+1
      }
    }
################################################################################
## END OF REPEAT
################################################################################

    #object remover
    rm(linhas, envir = .GlobalEnv)
    rm(limiteDistanciaMin, envir = .GlobalEnv)
    rm(limitePontos, envir = .GlobalEnv)
    rm(elementos, envir = .GlobalEnv)
    rm(nome, envir = .GlobalEnv)
    rm(configuracao, envir = .GlobalEnv)
    rm(configuracaoParametros, envir = .GlobalEnv)

    #comparison between elements
    cat("\nDeseja comparar elementos:\n0) Não comparar\n1) 2 elementos\n2) 3 elementos\n")
    compara <- scan(n=1)

    elementosSelecionados <- c()
    localizacoes <<- localiza
    medias <<- media
    if(!is.null(compara) && compara!=0){
      cat("\nElementos a serem comparados:\n",colnames(dfnomes),"\n")
      ascores<-c("(cor preta)","(cor vermelha)","(cor azul)")
      if(compara == 1){#dois elementos
        for(i in 1:2){
          cat("\nDigite o elemento",i,":",ascores[i],"\n")
          elementosSelecionados[i] <- scan(n=1)
          localizacoes[,i] <<- localiza[,elementosSelecionados[i]]
          medias[,i] <<- media[,elementosSelecionados[i]]
          elementosSelecionados[i] <- elementosSelecionados[i]+1
        }
      }else if(compara == 2){#três elementos
        for(i in 1:3){
          cat("\nDigite o elemento",i,":",ascores[i],"\n")
          elementosSelecionados[i] <- scan(n=1)
          localizacoes[,i] <<- localiza[,elementosSelecionados[i]]
          medias[,i] <<- media[,elementosSelecionados[i]]
          elementosSelecionados[i] <- elementosSelecionados[i]+1
        }
      }
      comparacao(compara, elementosSelecionados, localizacoes, medias, dados, amostra, legendaX)
    }

    #object remover
    rm(dfnomes, envir = .GlobalEnv)
    rm(media, envir = .GlobalEnv)
    rm(tt, envir = .GlobalEnv)

    write.table(resultados, file='resultados-PCecopesca.csv', sep=',', dec='.', row.names=FALSE)
    return("Processo finalizado.")
  }else{
    return("\nConfigure sua área de trabalho antes de executar o pacote.\n")
  }
}
