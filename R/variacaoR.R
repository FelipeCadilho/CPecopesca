#' Pacote para executar o changepoint por distância
#'
#' @param mudaEixoY Para personalizar o eixo y informe o valor 1, senão 0 (Padrão).
#' @param penalidade Escolha das penalidades "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic", "Manual" e "CROPS". Se for especificado Manual, a penalidade manual será incluída no parâmetro pen.value. Se Asymptotic for especificado, o erro teórico tipo I está contido no parâmetro pen.value. Se CROPS for especificado, o intervalo de penalidade está contido no parâmetro pen.value; observe que este é um vetor de comprimento 2 que contém o valor de penalidade mínimo e máximo. Nota CROPS só pode ser usado se o método for "PELT". As penalidades predefinidas listadas CONTAM o changepoint como um parâmetro, postfix a 0 por exemplo "SIC0" para NÃO contar o changepoint como um parâmetro.
#' @param pen.valor O erro teórico tipo I, por exemplo, 0,05 ao usar a penalidade Assintótica. Um vetor de comprimento 2 (min,max) se estiver usando a penalidade CROPS. O valor da penalidade ao usar a opção de penalidade manual - pode ser um valor numérico ou um texto que forneça a fórmula a ser usada. As variáveis disponíveis são, n=comprimento dos dados originais, nulo=probabilidade nula, alt=probabilidade alternativa, tau=ponto de mudança proposto, diffparam=diferença no número de parâmetros alternativos e nulos.
#' @param metodo Escolha de "AMOC", "PELT", "SegNeigh" ou "BinSeg".
#' @param teste.stat A estatística/distribuição de teste assumida dos dados. Atualmente, apenas "Normal" e "CUSUM" são suportados.
#' @param tamSegmMin Inteiro positivo fornecendo o comprimento mínimo do segmento (nº de observações entre as alterações), o padrão é o mínimo permitido pela teoria.
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
  #aviso inicial
  cat("\nConfigurou o local do seu arquivo de dados como
      \ndiretório de trabalho do ambiente R antes de executar este pacote? S/N\n")
  configuracao <<- toupper(readLines(n=1))

  if(configuracao == "S"||configuracao == "Y"){

    #pacotes necessários para o funcionamento deste pacote
    cat("\nEssa é a primeira execução após instalação deste pacote? S/N\n")
    pacote = toupper(readLines(n=1))

    if(pacote == "S"|| pacote == "Y"){

      #instala e chama pacote que lê planilha em excel
      install.packages("readxl")
      library("readxl")

      #instala e chama pacote de controle de dados
      install.packages("dplyr")
      library("dplyr")

      #instala e chama pacote de busca por mudança em frequencia
      install.packages("changepoint")
      library("changepoint")

      #instala e chama pacote de manipulação de strings
      install.packages("stringr")
      library("stringr")

    }else{
      #chama pacote que lê planilha em excel
      library("readxl")

      #chama pacote de controle de dados
      library("dplyr")

      #chama pacote de busca por mudança em frequencia
      library("changepoint")

      #instala e chama pacote de manipulação de strings
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

    #definição da unidade de medida
    cat("\nQual unidade de medida da distância?\n")
    un = paste("(",readLines(n=1),")",sep="")

    #definição do idioma do gráfico
    cat("\nQual idioma do gráfico:\n1) Inglês\n2) Português\n")
    idioma = scan(n=1)
    if(isTRUE(idioma) && idioma==2){
      legendaTitulo <- "Amostra:"
      legendaX <- paste("Distância do núcleo à borda",un)
    }else{
      legendaTitulo <- "Sample:"
      legendaX <- paste("Distance from core to edge",un)
    }

################################################################################
## INÍCIO DA ENTRADA DE DADOS
################################################################################
    #nome do arquivo de dados
    cat("\nDigite o nome do arquivo de dados com a extensão.(E.g. nome_dados.xlsx)\n")
    nome_dados <<- readLines(n=1)

    #extensão do aquivo de dados
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
## FIM DA ENTRADA DE DADOS
################################################################################

    #identificação da amostra
    cat("\nInforme a identificação da amostra.\n")
    amostra <<- readLines(n=1)

    #pergunta sobre eixo y
    cat("\nDeseja alterar os eixos y de cada gráfico? S/N\n")
    eixo = toupper(readLines(n=1))
    if(eixo=="S"||eixo=="Y"){
      mudaEixoY <- 1
    }else{
      mudaEixoY <- 0
    }

    #quantidade de elementos
    elementos <<- length(dados)

    #obtém a identificação da amostra
    amostra <<- paste(legendaTitulo,amostra)

    #cria dataframe que alocará os resultados
    resultados <<- data.frame()
    localiza <<- data.frame()
    media <<- data.frame()
    resultados[1,1] <<- "Posição do ponto de mudança"
    resultados[2,1] <<- "Valor do elemento no ponto de mudança"
    resultados[3,1] <<- "Médias entre as mudanças"
    resultados[4,1] <<- "Valor da distância no ponto de mudança"
    colnames(resultados) <<- c("DESCRIÇÃO")

    #nomes para comparação
    dfnomes <<- data.frame()
    dfnomes[1,1] <<- "dist"

################################################################################
## INÍCIO DA REPETIÇÃO
################################################################################
    #repetição para gerar os gráficos por elemento
    for(a in 2:elementos){

      #obtém o nome do elemento atual
      nome <<- colnames(dados[a])

      #função de identificação de mudança de ponto
      mudanca <<-cpt.mean(as.numeric(unlist(dados[,a])),
                          penalty=penalidade,
                          pen.value=pen.valor,
                          method=metodo,
                          test.stat=teste.stat,
                          class=TRUE,
                          param.estimates=TRUE,
                          minseglen=tamSegmMin)

      #linha inicial da coluna distância
      limiteDistanciaMin <<- 1

      #quantidade de pontos de mudanças
      limitePontos <<- length(mudanca@cpts)

      #nomes para comparação
      tt <<- a-1
      dfnomes[1,tt] <<- paste(tt,": ",nome,sep="")
      colnames(dfnomes)[tt] <<- c(dfnomes[1,tt])

      #alocação dos resultados
      for(o in 1:limitePontos){
        resultados[1,a] <<- paste(resultados[1,a],mudanca@cpts[o])
        localiza[o,tt] <<- mudanca@cpts[o]
        resultados[2,a] <<- paste(resultados[1,a],dados[mudanca@cpts[o],a])
        resultados[3,a] <<- paste(resultados[1,a],mudanca@param.est$mean[o])
        media[o,tt] <<- mudanca@param.est$mean[o]
        resultados[4,a] <<- paste(resultados[1,a],dados[mudanca@cpts[o],1])
      }

      #nome do elemento atual
      colnames(resultados)[a] <<- c(nome)

      if(mudaEixoY==1){
          #determinação do limite inferior do eixo y
          cat("\nInforme o limite inferior do eixo y para o elemento",nome,":\n")
          yMin <- scan(n=1)

          #determinação do limite superior do eixo y
          cat("\nInforme o limite superior do eixo y",nome,":\n")
          yMax <- scan(n=1)

          #geração do gráfico que receberá posteriormente as linhas de mudança
          plot(x = dados[,1], y = dados[,a], type = "l",
               xlab = paste(legendaX),
               ylab = nome,
               ylim = c(yMin,yMax))
          title(main = amostra,
                xlab = paste(legendaX),
                ylab = nome)
      }else{
        #geração do gráfico que receberá posteriormente as linhas de mudança
        plot(x = dados[,1], y = dados[,a], type = "l",
             xlab = paste(legendaX),
             ylab = nome)
        title(main = amostra,
              xlab = paste(legendaX),
              ylab = nome)
      }
      #repetição pela quantidade de pontos de mudanças
      for(i in 1:limitePontos){

          #variável de controle de linhas
          k = 1

          #iniciando o dataframe que aportará as linhas de mudança
          linhas <<- data.frame()

          #repetição até o ponto de mudança
          for(j in limiteDistanciaMin:mudanca@cpts[i]){

              #atribui valor da distancia
              linhas[k,1] <<- dados[j,1]

              #atribui valor do elemento
              linhas[k,2] <<- mudanca@param.est$mean[i]

              #variável de controle de linhas
              k = k+1
          }

          #exibição da linha de mudança gerada
          lines(x = linhas[,1], y = linhas[,2], col = "red", lwd = 3)

          #removedor do objeto contendo a linha
          rm(linhas, envir = .GlobalEnv)

          #elevando a linha de início para o próximo ciclo
          limiteDistanciaMin <<- mudanca@cpts[i]+1
      }
    }
################################################################################
## FIM DA REPETIÇÃO
################################################################################

    #removedor de objetos
    rm(linhas, envir = .GlobalEnv)
    rm(limiteDistanciaMin, envir = .GlobalEnv)
    rm(limitePontos, envir = .GlobalEnv)
    rm(elementos, envir = .GlobalEnv)
    rm(nome, envir = .GlobalEnv)
    rm(configuracao, envir = .GlobalEnv)
    rm(configuracaoParametros, envir = .GlobalEnv)

    #comparação entre elementos
    cat("\nDeseja comparar elementos:\n0) Não comparar\n1) 2 elementos\n2) 3 elementos\n")
    compara <- scan(n=1)

    elementosSelecionados <- c()
    localizacoes <<- localiza
    medias <<- media
    if(!is.null(compara) && compara!=0){
      cat("\nElementos a serem comparados:\n",colnames(dfnomes),"\n")

      if(compara == 1){#dois elementos
        for(i in 1:2){
          cat("\nDigite o elemento",i,":\n")
          elementosSelecionados[i] <- scan(n=1)
          localizacoes[,i] <<- localiza[,elementosSelecionados[i]]
          medias[,i] <<- media[,elementosSelecionados[i]]
          elementosSelecionados[i] <- elementosSelecionados[i]+1
        }
      }else if(compara == 2){#três elementos
        for(i in 1:3){
          cat("\nDigite o elemento",i,":\n")
          elementosSelecionados[i] <- scan(n=1)
          localizacoes[,i] <<- localiza[,elementosSelecionados[i]]
          medias[,i] <<- media[,elementosSelecionados[i]]
          elementosSelecionados[i] <- elementosSelecionados[i]+1
        }
      }
      comparacao(compara, elementosSelecionados, localizacoes, medias, dados, amostra, legendaX)
    }

    #removedor de objetos
    rm(dfnomes, envir = .GlobalEnv)
    rm(media, envir = .GlobalEnv)
    rm(tt, envir = .GlobalEnv)

    write.table(resultados, file='resultados-PCecopesca.csv', sep=',', dec='.', row.names=FALSE)
    return("Processo finalizado.")
  }else{
    return("\nConfigure sua área de trabalho antes de executar o pacote.\n")
  }
}
