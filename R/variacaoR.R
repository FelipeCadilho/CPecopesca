#' Pacote para executar o changepoint por distância
#'
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
                       tamSegmMin=1){
  cat("\nConfigurou o local do seu arquivo como diretório de trabalho antes de executar este pacote? S/N\n")
  configuracao <<- toupper(readLines(n=1))
  if(configuracao == "S"||configuracao == "Y"){

    cat("\nEssa é a primeira execução após instalação deste pacote? S/N\n")
    pacote = toupper(readLines(n=1))

    if(pacote == "S"|| pacote == "Y"){

      #chama pacote que lê planilha em excel
      install.packages("readxl")
      library("readxl")

      #chama pacote de controle de dados
      install.packages("dplyr")
      library("dplyr")

      #chama pacote de busca por mudança em frequencia
      install.packages("changepoint")
      library("changepoint")

    }else{
      #chama pacote que lê planilha em excel
      library("readxl")

      #chama pacote de controle de dados
      library("dplyr")

      #chama pacote de busca por mudança em frequencia
      library("changepoint")
    }

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
        dados <<- read_excel(nome_dados)
      }else{
        dados <<- read_excel(nome_dados, sheet = planilha)
      }
    }else if(dado == "dataframe"){
      #cria dataframe a partir dos dados do dataframe
      dados <<- nome_dados
    }else if(dado == "csv"){
      if(is.null(separador)){
        dados <<- read.csv(file = nome_dados, header = TRUE)
      }else{
        dados <<- read.csv(file = nome_dados, header = TRUE, sep = separador)
      }
    }else if(dado == "txt"){
      if(is.null(separador)){
          cat("Necessário informar separador de arquivos em txt.")
      }else{
        dados <<- read.delim2(nome_dados, sep=separador)
      }
    }

    #identificação da amostra
    cat("\nInforme a identificação da amostra.\n")
    amostra <<- readLines(n=1)

    #quantidade de elementos
    elementos <<- length(dados)

    #obtém a identificação da amostra
    amostra <<- paste("Sample:",amostra)

    #repetição para gerar os gráficos por elemento
    for(a in 2:elementos){

      #obtém o nome do elemento atual
      nome <<- colnames(dados[a])

      #função de identificação de mudança de ponto
      mudanca <<-cpt.mean(dados[,a],
                          penalty=penalidade,
                          pen.value=pen.valor,
                          method=metodo,
                          test.stat=teste.stat,
                          class=TRUE,
                          param.estimates=TRUE,
                          minseglen=tamSegmMin)

      #quantidade total de linhas da coluna distância
      limiteDistanciaMax <<- length(dados[,1])

      #menor contagem de linhas possível da coluna distância
      limiteDistanciaMin <<- 1

      #quantidade de pontos de mudanças
      limitePontos <<- length(mudanca@cpts)

      #geração do gráfico que receberá posteriormente as linhas de mudança
      plot(x = dados[,1], y = dados[,a], type = "l",
           xlab = "Distance (µm)",
           ylab = nome)
      title(main = amostra,
            xlab = "Distance (µm)",
            ylab = nome)

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
    #removedor de objetos
    rm(linhas, envir = .GlobalEnv)
    rm(limiteDistanciaMin, envir = .GlobalEnv)
    rm(limiteDistanciaMax, envir = .GlobalEnv)
    rm(limitePontos, envir = .GlobalEnv)
    rm(elementos, envir = .GlobalEnv)
    rm(nome, envir = .GlobalEnv)
    rm(configuracao, envir = .GlobalEnv)
    return("Processo finalizado.")
  }else{
    return("\nConfigure sua área de trabalho antes de executar o pacote.\n")
  }
}
