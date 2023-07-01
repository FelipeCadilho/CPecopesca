#' Comparação entre os gráficos de distancia de elementos
#'
#' @param comparar Opção de comparação, 1 = dois elementos e 2 = três elementos.
#' @param selecao Posição das colunas de cada elemento selecionado.
#' @param localizacoes Base de dados com as posições dos pontos de mudança.
#' @param medias Base de dados com as médias dos pontos de mudança.
#' @param dados Base de dados com as medidas originais.
#' @param legendaTitulo Título do gráfico.
#' @param legendaX Legenda do eixo x.
#'
#' @return
#' @export
#'
#' @examples
comparacao <- function(comparar, selecao, localizacoes, medias, dados, legendaTitulo, legendaX){

    if(comparar==1){#dois elementos
      #posição de cada elemento selecionado
      a <- selecao[1]
      b <- selecao[2]

      #linha inicial da coluna distância
      limiteDistanciaMin <<- 1

      #linha final da coluna distância
      limiteDistanciaMax <<- length(dados[,1])

      #determinação do limite inferior do eixo y
      cat("\nInforme o limite inferior do eixo y:\n")
      yMin <- scan(n=1)

      #determinação do limite superior do eixo y
      cat("\nInforme o limite superior do eixo y:\n")
      yMax <- scan(n=1)

      #geração do gráfico que receberá posteriormente as linhas de mudança
      par(mar = c(5, 4, 4, 4) + 0.25)
      plot(dados[,a]~dados[,1],
           type = "l",
           xlab = legendaX,
           ylab = colnames(dados[a]),
           col = "#336633",
           ylim = c(yMin,yMax))
      par(new=TRUE)
      plot(dados[,b]~dados[,1],
           type="l",
           axes=FALSE,
           ann=FALSE,
           col = "#0666cc",
           ylim = c(yMin,yMax))
      mtext(colnames(dados[b]),
            side=4,
            line=3)
      axis(4)
      title(main = legendaTitulo,
            xlab = legendaX)
      cores = c("red","red")
      for(h in 1:2){

        limitePontos <- length(localizacoes[,h])-length(which(is.na(localizacoes[,h])))
        posicaoMin <- 1
        #repetição pela quantidade de pontos de mudanças
        for(i in 1:limitePontos){

          #variável de controle de linhas
          k = 1

          #iniciando o dataframe que aportará as linhas de mudança
          limiteDistanciaMax <<- localizacoes[i,h]

          #exibição da linha de mudança gerada
          lines(c(dados[posicaoMin,1],dados[limiteDistanciaMax,1]), c(medias[i,h],medias[i,h]), col = cores[h], lwd = 3)

          #removedor do objeto contendo a linha
          rm(linhas, envir = .GlobalEnv)

          #elevando a linha de início para o próximo ciclo
          posicaoMin <- limiteDistanciaMax+1

        }
      }

      cat("\nLinha traçada 1:\n")
      rep <- scan(n=1)
      abline(h=rep,lty=2)
      #text(x = 200, y = rep+0.05,'Maximum')

      cat("\nLinha traçada 2:\n")
      rep <- scan(n=1)
      abline(h=rep,lty=2)
      #text(x = 200, y = rep+0.05,"Minimum")

      cat("\nLinha pontilhada 3:\n")
      rep <- scan(n=1)
      abline(h=rep,lty=3)
      #text(x = 200, y = rep+0.05,"Mean-1*SD")

      #cat("\nMean-2*SD:\n")
      #rep <- scan(n=1)
      #abline(h=rep,lty=2)
      #text(x = 200, y = rep+0.05,"Mean-2*SD")

      cat("\nValor da distância para linha idade 0:\n")
      rep <- scan(n=1)
      abline(v=rep,col="#cccccc")
      text(x = rep+0.03, y = yMax-0.05,"birth")

      cat("\nValor da distância para linha idade 1:\n")
      rep <- scan(n=1)
      abline(v=rep,col="#cccccc")
      text(x = rep+0.03, y = yMax-0.05,"1")

      cat("\nValor da distância para linha idade 2:\n")
      rep <- scan(n=1)
      abline(v=rep,col="#cccccc")
      text(x = rep+0.03, y = yMax-0.05,"2 years old")

      #removedor de objetos
      rm(limiteDistanciaMin, envir = .GlobalEnv)
      rm(limiteDistanciaMax, envir = .GlobalEnv)
      rm(limitePontos, envir = .GlobalEnv)
      rm(comparar, envir = .GlobalEnv)
      rm(selecao, envir = .GlobalEnv)
      rm(localizacoes, envir = .GlobalEnv)
      rm(medias, envir = .GlobalEnv)
      rm(legendaTitulo, envir = .GlobalEnv)
      rm(legendaX, envir = .GlobalEnv)
      #fim da execução
      return()

    }else if(comparar==2){#três elementos

        #posição de cada elemento selecionado
        a <- selecao[1]
        b <- selecao[2]
        c <- selecao[3]

        #linha inicial da coluna distância
        limiteDistanciaMin <<- 1

        #linha final da coluna distância
        limiteDistanciaMax <<- length(dados[,1])

        #determinação do limite inferior do eixo y
        cat("\nInforme o limite inferior do eixo y:\n")
        yMin <- scan(n=1)

        #determinação do limite superior do eixo y
        cat("\nInforme o limite superior do eixo y:\n")
        yMax <- scan(n=1)

        #geração do gráfico que receberá posteriormente as linhas de mudança
        par(mar = c(5, 4, 4, 4) + 0.25)
        plot(dados[,a]~dados[,1],
             type = "l",
             xlab = legendaX,
             ylab = colnames(dados[a]),
             col = "#336633",
             #lty  = 1,
             ylim = c(yMin,yMax))
        par(new=TRUE)
        plot(dados[,b]~dados[,1],
             type = "l",
             axes = FALSE,
             ann  = FALSE,
             col = "#0666cc",
             #lty   = 2,
             ylim = c(yMin,yMax))
        par(new=TRUE)
        plot(dados[,c]~dados[,1],
             type = "l",
             axes = FALSE,
             ann  = FALSE,
             #col = "#933300",
             #lty  = 3,
             ylim = c(yMin,yMax))
        mtext(paste(colnames(dados[b]),",",colnames(dados[c])),
              side=4,
              line=3)
        axis(4)
        title(main = legendaTitulo,
              xlab = legendaX)

      #cores das linhas de mudança
      cores = c("red","red","red")
      #cores = c(1,2,3)

      for(h in 1:3){
        limitePontos <- length(localizacoes[,h])-length(which(is.na(localizacoes[,h])))
        posicaoMin <- 1
        #repetição pela quantidade de pontos de mudanças
        for(i in 1:limitePontos){

          #variável de controle de linhas
          k = 1

          #iniciando o dataframe que aportará as linhas de mudança
          limiteDistanciaMax <<- localizacoes[i,h]

          #exibição da linha de mudança gerada
          lines(c(dados[posicaoMin,1],dados[limiteDistanciaMax,1]), c(medias[i,h],medias[i,h]), col = cores[h], lwd = 3)

          #removedor do objeto contendo a linha
          rm(linhas, envir = .GlobalEnv)

          #elevando a linha de início para o próximo ciclo
          posicaoMin <- limiteDistanciaMax+1

          }
      }

      cat("\nLinha traçada 1:\n")
      rep <- scan(n=1)
      abline(h=rep,lty=2)
      #text(x = 200, y = rep+0.05,'Maximum')

      cat("\nLinha traçada 2:\n")
      rep <- scan(n=1)
      abline(h=rep,lty=2)
      #text(x = 200, y = rep+0.05,"Minimum")

      cat("\nLinha pontilhada 3:\n")
      rep <- scan(n=1)
      abline(h=rep,lty=3)
      #text(x = 200, y = rep+0.05,"Mean-1*SD")

      #cat("\nMean-2*SD:\n")
      #rep <- scan(n=1)
      #abline(h=rep,lty=2)
      #text(x = 200, y = rep+0.05,"Mean-2*SD")

      cat("\nValor da distância para linha idade 0:\n")
      rep <- scan(n=1)
      abline(v=rep,col="#cccccc")
      text(x = rep+0.03, y = yMax-0.05,"birth")

      cat("\nValor da distância para linha idade 1:\n")
      rep <- scan(n=1)
      abline(v=rep,col="#cccccc")
      text(x = rep+0.03, y = yMax-0.05,"1")

      cat("\nValor da distância para linha idade 2:\n")
      rep <- scan(n=1)
      abline(v=rep,col="#cccccc")
      text(x = rep+0.03, y = yMax-0.05,"2 years old")

      #removedor de objetos
      rm(limiteDistanciaMin, envir = .GlobalEnv)
      rm(limiteDistanciaMax, envir = .GlobalEnv)
      rm(limitePontos, envir = .GlobalEnv)
      rm(comparar, envir = .GlobalEnv)
      rm(selecao, envir = .GlobalEnv)
      rm(localiza, envir = .GlobalEnv)
      rm(localizacoes, envir = .GlobalEnv)
      rm(medias, envir = .GlobalEnv)
      rm(legendaTitulo, envir = .GlobalEnv)
      rm(legendaX, envir = .GlobalEnv)
      #fim da execução
      return()
    }else{#opção inválida
      #fim da execução
      return()
    }
}
