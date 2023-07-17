# CPecopesca: Support package to evaluate the change point in the concentration of elements detected in calcified structures.
This package aims to identify the change point in the concentrations of detected elements in calcified structures, such as otoliths, 
where the distance between the core and the edge is essential on the x-axis to relate the year and the concentration change.

## Package Installation Instructions
### Prerequisites to Run the Package
* It is recommended to use **RStudio** preferably.
* Make sure you have the `devtools` or `remotes` packages properly installed.
### How to Install
* If you have the `devtools` package installed, use the following command:
>devtools::install_github("FelipeCadilho/CPecopesca")

* If you have the `remotes` package installed, use the following command:
>remotes::install_github("FelipeCadilho/CPecopesca")

## Prerequisites to Run the Package
### Database
* The data file must contain distance data in the first column and elements in the subsequent columns.
* The file can have the extensions `.xls`, `.xlsx`, `.csv`, `.txt`, or be of type `data.frame` in R.
* It is necessary to inform the column separator character for data files with extensions `.csv` and `.txt`.

## Step-by-step to Execute the Package
1. Set the working directory in RStudio to the directory where the data file is located.
  * Set the directory using the `setwd()` command or through the Files panel in the `Files` tab.
>Example 1: setwd("C:/User/Usuário da Máquina/Documents")

>Example 2: https://youtu.be/QzSV8wvA1Do

2. To load the package functions, execute the command `library(CPecopesca)` in RStudio.
3. Run the `CPecopesca()` function to initiate the analysis.
4. Answer the program's questions to customize the analysis according to your needs.
5. Upon completion, the function creates a results file in .csv format in the working directory.

# CPecopesca: Pacote de apoio para avaliar o ponto de mudança na concentração de elementos detectados em estruturas calcificadas.
Este pacote visa identificar o ponto de mudança nas concentrações de elementos detectados em estruturas calcificadas, como otólitos,
onde a distância entre o núcleo e a borda é essencial no eixo x para relacionar o ano e a mudança de concentração.

## Instruções de instalação do pacote
### Pré-requisitos para executar o pacote
* É recomendado utilizar o **RStudio** preferencialmente.
* Certifique-se de ter os pacotes `devtools` ou `remotes` devidamente instalados.
### Como instalar
* Caso tenha o pacote `devtools` instalado, utilize o seguinte comando:
>devtools::install_github("FelipeCadilho/CPecopesca")

* Caso tenha o pacote `remotes` instalado, utilize o seguinte comando:
>remotes::install_github("FelipeCadilho/CPecopesca")

## Pré-requisitos para executar o pacote
### Base de dados
* O arquivo de dados deve conter os dados de distância na primeira coluna e os elementos nas colunas subsequentes.
* O arquivo pode ter as extensões `.xls`, `.xlsx`, `.csv`, `.txt` ou ser do tipo objeto `data.frame` do R.
* É necessário informar o caractere separador de colunas do arquivo de dados com as extensões `.csv` e `.txt`.

## Passo a passo para executar o pacote
1. Configure a pasta de trabalho no RStudio para o diretório onde o arquivo de dados está localizado.
    - Configure o diretório usando o comando `setwd()` ou pelo painel de visualização na aba `Files`.
>Exemplo 1: setwd("C:/User/Usuário da Máquina/Documents")

>Exemplo 2:https://youtu.be/lnoQcMe63oA?t=156
2. Para carregar as funções do pacote, execute o comando `library(CPecopesca)` no RStudio.
3. Execute a função `CPecopesca()` para iniciar a análise.
4. Responda às perguntas do programa para personalizar a análise de acordo com suas necessidades.
5. Ao finalizar, a função cria um arquivo de resultados em formato .csv no diretório de trabalho.


