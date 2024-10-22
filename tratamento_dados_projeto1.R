##################################
###    TRATAMENTO DOS DADOS    ###
##################################

# BAIXAR PACOTES, CASO ELES AINDA N�O ESTEJAM BAIXADOS
if(!require(dplyr)) install.packages("dplyr") 

# CARREGAR PACOTES
library(dplyr)

# BUSCAR DIRET�RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Linguagem_R/dados-covid-sp-master/data")

# ABRIR ARQUIVO
covid_sp <- read.csv('dados_covid_sp.csv', sep = ";")
View(covid_sp)

covid_sp <- read.csv2('dados_covid_sp.csv', sep = ";", encoding="UTF-8")
View(covid_sp)
head(covid_sp)







# Renomeando vari�veis (colunas)
covid_sp_alterado <- rename(covid_sp, municipio = nome_munic)
View(covid_sp_alterado)


covid_sp_alterado <- rename(covid_sp_alterado, data = datahora,
                    rotulo_mapa = map_leg,codigo_mapa = map_leg_s)
View(covid_sp_alterado)

# EXCLUIR UMA COLUNA (POR NOME)
covid_sp_alterado$cod_ra <- NULL

# EXCLUIR UMA COLUNA (POR N�MERO)
covid_sp_alterado <- select(covid_sp_alterado, -c(21))

# EXCLUIR V�RIAS COLUNAS (POR NOME)
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(codigo_ibge, cod_drs))

# EXCLUIR V�RIAS COLUNAS (POR N�MERO)
covid_sp_alterado <- select(covid_sp_alterado, -c(14,15))

covid_sp_alterado <- select(covid_sp_alterado, -c(17:19))


# EXCLUIR UMA LINHA (POR N�MERO)
covid_sp_alterado <- slice(covid_sp_alterado, -c(239660))

covid_sp_alterado <- slice(covid_sp_alterado, -c(239661:239666))

# EXCLUIR V�RIAS LINHAS (POR NOME)
covid_sp_alterado <- covid_sp_alterado %>% filter(municipio!="Ignorado")
View(covid_sp_alterado)







# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(covid_sp_alterado, function(x) sum(is.na(x)))
sapply(covid_sp_alterado, function(x) sum(is.nan(x)))

#Substituir valores missing
if(!require(tidyr)) install.packages("tidyr")
library(tidyr)

covid_sp_alterado2 <- covid_sp_alterado %>% mutate_all(replace_na, 54)
View(covid_sp_alterado2)

### OU

covid_sp_alterado2 <- replace(x = covid_sp_alterado,list = is.na(covid_sp_alterado),
                 values = 54)

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$semana_epidem == 54] <- 2021

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-01' &
                                 covid_sp_alterado2$data <= '2021-01-07'  ] <- 54

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-08' &
                                   covid_sp_alterado2$data <= '2021-01-14'  ] <- 55

covid_sp_alterado2$semana_epidem[covid_sp_alterado2$data >= '2021-01-15' &
                                   covid_sp_alterado2$data <= '2021-01-21'  ] <- 56






#VERIFICA��O DA TIPAGEM DOS ATRIBUTOS (Vari�veis)
# EXISTEM 7 TIPOS B�SICOS:
# character (caracteres)
# integer (n�meros inteiros)
# numeric (n�meros reais)
# logical (falso ou verdadeiro)
# complex (n�meros complexos)
# factor (fator: Sequ�ncia de valores definidos por n�veis)
# date (data)
str(covid_sp_alterado2)
# OU
glimpse(covid_sp_alterado2)

#Transforma��o da tipagem de atributos
covid_sp_alterado2$semana_epidem <- as.integer(covid_sp_alterado2$semana_epidem)
glimpse(covid_sp_alterado2)

covid_sp_alterado2$data <- as.Date(covid_sp_alterado2$data, format ='%Y-%m-%d')
glimpse(covid_sp_alterado2)


# Cria��o de colunas
covid_sp_alterado2["idoso(%)"]<-100*covid_sp_alterado2$pop_60/covid_sp_alterado2$pop
View(covid_sp_alterado2)

#Exporta��o de arquivos
write.table(covid_sp_alterado2, file ="covid_sp_tratado.csv", sep = ",")


