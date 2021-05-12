############################################
###    EXPLORAÇÃO E ANÁLISE DOS DADOS    ###
############################################

# CARREGAR PACOTES
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)

# BUSCAR DIRETÓRIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Linguagem_R/dados-covid-sp-master/data")

# ABRIR ARQUIVO
covid_sp_tratado <- read.csv2('covid_sp_tratado.csv', sep = ";", encoding="UTF-8")
View(covid_sp_tratado)

covid_sp_tratado <- read.csv2('covid_sp_tratado.csv', sep = ",")
View(covid_sp_tratado)

glimpse(covid_sp_tratado)

covid_sp_tratado <- read.csv('covid_sp_tratado.csv', sep = ",")
View(covid_sp_tratado)

glimpse(covid_sp_tratado)

covid_sp_tratado$data <- as.Date(covid_sp_tratado$data, format ='%Y-%m-%d')
glimpse(covid_sp_tratado)

covid_sp_tratado$idoso <- as.numeric(covid_sp_tratado$idoso)
glimpse(covid_sp_tratado)

# Excluir coluna idoso(%)
covid_sp_tratado <- select(covid_sp_tratado, -c(18))

# Renomeando a coluna idoso
covid_sp_tratado <- rename(covid_sp_tratado, porcentagem_idoso = idoso)









# Filtro por linha (cidade)

# Campinas
covid_campinas <- covid_sp_tratado %>% filter(municipio=="Campinas")
View(covid_campinas)

covid_campinas["dens_demografica"] <- covid_campinas$pop/covid_campinas$area
View(covid_campinas)

covid_campinas["area"] <- covid_campinas$area/100

covid_campinas["dens_demografica"] <- covid_campinas$pop/covid_campinas$area
View(covid_campinas)

# Guarulhos
covid_guarulhos <- covid_sp_tratado[which(covid_sp_tratado$municipio=="Guarulhos"), ]

covid_guarulhos["area"] <- covid_guarulhos$area/100

covid_guarulhos["dens_demografica"] <- covid_guarulhos$pop/covid_guarulhos$area
View(covid_guarulhos)








### ANÁLISES ESTATÍSTICAS

# Medidas de centralidade

# Média
mean(covid_campinas$obitos_novos)
mean(covid_campinas$casos_novos)

summarise_at(covid_campinas, vars(obitos_novos, casos_novos), mean)

mean(covid_guarulhos$obitos_novos)
mean(covid_guarulhos$casos_novos)

# Média móvel
plot(covid_campinas$data,covid_campinas$casos_mm7d, title("MÉDIA MÓVEL"), col = "red")
plot(covid_campinas$data,covid_campinas$obitos_mm7d, title("MÉDIA MÓVEL"), col = "purple")




# Mediana
median(covid_campinas$obitos_novos)
median(covid_campinas$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), median)

median(covid_guarulhos$obitos_novos)
median(covid_guarulhos$casos_novos)


# Moda

# Criando uma função
moda <- function(m) {
  valor_unico <- unique(m) #Busca o valor único para a coluna.
  valor_unico[which.max(tabulate(match(m, valor_unico)))] #tabular (contabilizar quantas vezes o valor único aparece) e buscar o maior valor
}

# Obtenção da moda
moda(covid_campinas$obitos_novos)
moda(covid_campinas$casos_novos)

summarise_at(covid_campinas, vars(obitos_novos, casos_novos), moda)

moda(covid_guarulhos$obitos_novos)
moda(covid_guarulhos$casos_novos)










covid_julho_campinas <- covid_campinas %>% filter(mes==7)
moda(covid_julho_campinas$obitos_novos)
moda(covid_julho_campinas$casos_novos)
summarise_at(covid_julho_campinas, vars(obitos_novos, casos_novos), moda)

mean(covid_julho_campinas$obitos_novos)
mean(covid_julho_campinas$casos_novos)

# Histograma

hist(covid_julho_campinas$obitos_novos, col="blue")
hist(covid_julho_campinas$casos_novos, col="red")

hist(covid_campinas$obitos_novos, col="blue")
hist(covid_campinas$casos_novos, col="red")

hist(covid_guarulhos$obitos_novos, col="green")
hist(covid_guarulhos$casos_novos, col="yellow")










# Medidas de posição

# Mínimo
min(covid_campinas$obitos_novos)
min(covid_campinas$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), min)

min(covid_guarulhos$obitos_novos)
min(covid_guarulhos$casos_novos)


# Máximo
max(covid_campinas$obitos_novos)
max(covid_campinas$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), max)

max(covid_guarulhos$obitos_novos)
max(covid_guarulhos$casos_novos)

# Amplitude Total
range(covid_campinas$obitos_novos)
range(covid_campinas$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), range)

range(covid_guarulhos$obitos_novos)
range(covid_guarulhos$casos_novos)

# Quartis
quantile(covid_campinas$obitos_novos)
quantile(covid_campinas$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), quantile)

quantile(covid_guarulhos$obitos_novos)
quantile(covid_guarulhos$casos_novos)

# Amplitude Interquartil
IQR(covid_campinas$obitos_novos)
IQR(covid_campinas$casos_novos)
summarise_at(covid_campinas, vars(obitos_novos, casos_novos), IQR)

IQR(covid_guarulhos$obitos_novos)
IQR(covid_guarulhos$casos_novos)











summary(covid_campinas$obitos_novos)
summary(covid_campinas$casos_novos)

summary(covid_guarulhos$obitos_novos)
summary(covid_guarulhos$casos_novos)


# Box Plot
summary(covid_julho_campinas$obitos_novos)
boxplot(covid_julho_campinas$obitos_novos)

summary(covid_julho_campinas$casos_novos)
boxplot(covid_julho_campinas$casos_novos)

summary(covid_campinas$casos_novos)
boxplot(covid_campinas$casos_novos)

summary(covid_guarulhos$casos_novos)
boxplot(covid_guarulhos$casos_novos)

# Tratando os outliers

# Identificando e excluindo todos os outliers
covid_guarulhos %>% identify_outliers(casos_novos)
outliers = c(boxplot.stats(covid_guarulhos$casos_novos)$out)
covid_guarulhos_sem_outliers <- covid_guarulhos[-c(which(covid_guarulhos$casos_novos %in% outliers)),  ]
boxplot(covid_guarulhos_sem_outliers$casos_novos)


# Excluindo alguns outliers
covid_campinas %>% identify_outliers(casos_novos)
covid_campinas_sem_outliers<-covid_campinas %>% filter(data != "2020-06-19")
boxplot(covid_campinas_sem_outliers$casos_novos)


# O summary resulta em resumo estatístico
# de todas as variáveis numéricas/inteiras
summary (covid_guarulhos)





# Medidas de Dispersão

# Variância
var(covid_campinas$obitos_novos)
var(covid_campinas$casos_novos)

var(covid_guarulhos$obitos_novos)
var(covid_guarulhos$casos_novos)

var(covid_julho_campinas$obitos_novos)
var(covid_julho_campinas$casos_novos)


# Desvio padrão
sd(covid_campinas$obitos_novos)
sd(covid_campinas$casos_novos)

sd(covid_guarulhos$obitos_novos)
sd(covid_guarulhos$casos_novos)

sd(covid_julho_campinas$obitos_novos)
sd(covid_julho_campinas$casos_novos)









# TESTES DE NORMALIDADE

# Existem 4 testes de normalidade principais (numéricos) e dois testes gráficos:
# Shapiro-Wilk (limite de 5000 amostras)
# Anderson-Darling
# Kolmogorov_Smirnov
# Cramer-Von Mises
# Histograma
# QQplot

# Nível de significância DE 0,05(5%) ou nível de confiança de 95%(MAIS UTILIZADO):
# Quando o parâmetro p > 0,05 (distribuição normal).


if(!require(nortest)) install.packages("nortest")
library(nortest)

#Histograma
hist(covid_campinas$casos_novos, probability=T, col="blue")
lines(density(covid_campinas$casos_novos) , col="red")

# QQPLOT (GRÁFICO DE DISTRIBUIÇÃO NORMAL)
qqnorm(covid_campinas$casos_novos)
qqline(covid_campinas$casos_novos)

# Shapiro-Wilk
shapiro.test(covid_campinas$casos_novos)

# Anderson-Darling
ad.test(covid_campinas$casos_novos)

# Kolmogorov_Smirnov
ks.test(covid_campinas$casos_novos, pnorm)
lillie.test(covid_campinas$casos_novos)

#Cramer-Von Mises
cvm.test(covid_campinas$casos_novos)













# CORRELAÇÃO LINEAR
# method: "pearson" para dados paramétricos(normalidade e homocedasticidade))
#         "spearman" (volume grande de dados não paramétricos)
#         "kendall" (volume pequeno de dados não paramétricos)

plot(covid_campinas$casos,covid_campinas$obitos)
cor(covid_campinas$casos,covid_campinas$obitos,method = "spearman")

regressao <- lm(formula= obitos ~ casos, data=covid_campinas) #modelo de regressão
regressao$coefficients
summary(regressao)

### Equação: obitos=51.67+0,0337*casos

### Coeficiente de determinação (ajustado): 0,9832







### GRÁFICO DE LINHA COM AJUSTE DE RETA  COM GGPLOT2

if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(ggpubr)) install.packages("ggpubr") #equação da reta no gráfico
library(ggpubr)

ggplot(data = covid_campinas, mapping = aes(x = casos, y = obitos)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                            sep = "*plain(\" ,   \")~~")), label.x = 15000,
                            label.y = 1800) +
  theme_gray()


if(!require(corrplot)) install.packages("corrplot")                               
library(corrplot) # gráfico de correlação 

matriz_corr <- cor(covid_campinas[5:13], method = "spearman")
View(matriz_corr)

corrplot(matriz_corr, method = "color")
corrplot(matriz_corr, method="color", 
         type="full", order="original", 
         addCoef.col = "black", # adiciona o coeficiente à matriz
         tl.col="black", tl.srt=45, # cor e rotação do nome das variáveis
)



# GRÁFICOS LINEARES POR CIDADES

covid_cidades<-covid_sp_tratado %>% filter (municipio  %in% c("Campinas", "Guarulhos", "Sorocaba"))
View(covid_cidades)

ggplot(covid_cidades, aes(x = casos, y = obitos, color = municipio)) +
  geom_line() +
  labs(title = "Evolução dos óbitos em função dos casos de COVID",
       x = "Casos",
       y = "Óbitos") +
  theme_classic()




