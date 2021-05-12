#########################################
###   ESTRUTURA DOS DADOS - FATORES   ###
#########################################

# Sequência de valores, definidos por níveis, comumente expressa variáveis categóricas.
# Facilita quando se deseja saber a quantidade de cada categoria.

# Vetor
escolaridade <- c("fundamental", "médio", "superior", "médio", "superior", "fundamental")
print (escolaridade)


# Fator
escolaridade_fator <- as.factor(escolaridade)
print (escolaridade_fator)


posicao <- escolaridade[3]
posicao

posicao <- escolaridade_fator[3]
posicao


summary (escolaridade)
summary (escolaridade_fator)


table(escolaridade)

# Tensão elétrica em residências (110V, 220V)

tensao_casas <- c(110, 220, 110, 110, 110, 110, 220)
print(tensao_casas)
summary(tensao_casas)

tensao_casas_fator <- as.factor (tensao_casas)
print(tensao_casas_fator)
summary(tensao_casas_fator)



