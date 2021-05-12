#######################################
###    IMPORTAÇÃO DE DATA FRAMES    ###
#######################################

setwd("C:/Users/Luciano/Desktop/Linguagem_R")

# Arquivo txt

df1 <- read.table("partks.txt")
df1

# Arquivo csv

df2 <- read.csv("mola.csv")
df2

df3 <- read.csv("questoes.csv")

df3 <- read.csv("questoes.csv", encoding = "latin-1")

df3 <- read.csv("questoes.csv", encoding = "iso-8859-1")

df3 <- read.csv("questoes.csv", encoding = "UTF-8")


# Arquivo excel

install.packages("readxl")
library(readxl)

df4 <- read_xlsx("registro.xlsx")


