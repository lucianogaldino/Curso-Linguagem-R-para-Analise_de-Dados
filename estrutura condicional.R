###############################################
###    ESTRUTURA CONDICIONAL - if e else    ###
###############################################


x <- 10
if (x < 10) {
  print("x é menor que 10!")
} else {
  print("x é maior ou igual a 10")
}




y <- 21
if (y < 20) {
  print("y é menor que 20!")
} else if (y == 20){
  print("y é igual a 20")
} else {
  print("y é maior que 20")
}



w <- 13
ifelse(w %% 2 == 0, "par", "impar")



nota <- 4.5
if (nota >= 6){
  print('Aprovado')
} else if (nota >= 5 & nota< 6){
  print('Recuperação')
} else {
  print('Reprovado')
}
