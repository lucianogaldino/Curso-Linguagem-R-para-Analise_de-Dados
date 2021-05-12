#####################
###    FUNÇÕES    ###
#####################

maior = function (x,y) {
  if (x < y) {
    return (y)
  } else { 
    return (x)
  }
}

x <- 10
y <- 8
maior (x , y)

# OU

maior (12,25)



pitagoras_hipotenusa <- function (cat1, cat2) {
  sqrt (cat1**2+cat2**2)
}

pitagoras_hipotenusa(4,3)

pitagoras_hipotenusa(8,6)





pitagoras = function (cat1, cat2, hip) {
  if (hip == "?") {
    sqrt (cat1**2+cat2**2)
  } else if (cat1 == "?") {
    sqrt (hip**2-cat2**2)
  } else {
    sqrt (hip**2-cat1**2)
  }
}

pitagoras(6,8,"?")




