#######################################################
###    INSTALAÇÃO E CARREGAMENTO DE PACOTES NO R    ###
#######################################################

# O R possui pacotes básicos já baixados e carregados.
# Tem pacotes já baixados, mas que devem ser carregados (pacotes recomendados).
# Possui pacotes para serem baixados e carregados (pacotes contribuídos).

# http://cran.rstudio.com/

# BAIXAR PACOTES, CASO ELES AINDA NÃO ESTEJAM BAIXADOS
install.packages("argo") 

# CARREGAR PACOTES
library(argo)

?argo

# BAIXAR PACOTES, CASO ELES AINDA NÃO ESTEJAM BAIXADOS
if(!require(argo)) install.packages("argo") 

# CARREGAR PACOTES
library(argo)

# REMOVER PACOTES
remove.packages("argo")




