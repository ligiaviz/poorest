#para limpar a memória
rm(list = ls())

#loading packages
library(tidyverse)
library(readxl)
library(writexl)
library(FactoMineR)
library(factoextra)
library(openxlsx)

setwd("C:/Users/l_viz/OneDrive/ARTIGOS/SUBMETIDOS/SOCIO-ECO PLANNING SCIENCES/DADOS")

getwd()

#para ajustar a língua para português usando a função
Sys.setlocale(category = "LC_ALL", 
              locale = "pt_BR.UTF-8")

#opening the excel files
df <- read_xlsx("./var_subind_cad_mun.xlsx", sheet = 1)

df_dimensoes <- read_xlsx("./var_subind_contexto_mun.xlsx", sheet = 1)

#convert 'num' as character
df$mun <- as.character(df$mun)

#to assign zero to missing values
df[is.na(df)] <- 0

#to exclude the column 'mun'
df_1 <- df[names(df) != "mun"]

#to convert to numeric all the columns
df_1 <- df_1 %>%
  mutate_all(as.numeric)

#to add the value 10 to all columns
df_1 <- df_1 + 10

#to add the code of the municipality
rownames(df_1) <- df$mun

#first Principal Component Analysis (PCA)
variaveis = c()

#function to run the PCA
run_pca <- function(df, pattern) {
  df_aux <- select(df, contains(pattern))
  pca_aux <- PCA(df_aux)
  numero_componentes <- length(pca_aux$eig[,3]) - sum(pca_aux$eig[,3] >= 75) + 1
  variaveis_aux = c()
  for (i in c(1:numero_componentes)){
    variaveis_aux[i] = rownames(pca_aux$var$cor)[which.max(pca_aux$var$cor[,i])]
  }
  
  return(variaveis_aux)
}

for(i in 1:nrow(df_dimensoes)){
  print(paste0('Rodando PCA para a dimens?o', df_dimensoes$Dimens?o[i]))
  variaveis_aux = run_pca(df_1, df_dimensoes$Identificador[i])
  
  inicio = length(variaveis)
  
  for (j in 1:length(variaveis_aux)){
    variaveis[inicio + j] <- variaveis_aux[j]
  }
}

#second PCAdf_2 <- df_1[, variaveis]

pca_2 <- PCA(df_2)

aux = data.frame(pca_2$var[[2]])

media_dim1_pca2 = mean(abs(aux$Dim.1))

variaveis_2 = rownames(pca_2$var$cor)[which(abs(pca_2$var$cor[,1]) >= media_dim1_pca2)]

##to add at least one variable by dimension
for (dimensao in df_dimensoes$Identificador){
  if (sum(grepl(dimensao, variaveis_2)) == 0){
    df_aux = filter(aux, grepl(dimensao, rownames(aux)))
    variaveis_2[length(variaveis_2) + 1] =  rownames(df_aux)[which.max(abs(df_aux[,1]))]
  }
}

#final PCA
df_3 <- df_1[, variaveis_2]

res.pca <- PCA(df_3, ncp = 1)

res <- data.frame(res.pca$ind[[1]])
res <- data.frame(mun = rownames(res), ranking = res$Dim.1)

res$ranking_std <- -(((res$ranking - min(res$ranking))/(max(res$ranking) - min(res$ranking)))*(1 - (-1)) + (-1))

