
# Librerias aplicadas
library(readxl)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(psych)
library(ggcorrplot)
library(corrplot)
library(FactoMineR)
library(ggfortify)
library(cluster)    
library(factoextra)
library(mvnormalTest)
library(e1071)
library(moments)
library(plotly)

# Limpiando el workspace
rm(list = ls())

# Cargando los datos
dir_file <- file.choose()
data_1 <- read_excel(dir_file, sheet = "Data", col_types = c("text","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
str(data_1)

# Parte 1/16 --------------------------------------------------------------

# Eligiendo las variables y el año de referencia
data_2 <- data_1 %>%
  filter(`serie Code`=='EG.ELC.ACCS.ZS' | 
         `serie Code`=='SL.UEM.TOTL.NE.ZS' |
         `serie Code`=='SH.ANM.CHLD.ZS' |
           `serie Code`=='NY.GNP.MKTP.KD.ZG' |
           `serie Code`=='SL.EMP.VULN.ZS' |
           `serie Code`=='NV.IND.TOTL.ZS' |
           `serie Code`=='SN.ITK.DEFC.ZS' |
           `serie Code`== 'SL.TLF.CACT.FM.ZS') %>% 
  select(`país Code`,`serie Name`,`2017 [YR2017]`) %>% 
  filter(`país Code`!='LCN' &
         `país Code`!='ATG' &
           `país Code`!='ABW' &
           `país Code`!='BHS' &
           `país Code`!='BRB' &
           `país Code`!='BLZ' &
           `país Code`!='CUB' &
           `país Code`!='CUW' &
           `país Code`!='DMA' &
           `país Code`!='GRD' &
           `país Code`!='GUY' &
           `país Code`!='HTI' &
           `país Code`!='MAF' &
           `país Code`!='CYM' &
           `país Code`!='TCA' &
           `país Code`!='VIR' &
           `país Code`!='VGB' &
           `país Code`!='PRI' &
           `país Code`!='KNA' &
           `país Code`!='VCT' &
           `país Code`!='LCA' &
           `país Code`!='SXM' &
           `país Code`!='SUR' &
           `país Code`!='TTO' &
           `país Code`!='VEN' &
           `país Code`!='JAM')

names(data_2) = c("pais","var", "2017")

# Cambiando de formato Long a formato Wide
data_3 <- spread(data_2, "var", "2017")

# Cambiando el nombre de las columnas/variables
names(data_3) = c("pais","x1","x2","x3","x4","x5","x6","x7","x8")

# Quedandonos con todas las columnas numericas
data_F <- data_3[,2:ncol(data_3)]
rownames(data_F) <- data_3$pais

# Parte 2/16 --------------------------------------------------------------

# Media aritmetica por variables
medias <- apply(data_F,2,mean)

# Varianza por variables
varianzas <- diag(var(data_F))

# Desviacion estandar por variables
d.standar <- apply(data_F,2,sd)

# Asimetria por variables
skewness(data_F)

# Curtosis por variables
kurtosis(data_F)

# Varianza total
S <- cov(data_F)
traza <- function(X){
  if(!is.matrix(X)){
    stop('X tiene que ser una matriz')
  } else if(dim(X)[1]!=dim(X)[2]){
    stop('X tiene que ser una matriz cuadrada')
  } else{
    return(sum(diag(X)))
  }
}

Var_Total <- traza(S)

# Varianza media
Var_media <- Var_Total/ncol(data_F)

# Varianza generalizada
Var_gene <- det(S)

# Varianza efectiva
Var_efec <- Var_gene^(1/8)

# Parte 3/16 --------------------------------------------------------------

# Calcular la matriz de var y cov muestral y la de correlaciones muestral

var_matrix <- var(data_F)

cor_matrix <- cor(data_F)

# Visualizando la estructura de correlacion entre las variables
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_matrix, method="color", col=col(200),  
         type="lower", order="original", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=45,
         insig = "blank", 
         diag=T 
)

# Parte 4/16 --------------------------------------------------------------

# Datos sin escalar y utilizando la matriz de var y cov
pc1 <- princomp(data_F, cor = F)
summary(pc1)
pc1$loadings

# Datos sin escalar y utilizando la matriz de correlaciones
pc2 <- princomp(data_F, cor = T)
summary(pc2)
pc2$loadings

# Datos escalados y utilizando la matriz de var y cov (Elegido para el sig paso)
pc3 <- princomp(scale(data_F), cor = F)
summary(pc3)
pc3$loadings

# Parte 5/16 --------------------------------------------------------------

matriz_loadings <- matrix(pc3$loadings, ncol = 8)
matriz_variables <- matrix(unlist(scale(data_F)), ncol = 8, nrow = 17)

# Calculo de los componentes
pca_scores <- matriz_variables %*% matriz_loadings

# Esto se utilizara en el analisis de conglomerados
pca_scores_df <- data.frame(pca_scores)
row.names(pca_scores_df) <- data_3$pais

# Correlacion entre los dos primeros componentes y las variables
cor(data_F, pca_scores[,1:2])

# Comprobacion de la no correlacion entre los componentes principales
round(cor(pca_scores),2)

# Grafico del codo para ver con cuantos nos quedamos
fviz_screeplot(pc3, addlabels = TRUE, ylim = c(0, 100))

# Parte 6/16 --------------------------------------------------------------

# Primera componente
Comp1 <- pca_scores[,1]
median(Comp1)
var(Comp1)
sd(Comp1)

# Segunda componente
Comp2 <- pca_scores[,2]
median(Comp2)
var(Comp2)
sd(Comp2)

# Parte 7/16 --------------------------------------------------------------

# Biplot de los paises y variables
fviz_pca_biplot(pc3, repel = F,
                col.var = "red", 
                col.ind = "#696969",
                title = "Biplot" 
                )

#Biplot y los clusters
data_rm$group = c("2","4","2","2","1","2","1","1","3","3","1","3","1","4","1","1","2")
row.names(data_rm) = data_3$pais

autoplot(pc3, colour = data_rm$group, label = T, 
         label.size = 4, shape = FALSE)


# Parte 9/16 --------------------------------------------------------------

# Calculamos la matriz de distancias
d <- dist(pca_scores_df[,1:2], method = "euclidean")

# Aplicacion del cluster jerarquico utilizando el enlace completo
hc1 <- hclust(d, method = "complete")


# Aplicacion del cluster jerarquico utilizando el enlace de ward
hc2 <- hclust(d, method = "ward")

# Parte 10/16 -------------------------------------------------------------

# Caso 1
plot(hc1, cex = 1.4, hang = -1)
fviz_dend(hc1, k = 3, rect = T, palette = "lancet", type = "rectangle", horiz = T)

# Caso 2
plot(hc2, cex = 1.4, hang = -1)
fviz_dend(hc2, k = 3, rect = T, palette = "lancet", horiz = T)


# Parte 11/16 -------------------------------------------------------------
# Interpretación de resultados

# Parte 12/16 -------------------------------------------------------------
# Interpretracion de resultados

# Parte 13/16 -------------------------------------------------------------

# Verificamos la normalidad de los datos
mardia(scale(data_F)) # Se aplicara el metodo por verosimilitudes

# Modelo con un solo factor
m1 <- fa(scale(data_F), nfactors = 1, rotate = 'none', fm = 'ml', max.iter = 1000)  
m1$uniquenesses #Uniqueness
m1$communalities #Comunalidades o variabilidad comun
m1$loadings #Matriz de cargas

# Parte 14/16 -------------------------------------------------------------

# Estimación del modelo de var y covarianzas que surge de m1
matriz_loadings <- matrix(m1$loadings)
matriz_error <- diag(m1$uniquenesses)

S_estimada <- matriz_loadings%*%t(matriz_loadings) + matriz_error
var(scale(data_F))
write.xlsx(data.frame(var(scale(data_F))), "rapidooocmmtmrm.xlsx")

# Parte 15/16 -------------------------------------------------------------

# Modelo con dos factores
m2 <- fa(scale(data_F), nfactors = 2, rotate = 'none', fm = 'ml', max.iter = 1000)  

m2$uniquenesses #Uniqueness
m2$communalities #Comunalidades o variabilidad comun
m2$loadings #Matriz de cargas

# Estimación del modelo de var y covarianzas que surge de m2
matriz_loadings_m2 <- matrix(m2$loadings, ncol = 2)
matriz_error_m2 <- diag(m2$uniquenesses)

S_estimada_m2 <- matriz_loadings_m2%*%t(matriz_loadings_m2) + matriz_error_m2
write.xlsx(data.frame(S_estimada_m2), "rapdiOCTMR.xlsx")

# Bonda de ajuste segun el R^2 
# Por las notas se utilizo el det de la matriz de var y cov estimada segun el modelo
# Modelo 1
R2_m1 <- 1 - ((det(matriz_error)/det(S_estimada))^(1/8))

# Modelo 2
R2_m2 <- 1- ((det(matriz_error_m2)/det(S_estimada_m2))^(1/8))

# Funcion para realizar el test de hipotesis y saber cuantos factores hubieran sido mejor
m <- 4
psych::fa
alpha <- 0.05
ftest <- function(ind,m,alpha){
  #m <- 5
  modelo <- psych::fa(scale(ind), nfactors = m, rotate = 'none', fm = 'ml', max.iter = 1000)
  modelo$PVAL
  #modelo$values
  #alpha <- 0.05
  p <- ncol(ind)-1
  n <- nrow(ind)
  
  Co <- var(scale(ind[,2:ncol(ind)]))
  e2 <- 1-modelo$communality
  L <- as.matrix(unclass(modelo$loadings))
  LL_tr <- L %*% t(L)
  uu <- diag(e2)
  Vo <- LL_tr+uu
  ln0 <- log(det(Vo))
  ln1 <- log(det(Co))
  num <- ((n-1)-((2*p+4*m+5)/6))
  est <-  num*(ln0 - ln1)
  aic <-  n*(ln0 - ln1)-((p-m)^(2)-p-m)
  gl <-  (((p-m)^(2))-(p+m))/2
  v_c <- qchisq(1-alpha,gl)
  pv <- pchisq(est,gl,lower.tail = F)
  print(paste0("El p-value es ",pv))
  
}

ftest(scale(data_F), 1, 0.05)
ftest(scale(data_F), 2, 0.05)
ftest(scale(data_F), 3, 0.05)

# Parte 16/16 -------------------------------------------------------------
# Interpretracion de resultados





















                                               





