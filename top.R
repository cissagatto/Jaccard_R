##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of SÃ£o Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 4 - Top                                                                          #
##################################################################################################




##################################################################################################
# Setando caminho das pastas                                                                     #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder




##################################################################################################
# configuraÃ§Ã£o de notaÃ§Ã£o cientÃ­fica                                                             #
##################################################################################################
options(scipen=30)


library("reshape2")
library("plyr")
library("dplyr")
library("AggregateR")


# Merging the matrix correlation
merge_matrix <- function(matrix_correlation){
  cat("\nFundindo a matriz")
  matrix_correlation <- round(matrix_correlation,4)
  melt_mat_cor <- melt(matrix_correlation)
  return (melt_mat_cor)
}


# Getting the top of the correlation matrix
get_lower_tri<-function(matrix_correlation){
  cat("\nGet Lower Tri")
  matrix_correlation_1[upper.tri(matrix_correlation)] <- NA
  return(matrix_correlation_1)
}


# Getting the bottom of the correlation matrix
get_upper_tri <- function(matrix_correlation){
  cat("\nGet Upper Tri")
  matrix_correlation[lower.tri(matrix_correlation)]<- NA
  return(matrix_correlation)
}


# Cutting the correlation matrix
cut_matrix <- function(matrix_correlation, measure){
  cat("\nCorta a matriz")
  upper_tri <- get_upper_tri(matrix_correlation)
  melt_mat_cor <- melt(upper_tri, na.rm = TRUE) 
  return(melt_mat_cor)
}


# Reordering the correlation matrix
reorder_mat_cor <- function(matrix_correlation){
  cat("\nReordena a matriz")
  dd <- as.dist((1-matrix_correlation)/2)
  hc <- hclust(dd)
  print(hc)
  matrix_correlation <- matrix_correlation[hc$order, hc$order]
  return(matrix_correlation)
}


# Calculating the top measures
top <- function(measure, melt_mat_cor, columnsNames, folder){
  
  retorno = list()
  
  options(scipen=30)
  
  cat("\n|========== Compute the Top measures ==========|")
  
  setwd(folder)
  
  nomeA = paste(measure, "_melt.csv", sep="")
  write.csv(melt_mat_cor, nomeA)
  
  m = as.matrix(melt_mat_cor)
  x = seq(0.0, 1.0, by=0.1)
  x = 0.0
  k = 1
  l = 1
  i = 0
  j = 0
  contador <- c("Valores")
  write(contador, paste(measure, "limiares.csv", sep="_"), append = TRUE)
  while(x<=1.0){
    cat("\n limiar: ", x)
    cont = 0
    for (i in 1:nrow(m)){
      for (j in 3){
        if((m[i,3] >= x)&&(m[i,3] < 1.0)){
          str = paste(m[i,1], ",", m[i,2], ",", m[i,3], sep="")
          write(str, paste("rank", x, "to_1.csv", sep="_"), append = TRUE)
          cont = cont + 1
        }
      }
    }
    contador = cont
    write(contador, paste(measure, "limiares.csv", sep="_"), append = TRUE)
    x = x + 0.1
  }
  
  arquivo = data.frame(read.csv(nomeA))
  # arquivo = melt_mat_cor
  arquivo2 = arquivo[order(arquivo$Var1, decreasing = FALSE),]
  g = count(arquivo, vars=arquivo$Var1)
  g2 = g[order(g$n, decreasing = FALSE),]
  write.csv(g2, "totalPorRotulo.csv")
  
  retorno$measure = measure
  retorno$melt_mar_cor = melt_mat_cor
  retorno$columnsNames = columnsNames
  retorno$folder = folder
  retorno$totalPorRotulo = g2
  retorno$top = arquivo
  
  cat("\n|========== Top Measures Computed ==========|\n")
  
  return(retorno)
}


analise <- function(columnsNames, measure, folder){
  
  cat("\n|========== START ANALISE ==========|")
  setwd(folder)
  options(scipen=30)
  
  b = data.frame("total", "difZero", "difOne", "equalZero", "equalOne", "0.0_to_1.0", "0.1_to_1.0")
  write.csv(b, "result.csv", append = TRUE)
  
  setwd(folder)
  
  total <- c()
  dif_zero <- c()
  dif_one <- c()
  equal_zero <- c()
  equal_one <- c()
  entre1 = c()
  entre2 = c()
  result = data.frame(total, dif_zero, dif_one, equal_zero, equal_one, entre1, entre2)
  
  archive = paste(measure, "_melt.csv", sep="")
  rank = data.frame(read.csv(archive))
  
  total = nrow(rank)
  
  c1 = data.frame(rank %>% filter(., rank$value != 0))
  dif_zero = nrow(c1)
  
  c2 = data.frame(rank %>% filter(., rank$value != 1))
  dif_one = nrow(c2)
  
  c3 = data.frame(rank %>% filter(., rank$value == 0))
  equal_zero = nrow(c3)
  
  c4 = data.frame(rank %>% filter(., rank$value == 1))
  equal_one = nrow(c4)
  
  c5 = data.frame(rank %>% filter(., rank$value > 0.0 & rank$value < 0.1))
  entre1 = nrow(c5)
  
  c6 = data.frame(rank %>% filter(., rank$value >= 0.1 & rank$value < 1.0))
  entre2 = nrow(c6)
  
  # paste(measure, "result.csv", sep="")
  result <- rbind(result, data.frame(total, dif_zero, dif_one, equal_zero, equal_one, entre1, entre2))
  write.csv(result, "result.csv", append = TRUE)
  
  # Generating files for each measure
  l = length(columnsNames)
  i = 0
  for(i in 1:l){
    test = data.frame(c1 %>% filter(., c1$Var1 == columnsNames[i]))
    write.csv(test, paste("correlacoes_", columnsNames[i], ".csv", sep=""))
    i = i + 1
  }
  cat("\n|========== END ANALISE==========|\n")
}


organizeFiles <- function(measure, folder){
  cat("\n|========== START ORGANIZE FILES ==========|")
  setwd(folder)
  str = paste(measure, "_limiares.csv", sep="")
  total = read.csv(str)
  total = t(total)
  colnames(total) <- c("0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0")
  rownames(total) <- c(measure)
  write.csv(total, paste(measure, "_limiares2.csv", sep=""))
  unlink(str, recursive = TRUE)
  cat("\n|========== END ORGANIZE FILES ==========|\n")
}


# ARRUMAR ISTO AQUI T ERRADO!!
mergeFiles <- function(folderName){
  cosine = data.frame(read.csv("jaccard_limiares2.csv"))
  jaccard = data.frame(read.csv("cosine_limiares2.csv"))
  conditional = data.frame(read.csv("conditional_limiares2.csv"))
  final = rbind(jaccard, cosine, conditional)
  colnames(final) <- c("Medida", "0.0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0")
  write.csv(final, "limiares3.csv")
}
