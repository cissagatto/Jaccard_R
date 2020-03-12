##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of S�o Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 2 - Correlations                                                                     #
##################################################################################################




##################################################################################################
# configura��o de nota��o cient�fica                                                             #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder




##################################################################################################
# configura��o de nota��o cient�fica                                                             #
##################################################################################################
source("utils.R")
source("graficos.R")
source("top.R")




##################################################################################################
# configura��o de nota��o cient�fica                                                             #
##################################################################################################
library("philentropy")
library("stringr")



##################################################################################################
# configura��o de nota��o cient�fica                                                             #
##################################################################################################
options(scipen=30)




##################################################################################################
# Calculando as Correla��es JACCARD para todos os datasets                                       #
##################################################################################################

# nomes dos arquivos da pasta CSV
diretorios = directories()
fileNames = c(diretorios$dirDatasets)

# retirando o ".csv" dos nomes dos arquivos para se tornarem nomes de pastas
fon = fiCSV(fileNames)

setwd(FolderRoot)

# abrindo arquivo de informa��es dos datasets
datasets = data.frame(read.csv("datasets.csv"))

# Executa para todos os datasets que est�o presentes na pasta DATASET
i = 28
for(i in 1:diretorios$n_Datasets){
  
  cat("\nComecando: ", fileNames[i])
  
  a = datasets[i,]
  
  ids = infoDataSet(a)
  
  # mudando para a pasta dos datasets
  setwd(diretorios$folderDatasets)
  
  # abrindo o dataset corrente
  classes = data.frame(read.csv(fileNames[i]))
  
  # necess�rio inverter a matrix para dar o resultado correto
  classes_t = t(classes)
  
  # obtendo os nomes das colunas
  columnsNames = c(colnames(classes))
  
  Folder4 = paste(diretorios$folderResults, "/", fon[i], sep="")
  dir.create(Folder4)
  
  # criando e sentando a pasta espec�fica desta medida
  Folder5 = paste(Folder4, "/", "jaccard", sep="")
  dir.create(Folder5)
  setwd(Folder5)
  
  matrix_correlation = distance(classes_t, method = "jaccard", use.row.names = TRUE)
  write.csv(matrix_correlation, "matrix_correlation.csv")
  
  rownames(matrix_correlation) <- columnsNames
  matrix_correlation2 = as.matrix(matrix_correlation)
  
  matrix_correlation <- reorder_mat_cor(matrix_correlation)
  upper_tri <- get_upper_tri(matrix_correlation)
  melt_mat_cor <- melt(upper_tri, na.rm = TRUE)
  m.ord <- melt_mat_cor[order(melt_mat_cor[,3]),]
  write.csv(m.ord, "melt_order.csv")
  
  mapaDeCalor(fon[i], "jaccard", melt_mat_cor, Folder5)
  Dendrogramas(fon[i], "jaccard", matrix_correlation2, Folder5, columnsNames, ids)
  
  r = top("jaccard", m.ord, columnsNames, Folder5) 
  analise(columnsNames, "jaccard", Folder5) 
  organizeFiles("jaccard", Folder5) 
  
  i = i + 1
  
  rm(classes)
  rm(classes_t)
  rm(matrix_correlation)
  rm(matrix_correlation2)
  
  gc()
  
  cat("\n", fileNames[i], " Finalizado.\n")
}




