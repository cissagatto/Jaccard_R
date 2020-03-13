##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of São Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 1 - UTILS                                                                            #
##################################################################################################




##################################################################################################
# Function to set the folder according to your operational system                                #
##################################################################################################
sistema = c(Sys.info())
if (sistema[1] == "Linux"){
  Folder = paste("/home/", sistema[7], "/Jaccard", sep="")
  setwd(Folder)
} else {
  Folder = paste("C:/Users/", sistema[7], "/Jaccard", sep="")
  setwd(Folder)
}
setwd(Folder)
FolderRoot = Folder

setFolder <- function(){
  retorno = list()
  sistema = c(Sys.info())
  if (sistema[1] == "Linux"){
    Folder = paste("/home/", sistema[7], "/Jaccard", sep="")
    setwd(Folder)
  } else {
    Folder = paste("C:/Users/", sistema[7], "/Jaccard", sep="")
    setwd(Folder)
  }
  FolderRoot = Folder
  retorno$sistema = sistema
  retorno$Folder = Folder
  return(retorno)
}



##################################################################################################
# configuração de notação científica                                                             #
##################################################################################################
options(scipen=30)






##################################################################################################
# Function to set the folder according to your operational system                                #
##################################################################################################
directories <- function(){
  
  retorno = list()
  
  folderResults = paste(FolderRoot, "/Results", sep="")
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
  }
  
  folderDatasets = paste(FolderRoot, "/Datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  }
  
  folderIL = paste(FolderRoot, "/InstancesPerLabels", sep="")
  if(dir.exists(folderIL) == TRUE){
    setwd(folderIL)
    dirIL = dir(folderIL)
    n_IL = length(dirIL)
  } else {
    dir.create(folderIL)
    setwd(folderIL)
    dirIL = dir(folderIL)
    n_IL = length(dirIL)
  }
  
  folderP = paste(FolderRoot, "/Partitions", sep="")
  if(dir.exists(folderP) == TRUE){
    setwd(folderP)
    dirP = dir(folderP)
    n_P = length(dirP)
  } else {
    dir.create(folderP)
    setwd(folderP)
    dirP = dir(folderP)
    n_P = length(dirP)
  }
  
  folderSta = paste(FolderRoot, "/Statistics", sep="")
  if(dir.exists(folderSta) == TRUE){
    setwd(folderSta)
    dirSta = dir(folderSta)
    n_Sta = length(dirSta)
  } else {
    dir.create(folderSta)
    setwd(folderSta)
    dirSta = dir(folderSta)
    n_Sta = length(dirSta)
  }
  
  # folders
  retorno$folderResults = folderResults
  retorno$folderDatasets = folderDatasets
  retorno$folderIL = folderIL
  retorno$folderP = folderP
  retorno$folderSta = folderSta
  
  # arquivos
  retorno$dirResults = dirResults
  retorno$dirDatasets = dirDatasets
  retorno$dirIL = dirIL
  retorno$dirP = dirP
  retorno$dirSta = dirSta
  
  # return numbers
  retorno$n_Results = n_Results
  retorno$n_Datasets = n_Datasets
  retorno$n_IL = n_IL
  retorno$n_P = n_P
  retorno$n_Sta = n_Sta
  
  return(retorno)
  
  gc()
}




##################################################################################################
# FUNCTION FOLD NAMES                                                                            #
# Objective:                                                                                     #
#     Create folder names for each dataset                                                       #
# Parameters:                                                                                    #
#     fileNames: a vector with file names                                                        #
# Return:                                                                                        #
#     folderNames: a vector with folder names                                                    #
#     numberDataSets: number of files within folder                                              #
##################################################################################################
fiCSV <- function(filenames){
  ficsv = filenames
  # retirando "_labels_train.csv" do nome dos arquivos
  j = 0
  for(j in 1:length(ficsv)){
    a = str_length(ficsv[j])
    a = a - 18
    ficsv[j] = str_sub(ficsv[j], end = a)  
    j = j + 1
    gc()
  }
  return(ficsv)
}




##################################################################################################
# FUNCTION INFO DATA SET                                                                         #
# Objective:                                                                                     #
#     Gets the information that is in the "datsets.csv" file.                                    #  
# Parameters:                                                                                    #
#     dataset: the specific dataset                                                              #
# Return:                                                                                        #
#     Everything in the spreadsheet                                                              #
##################################################################################################
infoDataSet <- function(dataset){
  retorno = list()
  retorno$id = dataset$ID
  retorno$ds = dataset$DS
  retorno$name = dataset$Name
  retorno$domain = dataset$Domain
  retorno$instances = dataset$Instances
  retorno$labels = dataset$Labels
  retorno$predictiveAttributes = dataset$PredictiveAttributes
  retorno$attributesTotal = dataset$AttributesTotal
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabStart
  retorno$labEnd = dataset$LabEnd
  retorno$nominal = dataset$Nominal
  retorno$numeric = dataset$Numeric
  retorno$cardinality = dataset$Dardinality
  retorno$density = dataset$Density
  retorno$distinct = dataset$Distinct
  retorno$dimensionality = dataset$Dimensionality
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn
  retorno$xt = dataset$xt
  retorno$yt = dataset$yt
  retorno$gridt = dataset$gridt
  return(retorno)
}