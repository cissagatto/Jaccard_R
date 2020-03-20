##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 1 - UTILS                                                                            #
##################################################################################################



####################################################
# EXTERNAL LIBRARIES                               #
####################################################
library("stringr")



####################################################
# INTERNAL LIBRARIES                               #
####################################################



##################################################################################################
# configuraÃ§Ã£o de notaÃ§Ã£o cientÃ?fica                                                        #
##################################################################################################
options(scipen=30)



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
# Function to set the folder according to your operational system                                #
##################################################################################################
directories <- function(){
  
  retorno = list()
  
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
  
  folderILS = paste(FolderRoot, "/InstancesPerLabelsSpace", sep="")
  if(dir.exists(folderILS) == TRUE){
    setwd(folderILS)
    dirILS = dir(folderILS)
    n_ILS = length(dirILS)
  } else {
    dir.create(folderILS)
    setwd(folderILS)
    dirILS = dir(folderILS)
    n_ILS = length(dirILS)
  }
  
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
  
  # return folders
  retorno$folderDatasets = folderDatasets
  retorno$folderIL = folderIL
  retorno$folderILS = folderILS
  retorno$folderResults = folderResults
  
  # return files
  retorno$dirDatasets = dirDatasets
  retorno$dirIL = dirIL
  retorno$dirILS = dirILS
  retorno$dirResults = dirResults
  
  # return numbers
  retorno$n_Datasets = n_Datasets
  retorno$n_IL = n_IL
  retorno$n_ILS = n_ILS
  retorno$n_Results = n_Results
  
  return(retorno)
  
  gc()
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
  cat("\n FUNCAO INFO DATA SET \n")
  retorno = list()
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabStart
  retorno$labEnd = dataset$LabEnd
  return(retorno)
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
folderNames <- function(filenames, tipo){
  folderNames = filenames
  # _labels_train.csv
  j = 0
  for(j in 1:length(folderNames)){
    a = str_length(folderNames[j])
    a = a - 17
    folderNames[j] = str_sub(folderNames[j], end = a)  
    j = j + 1
    gc()
  }  
  return(folderNames)
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
fileNamesFinal <- function(filenames){
  fnf = filenames
  j = 1
  for(j in 1:length(fnf)){
    str = paste(fnf[j], ".csv", sep="")
    fnf[j] = str
    j = j + 1
    gc()
  }
  return(fnf)
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
    a = a - 17
    ficsv[j] = str_sub(ficsv[j], end = a)  
    j = j + 1
    gc()
  }
  return(ficsv)
}




