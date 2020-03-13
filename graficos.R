##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of São Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 3 - Gráficos                                                                          #
##################################################################################################




##################################################################################################
# Setando caminho das pastas                                                                     #
##################################################################################################
sf = setFolder()
setwd(sf$Folder)
FolderRoot = sf$Folder




##################################################################################################
# configuração de notação científica                                                             #
##################################################################################################
options(scipen=30)





##################################################################################################
# configuração de notação científica                                                             #
##################################################################################################
library("ggplot2")
library("dendextend")
library("ape")
library("pvclust")
library("factoextra")
library("corrgram")
library("GGally")
library("ggdendro")
library("cluster")


##################################################################################################
# configuração de notação científica                                                             #
##################################################################################################
source("utils.r")




##################################################################################################
#                                                              #
##################################################################################################
mapaDeCalor <- function(folderName, measure, melt_mat_cor, folder){
  setwd(folder)
  cat("\nCreating Heat Map! \n")
  jpeg("heatmap_1.jpeg",  width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
  ggheatmap <- ggplot(melt_mat_cor, aes(Var2, Var1, fill = round(value, 2))) + geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(0,1), space = "Lab", name = measure) + theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) + coord_fixed()
  print(ggheatmap)
  
  heatmap.plot <- ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 0.5) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))
  print(heatmap.plot)
  dev.off()
  cat("\n")
}



##################################################################################################
#                                                              #
##################################################################################################
Dendrogramas <- function(folderName, measure, matrix_correlation, Folder5, columnsNames, ids){
  
  retorno = list()
  cat("\nCriando Dendrograms!")
  setwd(Folder5)
  metodos = c("average", "single", "complete", "ward.D", "ward.D2", "mcquitty")
  
  metodo = c(0)
  coeficiente = c(0)
  coefHC = data.frame(metodo, coeficiente)
  
  i = 1
  for(i in i:length(metodos)){
    cat("\n\n| Metodo: ", metodos[i], "\n")
    
    folder10 = paste(Folder5, "/", metodos[i], sep="")
    dir.create(folder10)
    setwd(folder10)
    
    FolderG = paste(folder10, "/Graphics", sep="")
    dir.create(FolderG)
    
    FolderC = paste(folder10, "/Clusters", sep="")
    dir.create(FolderC)
    
    FolderT = paste(folder10, "/Tables", sep="")
    dir.create(FolderT)
    
    ############################################################################################################################
    # BIBLIOTECA DENDEXTED
  
    dend <- matrix_correlation %>% as.dist %>% hclust(method = metodos[i]) %>% as.dendrogram
    sink(file="dend_unclass.txt", type="output")
    dend %>% unclass %>% str
    sink()
    write(unlist(dend), paste("dend_", metodos[i], ".csv", sep=""))
    save(dend, file = paste("dend_", metodos[i], ".RData", sep=""))
    
    labels1 = get_leaves_attr(dend, "label")
    labels2 = labels(dend, "label")
    altura_folhas = get_leaves_attr(dend, "height") 
    altura_nos = get_nodes_attr(dend, "height")
    folhas = get_leaves_attr(dend, "leaf") 
    nos = get_nodes_attr(dend, "leaf") 
    folhas_membros = get_leaves_attr(dend, "members") 
    nos_membros1 = get_nodes_attr(dend, "members", include_branches = FALSE, na.rm = TRUE) 
    nos_membros2 = get_nodes_attr(dend, "members") 
    nos_membros3 = get_nodes_attr(dend, "members", simplify = FALSE)
    nos_membros4 = get_nodes_attr(dend, "members", include_leaves = FALSE, na.rm = TRUE) 
    
    write.csv(data.frame(dend %>% get_nodes_attr("height")), "dend_GetNodesAttr1.csv")
    write.csv(data.frame(dend %>% hang.dendrogram %>% get_nodes_attr("height")), "dend_GetNodesAttr2.csv")
    write.csv(data.frame(dend %>% get_nodes_attr("members")), "dend_Members.csv")
    write.csv(data.frame(dend %>% get_nodes_attr("midpoint")), "dend_MidPoint.csv")
    write.csv(data.frame(dend %>% get_nodes_attr("leaf")), "dend_Leaf.csv")
    write.csv(data.frame(dend %>% get_nodes_attr("label")), "dend_label.csv")
    
    setwd(FolderG)
    jpeg("dend_plot_1.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
    print(dend %>% hang.dendrogram(hang = -1) %>% plot )
    dev.off()
    cat("\n")
    
    d1 <- dend %>%
      set("branches_k_color", k=3) %>% 
      set("branches_lwd", c(1.5,1,1.5)) %>%
      set("branches_lty", c(1,1,3,1,1,2)) %>%
      set("labels_colors") %>% 
      set("labels_cex", c(.9,1.2)) %>% 
      set("nodes_pch", 19) %>% 
      set("nodes_col", c("orange", "black", "plum", NA))
    
    jpeg("dend_plot_2.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
    print(plot(d1))
    dev.off()
    
    
    ############################################################################################################
    # HCLUST NORMAL
    
    otter.dendro <- as.dendrogram(hclust(d = as.dist(matrix_correlation), method=metodos[i]))
    save(otter.dendro, file = paste("otter_dendro", metodos[i], ".RData", sep=""))
    
    listaDendograma = dendlist(otter.dendro)
    sink(file="lista_dendograma.txt", type="output")
    listaDendograma %>% unclass %>% str
    sink()
    write(unlist(listaDendograma), paste("listaDend_", metodos[i], ".txt", sep=""))
    
    d = as.dist(matrix_correlation)
    e = as.matrix(d)
    write.csv(e, paste("hc_as_dist_", metodos[i], ".csv", sep=""))
    
    hc = hclust(d, method=metodos[i])
    write.csv(e, paste("hc_", metodos[i], ".csv", sep=""))
    
    dendro <- as.dendrogram(hc)
    sink(file="hc_saida.txt", type="output")
    dendro
    sink()
    write(unlist(dendro), paste("listaDend_", metodos[i], ".txt", sep=""))
    save(dendro, file = paste("dendro_", metodos[i], ".RData", sep=""))
    
    write.csv(toString(dendro), paste("hc_", metodos[i], ".csv", sep=""))
    write.csv(hc$merge, paste("hc_merge_", metodos[i], ".csv", sep=""))
    write.csv(hc$height, paste("hc_height_", metodos[i], ".csv", sep=""))
    write.csv(hc$order, paste("hc_order_", metodos[i], ".csv", sep=""))
    
    metodo = metodos[i]
    coeficiente = coef.hclust(hc)
    coefHC = rbind(coefHC, data.frame(metodo, coeficiente))
    
    dend_data <- dendro_data(dendro, type = "rectangle")
    sink(file="hc_data_saida.txt", type="output")
    dend_data
    sink()
    
    write.csv(dend_data$segments, paste("hc_segments_", metodos[i], ".csv", sep=""))
    write.csv(dend_data$label, paste("hc_label_", metodos[i], ".csv", sep=""))
    
    jpeg("hc_plot_1.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
    print(plot(dendro))
    dev.off()
    cat("\n")
    
    jpeg("radial.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
    print(plot(as.phylo(hc), type = "radial", cex = 0.6, no.margin = TRUE))
    dev.off()
    cat("\n")
    
    jpeg("fan.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
    print(plot(as.phylo(hc), type = "fan", cex = 0.6, no.margin = TRUE))
    dev.off()
    cat("\n")
    
    jpeg("unroot.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
    print(plot(as.phylo(hc), type = "unrooted", cex = 0.6, no.margin = TRUE))
    dev.off()
    cat("\n")
    
    jpeg("cladogram.jpeg", width = 2560, height = 2048, units = "px", res = 300, pointsize = 12, quality = 100)
    print(plot(as.phylo(hc), type = "cladogram", cex = 0.6, no.margin = TRUE))
    dev.off()
    cat("\n")
    
    jpeg("phylogenic.jpeg", width = 2560, height = 2048, units = "px", res = 300, quality = 100)
    print(fviz_dend(hc, cex = 0.6, type = "phylogenic", main="", ggtheme=theme_gray()))
    dev.off()
    cat("\n")
    
    jpeg("hc_plot_2.jpeg", width = 2560, height = 2048, units = "px", res = 300, quality = 100)
    print(plot(dendro))
    print(with(pvclust:::hc2axes(as.hclust(dendro)), 
               text(x.axis, y.axis, round(y.axis, 2),col = "red", adj = c(0.5, 1.5), cex = 0.5)))
    dev.off()
    cat("\n")
    
    #jpeg("correlograma_1.jpeg", width = 2560, height = 2048, units = "px", res = 300, quality = 100)
    #print(corrgram(matrix_correlation2, order=TRUE, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt)) 
    #dev.off()
    #cat("\n")
    
    jpeg("correlograma_2.jpeg", width = 2560, height = 2048, units = "px", res = 300, quality = 100)
    print(ggcorr(matrix_correlation2, label = TRUE, label_color="blue", label_size = 1.5, hjust = 0.75, size = 4, nbreaks = 6))
    dev.off()
    cat("\n")
    
    jpeg("heatmap_matrix.jpeg", width = 2560, height = 2048, units = "px", res = 300, quality = 100)
    print(heatmap(matrix_correlation2, col = cm.colors(256))) 
    dev.off()
    cat("\n")
    
    jpeg("hc_plot_3.jpeg", width = 2560, height = 2048, units = "px", res = 300, quality = 100)
    dendro.plot <- ggdendrogram(data = otter.dendro, rotate = TRUE)
    print(dendro.plot)
    dev.off()
    cat("\n")
 
    
    #################################################################################################################### 
    # clusterização
    k = 1
    for(k in 1:ids$labels){
      cat("\ncluster: ", k)
      
      setwd(FolderC)
      clusters_plot = cutree(hc, k)
      clusters = data.frame(cutree(hc, k))
      names(clusters) = "grupo"
      clusters2 = clusters[order(clusters$grupo, decreasing = FALSE),]
      write.csv(clusters, paste("hc_clusters_", k, ".csv", sep=""))
      
      setwd(FolderT)
      a = data.frame(table(clusters))
      write.table(a, paste("hc_table_", k, ".csv", sep=""), sep=",", col.names = TRUE)
      
      k = k + 1
      gc()
    }
    
    i = i + 1
    gc()
  }
  
  retorno$folderG = FolderG
  retorno$folderT = FolderT
  retorno$folderC = FolderC
  return(retorno)
}






#############################################################################################################
# FUNCTION OPTIMAL NUMBER CLUSTERS                                                                          #
#   Objective: Generate graphics with the optimal number of clusters                                        #
# Parameters:                                                                                               #
#   codes = the codebooks vector                                                                            #
#   grid = total amount of kohonen map neurons                                                              #
#   folder = "C:/Users/elain/Documents/Kohonen/Results/[DATASET]/Graphics"                                               #
#   dist_adj = exponentiate euclidean distance by distance on map                                           #
#   dist_m =  generate distance matrix for codes                                                            #
#   dist_on_map =  generate seperate distance matrix for map location                                       #
#############################################################################################################
OptimalNumberClusters <- function(classes, ids){
  
  cat("\n|========== START FUNCTION: OPTIMAL NUMBER CLUSTERS ==========|\n")
  
  # Ellbow method
  set.seed(123)
  jpeg("NbClustWss.jpeg", width = 1920, height = 1080, units = "px", pointsize = 14, quality = 100) 
  print(factoextra::fviz_nbclust(classes, factoextra::hcut, method = "wss", hc_method = 'ward.D2', 
                                 k.max = ids$labels) + labs(subtitle = "NbClust WSS Ward.D2"))
  dev.off()
  cat("\n")
  
  set.seed(123)
  jpeg("NbClustElbow.jpeg", width = 1920, height = 1080, units = "px", pointsize = 14, quality = 100) 
  print(fviz_nbclust(classes, kmeans,  k.max = ids$labels, method = "wss") +
          geom_vline(xintercept = 4, linetype = 2) + labs(subtitle = "NbClust Elbow WSS"))
  dev.off()
  cat("\n")
  
  # Silhouette method
  set.seed(123)
  jpeg("NbClustSilhouete.jpeg", width = 1920, height = 1080, units = "px", pointsize = 14, quality = 100) 
  print(factoextra::fviz_nbclust(classes, factoextra::hcut, method = "silhouette", hc_method = "ward.D2", 
                                 k.max = ids$labels) + labs(subtitle = "NbClust Silhouete Ward.D2"))
  dev.off()
  cat("\n")
  
  set.seed(123)
  jpeg("NbClustSilhoueteOnly.jpeg", width = 1920, height = 1080, units = "px", pointsize = 14, quality = 100) 
  print(fviz_nbclust(classes, kmeans, k.max = ids$labels, method = "silhouette") + labs(subtitle = "NbClust Silhouette Only"))
  dev.off()
  cat("\n")
  
  # Gap statistic
  set.seed(123)
  jpeg("NbClustGap.jpeg", width = 1920, height = 1080, units = "px", pointsize = 14, quality = 100) 
  gap_stat = cluster::clusGap(classes, FUN = factoextra::hcut, K.max = ids$labels, B = 50, hc_method = "ward.D2")
  factoextra::fviz_gap_stat(gap_stat)
  print(factoextra::fviz_gap_stat(gap_stat) + labs(subtitle = "NbClust Gap Ward.D2"))
  dev.off()
  cat("\n")
  
  # nboot = 50 to keep the function speedy. 
  # recommended value: nboot= 500 for your analysis.
  # Use verbose = FALSE to hide computing progression.
  set.seed(123)
  jpeg("NbClustGapOnly.jpeg", width = 1920, height = 1080, units = "px", pointsize = 14, quality = 100) 
  print(fviz_nbclust(classes, kmeans, k.max = ids$labels, nstart = 25,  method = "gap_stat", nboot = 50)
        + labs(subtitle = "NbClust Gap Only"))
  dev.off()
  cat("\n")
  
  cat("\n|========== END FUNCTION: OPTIMAL NUMBER CLUSTERS ==========|\n")
}


