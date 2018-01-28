library(tidyverse)
library(pdist)
library(plotly)

Kmeans <- function(nReps, myScatterInput, myCLusterNum, maxIter){
  # Store and update best cluster with min distSum
  # initialize
  bestDist <- 1e10
  bestClusters <- c()

  
  #STEP 1
  # Random cluster assigment
  df_nrows <- nrow(myScatterInput)
  colNames <- names(myScatterInput)
    
  # Repeat for nReps time
  for(i in seq(nReps)){

    clusters_new <- sample(rep(x = seq(myCLusterNum), length.out = df_nrows))
    
    # STEPS 2,3,4,5
    # Initialize while loop
    stop <- FALSE
    iter <- 0

    while (stop == FALSE){
      iter = iter + 1
      # Get cluster centroids
      myScatterInput["Cluster_id"] <- clusters_new
      centroids <- myScatterInput %>% group_by(Cluster_id) %>% summarise_all(funs(mean(., na.rm = T)))

      # Compute distances
      distances <- as.matrix(pdist::pdist(myScatterInput[colNames], centroids[colNames]))

      # C) Get max for each point, new cluster assignments
      clusters_prev <- clusters_new
      clusters_new <- apply(distances, 1, which.min)

      # If max iter or clusters don't change return
      stop <- ifelse((iter == maxIter | identical(clusters_new, clusters_prev)), TRUE, FALSE)
      
    }
    
    #STEP 6, 7, 8
    #Subset m (euclidean distances matrix by clusters_new index and sum)
    distSum <- sum(apply(distances, 1, min))
    
    # Update best_cluster by comparing new total dist
    if (distSum < bestDist){
      bestDist <- distSum
      bestClusters <- clusters_new
      }
  }
  
  # Plot
  if (length(colNames) == 2){
    myScatterInput["Cluster_id"] <- bestClusters
    print(myScatterInput %>%
      ggplot()+
      geom_point(aes(x = myScatterInput[1], y = myScatterInput[2], color = factor(Cluster_id)), show.legend = FALSE)+
      xlab(colNames[1])+
      ylab(colNames[2])+
      labs(title = "Clusters"))  
  } else if(length(colNames) == 3){
    myScatterInput["Cluster_id"] <- bestClusters
    p <- plot_ly(x = myScatterInput[[1]], y = myScatterInput[[2]],
                 z = myScatterInput[[3]], type = "scatter3d", mode = "markers",
                 color = myScatterInput[["Cluster_id"]], 
                 marker = list(size = 2)) 
    print(p)
    }
  
  return(bestClusters)
  
}

