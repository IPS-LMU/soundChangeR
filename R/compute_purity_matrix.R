compute_purity_matrix <- function(fullClusters,params) {

  purity <- base::matrix(0, nrow = base::ncol(fullClusters)-1)
  fullRankCluster_list = list()
      for (r in 2:base::ncol(fullClusters)) { 
        fullRankCluster = reduced_clusters_incidence_matrix(fullClusters, r)
      for(i in seq(1,150)){
        if(is.element(FALSE,rowSums(fullRankCluster) >= rep(1, r)) | !check_for_reasonable_nmf(fullRankCluster, fullClusters)){
          fullRankCluster = reduced_clusters_incidence_matrix(fullClusters, r)
        }else{
          purity[r-1] <- fullRankCluster %>%
          get_reduced_clusters(fullClusters, .) %>%
          compute_purity()
          fullRankCluster_list[[length(fullRankCluster_list )+1]] = fullRankCluster
          break
          }
        if(i == 150){
          purity[r-1] <- fullRankCluster %>%
          get_reduced_clusters(fullClusters, .) %>%
          compute_purity()
          fullRankCluster_list[[length(fullRankCluster_list )+1]] = fullRankCluster
          write_log("After 15 tries no ideal incidence matrix was found. Therefore we choose a matrix we usually would not accept", params)
          
        }
      }
      }
      return(list(purity, fullRankCluster_list))
    }