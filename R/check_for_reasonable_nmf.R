check_for_reasonable_nmf = function(incidenceMatrix, fullWordClusters){

    if (length(unique(rowSums(incidenceMatrix))) == 1){ 
    return(T)
    }
    subphoneme_list = apply(incidenceMatrix , 1, which, simplify = F) 
    for (kombi in subphoneme_list){
      if(length(kombi) > 1){ 
        pot_overlap_col = c() 
        if (length(kombi) == 2){
          for(cluster in kombi){
            pot_overlap_col = c(pot_overlap_col, list(which(fullWordClusters[,cluster] > 0))) 
          }
          overlap = (Reduce(intersect, pot_overlap_col)) 
          if(length(overlap) == 0){
            return(F) 
            break
          }else{
            return(T) 
          }
        }
        else{ 
          redo_nmf = F
          pairings = c() 
          for(i in seq(2, length(kombi)-1)){
            pairs = combn(kombi, i, simplify = F)
            for(pair in pairs){ 
              counterpart = setdiff(kombi, pair)
              pairings[[length(pairings)+1]] = list(pair, counterpart)
            }
          }
          for(pair in pairings){
            subset1 = which(fullWordClusters[,pair[[1]]] %>% as_tibble() %>%  mutate(Summe = rowSums(fullWordClusters[,pair[[1]], drop = F])) %>% .$Summe > 0)
            subset2 = which(fullWordClusters[,pair[[2]]] %>% as_tibble() %>%  mutate(Summe = rowSums(fullWordClusters[,pair[[2]], drop = F])) %>% .$Summe > 0)
            overlap = intersect(subset1, subset2)
            if(length(overlap) == 0){
              redo_nmf = T
              break
            }
          }
        }
      }
    }
    if(redo_nmf){
      return(F)
    }
    else{
      return(T)
    }
  }