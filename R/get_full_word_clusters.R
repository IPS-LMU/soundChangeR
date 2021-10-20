get_full_word_clusters <- function(mclust.obj, agent, params) {

  agent$memory[valid == TRUE, .(word, cluster = mclust.obj$classification)] %>% 
    base::table() %>% 
    base::unclass()
}
