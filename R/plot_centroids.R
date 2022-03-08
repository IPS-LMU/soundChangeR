#' Plot the mean of the acoustic features over simulation time
#'
#' @param pop population data frame as loaded by load_pop()
#' @param Pcols string vector of columns in pop with acoustic features as identified by get_Pcols()
#' @param groupVar string vector (max. length: 4) of columns in pop which are used to group the data and plot it. Default is c("snapshot", "run"). Further variables are colour-coded or plotted in facets.
#' @param params list of parameters as loaded by get_params()
#'
#' @export
plot_centroids <- function(pop, Pcols, groupVar = c("snapshot", "run"), params) {
  
  if (base::is.data.frame(pop) && 
      tidyselect::all_of(groupVar) %in% base::colnames(pop) && 
      tidyselect::all_of(Pcols) %in% base::colnames(pop) && 
      "snapshot" %in% groupVar && 
      base::length(groupVar) < 5) {
    
    df <- pop %>% 
      dplyr::group_by_at(groupVar) %>% 
      dplyr::summarise_at(tidyselect::all_of(Pcols), base::mean, .groups = "drop") %>% 
      tidyr::pivot_longer(cols = tidyselect::all_of(Pcols)) %>% 
      dplyr::mutate(Interactions = base::as.numeric(snapshot)*params$interactionsPerSnapshot)
    
    if (base::length(groupVar) == 1) {
      ggplot2::ggplot(df) + 
        ggplot2::aes_string(x = "Interactions", y = "value") +
        ggplot2::geom_line() + 
        ggplot2::facet_grid(name~., scales = "free_y")
      
    } else if (base::length(groupVar) == 2) {
      if ("run" %in% groupVar) {
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", group = "run") +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(name~., scales = "free_y")
      } else {
        facetVar <- groupVar[groupVar != "snapshot"]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value") +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar)), 
                              scales = "free_y")
      }
      
    } else if (base::length(groupVar) == 3) {
      if ("run" %in% groupVar) {
        facetVar <- groupVar[!groupVar %in% base::c("snapshot", "run")]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", group = "run") +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar)), 
                              scales = "free_y")
      } else {
        facetVar <- groupVar[groupVar != "snapshot"][1]
        colVar <- groupVar[groupVar != "snapshot"][2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar)), 
                              scales = "free_y")
      }
      
    } else if (base::length(groupVar) == 4) {
      if ("run" %in% groupVar) {
        facetVar <- groupVar[!groupVar %in% base::c("snapshot", "run")][1]
        colVar <- groupVar[!groupVar %in% base::c("snapshot", "run")][2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar, 
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar)), 
                              scales = "free_y")
      } else {
        facetVar1 <- groupVar[groupVar != "snapshot"][1]
        facetVar2 <- groupVar[groupVar != "snapshot"][2]
        colVar <- groupVar[groupVar != "snapshot"][3]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar1, "+", facetVar2)), 
                              scales = "free_y")
      }
    }
    
  } else {
    stop("Please make sure you supply the correct arguments to this function.")
  }
}
