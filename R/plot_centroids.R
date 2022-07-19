#' Plot the mean of the acoustic features over simulation time
#'
#' @param pop population data frame as loaded by load_pop()
#' @param Pcols string vector of columns in pop with acoustic features as identified by get_Pcols()
#' @param groupVar string vector (max. length: 4) of columns in pop which are used to group the data and plot it, e.g. agent group, phoneme, etc. If run is included here, it is supplied to the `group` argument in `aes()`. Other variables are colour-coded or plotted in facets.
#' @param params list of parameters as loaded by get_params()
#'
#' @export
plot_centroids <- function(pop, Pcols, groupVar = NULL, params) {
  
  if (base::is.data.frame(pop) && 
      base::all(tidyselect::all_of(groupVar) %in% base::colnames(pop)) && 
      base::all(tidyselect::all_of(Pcols) %in% base::colnames(pop)) && 
      base::length(groupVar) < 5) { # TO DO, also see above
    
    fullGrouping <- base::c("snapshot", groupVar) %>% unique()
    partialGrouping <- fullGrouping[fullGrouping != "snapshot"]
    
    df <- pop %>% 
      dplyr::group_by_at(fullGrouping) %>% 
      dplyr::summarise_at(tidyselect::all_of(Pcols), base::mean, .groups = "drop") %>% 
      tidyr::pivot_longer(cols = tidyselect::all_of(Pcols)) %>% 
      dplyr::mutate(Interactions = base::as.numeric(snapshot)*params$interactionsPerSnapshot)
    
    if (base::length(partialGrouping) == 0) {
      ggplot2::ggplot(df) + 
        ggplot2::aes_string(x = "Interactions", y = "value") +
        ggplot2::geom_line() + 
        ggplot2::facet_grid(name~., scales = "free_y")
    
    } else if (base::length(partialGrouping) == 1) {
      if ("run" %in% partialGrouping) {
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", group = "run") +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(name~., scales = "free_y")
      } else {
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = partialGrouping) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(name~., scales = "free_y")
      }
      
    } else if (base::length(partialGrouping) == 2) {
      if ("run" %in% partialGrouping) {
        colVar <- partialGrouping[partialGrouping != "run"]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar, 
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(name~., scales = "free_y")
      } else {
        colVar <- partialGrouping[1]
        facetVar <- partialGrouping[2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar)), 
                              scales = "free_y")
      }
      
    } else if (base::length(partialGrouping) == 3) {
      if ("run" %in% partialGrouping) {
        colVar <- partialGrouping[partialGrouping != "run"][1]
        facetVar <- partialGrouping[partialGrouping != "run"][2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar, 
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar)), 
                              scales = "free_y")
      } else {
        colVar <- partialGrouping[1]
        facetVar1 <- partialGrouping[2]
        facetVar2 <- partialGrouping[3]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar1, "+", facetVar2)), 
                              scales = "free_y")
      }
      
    } else if (base::length(partialGrouping) == 4) {
      if ("run" %in% partialGrouping) {
        colVar <- partialGrouping[partialGrouping != "run"][1]
        facetVar1 <- partialGrouping[partialGrouping != "run"][2]
        facetVar2 <- partialGrouping[partialGrouping != "run"][3]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "value", col = colVar, 
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) +
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste("name", "~", facetVar1, "+", facetVar2)), 
                              scales = "free_y")
      } else {
        stop("Too many grouping variables, please remove one of them.")
      }
    }
    
  } else {
    stop("Please make sure you supply the correct arguments to this function.")
  }
}
