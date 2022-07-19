#' Plot the rejection rate over simulation time
#'
#' @param intLog interactions log data frame as loaded by load_intLog()
#' @param groupVar string vector (max. length: 4) of columns in pop which are used to group the data and plot it, e.g. agent group, phoneme, etc. If run is included here, it is supplied to the `group` argument in `aes()`. Further variables are colour-coded or plotted in facets.
#' @param params list of parameters as loaded by get_params()
#'
#' @export
plot_rejection <- function(intLog, groupVar = NULL, params) {
  
  if (base::is.data.frame(intLog) && 
      base::all(tidyselect::all_of(groupVar) %in% base::colnames(intLog)) && 
      base::length(groupVar) < 5) {
    
    fullGrouping <- base::c("snapshot", groupVar) %>% unique()
    partialGrouping <- fullGrouping[fullGrouping != "snapshot"]
    
    df <- intLog %>% 
      dplyr::group_by_at(fullGrouping) %>% 
      dplyr::summarise(rejection = 1-(base::sum(accepted)/dplyr::n()), .groups = "drop") %>% 
      dplyr::mutate(Interactions = base::as.numeric(snapshot)*params$interactionsPerSnapshot)
    
    if (base::length(partialGrouping) == 0) {
      ggplot2::ggplot(df) + 
        ggplot2::aes_string(x = "Interactions", y = "rejection") + 
        ggplot2::geom_line() + 
        ggplot2::ylim(0, 1) + 
        ggplot2::ylab("Rejection Rate")
      
    } else if (base::length(partialGrouping) == 1) {
      if ("run" %in% partialGrouping) {
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", group = "run") + 
          ggplot2::geom_line() + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      } else {
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = partialGrouping) + 
          ggplot2::geom_line() + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      }
      
    } else if (base::length(partialGrouping) == 2) {
      if ("run" %in% partialGrouping) {
        colVar <- partialGrouping[partialGrouping != "run"]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar,
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) +
          ggplot2::geom_line() + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      } else {
        colVar <- partialGrouping[1]
        facetVar <- partialGrouping[2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar) + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar, "~.")), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      }
      
    } else if (base::length(partialGrouping) == 3) {
      if ("run" %in% partialGrouping) {
        colVar <- partialGrouping[partialGrouping != "run"][1]
        facetVar <- partialGrouping[partialGrouping != "run"][2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar,
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar, "~.")), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      } else {
        colVar <- partialGrouping[partialGrouping != "run"][1]
        facetVar1 <- partialGrouping[partialGrouping != "run"][2]
        facetVar2 <- partialGrouping[partialGrouping != "run"][3]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar) + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar1, "~", facetVar2)), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      }
      
    } else if (base::length(partialGrouping) == 4) {
      if ("run" %in% partialGrouping) {
        colVar <- partialGrouping[partialGrouping != "run"][1]
        facetVar1 <- partialGrouping[partialGrouping != "run"][2]
        facetVar2 <- partialGrouping[partialGrouping != "run"][3]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar,
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar1, "~", facetVar2)), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      } else {
        stop("Too many grouping variables, please remove one of them.")
      }
    }
    
  } else {
    stop("Please make sure you supply the correct arguments to this function.")
  }
}
