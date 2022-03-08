#' Plot the rejection rate over simulation time
#'
#' @param intLog interactions log data frame as loaded by load_intLog()
#' @param groupVar string vector (max. length: 4) of columns in pop which are used to group the data and plot it. Default is c("snapshot", "run"). Further variables are colour-coded or plotted in facets.
#' @param params list of parameters as loaded by get_params()
#'
#' @export
plot_rejection <- function(intLog, groupVar = c("snapshot", "run"), params) {
  
  if (base::is.data.frame(intLog) && 
      tidyselect::all_of(groupVar) %in% base::colnames(intLog) && 
      "snapshot" %in% groupVar && 
      base::length(groupVar) < 5) {
    
    df <- intLog %>% 
      dplyr::group_by_at(tidyselect::all_of(groupVar)) %>% 
      dplyr::summarise(rejection = 1-(base::sum(accepted)/dplyr::n()), .groups = "drop") %>% 
      dplyr::mutate(Interactions = base::as.numeric(snapshot)*params$interactionsPerSnapshot)
    
    if (base::length(groupVar) == 1) {
      ggplot2::ggplot(df) + 
        ggplot2::aes_string(x = "Interactions", y = "rejection") + 
        ggplot2::geom_line() + 
        ggplot2::ylim(0, 1) + 
        ggplot2::ylab("Rejection Rate")
      
    } else if (base::length(groupVar) == 2) {
      if ("run" %in% groupVar) {
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", group = "run") + 
          ggplot2::geom_line() + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      } else {
        facetVar <- groupVar[groupVar != "snapshot"]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection") + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar, "~.")), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      }
      
    } else if (base::length(groupVar) == 3) {
      if ("run" %in% groupVar) {
        facetVar <- groupVar[!groupVar %in% base::c("snapshot", "run")]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", group = "run") + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar, "~.")), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      } else {
        facetVar <- groupVar[groupVar != "snapshot"][1]
        colVar <- groupVar[groupVar != "snapshot"][2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar) + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar, "~.")), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      }
      
    } else if (base::length(groupVar) == 4) {
      if ("run" %in% groupVar) {
        facetVar <- groupVar[!groupVar %in% base::c("snapshot", "run")][1]
        colVar <- groupVar[!groupVar %in% base::c("snapshot", "run")][2]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar, 
                              group = base::paste0("interaction(", base::paste0(base::c("run", colVar), collapse =  ", "), ")")) + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar, "~.")), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      } else {
        facetVar1 <- groupVar[groupVar != "snapshot"][1]
        facetVar2 <- groupVar[groupVar != "snapshot"][2]
        colVar <- groupVar[groupVar != "snapshot"][3]
        ggplot2::ggplot(df) + 
          ggplot2::aes_string(x = "Interactions", y = "rejection", col = colVar) + 
          ggplot2::geom_line() + 
          ggplot2::facet_grid(stats::as.formula(base::paste(facetVar1, "~", facetVar2)), 
                              scales = "free_y") + 
          ggplot2::ylim(0, 1) + 
          ggplot2::ylab("Rejection Rate")
      }
    }
  } else {
    stop("Please make sure you supply the correct arguments to this function.")
  }
}
