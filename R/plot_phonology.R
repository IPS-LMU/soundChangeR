#' Plot number of sub-phonemes and agreement with canonical phonemes over simulation time
#'
#' @param pop population data frame as loaded by load_pop()
#' @param canonical name of column in pop (added by the user) which contains the canonical phonemes
#' @param params list of parameters as loaded by get_params()
#'
#' @export
plot_phonology <- function(pop, canonical, params) {
  
  if (params$useFlexiblePhonology == FALSE) {
    stop("Use this plot only when useFlexiblePhonology is TRUE.")
  }
  
  groupVar <- base::c("speaker", "group", "run", "snapshot")
  if (base::is.data.frame(pop) && 
      base::all(tidyselect::all_of(groupVar) %in% base::colnames(pop))) {
    
    nPhon <- pop %>% 
      dplyr::group_by_at(tidyselect::all_of(groupVar)) %>% 
      dplyr::summarise(`Number of Sub-Phonemes` = dplyr::n_distinct(phoneme), .groups = "drop")
    
    agree <- pop %>% 
      dplyr::group_by_at(tidyselect::all_of(groupVar)) %>% 
      dplyr::group_modify(~ {
        .x %>% 
          dplyr::select(word, phoneme, tidyselect::all_of(canonical)) %>% 
          base::unique() %>% 
          dplyr::select(phoneme, tidyselect::all_of(canonical)) %>% 
          base::table() %>% 
          NMF::purity() %>% 
          tibble::as_tibble()
      }) %>% 
      dplyr::ungroup() %>%
      dplyr::rename(Agreement = value)
    
    df <- dplyr::inner_join(nPhon, agree, by = tidyselect::all_of(groupVar))
    
    if (base::length(base::unique(df$group)) == 1) {
      df %>% 
        dplyr::group_by(snapshot, run) %>% 
        dplyr::summarise(Agreement = base::mean(Agreement),
                         `Number of Sub-Phonemes` = base::mean(`Number of Sub-Phonemes`), 
                         .groups = "drop") %>%
        tidyr::pivot_longer(cols = base::c(`Number of Sub-Phonemes`, Agreement)) %>% 
        dplyr::mutate(Interactions = base::as.numeric(snapshot)*params$interactionsPerSnapshot) %>% 
        ggplot2::ggplot() + 
        ggplot2::aes(x = Interactions, y = value, group = run) + 
        ggplot2::geom_line() + 
        ggplot2::ylab("") + 
        ggplot2::facet_grid(name~., scales = "free_y", switch = "y") + 
        ggplot2::theme(strip.background.y = ggplot2::element_rect(fill = "white"), 
                       strip.placement = "outside")
    } else {
      df %>% 
        dplyr::group_by(snapshot, run, group) %>% 
        dplyr::summarise(Agreement = base::mean(Agreement),
                         `Number of Sub-Phonemes` = base::mean(`Number of Sub-Phonemes`),
                         .groups = "drop") %>%
        tidyr::pivot_longer(cols = c(`Number of Sub-Phonemes`, Agreement)) %>% 
        dplyr::mutate(Interactions = base::as.numeric(snapshot)*params$interactionsPerSnapshot) %>% 
        ggplot2::ggplot() + 
        ggplot2::aes(x = Interactions, y = value, group = run) + 
        ggplot2::geom_line() + 
        ggplot2::ylab("") + 
        ggplot2::facet_grid(name~group, scales = "free_y", switch = "y") + 
        ggplot2::theme(strip.background.y = ggplot2::element_rect(fill = "white"), 
                       strip.placement = "outside")
    }
    
  } else {
    stop("Please make sure you supply the correct arguments to this function.")
  }
}
