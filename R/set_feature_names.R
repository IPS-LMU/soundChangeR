set_feature_names <- function(input.df, cols) {
  
  base::stopifnot(base::all(cols %in% base::colnames(input.df)))
  input.df %>% data.table::setnames(cols, base::paste0("P", base::seq_along(cols)))
}
