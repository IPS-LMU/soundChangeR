set_feature_names <- function(input.df, cols, dim) {
  base::stopifnot(base::all(cols %in% base::colnames(input.df)))
  input.df %>% data.table::setnames(cols, base::paste0("P",dim, "_", base::seq_along(cols)))
}
