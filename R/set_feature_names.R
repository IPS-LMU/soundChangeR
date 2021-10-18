set_feature_names <- function(input.df, cols) {
  stopifnot(all(cols %in% colnames(input.df)))
  input.df %>% setnames(cols, paste0("P", seq_along(cols)))
}
