#' Load input data as data.table
#'
#' @param params list of params
#'
#' @export
load_input_data <- function(params) {
  
  input.df <- base::suppressWarnings(data.table::fread(params[["inputDataFile"]], stringsAsFactors = F))
  cols <- base::list(params$features, params$group, params$label, params$initial, params$word, params$speaker)
  if (base::any(base::vapply(cols, base::is.null, TRUE)) ||
      base::length(base::setdiff(base::unlist(cols), base::colnames(input.df))) != 0) {
    stop("Some of the columns you have chosen as features, group, label, initial, word, and/or speaker do not exist in the inputDataFile.")
  }
  input.df %>% data.table::setnames(base::c(params[["word"]], params[["speaker"]], params[["group"]]), 
                                    base::c("word", "speaker", "group"))
  input.df$initial <- input.df[, params[["initial"]], with = FALSE]
  input.df$label <- input.df[, params[["label"]], with = FALSE]
  if (!base::is.null(params[["subsetSpeakers"]])) {
    input.df <- input.df[speaker %in% params[["subsetSpeakers"]]]
  }
  if (!base::is.null(params[["subsetLabels"]])) {
    input.df <- input.df[label %in% params[["subsetLabels"]]]
  }
  set_feature_names(input.df, params[["features"]])
  input.df[, exemplar := matrix2exemplar(.SD), .SDcols = base::names(input.df) %like% "^P[[:digit:]]"]
  return(input.df)
}