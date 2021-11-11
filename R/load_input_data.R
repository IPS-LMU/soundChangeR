#' Load input data as data.table
#'
#' @param params list of params
#'
#' @export
load_input_data <- function(params) {
  
  input.df <- base::suppressWarnings(data.table::fread(params[["inputDataFile"]], stringsAsFactors = F))
  cols <- base::list(params$features, params$group, params$phoneme, params$word, params$speaker)
  if (base::any(base::vapply(cols, base::is.null, TRUE)) ||
      base::length(base::setdiff(base::unlist(cols), base::colnames(input.df))) != 0) {
    stop("Some of the columns you have chosen as features, group, phoneme, word, and/or speaker do not exist in the inputDataFile.")
  }
  input.df %>% data.table::setnames(base::c(params[["word"]], params[["speaker"]], params[["group"]], params[["phoneme"]]), 
                                    base::c("word", "speaker", "group", "phoneme"))
  if (!base::is.null(params[["subsetSpeakers"]])) {
    input.df <- input.df[speaker %in% params[["subsetSpeakers"]]]
  }
  if (!base::is.null(params[["subsetPhonemes"]])) {
    input.df <- input.df[phoneme %in% params[["subsetPhonemes"]]]
  }
  set_feature_names(input.df, params[["features"]])
  input.df[, exemplar := matrix2exemplar(.SD), .SDcols = base::names(input.df) %like% "^P[[:digit:]]"]
  return(input.df)
}
