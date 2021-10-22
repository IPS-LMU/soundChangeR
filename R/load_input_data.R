#' Load input data as data.table
#'
#' @param params list of params
#'
#' @export
#'
#' @examples load_input_data(rlist::list.load("./data/params.yaml"))
load_input_data <- function(params) {
  
  input.df <- base::suppressWarnings(data.table::fread(params[["inputDataFile"]], stringsAsFactors = F))
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