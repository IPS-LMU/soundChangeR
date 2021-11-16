#' Load input data as data.table
#'
#' @param params list of params
#'
#' @export
load_input_data <- function(params) {
  
  if (base::is.null(params[["inputDataFile"]]) || !base::file.exists(params[["inputDataFile"]])) {
    stop("Your input data file does not exist. Please make sure you are entering a correct relative or absolute path to an existing data file with extension .csv or .txt.")
  }
  input.df <- base::suppressWarnings(data.table::fread(params[["inputDataFile"]], stringsAsFactors = F))
  
  cols <- base::list(params$features, params$word, params$speaker)
  if (base::any(base::vapply(cols, base::is.null, TRUE)) ||
      base::length(base::setdiff(base::unlist(cols), base::colnames(input.df))) != 0) {
    stop("Some of the columns you have chosen as features, word, and/or speaker do not exist in the inputDataFile.")
  }
  input.df %>% data.table::setnames(base::c(params[["word"]], params[["speaker"]]), base::c("word", "speaker"))
  
  if (params[["useFlexiblePhonology"]]) {
    input.df$phoneme <- base::as.character()
  } else if (!base::is.null(params[["phoneme"]]) && params[["phoneme"]] %in% base::colnames(input.df)) {
    input.df %>% data.table::setnames(params[["phoneme"]], "phoneme")
  } else {
    stop("Please indicate an existing column in inputDataFile for argument 'phoneme' if you do not want to use the flexible phonology module.")
  }
  
  if (!base::is.null(params[["group"]]) && params[["group"]] %in% base::colnames(input.df)) {
    input.df %>% data.table::setnames(params[["group"]], "group")
  } else {
    if (grepl("between[groups]?", params[["interactionPartners"]], ignore.case = T)) {
      stop("Please indicate an existing column in inputDataFile for argument 'group' if you want to do betweenGroups interactions.")
    } else {
      input.df$group <- "none"
    }
  }
  
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
