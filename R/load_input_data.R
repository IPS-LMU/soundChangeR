##' Load input data as data.table
#'
#' @param params list of params
#'
#' @export
load_input_data <- function(params) {
  
  if (base::is.null(params[["inputDataFile"]]) || !base::file.exists(params[["inputDataFile"]])) {
    stop("Your input data file does not exist. Please make sure you are entering a correct relative or absolute path to an existing data file with extension .csv or .txt.")
  }
  input.df <- base::suppressWarnings(data.table::fread(params[["inputDataFile"]], stringsAsFactors = F))
  
  if (base::length(base::unique(input.df$speaker)) <= 1) {
    stop("Please only run simulations with two or more agents.")
  }
  
 
  if(params[["dim_cuespace"]] == 1){
    cols <- base::list(params$feature_set1, params$word, params$speaker)
  }
  else{ #params[["dim_cuespace"]] == 2
    cols <- base::list(params$feature_set1, params$feature_set2, params$word, params$speaker)
  }
  # phoneme wird in cols nicht aufegenommen, weil das nur benötigt wird, wenn Flexible Phonology verwendet wird
  if (base::any(base::vapply(cols, base::is.null, TRUE)) ||
      base::length(base::setdiff(base::unlist(cols), base::colnames(input.df))) != 0) {
    stop("Some of the columns you have chosen as features, word, and/or speaker do not exist in the inputDataFile.")
  }
  input.df %>% data.table::setnames(base::c(params[["word"]], params[["speaker"]]), base::c("word", "speaker"))
  

 if (params[["useFlexiblePhonology"]]) {
    input.df$phoneme_set1 <- NA_character_
    input.df$phoneme_set2 <- NA_character_
    params[["subsetPhonemes"]] <- NULL
  } else{
    if (!base::is.null(params[["phoneme_set1"]]) && params[["phoneme_set1"]] %in% base::colnames(input.df)) {
      input.df %>% data.table::setnames(params[["phoneme_set1"]], "phoneme_set1")
    }else{ 
      stop("Please indicate an existing column in inputDataFile for argument 'phoneme_set1' if you do not want to use the flexible phonology module.")
    }
    if(params[["dim_cuespace"]] == 2){
      if (!base::is.null(params[["phoneme_set2"]]) && params[["phoneme_set2"]] %in% base::colnames(input.df)){
      input.df %>% data.table::setnames(params[["phoneme_set2"]], "phoneme_set2")
      }else{
      stop("Please indicate an existing column in inputDataFile for argument 'phoneme_set2' if you do not want to use the flexible phonology module and multiple cue_spaces.")
    }
  }
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



  if (!base::is.null(params[["subsetPhonemes_set1"]])) {
    input.df <- input.df[phoneme_set1 %in% params[["subsetPhonemes_set1"]]]
  }

  if(!base::is.null(params[["subsetPhonemes_set2"]])){
    if(!base::is.null(params[["phoneme_set2"]])){
      input.df <- input.df[phoneme_set2 %in% params[["subsetPhonemes_set2"]]] 
  }else{
    stop("You entered subsetPhonemes_set 2 without supplying feature_set2 and phoneme_set2. Please provide values for feature_set2 and phoneme_set2 in order to use subsetPhoneme_set2.")
  }
  }
  set_feature_names(input.df, params[["feature_set1"]], 1)
  
  if(params[["dim_cuespace"]] == 2){
  set_feature_names(input.df, params[["feature_set2"]], params[["dim_cuespace"]])
  }
 
 
  input.df[, exemplar_set1 := matrix2exemplar(.SD), .SDcols = base::names(input.df) %like% "^P1_[[:digit:]]"]
  
  if(params[["dim_cuespace"]] == 2){
  input.df[, exemplar_set2 := matrix2exemplar(.SD), .SDcols = base::names(input.df) %like% "^P2_[[:digit:]]"]
  }
  # eine neue Spalte mit dem Namen exemplar wird erstellt und bekommt die Werte aus matrix2exemplar(.SD)
  # matrix2exemplar(.SD) bedeutet dass die Funktion mit input.df als Argument aufgrufen wird
  # .SDcols heißt, dass alle Saplten, deren Name mit "P" beginnt zurückgegeben werden (subset)
  # input.df bekommt durch den Aufruf eine neue Spalte Namens exemplar; rest des dataframes bleibt unverändert
  # in exemplar stehen die P werte eines spaces als Liste
  
  return(input.df)

}