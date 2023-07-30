# muss ich dann in validate params etwas rausnehmen, weil es hier schon geprüft wird? --> Nein, das wir dort nicht überprüft --> Aber wo dann?
# Warnung schreiben, wenn Nutzer die cuespaces vertauscht hat (z.B. 1 Cuespace, aber features für set 1 und phoneme für set 2)

check_space_compatibility = function(params){
  n_dim_features = 0
  n_dim_phonemes = 0
  for(x in names(params)){
      if (startsWith(x, "phoneme_set") && !is.null(params[[x]])){
        n_dim_phonemes = n_dim_phonemes + 1
      } else if(startsWith(x, "feature_set") && !is.null(params[[x]])){
        n_dim_features = n_dim_features + 1
      }
  }

  if(n_dim_features == n_dim_phonemes){
    if(n_dim_features == 0){
      write_log("Please provide feature_set(s) and phoneme_set(s).", params)
      stop("Oops, something went wrong! Please check the log.txt file in the simulation directory.")
    } else if (n_dim_features == 1) {
       if(is.null(params[["phoneme_set1"]])) {
        params[["phoneme_set1"]] = params[["phoneme_set2"]]
        params["phoneme_set2"] = list(NULL)
        warning("Warning! You entered a value for phoneme_set2 without using phoneme_set1. The sets are now changed. If you want to run the simulation with different settings, please cancel the simulation and correct the input.")
       } 
       if (is.null(params[["feature_set1"]])){
        params[["feature_set1"]] = params[["feature_set2"]]
        params["feature_set2"] = list(NULL) 
        warning("Warning! You entered a value for feature_set2 without using feature_set1. The sets are now changed. If you want to run the simulation with different settings, please cancel the simulation and correct the input.")
       }
       params = set_number_of_cue_spaces(params = params, dim = 1)
    } else{ # 2 Cuespaces werden verwendet --> umschreiben wenn es später mal mehr sind
      params = set_number_of_cue_spaces(params = params, dim = 2)
    }
  }else{
    write_log("Please provide the same number of feature_set(s) and phoneme_set(s).", params)
    stop("Oops, something went wrong! Please check the log.txt file in the simulation directory.")
  }
  return(params)
}


