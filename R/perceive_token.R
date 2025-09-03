perceive_token <- function(agent, sub_pop, producedToken, interactionsLog, nrSim, params) {

  if (base::is.null(producedToken)) {
    return()
  }
  

  filtered_agent = list()
  filtered_agent<- select_agent_from_subpop(sub_pop, agent, producedToken$stem)
  
  ### This is for debugging
  check_correct_filtering (agent, filtered_agent, producedToken$stem)

  
  #filtered_agent = filter_agent_by_stem(agent, producedToken$stem)  #### this is an adjustment of mine
  #estimate_GMM(filtered_agent, params) #### this is an adjustment of mine
  
  perceiverPhoneme <- base::unique(filtered_agent$memory$phoneme[filtered_agent$memory$word == producedToken$word & filtered_agent$memory$valid == TRUE]) ### this is an adjustment of mine fintered agent
 
  features <- exemplar2features(producedToken$exemplar, filtered_agent, params) #### this was adjusted as well; recalculation should not make a difference though

  
  #### This loop needs to be adjusted as well !?!

  if (base::length(perceiverPhoneme) == 0) {
    perceiverPhoneme <- base::names(base::which.max(base::table(filtered_agent$memory$phoneme[filtered_agent$memory$valid == TRUE][
      FNN::knnx.index(filtered_agent$features[filtered_agent$memory$valid == TRUE,], features, params[["perceptionOOVNN"]])
    ])))
  }

  #### Here are the most important changes
  memorise <- TRUE
  for (strategy in params[["memoryIntakeStrategy"]]) {
    memorise  <- memory_intake_strategy(strategy, producedToken$exemplar, features, perceiverPhoneme,  filtered_agent, params) #### This shold work ? I added the filtred agent to the return for debuggung which can be removed later
    if (!memorise) break
  }
  
 # if (!isFALSE(memorise_post_max) && !isFALSE(memorise_Mal)) { 
  #  memorise <- TRUE
   # strategy <- "None"
#  }
 # if (isFALSE(memorise_Mal)){
  #  memorise <- FALSE
   # strategy <- "mahalanobisDistance"
  #}
  #if (isFALSE(memorise_post_max) && !isFALSE(memorise_Mal)) {
   # memorise <- FALSE
  #  strategy <- "maxPosteriorProb"
  #}
  
  ### Here we go back to the original non-filtered agent, hence we adjust the  perceiverPhoneme  to the phonology across all words;recalculation should not make a difference though
  perceiverPhoneme_filtered = perceiverPhoneme 
  
  perceiverPhoneme <- base::unique(agent$memory$phoneme[agent$memory$word == producedToken$word & agent$memory$valid == TRUE]) ### this is an adjustment of mine fintered agent
  features <- exemplar2features(producedToken$exemplar, agent, params) #### this was adjusted as well 
  
  
  ### This part is for forgetting; We only apply it to the agent 
  
  if (memorise) {
    if (stats::runif(1) < params[["forgettingRate"]]) {
      candidateRows <- base::which(agent$memory$valid == TRUE & agent$memory$word == producedToken$word)
      
      if (base::length(candidateRows) > 0) {
        candidateRow <- base::sample(candidateRows, 1) 
        candidateWord <- agent$memory$word[candidateRow]
        candidateWord_for_comparison = agent$memory[candidateRow]
        
        if (base::sum(agent$memory$word == candidateWord & agent$memory$valid, na.rm = TRUE) > params[["minTokens"]]) {
          data.table::set(agent$memory, candidateRow, "valid", FALSE)
          set_cache_value(agent, "nForgotten", get_cache_value(agent, "nForgotten") + 1)
        }
      }
    # Forget the same word in the filtered agent
    ### merge the forgotten row with the filtered agent; 
    tolerance <- 1e-6 
    candidateRowsFiltered <- which( sapply(filtered_agent$memory$exemplar, 
                                       function(exemplar) { all(abs(unlist(exemplar) - unlist(candidateWord_for_comparison$exemplar)) < tolerance) }) & filtered_agent$memory$valid == TRUE)
    data.table::set(filtered_agent$memory, candidateRowsFiltered, "valid", FALSE)
    set_cache_value(filtered_agent, "nForgotten", get_cache_value(filtered_agent, "nForgotten") + 1)
    
    }
    ### This referes to the old agent again; 
    
    rowToWrite <- row_to_write(agent, producedToken, params)
    write_memory(agent, params, producedToken, rowToWrite, perceiverPhoneme)
    
    rowToWrite <- row_to_write(filtered_agent, producedToken, params)
    write_memory(filtered_agent, params, producedToken, rowToWrite, perceiverPhoneme_filtered)
    
    set_cache_value(agent, "nAccepted", get_cache_value(agent, "nAccepted") + 1)
    set_cache_value(filtered_agent, "nAccepted", get_cache_value(filtered_agent, "nAccepted") + 1)
    
    ### This is for updating
    if (base::any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr"))) {
      invalidate_cache(agent, "qda")
      invalidate_cache(filtered_agent, "qda") ### this is an adjustment of mine
    }
  } 
  
  
  write_interactions_log(interactionsLog, producedToken, agent, perceiverPhoneme, strategy, features, nrSim, memorise, perceiverPhoneme_filtered)

  
  if (get_cache_value(filtered_agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    update_features(filtered_agent, compute_features(filtered_agent, params)) # Recalculate features
    
    if (base::any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr"))) {
      invalidate_cache(filtered_agent, "qda") # Invalidate QDA cache if needed
    }
  }
  
  # Flexible phonology: Update GMM for filtered_agent and track nGMMUpdates
  if (params[["useFlexiblePhonology"]] && get_cache_value(filtered_agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    estimate_GMM(filtered_agent, params) # Recalculate GMM for filtered_agent
    set_cache_value(filtered_agent, "nGMMUpdates", get_cache_value(filtered_agent, "nGMMUpdates") + 1) # Update filtered_agent's GMM count
  }
  
  # Debugging: Check filtering correctness after updates
  check_correct_filtering(agent, filtered_agent, producedToken$stem)
  
  # Update features and GMM independently for agent
  if (get_cache_value(agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    update_features(agent, compute_features(agent, params)) # Recalculate features for agent
    
    if (base::any(params[["memoryIntakeStrategy"]] %in% c("maxPosteriorProb", "posteriorProbThr"))) {
      invalidate_cache(agent, "qda") # Invalidate QDA cache if needed
    }
  }
  
  # Flexible phonology: Update GMM for agent and track nGMMUpdates
  if (params[["useFlexiblePhonology"]] && get_cache_value(agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
    estimate_GMM(agent, params) # Recalculate GMM for agent
    set_cache_value(agent, "nGMMUpdates", get_cache_value(agent, "nGMMUpdates") + 1) # Update agent's GMM count
    
  }
  
  if (get_cache_value(agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
  #  print(paste("GMM update triggered for agent at nAccepted =", get_cache_value(agent, "nAccepted")))### this is for debugging
  }
  
  if (get_cache_value(filtered_agent, "nAccepted") %% params[["computeGMMsInterval"]] == 0) {
  #  print(paste("GMM update triggered for filtered agent at nAccepted =", get_cache_value(filtered_agent, "nAccepted"))) ### this is for debugging
  }
  
  check_correct_filtering (agent, filtered_agent, producedToken$stem)
  
}
