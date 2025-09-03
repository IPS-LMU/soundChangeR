filter_agent_by_stem <- function(agent, stem) {
  # Create a copy of the agent
  filtered_agent <- base::list(
    
    features = agent$features,
    memory = data.table::copy(agent$memory),
    cache = data.table::copy(agent$cache),
    agentID = agent$agentID, 
    group = agent$group, 
    speaker = agent$speaker
    
    
    
    
  )
  
  # Ensure the stem column is atomic
  if (is.list(filtered_agent$memory$stem)) {
    filtered_agent$memory[, stem := as.character(stem)]
  }
  
  # Ensure the input stem is atomic
  stem <- as.character(stem)
  
  # Filter features and memory based on the stem
  valid_indices <- which(filtered_agent$memory$valid == TRUE & filtered_agent$memory$stem == stem)
  if (length(valid_indices) == 0) {
    stop("No valid tokens found for the given stem.")
  }
  
  # Subset the features and memory
  filtered_agent$features <- filtered_agent$features[valid_indices, , drop = FALSE]
  filtered_agent$memory <- filtered_agent$memory[valid_indices]
  
  
  # Return the filtered agent
  return(filtered_agent)
}