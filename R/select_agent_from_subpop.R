select_agent_from_subpop <- function(subpop, agent, producedstem){
  
 filtered_agents <- list() 
  for (i in seq_along(subpop)) {
    if (subpop[[i]]$agentID == agent$agentID && subpop[[i]]$stem == producedstem) {
      filtered_agents <- append(filtered_agents, list(subpop[[i]]))  # Add the matching agent to the list
    }
  }
  if (length(filtered_agents) > 1) {
    stop("More than one filtered agent found! This should not happen.")
  }
  
  # Assign the filtered agent if only one match is found
  if (length(filtered_agents) == 1) {
    filtered_agents <- filtered_agents[[1]]
  } else if (length(filtered_agents) == 0) {
    warning("No corresponding sub-agent found for the given agentID and stem.")
    filtered_agent <- NULL
  } else {
    stop("Error: Multiple filtered agents found. Ensure subpop consistency.")
  }
  
  
}