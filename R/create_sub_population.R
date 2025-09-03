create_sub_population <- function(population, params) {
  sub_population <- list()  # Initialize an empty list for the sub-population
  
  # Iterate over each agent in the original population
  for (agent in population) {
    # Get unique stems for the agent's memory
    unique_stems <- unique(agent$memory[valid == TRUE, stem])
    unique_stems <- unique_stems[!is.na(unique_stems)]  # Remove NA stems
    
    # Filter the agent by each stem
    for (stem in unique_stems) {
      # Create a filtered sub-agent
      sub_agent <- filter_agent_by_stem(agent, stem)
      
      # Ensure sub-agent is valid
      if (is.null(sub_agent) || nrow(sub_agent$memory) == 0) {
        warning(paste("No valid sub-agent created for agentID:", agent$agentID, "and stem:", stem))
        next
      }
      
      # Assign unique ID and stem to the sub-agent
      sub_agent$agentID <- agent$agentID
      sub_agent$stem <- stem
      
      # Optionally estimate GMM
      if (params[["useFlexiblePhonology"]]) {
        estimate_GMM(sub_agent, params)
      }
      
      # Add the sub-agent to the sub-population
      sub_population <- c(sub_population, list(sub_agent))
    }
  }
  
  return(sub_population)
}
