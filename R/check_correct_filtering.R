

check_correct_filtering <- function(agent, filtered_agent, stem) {
  
agent_to_test = filter_agent_by_stem(agent, stem)

# Exclude the "phonology" (or "phoneme") column for comparison
agent_memory <- agent_to_test$memory[, !("phoneme"), with = FALSE]
filtered_memory <- filtered_agent$memory[, !("phoneme"), with = FALSE]


# Check if the filtered memory matches the agent memory
if (!identical(agent_memory, filtered_memory)) {
  stop("Filtered agent's memory does not match the expected filtered memory from agent!")
}

}