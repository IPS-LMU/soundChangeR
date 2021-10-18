perform_interactions <- function(pop, logDir, params) {

  groupsInfo <- rbindlist(lapply(pop, function(agent) {data.table(agentID = agent$agentID, group = agent$group)}))[order(agentID),]

  for (snap in 1:params[["nrOfSnapshots"]]) {
    interactionsLog <- create_interactions_log(params[["interactionsPerSnapshot"]])
    for (i in 1:params[["interactionsPerSnapshot"]]) {
      perform_single_interaction(pop, interactionsLog, snap, groupsInfo, params)
    }
    save_population(pop, extraCols = list(snapshot = snap), logDir = logDir)
    save_interactions_log(interactionsLog, extraCols = list(snapshot = snap), logDir = logDir)
  }
}
