perform_interactions <- function(pop, logDir, params) {

  groupsInfo <- data.table::rbindlist(base::lapply(pop, function(agent) {
    data.table::data.table(agentID = agent$agentID, group = agent$group)
    }))[base::order(agentID),]

  pb <- utils::txtProgressBar(min = 0, max = params[["nrOfSnapshots"]], initial = 0, style = 3)
  for (snap in 1:params[["nrOfSnapshots"]]) {
    utils::setTxtProgressBar(pb, snap)
    interactionsLog <- create_interactions_log(params)
    for (i in 1:params[["interactionsPerSnapshot"]]) {
      perform_single_interaction(pop, interactionsLog, snap, groupsInfo, params)
    }
    save_population(pop, extraCols = base::list(snapshot = snap), logDir = logDir)
    save_interactions_log(interactionsLog, extraCols = base::list(snapshot = snap), logDir = logDir)
  }
  close(pb)
}
