save_sub_population <- function(pop, extraCols = list(condition = "x"), logDir) {
  
  base::dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  base::saveRDS(convert_pop_list_to_dt(pop, extraCols),
                file = base::file.path(logDir, base::paste("sub_pop", base::unlist(extraCols), "rds", sep = "."))
  )
  
 # base::saveRDS(data.table::rbindlist(base::lapply(pop, function(agent) {agent$cache}), use.names = TRUE, idcol = "agentID"),
  #              file = base::file.path(logDir, base::paste("sub_pop_cache", base::unlist(extraCols), "rds", sep = "."))
#  )
  
  base::saveRDS(
    data.table::rbindlist(
      base::lapply(seq_along(pop), function(idx) {
        agent <- pop[[idx]]
        # Set agentID in cache to match the list index (idx)
        agent$cache[, originalfilteredAgentID := idx]
        # Add a column for the original agent's ID for reference
        agent$cache[, agentID := agent$agentID]
        
        return(agent$cache)
      }),
      use.names = TRUE,
     # idcol = "agentID" # Automatically includes an `agentID` column if needed
    ),
    file = base::file.path(logDir, base::paste("sub_pop_cache", base::unlist(extraCols), "rds", sep = "."))
  )
  
}
