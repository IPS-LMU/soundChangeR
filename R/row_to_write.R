row_to_write <- function(agent, producedToken, params) {

  if (all(agent$memory$valid)) {
    print(paste("agent", agent$agentID, "full"))
    rowToWrite <- row_to_overwrite(agent, producedToken, params)
  } else {
    rowToWrite <- which(agent$memory$valid == FALSE)[1]
  }
  return(rowToWrite)
}
