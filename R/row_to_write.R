row_to_write <- function(agent, producedToken, params) {

  if (base::all(agent$memory$valid)) {
    base::print(base::paste("agent", agent$agentID, "full"))
    rowToWrite <- row_to_overwrite(agent, producedToken, params)
  } else {
    rowToWrite <- base::which(agent$memory$valid == FALSE)[1]
  }
  return(rowToWrite)
}
