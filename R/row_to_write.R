row_to_write <- function(agent, producedToken, params) {

  if (base::all(agent$memory$valid)) {
    base::print(base::paste("agent", agent$agentID, "full"))
    rowToWrite <- base::sample(base::which(agent$memory$word == producedToken$word), 1)
  } else {
    rowToWrite <- base::which(agent$memory$valid == FALSE)[1]
  }
  return(rowToWrite)
}
