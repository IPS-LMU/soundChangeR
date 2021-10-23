perform_single_interaction <- function(pop, interactionsLog, nrSim, groupsInfo, params) {

  interactionPartners <- choose_interaction_partners(groupsInfo, params)
  
  producer <- pop[[interactionPartners[["prodNr"]]]]
  perceiver <- pop[[interactionPartners[["percNr"]]]]

  pt <- produce_token(producer, params)
  perceive_token(perceiver, pt, interactionsLog, nrSim, params)
}
