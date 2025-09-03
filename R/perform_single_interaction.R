perform_single_interaction <- function(pop, sub_pop, interactionsLog, nrSim, groupsInfo, params) {

  interactionPartners <- choose_interaction_partners(groupsInfo, params)
  
  producer <- pop[[interactionPartners[["prodNr"]]]]
  perceiver <- pop[[interactionPartners[["percNr"]]]]

  pt <- produce_token(producer, params)
  perceive_token(perceiver, sub_pop, pt, interactionsLog, nrSim, params)
}
