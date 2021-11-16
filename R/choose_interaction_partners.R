choose_interaction_partners <- function(groupsInfo, params) {
  
  prodNr <- 1
  percNr <- 1
  
  while (prodNr == percNr) {
    if (base::is.null(params[["interactionPartners"]]) || params[["interactionPartners"]] == "random") {
      prodNr <- base::sample(groupsInfo$agentID, 1, prob = params[["speakerProb"]])
      percNr <- base::sample(groupsInfo$agentID, 1, prob = params[["listenerProb"]])
      
    } else if (params[["interactionPartners"]] == "withinGroups") {
      randomGroup <- base::sample(base::unique(groupsInfo$group), 1)
      prodNr <- base::sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1,
                             prob = params[["speakerProb"]][groupsInfo$group == randomGroup])
      percNr <- base::sample(groupsInfo$agentID[groupsInfo$group == randomGroup], 1,
                             prob = params[["listenerProb"]][groupsInfo$group == randomGroup])
      
    } else if (params[["interactionPartners"]] == "betweenGroups") {
      randomGroups <- base::sample(base::unique(groupsInfo$group), 2)
      randomPercGroup <- randomGroups[1]
      randomProdGroup <- randomGroups[2]
      prodNr <- base::sample(groupsInfo$agentID[groupsInfo$group == randomProdGroup], 1,
                             prob = params[["speakerProb"]][groupsInfo$group == randomProdGroup])
      percNr <- base::sample(groupsInfo$agentID[groupsInfo$group == randomPercGroup], 1,
                             prob = params[["listenerProb"]][groupsInfo$group == randomPercGroup])
      
    } else if (params[["interactionPartners"]] == "selfTalk") {
      prodNr <- base::sample(groupsInfo$agentID, 1, prob = params[["speakerProb"]])
      percNr <- 0
    }
  }
  
  if (params[["interactionPartners"]] == "selfTalk") {
    percNr <- prodNr
  }
  return(base::list(prodNr = prodNr, percNr = percNr))
}
