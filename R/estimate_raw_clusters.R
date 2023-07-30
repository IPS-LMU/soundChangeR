estimate_raw_clusters <- function(agent, params) {
 # estimate_rwa_clusters wird nur mit einem cue_spaces aufgerufen, aber es ist nicht bekannt, ob es feature_set1 oder feature_set2 ist
 # wenn es mehrere Cue-Spaces gibt, wird estimate GMM und damit auch estimate rwa-clusters mehrfach aufgerufen (1x pro Cuespace)
# names(agent)[startsWith(names(agent), "feature_set")] --> gibt die feature-Werte *eines* subspaces zurück
#stop(agent[names(agent)[startsWith(names(agent), paste0("feature_set"))]])

# org: mclust::Mclust(base::as.matrix(agent$features)[agent$memory$valid, , drop = FALSE],

     mclust::Mclust(base::as.matrix(agent[names(agent)[startsWith(names(agent), "feature_set")]])[[1]][agent$memory$valid, , drop = FALSE],
                 modelNames = get_model_names(agent, params),
                 verbose = FALSE)
}
