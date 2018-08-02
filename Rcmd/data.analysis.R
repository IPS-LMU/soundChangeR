# Examples of data analysis 

# equivalence classes

pop <- readRDS(file.path(logDir, "pop.0.rds"))
equivalenceLabels <- make_equivalence_labels(pop$initial)
get_equivalence_clusters(pop, equivalenceLabels)
