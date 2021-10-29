#' Run demo simulation, based on run_simulation_args
#'
#' @export
run_demo_simulation_args <- function() {
  demoInputDataFile <- base::system.file("extdata", "u_fronting.csv", package = "soundChangeR")
  demoFeatures <- c("DCT0", "DCT1", "DCT2")
  demoGroup <- "age"
  demoLabel <- "phoneme"
  demoWord <- "word"
  demoSpeaker <- "spk"
  run_simulation_args(inputDataFile = demoInputDataFile,
                      features = demoFeatures,
                      group = demoGroup,
                      label = demoLabel,
                      initial = demoLabel,
                      word = demoWord,
                      speaker = demoSpeaker)
}