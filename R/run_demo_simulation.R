#' Run demo simulation
#'
#' @export
run_demo_simulation <- function() {
  demoInputDataFile <- base::system.file("extdata", "u_fronting.csv", package = "soundChangeR")
  demoFeatures <- c("DCT0", "DCT1", "DCT2")
  demoGroup <- "age"
  demoPhoneme <- "label"
  demoWord <- "word"
  demoSpeaker <- "speaker"
  run_simulation(inputDataFile = demoInputDataFile,
                 features = demoFeatures,
                 group = demoGroup,
                 phoneme = demoPhoneme,
                 word = demoWord,
                 speaker = demoSpeaker)
}
