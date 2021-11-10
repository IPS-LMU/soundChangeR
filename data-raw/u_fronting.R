library(tidyr)
library(usethis)
library(magrittr)
library(data.table)
library(dtt)
library(dplyr, warn.conflicts = FALSE)

tracks <- data.table::fread("https://www.phonetik.uni-muenchen.de/~jmh/lehre/Rdf/u-fronting_NoLob_F1F2bark_tracks.csv")
tracks %<>%
  dplyr::group_by(sl_rowIdx, bundle, label, speaker, age, word) %>%
  dplyr::summarise(F2_bark = stats::approxfun(times_norm, F2_bark)(base::seq(0, 1, length.out = 100))) %>% 
  dplyr::mutate(times_norm = seq(0, 1, length.out = 100)) %>% 
  tidyr::fill(F2_bark, .direction = "up") %>%
  dplyr::ungroup()

u_fronting <- tracks %>%
  dplyr::group_by(sl_rowIdx, bundle, label, speaker, age, word) %>%
  dplyr::summarise(DCT0 = dtt::dct(F2_bark)[1],
                   DCT1 = dtt::dct(F2_bark)[2],
                   DCT2 = dtt::dct(F2_bark)[3], .groups = "drop") %>% 
  dplyr::arrange(sl_rowIdx) %>%
  dplyr::select(-bundle) %>% 
  dplyr::relocate(label, .after = "word")

data.table::fwrite(u_fronting, "data-raw/u_fronting.csv")
usethis::use_data(u_fronting, overwrite = TRUE)
