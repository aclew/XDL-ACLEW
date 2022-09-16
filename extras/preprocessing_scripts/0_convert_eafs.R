library(phonfieldwork)
library(tidyverse)

files <- list.files("raw/eaf/", "*.eaf")

for (file in files) {
  txt.eaf <- eaf_to_df(paste0("./raw/eaf/", file)) %>%
    mutate(speaker = str_extract(tier_name, "([MFUE][ACUE][E0-9]$)|(CHI$)"),
           start = round(time_start * 1000),
           end = round(time_end * 1000),
           dur = end - start) %>%
    replace_na(list(speaker = "")) %>%
    arrange(tier_name, start) %>%
    select(tier_name, speaker, start, end, dur, content)
  write_delim(txt.eaf, paste(c("raw/",
                               unlist(str_split(file, "[.]"))[1], ".txt"),
                             collapse = ""),
              delim = "\t", col_names = FALSE)
}
