# Use this file to anonymize and check transcript data
if (exists("user") == FALSE) {
  print("user is not set. Assuming that user is 'other'.")
  user <- "other"
}
source("0_setup.R")

###### anonymizes new files
if (exists("rerun.dataanon") == FALSE) {
  print("rerun.dataanon is not set. Assuming that no anonymization is needed.")
  rerun.dataanon <- "n"
}

if (rerun.dataanon == "y") {
  files <- list.files(path = raw.data.path, pattern = "*.txt",
                      recursive = TRUE)
  all.data <- data.frame()
  for (i in 1:length(files)) {
    # print(files[i])
    newfile <- read.table(paste0(raw.data.path, files[i]), quote = "",
      sep="\t", header=FALSE, stringsAsFactors = FALSE)
    names(newfile)[1:6] <- c("tier", "speaker", "start", "stop", "dur", "val")
    transcription.tiers <- which(newfile$tier == newfile$speaker)
    newfile$val[transcription.tiers] <- "0."
    uniq.tiers <- unique(newfile$tier)
    if ("code_num" %in% uniq.tiers) {
      newfile <- newfile %>% filter(
        !(str_detect(tier,
                     "AudioOnly|notes|context|on_off|tlen|tlsp")) &
          tier != "code")
      write_csv(newfile, paste0(anon.data.path, files[i]))
    } else {
      print("Tiers missing!")
    }
  }
}


###########Reads in labels 
#<!--Block below: R code that reads in and labels the data so we can make the summary measures-->
# ```{r prepare_data, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}

# Read in annotation files
files <- list.files(path = data.path, pattern = "*.txt")
all.data <- data.frame()
for (i in 1:length(files)) {
#  print(files[i])
  newfile <- read_csv(paste0(data.path, files[i]),
                      col_types = cols(
                        val = col_character()))
  newfile$aclew_child_id <- unlist(strsplit(files[i], '\\.'))[1]
  all.data <- rbind(all.data, newfile)
}
all.data$row <- c(1:nrow(all.data))

# Read in supplementary data
ptcp.info <- read_csv(paste0(supp.data.path, ptcp.info.file),
                      col_types = cols(aclew_child_id = col_character())) %>%
  dplyr::select(-row)

force.four.digits <- function(id.string) {
  empty.buffer <- 4 - nchar(id.string)
  id.string <- paste0(rep(0, empty.buffer), id.string)
  return(id.string)
}
seg.info <- read_csv(paste0(supp.data.path, seg.index.file),
                     col_types = cols(aclew_id = col_character())) %>%
  rowwise() %>%
  mutate(aclew_id = force.four.digits(aclew_id))

# Add mean and sd values for participant-level predictors to ptcp.info
ptcp.info <- ptcp.info %>%
  mutate(
    tchiyr.m = mean(age_mo_round),
    motyr.m = mean(mother_age, na.rm=TRUE),
    nsb.m = mean(number_older_sibs),
    hsz.m = mean(household_size, na.rm=TRUE),
    tchiyr.sd = sd(age_mo_round),
    motyr.sd = sd(mother_age, na.rm=TRUE),
    nsb.sd = sd(number_older_sibs),
    hsz.sd = sd(household_size, na.rm=TRUE)
  )

#sets group_corp to have NA_English as the reference group
ptcp.info$group_corp <- factor(ptcp.info$group_corp)
ptcp.info$group_corpNE <- factor(ptcp.info$group_corp,
                                 levels = c("NA_English",
                                            "Arg_Spanish",
                                            "Tseltal",
                                            "UK_English",
                                            "Yeli_Dnye"))

# Add participant and segment info (only include random clips)
codes <- all.data %>% filter(tier == "code_num") %>%
  filter(!grepl("(ext)|(tt)|(va)", val))

all.data <- all.data %>%
  filter(!grepl("(pmt)|(rsp)|(Notes)|(code_num)", tier)) %>%
  left_join(ptcp.info, by = "aclew_child_id") %>%
  mutate(segment = "", sample = "",
         sample_type = "", segment_dur = 0)

for (i in 1:nrow(codes)) {
  rec <- codes$aclew_child_id[i]
  seg <- as.character(codes$val[i])
  seg.on <- codes$start[i]
  seg.off <- codes$stop[i]
  seg.idx <- which(all.data$aclew_child_id == rec &
                     all.data$start < seg.off &
                     all.data$stop > seg.on)
  all.data$segment[seg.idx] <- seg
}

# Remove vocalizations that aren't associated with a random clip,
all.data <- filter(all.data, segment != "") %>%
  # label clips as random, and set clip durations
  mutate(sample = "random",
         sample_type = "random",
         segment_dur = case_when(
           group_corp == "Yeli_Dnye" ~ 2.5,
           group_corp == "Tseltal" ~ 5,
           TRUE ~ 2
         ))

# For tidiness, replace all forms of 'multi' speaker with the name of the tier
# (multi distinction doesn't matter for the current analysis)
multi.subtiers <- which(grepl("(multi)|(MULTI)|(Multi)", all.data$speaker) &
                          nchar(all.data$tier) > 3)
all.data$speaker[multi.subtiers] <- substr(all.data$tier[multi.subtiers], 5, 7)
multi.tiers <- which(grepl("(multi)|(MULTI)|(Multi)", all.data$speaker))
all.data$speaker[multi.tiers] <- all.data$tier[multi.tiers]

# sum(subset(all.data, tier == speaker & speaker != "CHI")$dur)/(60*60*1000)


