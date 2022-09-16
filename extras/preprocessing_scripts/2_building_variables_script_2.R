source("1_Anonymize_transcripts_plusCorp_Script 1.R")

# Get min/hr speech measures
all.rand.segments <- codes %>%
  select(aclew_child_id, val, dur) %>%
  distinct() %>%
  mutate(dur = dur/60000) %>%
  dplyr::rename(segment_dur = dur) %>%
  arrange(aclew_child_id, val) %>%
  dplyr::rename(segment = val)

all.rand.segments.bychild <- all.rand.segments %>%
  group_by(aclew_child_id) %>%
  summarize(n_segments = n())

# Simplify addressee codes
all.data <- all.data %>%
  #select(val, corp) %>%
  mutate(
    addressee = case_when(
      val == "A" & grepl("xds@", tier) ~ "A",
      val == "T" & grepl("xds@", tier) ~ "T",
      val == "C" & grepl("xds@", tier) ~ "C",
      nchar(tier) == 3 ~ "NA",
      grepl("(vcm)|(lex)|(mwu)", tier) ~ "NA",
      TRUE ~ "OTHER"),
    detailed_addressee = case_when(
      val == "A" & grepl("xds@", tier) ~ "A",
      val == "T" & grepl("xds@", tier) ~ "T",
      val == "C" & grepl("xds@", tier) ~ "C",
      val == "B" & grepl("xds@", tier) ~ "B",
      val == "U" & grepl("xds@", tier) ~ "U",
      val == "P" & grepl("xds@", tier) ~ "P",
      val == "O" & grepl("xds@", tier) ~ "O",
      addressee == "NA" ~ "NA",
      TRUE ~ "CHECK ME -- invalid value?"))

# We only include speech from live, co-present humans
all.data <- all.data %>%
  filter(grepl("(^[FMU][ACU][0-9]$)|(^CHI$)", speaker))

# Remove data from rec 3634
# (accidental duplicate file discovered post annotation/preliminary analysis)
all.data <- all.data %>%
  filter(aclew_child_id != "3634")
ptcp.info <- ptcp.info %>%
  filter(aclew_child_id != "3634")
all.rand.segments <- all.rand.segments %>%
  filter(aclew_child_id != "3634")

# ADS (speech from non-target child speakers directed to an adult)
ads.per.seg.rand <- all.data %>%
  filter(grepl("xds@", tier) & (addressee == "A")) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(ads_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(ads_min = 0)) %>%
  mutate(ads_mph = (ads_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)

# TCDS (speech from non-target child speakers directed to the target child
# specifically and exclusively)
tds.per.seg.rand <- all.data %>%
  filter(grepl("xds@", tier) & (addressee == "T")) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(tds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(tds_min = 0)) %>%
  mutate(tds_mph = (tds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)

# CDS (speech from non-target child speakers directed to any child, which can
# include the target child when they are addressed w/ one other child PLUS
# speech exclusively directed to the target child ... i.e., all hearable CDS)
cds.per.seg.rand <- all.data %>%
  filter(grepl("xds@", tier) & (addressee == "C" | addressee == "T")) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(cds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(cds_min = 0)) %>%
  mutate(cds_mph = (cds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)

# OCDS by spkr age (speech from non-target child speakers directed to any child,
# excluding speech exclusively directed to the target child)
ocds.per.seg.rand <- all.data %>%
  filter(grepl("xds@", tier) & (addressee == "C")) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(ocds_min = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(ocds_min = 0)) %>%
  mutate(ocds_mph = (ocds_min/segment_dur)*60) %>%
  arrange(aclew_child_id, segment)

# Number of live, co-present human speakers per clip
spkrs.per.seg.rand <- all.data %>%
  filter(speaker != "CHI" & !(grepl("@", tier))) %>%
  group_by(aclew_child_id, segment) %>%
  summarise(n_spkrs_clip = length(unique(speaker))) %>%
  full_join(all.rand.segments, by = c("aclew_child_id", "segment")) %>%
  replace_na(list(n_spkrs_clip = 0)) %>%
  arrange(aclew_child_id, segment)

# All together
quantity.rand <- ads.per.seg.rand %>%
  full_join(tds.per.seg.rand,
            by = c("aclew_child_id", "segment", "segment_dur")) %>%
  full_join(cds.per.seg.rand,
            by = c("aclew_child_id", "segment", "segment_dur")) %>%
  full_join(ocds.per.seg.rand,
            by = c("aclew_child_id", "segment", "segment_dur")) %>%
  full_join(spkrs.per.seg.rand,
            by = c("aclew_child_id", "segment", "segment_dur")) %>%
  full_join(ptcp.info, by = "aclew_child_id") %>%
  replace_na(list(ads_min = 0, ads_mph = 0,
                  tds_min = 0, tds_mph = 0,
                  cds_min = 0, cds_mph = 0,
                  ocds_min = 0, ocds_mph = 0,
                  n_spkrs_clip = 0)) %>%
  mutate(atcds_min = ads_min + tds_min + cds_min,
         # note that these don't necessarily add to 1 because cds includes
         # both tcds (addressee == T) and ocds (addressee == C)
         prop_ads = ads_min/atcds_min,
         prop_tds = tds_min/atcds_min,
         prop_cds = cds_min/atcds_min,
         prop_ocds = ocds_min/atcds_min)
# Don't replace NAs with 0s in this case;
# proportion is not meaningful w/o any speech

## Prepping plots and models
quantity.rand.bychild <- quantity.rand %>%
  group_by(aclew_child_id) %>%
  summarise(
    ads_min = mean(ads_min),
    ads_mph = mean(ads_mph),
    tds_min = mean(tds_min),
    tds_mph = mean(tds_mph),
    cds_min = mean(cds_min),
    cds_mph = mean(cds_mph),
    ocds_min = mean(cds_min),
    ocds_mph = mean(ocds_mph),
    prop_tds = mean(prop_tds, na.rm = TRUE),
    prop_ads = mean(prop_ads, na.rm = TRUE),
    prop_cds = mean(prop_cds, na.rm = TRUE),
    prop_ocds = mean(prop_ocds, na.rm = TRUE),
    m_n_spkrs = mean(n_spkrs_clip),
    tot_annot_min = sum(segment_dur)) %>%
  full_join(ptcp.info, by = "aclew_child_id")


#---- by speaker type ----
# Get ads, tds, and cds min/hr by speaker type
all.data$SpkrAge <- "Not known"
all.data$SpkrSex <- "Not known"
all.data$SpkrAge[grepl("FA\\d|MA\\d|UA\\d", all.data$speaker)] <- "Adult"
all.data$SpkrAge[grepl("FC\\d|MC\\d|UC\\d|MI\\d", all.data$speaker)] <- "Child"
all.data$SpkrSex[grepl("FA\\d|FC\\d", all.data$speaker)] <- "Female"
all.data$SpkrSex[grepl("MA\\d|MC\\d", all.data$speaker)] <- "Male"
all.data <- all.data %>%
  mutate(SpkrType = case_when(
    SpkrAge == "Adult" & SpkrSex == "Female" ~ "Woman",
    SpkrAge == "Adult" & SpkrSex == "Male" ~ "Man",
    SpkrAge == "Child" ~ "Child",
    TRUE ~ "Other"
  ))
all.rand.segments.st <- bind_rows(
  add_column(all.rand.segments, SpkrType = "Woman"),
  add_column(all.rand.segments, SpkrType = "Man"),
  add_column(all.rand.segments, SpkrType = "Child")) %>%
  arrange(aclew_child_id, segment, SpkrType)

# ADS by spkr age (speech from non-target child speakers directed to an adult)
ads.per.seg.rand.st <- all.data %>%
  filter(SpkrType != "Other" & grepl("xds@", tier) & addressee == "A") %>%
  group_by(aclew_child_id, SpkrType, segment) %>%
  summarise(ads_min.st = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.st, by = c(
    "aclew_child_id", "segment", "SpkrType")) %>%
  replace_na(list(ads_min.st = 0)) %>%
  mutate(ads_mph.st = (ads_min.st/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrType)

# TCDS by spkr age (speech from non-target child speakers directed to the
# target child specifically and exclusively
tds.per.seg.rand.st <- all.data %>%
  filter(SpkrType != "Other" & grepl("xds@", tier) & addressee == "T") %>%
  group_by(aclew_child_id, SpkrType, segment) %>%
  summarise(tds_min.st = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.st, by = c(
    "aclew_child_id", "segment", "SpkrType")) %>%
  replace_na(list(tds_min.st = 0)) %>%
  mutate(tds_mph.st = (tds_min.st/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrType)

# CDS (speech from non-target child speakers directed to any child, which can
# include the target child when they are addressed w/ one other child PLUS
# speech exclusively directed to the target child ... i.e., all hearable CDS)
cds.per.seg.rand.st <- all.data %>%
  filter(SpkrType != "Other" & grepl("xds@", tier) &
           (addressee == "C" | addressee == "T")) %>%
  group_by(aclew_child_id, SpkrType, segment) %>%
  summarise(cds_min.st = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.st, by = c(
    "aclew_child_id", "segment", "SpkrType")) %>%
  replace_na(list(cds_min.st = 0)) %>%
  mutate(cds_mph.st = (cds_min.st/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrType)

# OCDS by spkr age (speech from non-target child speakers directed to any child,
# excluding speech exclusively directed to the target child)
ocds.per.seg.rand.st <- all.data %>%
  filter(SpkrType != "Other" & grepl("xds@", tier) & addressee == "C") %>%
  group_by(aclew_child_id, SpkrType, segment) %>%
  summarise(ocds_min.st = round(sum(dur)/60000,3)) %>%
  full_join(all.rand.segments.st, by = c(
    "aclew_child_id", "segment", "SpkrType")) %>%
  replace_na(list(ocds_min.st = 0)) %>%
  mutate(ocds_mph.st = (ocds_min.st/segment_dur)*60) %>%
  arrange(aclew_child_id, segment, SpkrType)

# Number of speakers per clip by spkr age
spkrs.per.seg.rand.st <- all.data %>%
  filter(speaker != "CHI" & SpkrType != "Other" & !(grepl("@", tier))) %>%
  group_by(aclew_child_id, SpkrType, segment) %>%
  summarise(n_spkrs_clip.st = length(unique(speaker))) %>%
  full_join(all.rand.segments.st, by = c(
    "aclew_child_id", "segment", "SpkrType")) %>%
  replace_na(list(n_spkrs_clip.st = 0)) %>%
  arrange(aclew_child_id, segment, SpkrType)

# All together
quantity.rand.st <- ads.per.seg.rand.st %>%
  full_join(tds.per.seg.rand.st, by = c(
    "aclew_child_id", "SpkrType",
    "segment", "segment_dur")) %>%
  full_join(cds.per.seg.rand.st, by = c(
    "aclew_child_id", "SpkrType",
    "segment", "segment_dur")) %>%
  full_join(ocds.per.seg.rand.st, by = c(
    "aclew_child_id", "SpkrType",
    "segment", "segment_dur")) %>%
  full_join(spkrs.per.seg.rand.st, by = c(
    "aclew_child_id", "SpkrType",
    "segment", "segment_dur")) %>%
  full_join(dplyr::select(quantity.rand, c(
    "aclew_child_id", "segment", "tds_min", "ads_min", "cds_min")),
    by = c("aclew_child_id", "segment")) %>%
  full_join(ptcp.info, by = "aclew_child_id") %>%
  replace_na(list(
    ads_min.st = 0, ads_mph.st = 0,
    tds_min.st = 0, tds_mph.st = 0,
    cds_min.st = 0, cds_mph.st = 0,
    ocds_min.st = 0, ocds_mph.st = 0,
    n_spkrs_clip.st = 0))

# Write out tables for quick use in the ms construction
# write_csv(all.data, paste0(processed.data.path, "all.data-ALLinputtypes.csv"))
# write_csv(quantity.rand, paste0(processed.data.path, "quantity.rand-ALLinputtypes.csv"))
# write_csv(quantity.rand.bychild, paste0(processed.data.path, "quantity.rand.bychild-ALLinputtypes.csv"))
# write_csv(quantity.rand.st, paste0(processed.data.path, "quantity.rand.st-ALLinputtypes.csv"))

# Write out tables for quick use in the ms construction
write_csv(all.data, paste0(processed.data.path, "all.data.csv"))
write_csv(quantity.rand, paste0(processed.data.path, "quantity.rand.csv"))
write_csv(quantity.rand.bychild, paste0(processed.data.path, "quantity.rand.bychild.csv"))
write_csv(quantity.rand.st, paste0(processed.data.path, "quantity.rand.st.csv"))

########
# assumption-check raw DV distribution plots

## ADS random sample ####
ads.random.distribution <- ggplot(
  quantity.rand,
  aes(round(ads_mph,0))) +
  geom_histogram() +
  ylab("# of clips") +
  xlab ("ADS min/hr") +
  basic.theme
ggsave(paste0(plot.path, "ADS_random.distribution.png"),
  plot = ads.random.distribution,
  width = 8, height = 6, dpi = 300)

#sd(round(quantity.rand$ads_mph,0))^2
#mean(round(quantity.rand$ads_mph,0))
# mean is much smaller than variance

## TCDS random sample ####
tds.random.distribution <- ggplot(
  quantity.rand,
  aes(round(tds_mph,0))) +
  geom_histogram() +
  ylab("# of clips") +
  xlab ("TCDS min/hr") +
  basic.theme
ggsave(paste0(plot.path, "TCDS_random.distribution.png"),
       plot = tds.random.distribution,
       width = 8, height = 6, dpi = 300)

#sd(round(quantity.rand$tds_mph,0))^2
#mean(round(quantity.rand$tds_mph,0))
# mean is much smaller than variance

## CDS random sample ####
cds.random.distribution <- ggplot(
  quantity.rand,
  aes(round(cds_mph,0))) +
  geom_histogram() +
  ylab("# of clips") +
  xlab ("CDS min/hr") +
  basic.theme
ggsave(paste0(plot.path, "CDS_random.distribution.png"),
       plot = cds.random.distribution,
       width = 8, height = 6, dpi = 300)

#sd(round(quantity.rand$cds_mph,0))^2
#mean(round(quantity.rand$cds_mph,0))
# mean is much smaller than variance
