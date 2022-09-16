# XDL-ACLEW
Replicable manuscript (i.e., with pre-aggregated data given privacy constraints) for AUTHORS et al. (under review) A cross-cultural examination of young children's everyday language experiences.

The `extras/`subdirectory contains:

1. Scripts that were used in data pre-processing in the original manuscript (they do not apply to this repo directly since we have only shared aggregated data based on our collaborators' data sharing agreements) and
2. Extra demographic information regarding the recordings to the extent that sharing was allowed.

### How these input data were generated from the originals
_(for posterity, for those who have access)_

```
library(tidyverse)

# quantity rand
qr <- read_csv("processed_data/quantity.rand.csv")
qr.min <- qr %>% select(aclew_child_id,
                        corp, group_corp, group_corpNE,
                        language, child_sex, age_mo_round,
                        segment, segment_dur, n_spkrs_clip,
                        tds_min, ocds_min, cds_min, ads_min,
                        tds_mph, ocds_mph, cds_mph, ads_mph,
                        prop_tds, prop_ocds, prop_cds, prop_ads)
write_csv(qr.min, "quantity.rand.anon_col_subset.csv")

# quantity rand by talker type
qrs <- read_csv("processed_data/quantity.rand.st.csv")
qrs.min <- qrs %>% select(aclew_child_id,
                          corp, group_corp, group_corpNE,
                          language, child_sex, age_mo_round,
                          segment, segment_dur,
                          n_spkrs_clip.st, SpkrType,
                          tds_min.st, ocds_min.st, cds_min.st, ads_min.st,
                          tds_mph.st, ocds_mph.st, cds_mph.st, ads_mph.st)
write_csv(qrs.min, "quantity.rand.st.anon_col_subset.csv")

# quantity rand by child recording
qrc <- read_csv("processed_data/quantity.rand.bychild.csv")
qrc.min <- qrc %>% select(aclew_child_id,
                          corp, group_corp, group_corpNE,
                          language, child_sex, age_mo_round,
                          m_n_spkrs,
                          tds_min, ocds_min, cds_min, ads_min,
                          tds_mph, ocds_mph, cds_mph, ads_mph)
write_csv(qrc.min, "quantity.rand.bychild.anon_col_subset.csv")

# NB: the stuff in the 'extras' subdir was manually checked & added
```