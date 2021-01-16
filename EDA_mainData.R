## Post-Analysis EDA of the Thesis

# library initialization
library(dplyr)
library(ggplot2)
library(tidyr)

# Load dataset -----------------------------------------------------------------
main_data <- read.csv("input/experiment_data.csv", stringsAsFactors = FALSE)
main_data$F2017 <- as.numeric(working_dat$F2017)

## 1. Data completeness --------------------------------------------------------
# 1.1 Overall (Experiment 1) ---------------------------------------------------
dest_org_sum <- main_data %>% 
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))

# 1.2 Per income group - origin per destination --------------------------------
# Train set
# low income
dest_org_low <- trn.Low.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# lower middle income
dest_org_lm <- trn.LM.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# upper middle income
dest_org_um <- trn.UM.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# high income
dest_org_high <- trn.H.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))

# Test set
# low income
dest_org_low_tst <- tst.Low.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# lower middle income
dest_org_lm_tst <- tst.LM.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# upper middle income
dest_org_um_tst <- tst.UM.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# high income
dest_org_high_tst <- tst.H.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))

# 1.3 Per income group - destination per origin --------------------------------
# Train set
# low income
org_dest_low_trn <- trn.Low.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))
# lower middle income
org_dest_lm_trn <- trn.LM.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))
# upper middle income
org_dest_um_trn <- trn.UM.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))
# high income
org_dest_hi_trn <- trn.H.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))

# Test set
# low income
org_dest_low_tst <- tst.Low.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))
# lower middle income
org_dest_lm_tst <- tst.LM.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))
# upper middle income
org_dest_um_tst <- tst.UM.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))
# high income
org_dest_hi_tst <- tst.H.ts1 %>%
  group_by(Origin_code) %>%
  summarise("No.Dests" = n()) %>%
  arrange(desc(No.Dests))

# 1.4 Per destination  access - origin per destination -------------------------
# Train set
# High access (Easy)
dest_org_easy_trn <- trn.Easy.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# Moderate access
dest_org_mo_trn <- trn.moderate.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# Difficult access
dest_org_diff_trn <- trn.difficult.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# Very difficult access
dest_org_vdiff_trn <- trn.Vdifficult.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))

# Test set
# High access (Easy)
dest_org_easy_tst <- tst.Easy.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# Moderate access
dest_org_mo_tst <- tst.moderate.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# Difficult access
dest_org_diff_tst <- tst.difficult.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
# Very difficult access
dest_org_vdiff_tst <- tst.Vdifficult.ts1 %>%
  group_by(Destination_code) %>%
  summarise("No.Origins" = n()) %>%
  arrange(desc(No.Origins))
