## Thesis Working File
## Name: Natanop Pimonsathian
## u-number: u786200

## Load packages and set seed --------------------------------------------------
library(dplyr)
library(ggplot2)
library(imputeTS)
library(MASS)
library(mice)
library(tidyr)
library(glmnet)
library(caret)
library(randomForest)
library(mltools)
library(e1071)
library(reshape)

set.seed(1)

## Setup working directory -----------------------------------------------------
setwd("C:/Users/Natanop/Desktop/Study/Tillburg University/Tilburg Uni Study/
      DSBG/Thesis/Scripts/R")

## Load UNWTO Tourist dataset -------------------------------------------------- 
tourist <- read.csv("input/UNWTO_working.csv", stringsAsFactors = FALSE)

## Load GEDLT dataset ----------------------------------------------------------
GDELT <- read.csv("input/GDELT_worldwide.csv", stringsAsFactors = FALSE)

## check dyad completeness in both datasets ------------------------------------
tourist <- unite(tourist, "dyad", c(2, 4), sep = "_", remove = FALSE) 
GDELT <- unite(GDELT, "dyad", c(2, 3), sep = "_", remove = FALSE)

tourist_dat <- tourist %>% 
  group_by(dyad, Destination_code, Destination, Origin_code, Origin) %>%
  summarise(F2013 = sum(Flow_Y2013),
            F2014 = sum(Flow_Y2014), 
            F2015 = sum(Flow_Y2015),
            F2016 = sum(Flow_Y2016),
            F2017 = sum(Flow_Y2017))%>%
  as.data.frame(tourist_dat)
#there are 12675 pairs of dyads in tourism data
num_pairs_tourist <- nrow(tourist_dat)

GDELT_sum <- GDELT  %>% 
  group_by(dyad) %>%
  summarise(total.count = n())
#there are 37219 pairs of dyads in GDELT data
  num_pairs_GDELT <- nrow(GDELT_sum)
  
#create list of valid dyads
valid_dyads <- inner_join(GDELT_sum[ ,1] , tourist_dat[ ,1:2] , by = "dyad")
valid_dyads <- valid_dyads[ ,-2]

## tourist database - preprocessing --------------------------------------------
  
## missing values imputation  
# tourist dataset: compute proportion of missing values per dyad (row-wise)
  for(i in 1:nrow(tourist_dat)) {
    tourist_dat$missing_percent[i] <- (sum(is.na(tourist_dat[i, 6:10]))/5)*100
  }
  
# visualize miss data distribution
tourist_missing <-ggplot(data = tourist_dat, aes(x = factor(missing_percent))) +
  geom_bar(stat = "count") 
tourist_missing

tourist_missing_sum <- tourist_dat %>% 
  group_by(factor(missing_percent))%>%
  summarise(missing_count = n())

# 552 observations (rows) were excluded from an analysis (missing more than 80%)
tourist_dat <- tourist_dat %>% 
  filter(missing_percent < 80)
# 12123 dyads left 

# impute by interpolation
tourist_flow_impute <-t(tourist_dat[6:10])
a <- na.interpolation(tourist_flow_impute, option = "linear")
tourist_flow_impute <- t(a)

# log normalization for flow 
#tourist_flow_impute <- log(tourist_flow_impute)

# tourist ready for time sliding data
tourist_dat <- data.frame(tourist_dat[ ,1:5], tourist_flow_impute)


# plot flow sample (USA)

USA_flow_plot <- tourist_dat %>%
  select(Origin_code, F2013, F2014, F2015, F2016, F2017)
  
  

## GEDLT data preprocessing  -----------------------------------------------------

# summarize event code into the root level 
GDELT_dat <- GDELT
GDELT_dat$EventRootCode <- substr(GDELT_dat$EventCode, 1, 2)
as.factor(GDELT_dat$EventRootCode)

# choose only valid dyads
GDELT_dat <- GDELT_dat %>%
  filter( dyad %in% valid_dyads$dyad) 

# aggegrate variables [dyad and avg tone] 
GDELT_dat <- GDELT %>% 
  group_by(Year, dyad, Actor1Countrycode, Actor2Countrycode) %>%
  summarise(Tone = mean(avg_AvgTone), Sources = mean(avg_NumSources),
  Articles = mean(avg_NumArticles), Mentions = mean(avg_NumMentions)) %>%
  as.data.frame(GDELT_dat)

# put avg tone, sources, articles, mentions into time series 
GDELT_tone <- GDELT_dat[ ,1:5] %>% 
  spread(Year, Tone) #%>% 
  #rename("T2013" = "2013", "T2014" = "2014", "T2015" = "2015", 
         #"T2016" = "2016", "T2017" = "2017")

GDELT_sources <- GDELT_dat[ ,c(1:4, 6)] %>% 
  spread(Year, Sources) %>% 
  rename("S2013" = "2013", "S2014" = "2014", "S2015" = "2015", 
                "S2016" = "2016", "S2017" = "2017")

GDELT_articles <- GDELT_dat[ ,c(1:4, 7)] %>% 
  spread(Year, Articles) %>% 
  rename("A2013" = "2013", "A2014" = "2014", "A2015" = "2015", 
         "A2016" = "2016", "A2017" = "2017")

GDELT_mentions <- GDELT_dat[ ,c(1:4, 8)] %>% 
  spread(Year, Mentions) %>% 
  rename("M2013" = "2013", "M2014" = "2014", "M2015" = "2015", 
         "M2016" = "2016", "M2017" = "2017")

GDELT_dat <- data.frame(GDELT_tone) #, #GDELT_sources[ ,4:8], GDELT_articles[ ,4:8],
                        #GDELT_mentions[ ,4:8])

# GEDLT dataset: compute proportion of missing values per dyad (row-wise)
for(i in 1:nrow(GDELT_dat)) {
  GDELT_dat$t_missing_percent[i] <- (sum(is.na(GDELT_dat[i, 4:8]))/5)*100
}

for(i in 1:nrow(GDELT_dat)) {
  GDELT_dat$s_missing_percent[i] <- (sum(is.na(GDELT_dat[i, 9:13]))/5)*100
}

for(i in 1:nrow(GDELT_dat)) {
  GDELT_dat$a_missing_percent[i] <- (sum(is.na(GDELT_dat[i, 14:18]))/5)*100
}

for(i in 1:nrow(GDELT_dat)) {
  GDELT_dat$m_missing_percent[i] <- (sum(is.na(GDELT_dat[i, 19:23]))/5)*100
}

# visualize missing data distribution
GDELT_t_missing <-ggplot(data = GDELT_dat, aes(x = factor(t_missing_percent))) +
  geom_bar(stat = "count") 
GDELT_t_missing

GDELT_s_missing <-ggplot(data = GDELT_dat, aes(x = factor(s_missing_percent))) +
  geom_bar(stat = "count") 
GDELT_s_missing

GDELT_a_missing <-ggplot(data = GDELT_dat, aes(x = factor(a_missing_percent))) +
  geom_bar(stat = "count") 
GDELT_a_missing

GDELT_m_missing <-ggplot(data = GDELT_dat, aes(x = factor(m_missing_percent))) +
  geom_bar(stat = "count") 
GDELT_m_missing


# 31736 observations (rows) left for an analysis (missing less than 80%)
GDELT_dat <- GDELT_dat %>% 
  filter(t_missing_percent < 80) # | s_missing_percent < 80 | 
  #a_missing_percent < 80 | m_missing_percent < 80 )

# impute by interpolation - tone
GDELT_tone_impute <-t(GDELT_dat[ ,4:8])
b <- na.interpolation(GDELT_tone_impute, option = "linear")
GDELT_tone_impute <- as.data.frame(t(b))

# normalize av tone for each dyad z- score method 
# calculate mean
mean_13 <- mean(GDELT_tone_impute$X2013)
mean_14 <- mean(GDELT_tone_impute$X2014)
mean_15 <- mean(GDELT_tone_impute$X2015)
mean_16 <- mean(GDELT_tone_impute$X2016)
mean_17 <- mean(GDELT_tone_impute$X2017)

# calculate standard deviation
sd_13 <- sd(GDELT_tone_impute$X2013)
sd_14 <- sd(GDELT_tone_impute$X2014)
sd_15 <- sd(GDELT_tone_impute$X2015)
sd_16 <- sd(GDELT_tone_impute$X2016)
sd_17 <- sd(GDELT_tone_impute$X2017)

# add z-score back to the dataset
for(i in 1:nrow(GDELT_tone_impute)) {
  GDELT_tone_impute$ZT2013[i] <- (GDELT_tone_impute$X2013[i] - mean_13)/sd_13
  GDELT_tone_impute$ZT2014[i] <- (GDELT_tone_impute$X2014[i] - mean_14)/sd_14
  GDELT_tone_impute$ZT2015[i] <- (GDELT_tone_impute$X2015[i] - mean_15)/sd_15
  GDELT_tone_impute$ZT2016[i] <- (GDELT_tone_impute$X2016[i] - mean_16)/sd_16
  GDELT_tone_impute$ZT2017[i] <- (GDELT_tone_impute$X2016[i] - mean_17)/sd_17
}

# impute by interpolation - sources
GDELT_sources_impute <-t(GDELT_dat[ ,9:13])
c <- na.interpolation(GDELT_sources_impute, option = "linear")
GDELT_sources_impute <- as.data.frame(t(c))

# impute by interpolation - articles
GDELT_articles_impute <-t(GDELT_dat[ ,14:18])
c <- na.interpolation(GDELT_articles_impute, option = "linear")
GDELT_articles_impute <- as.data.frame(t(c))

# impute by interpolation - mentions
GDELT_mentions_impute <-t(GDELT_dat[ ,19:23])
c <- na.interpolation(GDELT_mentions_impute, option = "linear")
GDELT_mentions_impute <- as.data.frame(t(c))

GDELT_dat <- data.frame(GDELT_dat[ ,1:3], GDELT_tone_impute[ ,6:10]) 
 #GDELT_sources_impute, GDELT_articles_impute, GDELT_mentions_impute)

## additional features functions -----------------------------------------------
# mean
calc_mean <- function(data, scol, ecol) {
  TS_mean <- rowMeans(data[ ,scol:ecol])
}

# median
calc_median <- function(data, scol, ecol) {
  TS_median <- c()
  for(i in 1:nrow(data)) {
    TS_median[i] <- median(as.numeric(data[i, scol:ecol]))
  }
  return(TS_median)
}

# max 
calc_max <- function(data, scol, ecol) {
  TS_max <- c()
  for(i in 1:nrow(data)) {
    TS_max[i] <- max(data[i, scol:ecol])
  }
  return (TS_max)
}

# min
calc_min <- function(data, scol, ecol) {
  TS_min <- c()
  for(i in 1:nrow(data)) {
    TS_min[i] <- min(data[i, scol:ecol])
  }
  return (TS_min)
}

# range
calc_range <- function(data, scol, ecol) {
  TS_range <-c()
  for(i in 1:nrow(data)) {
    TS_range[i] <- max(data[i, scol:ecol]) - min(data[i, scol:ecol])
  }
  return (TS_range)
}

# difference
calc_adif <- function(data, scol, ecol) {
  TS_adif <- c()
  for(i in 1:nrow(data)) {
    TS_adif[i] <- data[i, ecol] - data[i, scol]
  }
  return (TS_adif)
}

# std. deviation
calc_sd <- function(data, scol, ecol) {
  TS_sd <- c()
  for(i in 1:nrow(data)) {
    TS_sd[i] <- sd(data[i, scol:ecol])
  }
  return (TS_sd)
}

# variance
calc_var <- function(data, scol, ecol) {
  TS_var <- c()
  for(i in 1:nrow(data)) {
    TS_var[i] <- sd(data[i, scol:ecol])^2
  }
  return (TS_var)
}

# slope 
calc_slope <- function(data, scol, ecol) {
  TS_slope <-c()
  for(i in 1:nrow(data)) {
    TS_slope[i] <- (data[i, ecol] - data[i, scol])/2
  }
  return (TS_slope)
}

# precentage change 
calc_perchg <- function(data, scol, ecol) {
  TS_perchg <-c()
  for(i in 1:nrow(data)) {
    TS_perchg[i] <- (data[i, ecol] - data[i, scol])/data[i, scol]
  }
  return (TS_perchg)
}

## Time-sliding windows - Tourist flow -----------------------------------------  
  # window size = 3 
  # non-temporal features 
  # mean
  # TS1
  tourist_dat$F.mean.TS1 <- calc_mean(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.mean.TS2 <- calc_mean(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.mean.TS3 <- calc_mean(tourist_dat, 8, 10)
  
  # median 
  # TS1
  tourist_dat$F.med.TS1 <- calc_median(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.med.TS2 <- calc_median(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.med.TS3 <- calc_median(tourist_dat, 8, 10)
  
  # max 
  # TS1
  tourist_dat$F.max.TS1 <- calc_max(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.max.TS2 <- calc_max(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.max.TS3 <- calc_max(tourist_dat, 8, 10)
  
  # min
  # TS1
  tourist_dat$F.min.TS1 <- calc_min(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.min.TS2 <- calc_min(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.min.TS3 <- calc_min(tourist_dat, 8, 10)
  
  # temporal features 
  # range (max-min)
  # TS1
  tourist_dat$F.range.TS1 <- calc_range(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.range.TS2 <- calc_range(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.range.TS3 <- calc_range(tourist_dat, 8, 10)
  
  # difference (new-old)
  # TS1
  tourist_dat$F.diff.TS1 <- calc_adif(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.diff.TS2 <- calc_adif(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.diff.TS3 <- calc_adif(tourist_dat, 8, 10) 
  
  # std. deviation 
  # TS1
  tourist_dat$F.sd.TS1 <- calc_sd(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.sd.TS2 <- calc_sd(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.sd.TS3 <- calc_sd(tourist_dat, 8, 10) 
  
  # variance 
  # TS1
  tourist_dat$F.var.TS1 <- calc_var(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.var.TS2 <- calc_var(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.var.TS3 <- calc_var(tourist_dat, 8, 10) 
  
  # slope
  # TS1
  tourist_dat$F.sl.TS1 <- calc_slope(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.sl.TS2 <- calc_slope(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.sl.TS3 <- calc_slope(tourist_dat, 8, 10) 
  
  # percentage change 
  # TS1
  tourist_dat$F.pc.TS1 <- calc_perchg(tourist_dat, 6, 8)
  # TS2
  tourist_dat$F.pc.TS2 <- calc_perchg(tourist_dat, 7, 9)
  # TS3
  tourist_dat$F.pc.TS3 <- calc_perchg(tourist_dat, 8, 10) 
  
## Time-sliding windows - Sentiment tone ---------------------------------------  
  # window size = 3 
  
  # non-temporal features 
  # mean
  # TS1
  GDELT_dat$T.mean.TS1 <- calc_mean(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.mean.TS2 <- calc_mean(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.mean.TS3 <- calc_mean(GDELT_dat, 6, 8)
  
  # median 
  # TS1
  GDELT_dat$T.med.TS1 <- calc_median(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.med.TS2 <- calc_median(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.med.TS3 <- calc_median(GDELT_dat, 6, 8)
  
  # max 
  # TS1
  GDELT_dat$T.max.TS1 <- calc_max(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.max.TS2 <- calc_max(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.max.TS3 <- calc_max(GDELT_dat, 6, 8)
  
  # min
  # TS1
  GDELT_dat$T.min.TS1 <- calc_min(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.min.TS2 <- calc_min(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.min.TS3 <- calc_min(GDELT_dat, 6, 8)
  
  # temporal features 
  # range (max-min)
  # TS1
  GDELT_dat$T.range.TS1 <- calc_range(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.range.TS2 <- calc_range(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.range.TS3 <- calc_range(GDELT_dat, 6, 8)
  
  # difference (new-old)
  # TS1
  GDELT_dat$T.diff.TS1 <- calc_adif(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.diff.TS2 <- calc_adif(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.diff.TS3 <- calc_adif(GDELT_dat, 6, 8)
  
  # std. deviation 
  # TS1
  GDELT_dat$T.sd.TS1 <- calc_sd(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.sd.TS2 <- calc_sd(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.sd.TS3 <- calc_sd(GDELT_dat, 6, 8)
  
  # variance 
  # TS1
  GDELT_dat$T.var.TS1 <- calc_var(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.var.TS2 <- calc_var(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.var.TS3 <- calc_var(GDELT_dat, 6, 8)
  
  # slope
  # TS1
  GDELT_dat$T.sl.TS1 <- calc_slope(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.sl.TS2 <- calc_slope(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.sl.TS3 <- calc_slope(GDELT_dat, 6, 8) 

  # precentage change
  # TS1
  GDELT_dat$T.pc.TS1 <- calc_perchg(GDELT_dat, 4, 6)
  # TS2
  GDELT_dat$T.pc.TS2 <- calc_perchg(GDELT_dat, 5, 7)
  # TS3
  GDELT_dat$T.pc.TS3 <- calc_perchg(GDELT_dat, 6, 8) 
## Time-sliding windows - numSources -------------------------------------------  
  # window size = 3 
  # non-temporal features 
  # mean
  # TS1
  GDELT_dat$S.mean.TS1 <- calc_mean(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.mean.TS2 <- calc_mean(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.mean.TS3 <- calc_mean(GDELT_dat, 11, 13)
  
  # median 
  # TS1
  GDELT_dat$S.med.TS1 <- calc_median(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.med.TS2 <- calc_median(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.med.TS3 <- calc_median(GDELT_dat, 11, 13)
  
  # max 
  # TS1
  GDELT_dat$S.max.TS1 <- calc_max(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.max.TS2 <- calc_max(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.max.TS3 <- calc_max(GDELT_dat, 11, 13)
  
  # min
  # TS1
  GDELT_dat$S.min.TS1 <- calc_min(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.min.TS2 <- calc_min(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.min.TS3 <- calc_min(GDELT_dat, 11, 13)
  
  # temporal features 
  # range (max-min)
  # TS1
  GDELT_dat$S.range.TS1 <- calc_range(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.range.TS2 <- calc_range(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.range.TS3 <- calc_range(GDELT_dat, 11, 13)
  
  # difference (new-old)
  # TS1
  GDELT_dat$S.diff.TS1 <- calc_adif(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.diff.TS2 <- calc_adif(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.diff.TS3 <- calc_adif(GDELT_dat, 11, 13)
  
  # std. deviation 
  # TS1
  GDELT_dat$S.sd.TS1 <- calc_sd(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.sd.TS2 <- calc_sd(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.sd.TS3 <- calc_sd(GDELT_dat, 11, 13)
  
  # variance 
  # TS1
  GDELT_dat$S.var.TS1 <- calc_var(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.var.TS2 <- calc_var(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.var.TS3 <- calc_var(GDELT_dat, 11, 13)
  
  # slope
  # TS1
  GDELT_dat$S.sl.TS1 <- calc_slope(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.sl.TS2 <- calc_slope(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.sl.TS3 <- calc_slope(GDELT_dat, 11, 13) 
  
  # percentage change
  # TS1
  GDELT_dat$S.pc.TS1 <- calc_perchg(GDELT_dat, 9, 11)
  # TS2
  GDELT_dat$S.pc.TS2 <- calc_perchg(GDELT_dat, 10, 12)
  # TS3
  GDELT_dat$S.pc.TS3 <- calc_perchg(GDELT_dat, 11, 13)
  
## Time-sliding windows - numArticles ------------------------------------------  
  # non-temporal features 
  # mean
  # TS1
  GDELT_dat$A.mean.TS1 <- calc_mean(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.mean.TS2 <- calc_mean(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.mean.TS3 <- calc_mean(GDELT_dat, 16, 18)
  
  # median 
  # TS1
  GDELT_dat$A.med.TS1 <- calc_median(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.med.TS2 <- calc_median(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.med.TS3 <- calc_median(GDELT_dat, 16, 18)
  
  # max 
  # TS1
  GDELT_dat$A.max.TS1 <- calc_max(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.max.TS2 <- calc_max(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.max.TS3 <- calc_max(GDELT_dat, 16, 18)
  
  # min
  # TS1
  GDELT_dat$A.min.TS1 <- calc_min(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.min.TS2 <- calc_min(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.min.TS3 <- calc_min(GDELT_dat, 16, 18)
  
  # temporal features 
  # range (max-min)
  # TS1
  GDELT_dat$A.range.TS1 <- calc_range(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.range.TS2 <- calc_range(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.range.TS3 <- calc_range(GDELT_dat, 16, 18)
  
  # difference (new-old)
  # TS1
  GDELT_dat$A.diff.TS1 <- calc_adif(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.diff.TS2 <- calc_adif(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.diff.TS3 <- calc_adif(GDELT_dat, 16, 18)
  
  # std. deviation 
  # TS1
  GDELT_dat$A.sd.TS1 <- calc_sd(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.sd.TS2 <- calc_sd(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.sd.TS3 <- calc_sd(GDELT_dat, 16, 18)
  
  # variance 
  # TS1
  GDELT_dat$A.var.TS1 <- calc_var(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.var.TS2 <- calc_var(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.var.TS3 <- calc_var(GDELT_dat, 16, 18)
  
  # slope
  # TS1
  GDELT_dat$A.sl.TS1 <- calc_slope(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.sl.TS2 <- calc_slope(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.sl.TS3 <- calc_slope(GDELT_dat, 16, 18) 
  
  # percentage change
  # TS1
  GDELT_dat$A.pc.TS1 <- calc_perchg(GDELT_dat, 14, 16)
  # TS2
  GDELT_dat$A.pc.TS2 <- calc_perchg(GDELT_dat, 15, 17)
  # TS3
  GDELT_dat$A.pc.TS3 <- calc_perchg(GDELT_dat, 16, 18)
  
## Time-sliding windows - numMentions ------------------------------------------   
  # non-temporal features 
  # mean
  # TS1
  GDELT_dat$M.mean.TS1 <- calc_mean(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.mean.TS2 <- calc_mean(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.mean.TS3 <- calc_mean(GDELT_dat, 21, 23)
  
  # median 
  # TS1
  GDELT_dat$M.med.TS1 <- calc_median(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.med.TS2 <- calc_median(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.med.TS3 <- calc_median(GDELT_dat, 21, 23)
  
  # max 
  # TS1
  GDELT_dat$M.max.TS1 <- calc_max(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.max.TS2 <- calc_max(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.max.TS3 <- calc_max(GDELT_dat, 21, 23)
  
  # min
  # TS1
  GDELT_dat$M.min.TS1 <- calc_min(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.min.TS2 <- calc_min(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.min.TS3 <- calc_min(GDELT_dat, 21, 23)
  
  # temporal features 
  # range (max-min)
  # TS1
  GDELT_dat$M.range.TS1 <- calc_range(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.range.TS2 <- calc_range(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.range.TS3 <- calc_range(GDELT_dat, 21, 23)
  
  # difference (new-old)
  # TS1
  GDELT_dat$M.diff.TS1 <- calc_adif(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.diff.TS2 <- calc_adif(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.diff.TS3 <- calc_adif(GDELT_dat, 21, 23)
  
  # std. deviation 
  # TS1
  GDELT_dat$M.sd.TS1 <- calc_sd(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.sd.TS2 <- calc_sd(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.sd.TS3 <- calc_sd(GDELT_dat, 21, 23)
  
  # variance 
  # TS1
  GDELT_dat$M.var.TS1 <- calc_var(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.var.TS2 <- calc_var(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.var.TS3 <- calc_var(GDELT_dat, 21, 23)
  
  # slope
  # TS1
  GDELT_dat$M.sl.TS1 <- calc_slope(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.sl.TS2 <- calc_slope(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.sl.TS3 <- calc_slope(GDELT_dat, 21, 23) 
  
  # percentage change
  # TS1
  GDELT_dat$M.pc.TS1 <- calc_perchg(GDELT_dat, 19, 21)
  # TS2
  GDELT_dat$M.pc.TS2 <- calc_perchg(GDELT_dat, 20, 22)
  # TS3
  GDELT_dat$M.pc.TS3 <- calc_perchg(GDELT_dat, 21, 23)
  
## export datasets
  write.csv(tourist_dat, file = "tourist_dat.csv")
  write.csv(GDELT_dat, file = "GDELT_dat.csv")
  
## prepare GDELT data for experiment 1 -----------------------------------------
GDELT_exp1 <- GDELT_dat[ ,c(1:3, 5:9)]
GDELT_exp1 <- GDELT_exp1 %>%
    group_by(dyad, Actor1Countrycode, Actor2Countrycode) %>%
    summarise(T2013 = sum(ZT2013), T2014 = sum(ZT2014), T2015 = sum(ZT2015),
    T2016 = sum(ZT2016), T2017 = sum(ZT2017)) %>% 
    as.data.frame(GDELT_exp1)          
              
## time sliding - GDELT Exp1 ---------------------------------------------------
# window size = 3 

# non-temporal features 
# mean
# TS1
GDELT_exp1$T.mean.TS1 <- calc_mean(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.mean.TS2 <- calc_mean(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.mean.TS3 <- calc_mean(GDELT_exp1, 7, 9)

# median 
# TS1
GDELT_exp1$T.med.TS1 <- calc_median(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.med.TS2 <- calc_median(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.med.TS3 <- calc_median(GDELT_exp1, 7, 9)

# max 
# TS1
GDELT_exp1$T.max.TS1 <- calc_max(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.max.TS2 <- calc_max(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.max.TS3 <- calc_max(GDELT_exp1, 7, 9)

# min
# TS1
GDELT_exp1$T.min.TS1 <- calc_min(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.min.TS2 <- calc_min(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.min.TS3 <- calc_min(GDELT_exp1, 7, 9)

# temporal features 
# range (max-min)
# TS1
GDELT_exp1$T.range.TS1 <- calc_range(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.range.TS2 <- calc_range(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.range.TS3 <- calc_range(GDELT_exp1, 7, 9)

# difference (new-old)
# TS1
GDELT_exp1$T.diff.TS1 <- calc_adif(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.diff.TS2 <- calc_adif(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.diff.TS3 <- calc_adif(GDELT_exp1, 7, 9)

# std. deviation 
# TS1
GDELT_exp1$T.sd.TS1 <- calc_sd(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.sd.TS2 <- calc_sd(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.sd.TS3 <- calc_sd(GDELT_exp1, 7, 9)

# variance 
# TS1
GDELT_exp1$T.var.TS1 <- calc_var(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.var.TS2 <- calc_var(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.var.TS3 <- calc_var(GDELT_exp1, 7, 9)

# slope
# TS1
GDELT_exp1$T.sl.TS1 <- calc_slope(GDELT_exp1, 5, 7)
# TS2
GDELT_exp1$T.sl.TS2 <- calc_slope(GDELT_exp1, 6, 8)
# TS3
GDELT_exp1$T.sl.TS3 <- calc_slope(GDELT_exp1, 7, 9) 

## Experiment 1: Sentiment tone predicts flows ---------------------------------
# Join data 
dat_exp1 <- inner_join(GDELT_exp1, tourist_dat, by = "dyad")

#factorization of dyad 
is.factor(dat_exp1$dyad)
dat_exp1$dyad <- factor(dat_exp1$dyad)
is.factor(dat_exp1$dyad)

# one-hot encode our categorical variables

# TS1 set
TS1_exp1 <- dat_exp1[ ,c("dyad", "T2013", "T2014", "T2015", "F2013", "F2014", 
                         "F2015","T.mean.TS1", "T.med.TS1", "T.max.TS1", "T.min.TS1",
                         "T.range.TS1", "T.diff.TS1", "T.sd.TS1", "T.var.TS1",
                         "T.sl.TS1" , "F.mean.TS1", "F.med.TS1", "F.max.TS1", 
                         "F.min.TS1", "F.range.TS1", "F.diff.TS1", "F.sd.TS1", 
                         "F.var.TS1", "F.sl.TS1")]
# TS2 set
TS2_exp1 <- dat_exp1[ ,c("dyad", "T2014", "T2015", "T2016", "F2014", "F2015", "F2016",
                         "T.mean.TS2", "T.med.TS2", "T.max.TS2", "T.min.TS2",
                         "T.range.TS2", "T.diff.TS2", "T.sd.TS2", "T.var.TS2",
                         "T.sl.TS2" , "F.mean.TS2", "F.med.TS2", "F.max.TS2", 
                         "F.min.TS2", "F.range.TS2", "F.diff.TS2", "F.sd.TS2", 
                         "F.var.TS2", "F.sl.TS2")] 
# TS3 set
TS3_exp1 <- dat_exp1[ ,c("dyad", "T2015", "T2016", "T2017", "F2015", "F2016", "F2017",
                         "T.mean.TS3", "T.med.TS3", "T.max.TS3", "T.min.TS3",
                         "T.range.TS3", "T.diff.TS3", "T.sd.TS3", "T.var.TS3",
                         "T.sl.TS3" , "F.mean.TS3", "F.med.TS3", "F.max.TS3", 
                         "F.min.TS3", "F.range.TS3", "F.diff.TS3", "F.sd.TS3", 
                         "F.var.TS3", "F.sl.TS3")]  

# data partitioning: train 80%, test 20%
trn_index_1 = createDataPartition(y = dat_exp1$F2015, p = 0.80, list = FALSE)

trn_exp1_TS1 = TS1_exp1[trn_index_1, ]
tst_exp1_TS1 = TS1_exp1[-trn_index_1, ]

trn_exp1_TS2 = TS2_exp1[trn_index_1, ]
tst_exp1_TS2 = TS2_exp1[-trn_index_1, ]

trn_exp1_TS3 = TS3_exp1[trn_index_1, ]
tst_exp1_TS3 = TS3_exp1[-trn_index_1, ]

# determine x and y used in prediction tasks
#TS1 
x1.trn = as.matrix(trn_exp1_TS1[ , c( -1, -7, -5, -6)])
y1.trn = as.matrix(trn_exp1_TS1[ , 7])

x1.tst = as.matrix(tst_exp1_TS1[ , c( -1, -7, -5, -6)])
y1.tst = as.matrix(tst_exp1_TS1[ , 7])

#TS2
x2.trn = as.matrix(trn_exp1_TS2[ , c( -1, -7, -5, -6)])
y2.trn = as.matrix(trn_exp1_TS2[ , 7])

x2.tst = as.matrix(tst_exp1_TS2[ , c( -1, -7, -5, -6)])
y2.tst = as.matrix(tst_exp1_TS2[ , 7])

#TS3
x3.trn = as.matrix(trn_exp1_TS3[ , c( -1, -7, -5, -6)])
y3.trn = as.matrix(trn_exp1_TS3[ , 7])

x3.tst = as.matrix(tst_exp1_TS3[ , c( -1, -7, -5, -6)])
y3.tst = as.matrix(tst_exp1_TS3[ , 7])

# baseline model: use t-1  to predict t+1 --------------------------------------
pred_baseline <- function(data, scol, ecol) {
  y_pred <- c()
  y_true <- c()
  SE <- c()
  for(i in 1:nrow(data)) {
    y_pred[i] <- data[i, scol]
    y_true[i] <- data[i, ecol]
    SE <- (y_true[i]-y_pred[i])^2
  }
  MSE = SE/nrow(data)
  return(MSE)
}

MSE_trn_TS1 <- pred_baseline(trn_exp1_TS1,"F2013", "F2015")
MSE_tst_TS1 <- pred_baseline(tst_exp1_TS1,"F2013", "F2015")

MSE_trn_TS2 <- pred_baseline(trn_exp1_TS2,"F2014", "F2016")
MSE_tst_TS2 <- pred_baseline(tst_exp1_TS2,"F2014", "F2016")

MSE_trn_TS3 <- pred_baseline(trn_exp1_TS3,"F2015", "F2017")
MSE_tst_TS3 <- pred_baseline(tst_exp1_TS3,"F2015", "F2017")

trn_err_baseline <- c(MSE_trn_TS1, MSE_trn_TS2, MSE_trn_TS3)
tst_err_baseline <- c(MSE_tst_TS1, MSE_tst_TS2, MSE_tst_TS3)

b_actual_Y_trn <- c(sum(trn_exp1_TS1$F2015), sum(trn_exp1_TS2$F2016), 
                    sum(trn_exp1_TS3$F2017))
b_predict_Y_trn <- c(sum(trn_exp1_TS1$F2013), sum(trn_exp1_TS2$F2014), 
                   sum(trn_exp1_TS3$F2015))

b_actual_Y_tst <- c(sum(tst_exp1_TS1$F2015), sum(tst_exp1_TS2$F2016), 
                  sum(tst_exp1_TS3$F2017))
b_predict_Y_tst <- c(sum(tst_exp1_TS1$F2013), sum(tst_exp1_TS2$F2014), 
                   sum(tst_exp1_TS3$F2015))

b_results <- data.frame(b_actual_Y_trn, b_predict_Y_trn, trn_err_baseline, 
                        b_actual_Y_tst, b_predict_Y_tst, tst_err_baseline)

baselineTS1 <- ggplot(data= TS1_exp1 , aes(x = dyad, y= F2015)) +
  +   geom_line()+
  +   geom_point()

# supervised 1: lasso regression 
grid_list <- 10^seq(10,-2,length = 100) #define grid list 

# TS1 
lasso.mod1.1 <- glmnet(x1.trn, y1.trn, alpha = 1, lambda = grid_list, 
                intercept = TRUE)
cv.lasso.1.1 <- cv.glmnet(x1.trn, y1.trn, type.measure = "mse", nfolds = 10, 
                lambda = grid_list)
bestlambda.lasso.1.1 <- cv.lasso.1.1$lambda.min
fit.lasso.1.1 <- glmnet(x1.trn, y1.trn, alpha = 1, 
                        lambda = bestlambda.lasso.1.1, intercept = TRUE)

pred.lasso.1.1 <- predict(fit.lasso.1.1, x1.tst)
mse.pred.lasso.1.1 <- (sum((y1.tst - pred.lasso.1.1)^2))/nrow(y1.tst)

plot(lasso.mod1.1)
plot(cv.lasso.1.1)
coef(cv.lasso.1.1)
lasso.1.1.results <- data.frame(pred.lasso.1.1, y1.tst)

# TS2 
lasso.mod1.2 <- glmnet(x2.trn, y2.trn, alpha = 1, lambda = grid_list, 
                       intercept = TRUE)
cv.lasso.1.2 <- cv.glmnet(x2.trn, y2.trn, type.measure = "mse", nfolds = 10, 
                          lambda = grid_list)
bestlambda.lasso.1.2 <- cv.lasso.1.2$lambda.min
fit.lasso.1.2 <- glmnet(x2.trn, y2.trn, alpha = 1, 
                        lambda = bestlambda.lasso.1.2, intercept = TRUE)

pred.lasso.1.2 <- predict(fit.lasso.1.2, x2.tst)
mse.pred.lasso.1.2 <- (sum((y2.tst - pred.lasso.1.2)^2))/nrow(y2.tst)

plot(lasso.mod1.2)
plot(cv.lasso.1.2)
coef(cv.lasso.1.2)
lasso.1.2.results <- data.frame(pred.lasso.1.2, y2.tst)

# TS3 
lasso.mod1.3 <- glmnet(x3.trn, y3.trn, alpha = 1, lambda = grid_list, 
                       intercept = TRUE)
cv.lasso.1.3 <- cv.glmnet(x3.trn, y3.trn, type.measure = "mse", nfolds = 10, 
                          lambda = grid_list)
bestlambda.lasso.1.3 <- cv.lasso.1.3$lambda.min
fit.lasso.1.3 <- glmnet(x3.trn, y3.trn, alpha = 1, 
                        lambda = bestlambda.lasso.1.3, intercept = TRUE)

pred.lasso.1.3 <- predict(fit.lasso.1.3, x3.tst)
mse.pred.lasso.1.3 <- (sum((y3.tst - pred.lasso.1.3)^2))/nrow(y3.tst)

plot(lasso.mod1.3)
plot(cv.lasso.1.3)
coef(cv.lasso.1.3)
lasso.1.3.results <- data.frame(pred.lasso.1.3, y3.tst)

#lasso summary 
lasso.1.tst.summary <- c(mse.pred.lasso.1.1, mse.pred.lasso.1.2, 
                         mse.pred.lasso.1.3)
lasso.1.pred.tst.summary <- c(sum(pred.lasso.1.1), sum(pred.lasso.1.2), 
                              sum(pred.lasso.1.3))

# supervised 2: ridge regression 
# TS1 
ridge.mod1.1 <- glmnet(x1.trn, y1.trn, alpha = 0, lambda = grid_list, 
                       intercept = TRUE)
cv.ridge.1.1 <- cv.glmnet(x1.trn, y1.trn, type.measure = "mse", nfolds = 10, 
                          lambda = grid_list)
bestlambda.ridge.1.1 <- cv.ridge.1.1$lambda.min
fit.ridge.1.1 <- glmnet(x1.trn, y1.trn, alpha = 0, 
                        lambda = bestlambda.ridge.1.1, intercept = TRUE)

pred.ridge.1.1 <- predict(fit.ridge.1.1, x1.tst)
mse.pred.ridge.1.1 <- (sum((y1.tst - pred.ridge.1.1)^2))/nrow(y1.tst)

plot(ridge.mod1.1)
plot(cv.ridge.1.1)
coef(cv.ridge.1.1)
ridge.1.1.results <- data.frame(pred.ridge.1.1, y1.tst)

# TS2 
ridge.mod1.2 <- glmnet(x2.trn, y2.trn, alpha = 0, lambda = grid_list, 
                       intercept = TRUE)
cv.ridge.1.2 <- cv.glmnet(x2.trn, y2.trn, type.measure = "mse", nfolds = 10, 
                          lambda = grid_list)
bestlambda.ridge.1.2 <- cv.ridge.1.2$lambda.min
fit.ridge.1.2 <- glmnet(x2.trn, y2.trn, alpha = 0, 
                        lambda = bestlambda.ridge.1.2, intercept = TRUE)

pred.ridge.1.2 <- predict(fit.ridge.1.2, x2.tst)
mse.pred.ridge.1.2 <- (sum((y2.tst - pred.ridge.2.2)^2))/nrow(y2.tst)

plot(ridge.mod1.2)
plot(cv.ridge.1.2)
coef(cv.ridge.1.2)
ridge.1.2.results <- data.frame(pred.ridge.1.2, y2.tst)

# TS3 
ridge.mod1.3 <- glmnet(x3.trn, y3.trn, alpha = 0, lambda = grid_list, 
                       intercept = TRUE)
cv.ridge.1.3 <- cv.glmnet(x3.trn, y3.trn, type.measure = "mse", nfolds = 10, 
                          lambda = grid_list)
bestlambda.ridge.1.3 <- cv.ridge.1.3$lambda.min
fit.ridge.1.3 <- glmnet(x3.trn, y3.trn, alpha = 0, 
                        lambda = bestlambda.ridge.1.3, intercept = TRUE)

pred.ridge.1.3 <- predict(fit.ridge.1.3, x3.tst)
mse.pred.ridge.1.3 <- (sum((y3.tst - pred.ridge.1.3)^2))/nrow(y3.tst)

plot(ridge.mod1.3)
plot(cv.ridge.1.3)
coef(cv.ridge.1.3)
ridge.1.3.results <- data.frame(pred.ridge.1.3, y3.tst)

# summarize results ridge
ridge.1.tst.summary <- c(mse.pred.ridge.1.1, mse.pred.ridge.1.2, 
                         mse.pred.ridge.1.3)
ridge.1.pred.tst.summary <- c(sum(pred.ridge.1.1), sum(pred.ridge.1.2), 
                              sum(pred.ridge.1.3))

# Supervised 3: random forest 
# TS1 
rf.tune.1.1 <- tuneRF(x = x1.trn, y = y1.trn, ntreeTry = 50, mtryStart  = 5,
  stepFactor = 1.5, improve = 0.01, trace = FALSE)

rf.model.1.1 <- randomForest(formula = y1.trn ~ x1.trn)

# Supervised 4: support vector machine 
# TS1 
Y1.1 = y1.trn
X1.1 = x1.trn

#basic model
svm.1.1 <- svm(Y1.1 ~ X1.1)
predictedY <- predict(svm.1.1, trn_exp1_TS1)

##Calculate parameters of the SVR model
#Find value of W
W = t(svm.1.1$coefs) %*% svm.1.1$SV

#Find value of b
b = svm.1.1$rho

svm.1.1.trn.error <- y1.trn - predictedY
svm.1.1.trn.mse <- (sum(y1.trn - predictedY)^2)/nrow(y1.trn)

svm.1.1$coefs

#Tune the SVM model
OptModelsvm1.1 = tune(svm, Y1.1~X1.1, ranges=list(elsilon=seq(0,1,0.1), 
                                                  cost=1:100))

# experiment 1 summary
TS <- c(1, 2, 3)
as.integer(TS)
Exp1.summary.error <- data.frame(TS, tst_err_baseline, 
                           lasso.1.tst.summary, ridge.1.tst.summary)
M.Exp1.summary.error <- melt(Exp1.summary.error, id = 'TS')

Exp1.error.plot <- ggplot(M.Exp1.summary.error, aes(x = TS, y = value, colour = variable)) + 
  geom_line() + 
  geom_point() +
  ylab(label="MSE") + 
  xlab("Time window") + 
  scale_colour_manual(values=c("grey","red", "blue"))
Exp1.error.plot

Exp1.summary.valueY <- data.frame(TS, b_actual_Y_tst, b_predict_Y_tst, 
                                  lasso.1.pred.tst.summary, 
                                  ridge.1.pred.tst.summary)
M.Exp1.summary.valueY <- melt(Exp1.summary.valueY, id = 'TS')

Exp1.valueY.plot <- ggplot(M.Exp1.summary.valueY, aes(x = TS, y = value, colour = variable)) + 
  geom_line() + 
  geom_point() +
  ylab(label="Tourist Flow") + 
  xlab("Time window")
Exp1.valueY.plot

## Experiment 2: Taking event type into account --------------------------------
# Join data 
dat_exp2 <- inner_join(GDELT_dat, tourist_dat, by = "dyad")
factor(dat_exp2$EventRootCode)
factor(dat_exp2$dyad)

# TS1 set
TS1_exp2 <- dat_exp2[ ,c("dyad", "EventRootCode", "ZT2013", "ZT2014", "ZT2015", 
                         "F2013", "F2014", "F2015","T.mean.TS1", "T.med.TS1", 
                         "T.max.TS1", "T.min.TS1","T.range.TS1", "T.diff.TS1", 
                         "T.sd.TS1", "T.var.TS1","T.sl.TS1" , "F.mean.TS1", 
                         "F.med.TS1", "F.max.TS1", "F.min.TS1", "F.range.TS1", 
                         "F.diff.TS1", "F.sd.TS1", "F.var.TS1", "F.sl.TS1")]
# TS2 set
TS2_exp2 <- dat_exp2[ ,c("dyad", "EventRootCode", "ZT2014", "ZT2015", "ZT2016", 
                          "F2014", "F2015", "F2016","T.mean.TS1", "T.med.TS1", 
                          "T.max.TS1", "T.min.TS1","T.range.TS1", "T.diff.TS1", 
                          "T.sd.TS1", "T.var.TS1","T.sl.TS1" , "F.mean.TS1", 
                          "F.med.TS1", "F.max.TS1", "F.min.TS1", "F.range.TS1", 
                          "F.diff.TS1", "F.sd.TS1", "F.var.TS1", "F.sl.TS1")] 
# TS3 set
TS3_exp2 <- dat_exp2[ ,c("dyad", "EventRootCode", "ZT2015", "ZT2016", "ZT2017", 
                         "F2015", "F2016", "F2017","T.mean.TS1", "T.med.TS1", 
                         "T.max.TS1", "T.min.TS1","T.range.TS1", "T.diff.TS1", 
                         "T.sd.TS1", "T.var.TS1","T.sl.TS1" , "F.mean.TS1", 
                         "F.med.TS1", "F.max.TS1", "F.min.TS1", "F.range.TS1", 
                         "F.diff.TS1", "F.sd.TS1", "F.var.TS1", "F.sl.TS1")]

# data partitioning: train 80%, test 20%
trn_index_2 = createDataPartition(y = dat_exp2$F2015, p = 0.80, list = FALSE)

trn_exp2_TS1 = TS1_exp2[trn_index_2, ]
tst_exp2_TS1 = TS1_exp2[-trn_index_2, ]

trn_exp2_TS2 = TS2_exp2[trn_index_2, ]
tst_exp2_TS2 = TS2_exp2[-trn_index_2, ]

trn_exp2_TS3 = TS3_exp2[trn_index_2, ]
tst_exp2_TS3 = TS3_exp2[-trn_index_2, ]

# determine x and y used in prediction tasks
#TS1 
x2.1.trn = as.matrix(trn_exp2_TS1[ , c(-1, -6, -7, -8)])
y2.1.trn = as.matrix(trn_exp2_TS1[ , 8])

x2.1.tst = as.matrix(tst_exp2_TS1[ , c( -1, -6, -7, -8)])
y2.1.tst = as.matrix(tst_exp2_TS1[ , 8])

#TS2
x2.2.trn = as.matrix(trn_exp2_TS2[ , c( -1, -6, -7, -8)])
y2.2.trn = as.matrix(trn_exp2_TS2[ , 8])

x2.2.tst = as.matrix(tst_exp2_TS2[ , c( -1, -6, -7, -8)])
y2.2.tst = as.matrix(tst_exp2_TS2[ , 8])

#TS3
x2.3.trn = as.matrix(trn_exp2_TS3[ , c( -1, -6, -7, -8)])
y2.3.trn = as.matrix(trn_exp2_TS3[ , 8])

x2.3.tst = as.matrix(tst_exp2_TS3[ , c( -1, -6, -7, -8)])
y2.3.tst = as.matrix(tst_exp2_TS3[ , 8])

# baseline model 2: use t-1  to predict t+1 ------------------------------------
MSE_trn_TS1_2 <- pred_baseline(trn_exp2_TS1,"F2013", "F2015")
MSE_tst_TS1_2 <- pred_baseline(tst_exp2_TS1,"F2013", "F2015")

MSE_trn_TS2_2 <- pred_baseline(trn_exp2_TS2,"F2014", "F2016")
MSE_tst_TS2_2 <- pred_baseline(tst_exp2_TS2,"F2014", "F2016")

MSE_trn_TS3_2 <- pred_baseline(trn_exp2_TS3,"F2015", "F2017")
MSE_tst_TS3_2 <- pred_baseline(tst_exp2_TS3,"F2015", "F2017")

trn_err_baseline_2 <- c(MSE_trn_TS1_2, MSE_trn_TS2_2, MSE_trn_TS3_2)
tst_err_baseline_2 <- c(MSE_tst_TS1_2, MSE_tst_TS2_2, MSE_tst_TS3_2)

b_actual_Y_trn_2 <- c(sum(trn_exp2_TS1$F2015), sum(trn_exp2_TS2$F2016), 
                    sum(trn_exp2_TS3$F2017))
b_predict_Y_trn_2 <- c(sum(trn_exp2_TS1$F2013), sum(trn_exp2_TS2$F2014), 
                     sum(trn_exp2_TS3$F2015))

b_actual_Y_tst_2 <- c(sum(tst_exp2_TS1$F2015), sum(tst_exp2_TS2$F2016), 
                    sum(tst_exp2_TS3$F2017))
b_predict_Y_tst_2 <- c(sum(tst_exp2_TS1$F2013), sum(tst_exp2_TS2$F2014), 
                     sum(tst_exp2_TS3$F2015))

# supervised 1: lasso regression 
grid_list <- 10^seq(10,-2,length = 100) #define grid list 

# TS1 
lasso.mod2.1 <- glmnet(x2.1.trn, y2.1.trn, alpha = 1, lambda = grid_list, 
                       intercept = TRUE)
cv.lasso.2.1 <- cv.glmnet(x2.1.trn, y2.1.trn, type.measure = "mse", nfolds = 10, 
                          lambda = grid_list)
bestlambda.lasso.1.1 <- cv.lasso.1.1$lambda.min
fit.lasso.1.1 <- glmnet(x1.trn, y1.trn, alpha = 1, 
                        lambda = bestlambda.lasso.1.1, intercept = TRUE)

pred.lasso.1.1 <- predict(fit.lasso.1.1, x1.tst)
mse.pred.lasso.1.1 <- (sum((y1.tst - pred.lasso.1.1)^2))/nrow(y1.tst)

plot(lasso.mod1.1)
plot(cv.lasso.1.1)
coef(cv.lasso.1.1)
lasso.1.1.results <- data.frame(pred.lasso.1.1, y1.tst)
