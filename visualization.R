## visualization Thesis 

## Load packages and set seed --------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(imputeTS)
library(reshape2)
library(scales)

## Load data -------------------------------------------------------------------
GDELT <- read.csv("input/GDELT_worldwide.csv", stringsAsFactors = FALSE)
tourist <- read.csv("input/UNWTO_working.csv", stringsAsFactors = FALSE)
GDELT_r <- read.csv("input/GDELT_dat.csv", stringsAsFactors = FALSE)

WB <- read.csv("input/worldbank.csv", stringsAsFactors = FALSE)
WB$Year <- factor(WB$Year)

working_dat <- read.csv("input/model.dat.csv", stringsAsFactors = FALSE)
SL <- read.csv("input/sliding.csv", stringsAsFactors = FALSE)

# flow and percentage change 
f_pc <- read.csv("input/flow_and_pc.csv", stringsAsFactors = FALSE)

e1.tst <- read.csv("input/e1_test_new.csv", stringsAsFactors = TRUE)
e1.tst$Time.Window <- as.factor(e1.tst$Time.Window)

e2.tst <- read.csv("input/e2_test.csv", stringsAsFactors = TRUE)
e2.tst$Time.Window <- as.factor(e2.tst$Time.Window)

e3.tst <- read.csv("input/e3_test.csv", stringsAsFactors = TRUE)
e3.tst$Time.Window <- as.factor(e3.tst$Time.Window)

e1_act <- read.csv("input/act_e1.csv", stringsAsFactors = FALSE)
e1_act$Time_window <- as.factor(e1_act$Time_window )

pro_dat <- read.csv("input/experiment_data.csv", stringsAsFactors = FALSE)
pro_dat$F2017 <- as.numeric(pro_dat$F2017)

# flow per destination 
flow_dest_year <- read.csv("input/flow_dest_year.csv", stringsAsFactors = FALSE)

# actual vs predicted values per experiment condition. 
# experiment 1 
act_experiment1 <- read.csv("input/actual_values_experiment1.csv", stringsAsFactors = FALSE) 
#act_experiment1_r <- data.frame(windows = c("TS1", "TS2","TS3"),act_experiment1)
# experiment 2 
act_experiment2 <- read.csv("input/actual_values_experiment2.csv", stringsAsFactors = FALSE) 
# experiment 3 
act_experiment3 <- read.csv("input/actual_values_experiment3.csv", stringsAsFactors = FALSE) 

# MSE per destination of experiment 1 test set 
ex1_err_dest_enet_r <- read.csv("input/ex1_err_dest_enet.csv", stringsAsFactors = FALSE)

## tourist flows ---------------------------------------------------------------
tourist_flow <- pro_dat %>% 
  group_by(Destination_code) %>%
  summarise(F2013 = sum(F2013),
            F2014 = sum(F2014), 
            F2015 = sum(F2015),
            F2016 = sum(F2016),
            F2017 = sum(F2017))%>%
  as.data.frame(tourist_flow)

tourist_flow_t <- t(tourist_flow)
year <- c("dest_code","2013","2014","2015","2016","2017")
year_t1 <- cbind(year)
#dest_code <- tourist_flow$Destination_code
#dest_code <- as.data.frame(dest_code)
tourist_flow_t <- data.frame(year_t1, tourist_flow_t)



flow_dest_plot <- ggplot(data = flow_dest_year, aes(x = Destination_code, 
                  group = 1, y =  )) +
                  geom_line()


flow_dest_year_df <- flow_dest_year %>%
  gather(key = "variable", value = "value", -year)

flow_dest_year_df_plt <- ggplot(flow_dest_year_df, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 0.5) 

## GDELT -----------------------------------------------------------------------
 # summarize data 
  GDELT_dat <- GDELT %>% 
    group_by(Year, Actor1Countrycode, Actor2Countrycode) %>%
    summarise(Tone = mean(avg_AvgTone)) %>%
    as.data.frame(GDELT_dat)
  
  GDELT_dat <- GDELT_dat %>% 
    spread(Year, Tone)

  # missing values identification
  for(i in 1:nrow(GDELT_dat)) {
    GDELT_dat$t_missing_percent[i] <- (sum(is.na(GDELT_dat[i, 3:7])))
  }

  # visualize 
  GDELT_t_missing <-ggplot(data = GDELT_dat, aes(x = factor(t_missing_percent))) +
    geom_bar(stat = "count") +
    geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
    labs(x = "Number of missing data points per row") + 
    labs(y = "Number of observations (rows)") +
    labs(title = "Distribution of missing values: Sentiment Tone")

## UNWTO -----------------------------------------------------------------------
  tourist_dat <- tourist %>% 
    group_by(Destination_code, Origin_code) %>%
    summarise(F2013 = sum(Flow_Y2013),
              F2014 = sum(Flow_Y2014), 
              F2015 = sum(Flow_Y2015),
              F2016 = sum(Flow_Y2016),
              F2017 = sum(Flow_Y2017))%>%
    as.data.frame(tourist_dat)  

  # missing values identification  
  for(i in 1:nrow(tourist_dat)) {
    tourist_dat$missing_percent[i] <- (sum(is.na(tourist_dat[i, 3:7])))
  }
  
  # visualize 
  UNWTO_missing <-ggplot(data = tourist_dat, aes(x = factor(missing_percent))) +
    geom_bar(stat = "count") +
    geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) +
    labs(x = "Number of missing data points per row") + 
    labs(y = "Number of observations (rows)") +
    labs(title = "Distribution of missing values: Inbound tourist flows")

## Missing values imputation ---------------------------------------------------
  GDELT_dat <- GDELT_dat %>% 
    filter(t_missing_percent < 4)
  
  GDELT_tone_impute <-t(GDELT_dat[ ,3:7])
  b <- na.interpolation(GDELT_tone_impute, option = "linear")
  GDELT_tone_impute <- as.data.frame(t(b))
  
  GDELT_dat <- data.frame(GDELT_dat[,1:2], GDELT_tone_impute)
  
  GDELT_t_dest <- GDELT_dat %>% 
    group_by(Actor1Countrycode) %>%
    summarise("2013" = mean(X2013),
              "2014" = mean(X2014), 
              "2015" = mean(X2015),
              "2016" = mean(X2016),
              "2017" = mean(X2017))%>%
    as.data.frame(GDELT_t_dest)
  
  sample_country = c("USA", "NLD" , "THA" , "AFG")
  
  GDELT_t_dest <- GDELT_t_dest %>%
    filter(Actor1Countrycode %in% sample_country)
  
  GDELT_t_dest_d <- melt(GDELT_t_dest, id.vars="Actor1Countrycode")
  
  GDELT_dest_plot <- ggplot(GDELT_t_dest_d, aes(variable,value, group=1)) + 
      geom_point() +
      geom_line() +
      #stat_smooth() +
      facet_wrap(~Actor1Countrycode) +
      labs(x = "Year") + 
      labs(y = "Average Sentiment Tone") #+
      #labs(title = "Distribution of missing values: Inbound tourist flows")
  
  # GDELT after normalization 
  GDELT_r_dest <- GDELT_r %>% 
    group_by(Actor1Countrycode) %>%
    summarise("2013" = mean(ZT2013),
              "2014" = mean(ZT2014), 
              "2015" = mean(ZT2015),
              "2016" = mean(ZT2016),
              "2017" = mean(ZT2017))%>%
    as.data.frame(GDELT_r_dest)
  
  GDELT_r_dest <- GDELT_r_dest %>%
    filter(Actor1Countrycode %in% sample_country)
  
  GDELT_r_dest_d <- melt(GDELT_r_dest, id.vars="Actor1Countrycode")
  
  GDELT_dest_r_plot <- ggplot(GDELT_r_dest_d, aes(variable,value, group=1)) + 
    geom_point() +
    geom_line() +
    #stat_smooth() +
    facet_wrap(~Actor1Countrycode) +
    labs(x = "Year") + 
    labs(y = "Average Sentiment Tone")
  
## plot worldbank stat ---------------------------------------------------------
WB$Arrivals <- (WB$Number.of.international.tourist.arrivals) / (10^6)
WB$Arrivals <- round(WB$Arrivals, digits = 0)  
  
WB_plot<- ggplot(WB, aes (x = Year, y = Arrivals, group = 1)) +
    geom_line(size = 1, color = "grey50") +
    scale_y_continuous(limits = c(500, 1450)) +
    geom_point(color = "grey20") +
    geom_text(aes(label = Arrivals), 
              size = 3, angle = 0,
              hjust = 0.5,
              vjust = -1.1) +
    labs(y = "Number of arrivals (in millions)") +
    labs(x = "Calendar year", angle = 90) +
    labs(title = "Yearly Aggregate International Tourist Arrivals") +
    theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.25)) 

## subset data to work with sliding window -------------------------------------
dat_SL <- working_dat %>%
  select(F2013, F2014, F2015, F2016, F2017,
         ZT2013, ZT2014, ZT2015, ZT2016, ZT2017)

dat_SL <- dat_SL[37:37,]
dat_SL$F2017 <- as.numeric(dat_SL$F2017)  

SL$Year <- as.factor(SL$Year)

SL_plot <- ggplot(SL, aes (x = Year, group = 1)) +
  geom_line(aes(y = S, linetype = "S")) +
  geom_point(aes(y = S, shape = "S")) +
  geom_line(aes(y = F, linetype = "F")) +
  geom_point(aes(y = F, shape = "F")) +
  theme_classic()

SL_1 <- ggplot(SL[1:3,], aes (x = Year, group = 1)) +
  geom_line(aes(y = S, linetype = "S")) +
  geom_point(aes(y = S, shape = "S")) +
  geom_line(aes(y = F, linetype = "F")) +
  geom_point(aes(y = F, shape = "F")) 
  theme_classic()

SL_2 <- ggplot(SL[2:4,], aes (x = Year, group = 1)) +
  geom_line(aes(y = S, linetype = "S")) +
  geom_point(aes(y = S, shape = "S")) +
  geom_line(aes(y = F, linetype = "F")) +
  geom_point(aes(y = F, shape = "F")) +
  theme_classic()

SL_3 <- ggplot(SL[3:5,], aes (x = Year, group = 1)) +
  geom_line(aes(y = S, linetype = "S")) +
  geom_point(aes(y = S, shape = "S")) +
  geom_line(aes(y = F, linetype = "F")) +
  geom_point(aes(y = F, shape = "F")) +
  theme_classic()

## Experiment 1 results --------------------------------------------------------

# error by time sliding 
e1_tst_sl_new <- e1.tst %>%
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', breaks = c(0.3, 0.5, 0.7), limit = c(0.2,0.8)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 3, 5, 2)) +
  theme(legend.position = "bottom") +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") 
  
# actual value - flow
flows_ex1_plot <- ggplot(act_experiment1[-1,], aes(Time.window, group =1)) +
  geom_line(aes(y = flow.act, linetype = "Actual")) +
  geom_point(aes(y = flow.act)) +
  geom_line(aes(y = flow.pred.base, linetype = "Baseline")) +
  geom_point(aes(y = flow.pred.base)) +
  geom_line(aes(y = flow.pred.enet, linetype = "Elastic Net" )) +
  geom_point(aes(y = flow.pred.enet)) +
  labs(x = "Time window") + 
  labs(y = "Tourist inbound flows (millions)") +
  theme_classic()

# actual value - percentage change 
pc_ex1_df <- data.frame(act_experiment1[-1, 1], act_experiment1[-1, 5:7]) 
pc_ex1_df <- pc_ex1_df %>%
    rename(Time.window = act_experiment1..1..1.)
pc_ex1_col <- c(pc_ex1_df[,2], pc_ex1_df[,3], pc_ex1_df[,4])
pc_ex1_df_r <- data.frame(Algorithm = rep(c("Act", "Baseline", "Elastic Net"), each = 3),
                          Time.window = rep(c("TS1", "TS2", "TS3"), each = 3),
                          pc = pc_ex1_col)

pc_ex1_plot <- ggplot(pc_ex1_df_r, aes(x = Algorithm, y = pc, fill = Time.window)) +
  geom_bar(position = "dodge2", stat = "identity") +
  theme_classic()

# MSEs per Destination (trained Elastic Net)
# Central value = mean
ex1_err_dest_enet_r <- ex1_err_dest_enet_r %>%
  arrange(desc(MSE.dest))
ex1_err_dest_enet_r$test_e1.Destination_code <- 
  factor(ex1_err_dest_enet_r$test_e1.Destination_code,
         levels = c("TJK","SLE","RUS","SUR","TUN","HND","TUR","MDV","ISL",
                    "LSO","PAN","BRB","RWA","KNA","VCT","GRC","COL","GBR",
                    "MMR","PRI","TWN", "JPN"))

# Central value = median 
ex1_err_dest_enet_med <- ex1_err_dest_enet_r %>%
  group_by(test_e1.Destination_code) %>%
  summarise("MSE.Median" = median(MSE.enet)) %>%
  arrange(desc(MSE.Median)) %>%
  as.data.frame(ex1_err_dest_enet_med)

# Barplot
barp.ex1.dest.err <- ex1_err_dest_enet_r%>%
  group_by(test_e1.Destination_code) %>%
  summarise("MSE" = mean(MSE.enet)) %>%
  ggplot(aes(x=test_e1.Destination_code, y=MSE)) +
  geom_bar(stat = "identity") +
  labs(x = "Destination") + 
  labs(y = "MSE Score") +
  labs(title = "Comparison of the MSE scores per country of destination")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.25)) +
  theme(plot.title = element_text(size = 13))

bk <- c(0.01, 0.1, 0.5, 1,2,3,5,10,15)
# Boxplot
boxp.ex1.dest.err <- ex1_err_dest_enet_r%>%
  ggplot(aes(x=test_e1.Destination_code, y=MSE.enet)) +
  geom_boxplot() +
  coord_trans(y = "log1p") +
  scale_y_continuous(breaks = bk, limit = c(0,15)) +
  labs(x = "Destination") + 
  labs(y = "MSE Score (in a log1p scale)") +
  labs(title = "Comparison of the MSE scores per country of destination")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.25)) +
  theme(plot.title = element_text(size = 13)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

# feature importace : Random forest 
RF.IMP.EX1.r <- RF.IMP.EX1 %>%
  select(row, MeanPurity) %>%
  arrange(desc(MeanPurity))

RF.IMP.EX1.r$row <-factor(RF.IMP.EX1.r$row,
       levels = c("Fpc, t-2, t-1", "Spc, t-2, t-1",
                  "Spc, t-1 ,t", "St-1", "St", "Sdiff", 
                  "St-2", "Smed", "Smax", "Ssl", "Smean", 
                  "Smin", "Sr", "Svar", "Ssd"))

RF.IMP.EX1.plt <- RF.IMP.EX1.r %>%
  ggplot(aes(x = row, y = MeanPurity)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(MeanPurity,2)), size = 3, 
            vjust = -0.2) +
  labs(x = "Feature", y = "Mean Decrease Purity") +
  scale_y_continuous(limits = c(0,305)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        axis.line = element_line(colour = "black"))
  
## Experiment 2 results --------------------------------------------------------
# Low
e2_tst_low <- e2.tst %>% 
  filter(Income == "L")

e2.tst.Lo.plt <- e2_tst_low %>%
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.1, 1.8)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 19, 24, 7)) +
  theme(legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        axis.line = element_line(colour = "black")) +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 1: Low Income")

# Lower middle
e2_tst_LM <- e2.tst %>% 
  filter(Income == "LM")

e2.tst.LM.plt <- e2_tst_LM %>%
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.1, 1.7)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 19, 24, 7)) +
  theme(legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        axis.line = element_line(colour = "black")) +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 2: Lower Middle Income")

# Uppermiddle
e2_tst_UM <- e2.tst %>% 
  filter(Income == "UM")

e2.tst.UM.plt <- e2_tst_UM %>%
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.1, 1.7)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 19, 24, 7)) +
  theme(legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        axis.line = element_line(colour = "black")) +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 3: Upper Middle Income")

# High
e2_tst_H <- e2.tst %>% 
  filter(Income == "H")

e2.tst.H.plt <- e2_tst_H %>%
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.07, 1.7)) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 19, 24, 7)) +
  theme(legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        axis.line = element_line(colour = "black")) +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 4: High Income")

## Experiment 3 results --------------------------------------------------------
# High
e3_tst_h <- e3.tst %>% 
  filter(Access == "H")

e3.tst.h.plt <- e3_tst_h %>% 
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.07, 1.7)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 3, 5, 2)) +
  theme(legend.position = "bottom") +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 1: High Accessibility")

# Moderate
e3_tst_m <- e3.tst %>% 
  filter(Access == "M")

e3.tst.m.plt <- e3_tst_m %>% 
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.07, 1.7)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 3, 5, 2)) +
  theme(legend.position = "bottom") +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 2: Moderate Accessibility")

# Low
e3_tst_L <- e3.tst %>% 
  filter(Access == "L")

e3.tst.L.plt <- e3_tst_L %>% 
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.07, 1.7)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 3, 5, 2)) +
  theme(legend.position = "bottom") +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 3: Low Accessibility")

# Very low
e3_tst_VL <- e3.tst %>% 
  filter(Access == "VL")

e3.tst.VL.plt <- e3_tst_VL %>% 
  ggplot(aes(x = Time.Window, y = MSE, group = Algorithm)) +
  geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_y_continuous(trans = 'log10', 
                     breaks = c(0.1, 0.3, 0.5, 1.0, 1.5),
                     limits = c(0.07, 1.7)) +
  theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_shape_manual(values = c(15, 3, 5, 2)) +
  theme(legend.position = "bottom") +
  labs(x = "Time window") + 
  labs(y = "MSE Score (in a log10 scale)") +
  labs(title = "Condition 4: Very Low Accessibility")

## display flows and rate of change ---------------------------------------------
 # world flows(global sum)
  g_flow_plot <- ggplot(f_pc, aes(x = Year, y = Flow_norm)) +
    geom_line(size = 1, color = "grey50") +
    geom_point(color = "grey20") +
    geom_text(aes(label = Flow_norm), hjust=0.6, vjust= -0.9) +
    scale_y_continuous(limits = c(900, 1200)) +
    labs(y = "Tourist inbound flows (millions)") +
    theme(panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

 # world rate of change
  g_pc_plot <- ggplot(f_pc, aes(x = Year, y = Percentage.change.norm)) +
    geom_line(size = 1, color = "grey50") +
    geom_point(color = "grey20") +
    geom_text(aes(label = Percentage.change.norm), hjust=0.6, vjust= -0.9) +
    scale_y_continuous(limits = c(0, 6)) +
    labs(y = "Percentage change (percent)") +
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))
 
 # experiment1 - flow 
  
  
