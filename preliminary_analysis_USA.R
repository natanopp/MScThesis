## Preliminary analysis
## Name: Natanop Pimonsathian
## u-number: u786200

setwd("C:/Users/Natanop/Desktop/Study/Tillburg University/Tilburg Uni Study/DSBG/Thesis/Scripts/R")

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

## Load data -------------------------------------------------------------------
## USA Tourists statistics 
tourist <- read.csv("input/unitedstates_cleaned.csv", stringsAsFactors = FALSE)
gdelt <- read.csv("input/USA_final_GDELT.csv", stringsAsFactors = FALSE)

## join 
joined_dat <- left_join(gdelt,tourist, by = "CountryYear")

year_v_flow <- ggplot() +
  geom_point(data = joined_dat, aes(x = Year.y, y = Flow))

Tone_v_flow <- ggplot() +
  geom_point(data = joined_dat, aes(x = Flow, y = Tone)) 

year_v_tone <- ggplot() +
  geom_point(data = joined_dat, aes(x = Year.y, y = Tone))
  
mentions_v_flow <- ggplot() +
  geom_point(data = joined_dat, aes(x = Mentions, y = Flow)) 

articles_v_flow <- ggplot() +
  geom_point(data = joined_dat, aes(x = Articles, y = Flow))

sources_v_flow <- ggplot() +
  geom_point(data = joined_dat, aes(x = Sources, y = Flow))
