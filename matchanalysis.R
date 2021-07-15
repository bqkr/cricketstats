library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
setwd("~/Documents/cricstat/tests_csv2")

sample <- read_csv("64128.csv")
#results of each inning
inning_results <- group_by(sample,innings,batting_team) %>% summarise(runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))

#batsmen performance 
#also allocates all extras to the batter
batting_results <- group_by(sample,innings,batting_team,striker) %>% summarise(runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))

batting_results %>% ggplot(aes(x=striker,y=runs,color=striker))+geom_col()+facet_grid(innings ~ batting_team)
filter(batting_results, batting_team == "Australia") %>% ggplot(aes(x=striker,y=runs,color=striker))+geom_text(aes(label=striker))+facet_grid(innings ~ batting_team)

mutate(sample, over = trunc(ball)) %>% 
  group_by(innings, batting_team, over) %>% 
  summarise(runs = sum(runs_off_bat, extras),  wicket = any(!is.na(wicket_type))) %>% 
  ggplot(aes(over, runs, fill=batting_team)) + geom_col() + facet_grid(innings ~ . ) + labs(title = paste(sample$start_date, "at", sample$venue)) 

mutate(sample, over = trunc(ball)) %>% 
  group_by(innings, batting_team, over) %>% 
  summarise(runs = sum(runs_off_bat, extras),  wicket = any(!is.na(wicket_type))) %>% 
  ggplot(aes(over, runs, fill=wicket, color=batting_team)) + geom_col() + facet_grid(innings ~ . ) + labs(title = paste(sample$start_date, "at", sample$venue)) 


mutate(sample, over = trunc(ball)) %>% 
  group_by(innings, batting_team, over) %>% 
  summarise(runs = sum(runs_off_bat, extras)) %>% 
  summarise(innings, over, runs, total = cumsum(runs)) %>% 
  ggplot(aes(over,total, fill=batting_team)) + geom_area() + facet_grid(innings ~ .) + labs(title = paste(sample$start_date, "at", sample$venue)) 

#read all CSVs
temp = list.files(pattern="*.csv")
DATA_ALL <- as_tibble (rbindlist (lapply (temp, fread)))

inning_results <- group_by(DATA_ALL,match_id,innings,batting_team) %>% summarise(season, runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))
inning_results %>% ggplot(aes(runs)) + geom_histogram(binwidth=10) + facet_grid(batting_team ~ .)

team_results <- filter(DATA_ALL, batting_team == "Australia") %>%
  group_by(batting_team,season,match_id,start_date,innings) %>% 
  summarise(runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))

group_by(team_results, season) %>% summarise(runs = mean(runs)) %>% ggplot(aes(season, runs)) + geom_col() + theme_bw() 

group_by(team_results, season) %>% summarise(innings_played=n()) %>% ggplot(aes(season,innings_played)) + geom_col()

##results on all balls struck by australian team
team_results <- filter(DATA_ALL, batting_team == "Australia") %>%
  mutate(over = trunc(ball))

#average runs off each bat, split by over and innings for Australia
  group_by(team_results, innings, over) %>% 
  summarise(runs = mean(runs_off_bat)) %>% 
  ggplot(aes(over, runs)) + geom_col() + facet_grid(innings ~ .)+ labs(title=paste("Average runs off each ball, split by over and innings, for", team_results$batting_team, "in tests"))

