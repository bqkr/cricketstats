library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
setwd("~/Documents/cricstat/t20s_csv2")

sample <- read_csv("1001349.csv")
#results of each inning
inning_results <- group_by(sample,innings,batting_team) %>% summarise(runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))

#batsmen performance 
#also allocates all extras to the batter
batting_results <- group_by(sample,innings,batting_team,striker) %>% summarise(runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))

batting_results %>% ggplot(aes(x=striker,y=runs,color=striker))+geom_col()+facet_grid(innings ~ batting_team)
ilter(batting_results, batting_team == "Australia") %>% ggplot(aes(x=striker,y=runs,color=striker))+geom_text(aes(label=striker))+facet_grid(innings ~ batting_team)

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

#over data
over_totals <- mutate(DATA_ALL, over = trunc(ball)) %>% 
  group_by(innings, match_id, batting_team, over) %>% 
  summarise(runs = sum(runs_off_bat, extras))# %>% 
 # summarise(innings, over, runs, total = cumsum(runs))

#for australia only
filter(over_totals, batting_team == "India") %>% 
  group_by(innings, batting_team, over) %>% 
  summarise(avg_runs = mean(runs)) %>%
  ggplot(aes(over,avg_runs, fill=batting_team)) + geom_col() + facet_grid(innings ~ .)

#for all teams
group_by(over_totals,innings, batting_team, over) %>% 
  summarise(avg_runs = mean(runs)) %>%
  ggplot(aes(over,avg_runs)) + geom_col() + facet_grid(innings ~ .)

#performance in super overs
super_overs <- filter(over_totals, innings >= 3) %>% 
  group_by(batting_team) 

ggplot(super_overs, aes(batting_team,runs, color=batting_team, group=batting_team)) + geom_boxplot()

#return the ball by ball data for super over performance
left_join(super_overs, DATA_ALL) %>% 
  filter(batting_team == "West Indies") %>% 
  ggplot(aes(ball,runs_off_bat)) + geom_col() + facet_grid(match_id ~ .)

inning_results <- group_by(DATA_ALL,match_id,innings,batting_team) %>% summarise(season, runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))
inning_results %>% ggplot(aes(runs)) + geom_histogram(binwidth=10) + facet_grid(batting_team ~ .)

team_results <- filter(DATA_ALL, batting_team == "Australia") %>%
  group_by(batting_team,season,match_id,start_date,innings) %>% 
  summarise(runs = sum(runs_off_bat,extras),wickets = length(wicket_type[!is.na(wicket_type)]))

group_by(team_results, season) %>% summarise(runs = mean(runs)) %>% ggplot(aes(season, runs)) + geom_col() + theme_bw() 

group_by(DATA_ALL, season) %>% summarise(innings_played=n()) %>% ggplot(aes(season,innings_played)) + geom_col()

##results on all balls struck by australian team
team_results <- filter(DATA_ALL, batting_team == "Australia") %>%
  mutate(over = trunc(ball))

#average runs off each bat, split by over and innings for Australia
  group_by(team_results, innings, over) %>% 
  summarise(runs = mean(runs_off_bat)) %>% 
  ggplot(aes(over, runs)) + geom_col() + facet_grid(innings ~ .) + labs(title=paste("Average runs off each ball, split by over and innings, for", team_results$batting_team, "in T20I"))

