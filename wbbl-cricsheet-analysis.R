library(readr)
library(dplyr)
library(tidyr)
library(widyr)
library(data.table)
library(ggplot2)

setwd("./wbbl/cricsheet")
temp = list.files(pattern="[0-9]+.csv")
DATA_ALL <- as_tibble (rbindlist (lapply (temp, fread), fill = TRUE))
current <- DATA_ALL %>% filter(season == "2021/22")

#Total runs in Season
batter <- "EA Perry"
current %>% filter(striker == batter) %>% summarise(runs = sum(runs_off_bat))

#Runs for batter off bowler
runs_by_batter_against_bowler <- current %>% filter(striker == batter) %>% group_by(striker, bowler) %>% 
  summarise(runs = sum(runs_off_bat)) %>%
  arrange(desc(runs))

runs_by_batter_against_bowler %>% ggplot(aes(bowler, runs)) + geom_point()

#Runs for batter off fielding team
current %>% filter(striker == batter) %>% group_by(striker, bowling_team) %>% 
  summarise(runs = sum(runs_off_bat)) %>%
  arrange(desc(runs))

#Runs for batter by over across season
current %>% filter(striker == batter) %>% 
  summarise(striker, match_id, over = trunc(ball), runs_off_bat) %>%
  group_by(striker, match_id, over) %>%
  summarise(runs_off_over = sum(runs_off_bat), balls_faced = n()) %>%
  group_by(striker, over) %>% 
  summarise(balls_faced = sum(balls_faced), average_runs = round(mean(runs_off_over)))

#Runs for batter by over against Bowler
runs_by_batter_per_over_against_bowler <- current %>% filter(striker == batter) %>% 
  summarise(striker, match_id, over = trunc(ball), runs_off_bat, bowler) %>%
  group_by(striker, match_id, bowler, over) %>%
  summarise(runs_off_over = sum(runs_off_bat), balls_faced = n()) %>%
  group_by(striker, bowler, over) %>% 
  summarise(balls_faced = sum(balls_faced), average_runs = round(mean(runs_off_over)), total_runs = sum(runs_off_over)) %>%
  arrange(desc(balls_faced))

runs_by_batter_per_over_against_bowler %>% ggplot(aes(over,balls_faced, fill = total_runs)) + 
  geom_col() + 
  facet_wrap(bowler ~ . ) + 
  ggtitle(paste(batter,'batting'))


#Runs for batter by over against Team
runs_by_batter_per_over_against_team <- current %>% filter(striker == batter) %>% 
  summarise(match_id, striker, over = trunc(ball), runs_off_bat, bowling_team) %>%
  group_by(striker, match_id, bowling_team, over) %>%
  summarise(striker, runs_off_over = sum(runs_off_bat), balls_faced = n()) %>%
  group_by(striker, bowling_team, over) %>% 
  summarise(balls_faced = sum(balls_faced), average_runs = round(mean(runs_off_over)))

runs_by_batter_per_over_against_team %>% ggplot(aes(over,average_runs, fill = balls_faced)) + 
  geom_col() + 
  facet_wrap(bowling_team ~ . ) + 
  ggtitle(paste(batter,'batting'))

#Runs by team by over
runs_per_over_by_team <- current %>% 
  summarise(batting_team, match_id, over = trunc(ball), runs_off_bat) %>%
  group_by(batting_team, match_id, over) %>%
  summarise(runs_off_over = sum(runs_off_bat), balls_faced = n()) %>%
  group_by(batting_team, over) %>% 
  summarise(balls_faced = sum(balls_faced), average_runs = round(mean(runs_off_over)), total_runs = sum(runs_off_over))

runs_per_over_by_team %>%  ggplot(aes(over, total_runs)) + geom_col() + facet_wrap(batting_team ~ . )

runs_per_over_by_team %>%  ggplot(aes(over, average_runs)) + geom_line() + 
  geom_smooth(method = "lm") + 
  facet_wrap(batting_team ~ . )

#average runs in powerplay 
runs_in_pp_by_team <- current %>% 
  summarise(batting_team, match_id, over = trunc(ball) + 1, runs_off_bat, extras) %>%
  filter(over <= 6) %>%
  group_by(batting_team, match_id) %>%
  summarise(runs_off_pp = sum(runs_off_bat), balls_faced = n(), extras = sum(extras)) 

runs_in_pp_by_team %>%  summarise(balls_faced = sum(balls_faced), average_runs = mean(runs_off_pp), extras = sum(extras), excess = balls_faced - extras, matches = excess/6/6) %>% 
  arrange(desc(average_runs))

#average runs in death overs
runs_in_death_by_team <- current %>% 
  summarise(batting_team, match_id, over = trunc(ball) + 1, runs_off_bat) %>%
  filter(over > 16) %>%
  group_by(batting_team, match_id) %>%
  summarise(runs_in_death = sum(runs_off_bat), balls_faced = n()) 

runs_in_death_by_team %>% summarise(balls_faced = sum(balls_faced), average_runs = mean(runs_in_death))

selectedBowler <- "EA Perry"
current <- current %>% mutate(wicket = case_when(wicket_type != "" ~ 1, wicket_type == "" ~ 0))

current %>% filter(bowler == selectedBowler) %>% 
  summarise(deliveries = n(), runs_conceded = sum(runs_off_bat), extras = sum(extras), wickets = sum(wicket))

#Runs for bowler against batter
current %>% filter(bowler == selectedBowler) %>% group_by(bowler, striker) %>% 
  summarise(runs_conceded = sum(runs_off_bat), extras = sum(extras), wickets = sum(wicket), deliveries = n()) %>%
  arrange(desc(deliveries))

#Runs for bowler against batting team
current %>% filter(bowler == selectedBowler) %>% group_by(bowler, batting_team) %>% 
  summarise(runs_conceded = sum(runs_off_bat), extras = sum(extras), wickets = sum(wicket), deliveries = n()) %>%
  arrange(desc(deliveries))

#Runs against bowler by over across season
current %>% filter(bowler == selectedBowler) %>% 
  summarise(bowler, match_id, over = trunc(ball) + 1, runs_off_bat, extras, wicket) %>%
  group_by(bowler, match_id, over) %>%
  summarise(runs_off_over = sum(runs_off_bat), deliveries = n(), extras = sum(extras), wickets = sum(wicket)) %>%
  group_by(bowler, over) %>% 
  summarise(deliveries = sum(deliveries), average_runs_conceded = round(mean(runs_off_over)), total_extras = sum(extras), total_wickets = sum(wickets))

#Runs against bowler by over against batter
bowling_per_over_against_bowler <- current %>%
  summarise(bowler, striker, match_id, over = trunc(ball) + 1, runs_off_bat, extras, wicket) %>%
  group_by(bowler, striker, match_id, over) %>%
  summarise(runs_off_over = sum(runs_off_bat), deliveries = n(), extras = sum(extras), wickets = sum(wicket)) %>%
  group_by(bowler, striker, over) %>% 
  summarise(deliveries = sum(deliveries), average_runs_conceded = round(mean(runs_off_over)), total_extras = sum(extras), total_wickets = sum(wickets))

#Bowling for team by over across season
bowling_per_over_by_team <- current %>% 
  summarise(bowling_team, match_id, over = trunc(ball) + 1, runs_off_bat, extras, wicket) %>%
  group_by(bowling_team, match_id, over) %>%
  summarise(runs_off_over = sum(runs_off_bat), deliveries = n(), extras = sum(extras), wickets = sum(wicket)) %>%
  group_by(bowling_team, over) %>% 
  summarise(deliveries = sum(deliveries), average_runs_conceded = round(mean(runs_off_over)), total_extras = sum(extras), total_wickets = sum(wickets))

bowling_per_over_by_team %>% 
  ggplot(aes(over, total_wickets, fill = average_runs_conceded)) + geom_col() + 
  facet_wrap(bowling_team ~ .)

bowling_per_over_by_team_against_team <- current %>% 
  summarise(bowling_team, batting_team, match_id, over = trunc(ball) + 1, runs_off_bat, extras, wicket) %>%
  group_by(bowling_team, batting_team, match_id, over) %>%
  summarise(runs_off_over = sum(runs_off_bat), deliveries = n(), extras = sum(extras), wickets = sum(wicket)) %>%
  group_by(bowling_team, batting_team, over) %>% 
  summarise(deliveries = sum(deliveries), average_runs_conceded = round(mean(runs_off_over)), total_extras = sum(extras), total_wickets = sum(wickets))

bowling_per_over_by_team_against_team %>% 
  ggplot(aes(over, total_wickets, fill = average_runs_conceded)) + geom_col() + 
  facet_wrap(bowling_team ~ batting_team)

#dot balls
dots_per_over_by_team <- current %>% 
  summarise(bowling_team, batting_team, match_id, over = trunc(ball) + 1, runs_off_bat, extras, wicket) %>%
  filter(runs_off_bat == 0) %>%
  group_by(bowling_team, batting_team,  match_id, over) %>%
  summarise(deliveries = n(), wickets = sum(wicket)) %>%
  group_by(bowling_team, batting_team, over) %>% 
  summarise(deliveries = mean(deliveries), total_wickets = sum(wickets))

dots_per_over_by_team %>% ggplot(aes(over, deliveries, fill = total_wickets)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(bowling_team ~ batting_team) + 
  ggtitle("Average number of dot balls per over by team bowling") +
  ylim(c(0,6))

#add batting and bowling style from csv file
#wbbl_players <- left_join(players, p1, by = c("key_cricinfo" = "player_id")) 
wbbl_players <- read_csv('wbbl-players.csv')
bowlers <- wbbl_players %>% select(bowler = name.x, bowlerStyle = `Bowling Style`)
strikers <- wbbl_players %>% select(striker = name.x, battingStyle = `Batting Style`)

DATA_ALL <- mutate(DATA_ALL, bowlerStyle = bowlers$bowlerStyle[match(DATA_ALL$bowler, bowlers$bowler)])
DATA_ALL <- mutate(DATA_ALL, battingStyle = strikers$battingStyle[match(DATA_ALL$striker, strikers$striker)])

#style of bowling used by team in over
bowlingStyle_by_over_by_team <- DATA_ALL %>% group_by(match_id, bowling_team) %>%
  summarise(over = trunc(ball) + 1, bowlerStyle) %>%
  group_by(bowling_team, over, bowlerStyle) %>%
  summarise(deliveries = n())

#convert to factores and simplify
bowlingStyle_by_over_by_team$bowlerStyle = factor(bowlingStyle_by_over_by_team$bowlerStyle)
#factors for 2021/22 WBBL season
#levels(bowlingStyle_by_over_by_team$bowlerStyle) <- c("pace", "pace", "spin", "spin", "pace", "pace", "pace", "pace", "spin", "spin", "spin")

#factors for all WBBL seasons
levels(bowlingStyle_by_over_by_team$bowlerStyle) <- c("pace", "pace", 
                                                      "pace", "spin", 
                                                      "spin", "pace", 
                                                      "pace", "both", 
                                                      "pace", "pace", 
                                                      "spin", "spin",
                                                      "spin")

bowlingStyle_by_over_by_team %>% ggplot(aes(over, deliveries, fill = bowlerStyle)) + 
  geom_col() +
  facet_wrap(bowling_team ~ .) +
  ggtitle("Frequency of style of bowling used per over by team")