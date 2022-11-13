## install required packages to complete the project

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("data.table")

## load libraries from packages

library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)

## load dataset from local file location

player_general = read.csv("/Users/zeanli/Library/Mobile Documents/com~apple~CloudDocs/UIUC/stat 447/project/players_22.csv")
player_season = read.csv("/Users/zeanli/Library/Mobile Documents/com~apple~CloudDocs/UIUC/stat 447/project/2021-2022 Football Player Stats.csv", sep = ";")
revised_player = read.csv("/Users/zeanli/Library/Mobile Documents/com~apple~CloudDocs/UIUC/stat 447/project/joined_df.csv")

## convert dataset into data.table or tibble for better operation

p_gen = as_tibble(player_general)
p_sea = as_tibble(player_season)
p_revised = as_tibble(revised_player)

##
## plot weekly wage by players' age, and then analysis the factors that affect their wage, such as their potential
##

p_gen_age = p_gen |> 
  group_by(age) |> 
  summarise(wage = mean(na.omit(wage_eur)), .groups = "drop")
  
ggplot(data = p_gen_age) +
  geom_point(aes(x = age, y = wage), color = "black") +
  geom_smooth(aes(x = age, y = wage), method = "loess", formula = y ~ x, color = "blue") +
  ggtitle("FIFA 2021-2022 Players Average Weekly Wage by Age", subtitle = "wage measured in EUR") +
  xlab("Player Age") +
  ylab("Average Weekly Wage")

##
## plot weekly wage by players' league 
##

p_gen_lea_avg = p_gen |>
  drop_na(league_name, wage_eur, league_level) |> 
  filter(league_level == 1) |> 
  select(league_name, wage_eur) |>
  group_by(league_name) |>
  summarise(avg_wage = mean(wage_eur),
            tot_wage = sum(wage_eur),
            med_wage = median(wage_eur),
            .groups = "drop")

p_gen_lea_avg_order = tibble::rowid_to_column(p_gen_lea_avg, "index")

p_gen_lea_avg_order_filter = p_gen_lea_avg_order |> 
  filter(avg_wage >= 10000) |> 
  mutate(avg_wage = avg_wage - 1000)

ggplot(data = p_gen_lea_avg_order) +
  geom_point(aes(x = index, y = avg_wage)) +
  geom_text(data = p_gen_lea_avg_order_filter, aes(x = index, y = avg_wage, label = league_name), color = "purple") +
  ggtitle("Dot Plot for Average Weekly Wage", subtitle = "wage measured by EUR") +
  xlab("League Index") +
  ylab("Average Weekly Wage")

ggplot(data = p_gen_lea_avg_order) +
  geom_density(aes(x = avg_wage), color = "purple") +
  ggtitle("Density Plot for Average Weekly Wage", subtitle = "wage measured in EUR") +
  xlab("Average Weekly Wage")

## plot average weekly wage by position
## create a tibble with position and average weekly wage for that position

p_full = p_gen |> 
  inner_join(p_sea, by = c("long_name" = "Player")) |> 
  group_by(Pos) |> 
  summarise(wage = round(mean(wage_eur), digits = 0), .groups = "drop")

## using ggplot to plot the average weekly wage by position

ggplot(data = p_full) +
  geom_col(aes(x = Pos, y = wage, fill = Pos), alpha = 0.6) +
  ggtitle("FIFA 2021-2022 Players Average Weekly Wage by Position", subtitle = "(Wage measured in EUR)") +
  geom_text(aes(x = Pos, y = wage, label = wage), alpha = 1) +
  theme_bw() +
  xlab("Player Position") +
  ylab("Average Weekly Wage")

## analyze player wage by position in the richest 5 clubs and the poorest 5 clubs from league 1

p_top10 = p_gen |> 
  drop_na(wage_eur, league_level, club_name) |> 
  filter(league_level == 1) |> 
  select(club_name, wage_eur) |> 
  group_by(club_name) |> 
  summarise(club_avg_wage = mean(wage_eur), .groups = "drop") |> 
  arrange(desc(club_avg_wage)) |> 
  head(10)

p_top10_pw = p_revised |> 
  filter(club_name %in% c("Real Madrid CF", "Manchester City", "Manchester United", "FC Barcelona")) |> 
  drop_na(wage_eur) |> 
  group_by(club_name, Pos) |> 
  summarise(avg_wage = mean(wage_eur), .groups = "drop")
  
  
ggplot(data = p_top10_pw) +
  geom_col(aes(x = Pos, y = avg_wage, fill = Pos)) +
  facet_wrap( ~ club_name)


## detailed information for wage

# international reputation

p_int = p_gen |> 
  drop_na(international_reputation) |> 
  group_by(international_reputation) |> 
  summarise(avg_wage = round(mean(na.omit(wage_eur)), digits = 2), .groups = "drop") |> 
  mutate(index = avg_wage + 5000)

ggplot(data = p_int) +
  geom_col(aes(x = international_reputation, y = avg_wage, fill = international_reputation), alpha = 0.7) +
  geom_text(aes(x = international_reputation, y = index, label = avg_wage, color = international_reputation))

# 

p_overall = p_gen |> 
  drop_na(overall, wage_eur) |> 
  mutate(wage = wage_eur / 1000) |> 
  select(overall, wage)

ggplot(data = p_overall) +
  geom_point(aes(x = overall, y = wage)) +
  geom_smooth(aes(x = overall, y = wage), method = "loess", formula = y ~ x)
  
p_potential = p_gen |> 
  drop_na(potential, wage_eur) |> 
  mutate(wage = wage_eur / 1000) |> 
  select(potential, wage)

ggplot(data = p_potential) +
  geom_point(aes(x = potential, y = wage)) +
  geom_smooth(aes(x = potential, y = wage), method = "loess", formula = y ~ x)



