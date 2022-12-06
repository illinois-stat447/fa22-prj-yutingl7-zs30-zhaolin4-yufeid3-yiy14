## load libraries

library(readr)
library(dplyr)
library(ISLR2)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(data.table)

## load dataset from GitHub url 

url = "https://raw.githubusercontent.com/illinois-stat447/fa22-prj-yutingl7-zs30-zhaolin4-yufeid3-yiy14/main/datasets/players_22.csv"
player_general = read.csv(url)

## convert dataset into tibble for better operation

p_gen = as_tibble(player_general)

## summary and plot a boxplot for players' weekly wage

summary(p_gen$wage_eur)
boxplot(p_gen$wage_eur)

## plot weekly wage by players' age

p_gen_age = p_gen |> 
  group_by(age) |> 
  summarise(wage = mean(na.omit(wage_eur)), .groups = "drop")
  
ggplot(data = p_gen_age) +
  geom_point(aes(x = age, y = wage), color = "black") +
  geom_smooth(aes(x = age, y = wage), method = "loess", formula = y ~ x, color = "blue") +
  ggtitle("FIFA 2021-2022 Players Average Weekly Wage by Age", subtitle = "wage measured in EUR") +
  xlab("Player Age") +
  ylab("Average Weekly Wage")

## plot weekly wage by players' league 

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

## international reputation

p_int = p_gen |> 
  drop_na(international_reputation) |> 
  group_by(international_reputation) |> 
  summarise(avg_wage = round(mean(na.omit(wage_eur)), digits = 2), .groups = "drop") |> 
  mutate(index = avg_wage + 5000)

ggplot(data = p_int) +
  geom_col(aes(x = international_reputation, y = avg_wage, fill = international_reputation), alpha = 0.7) +
  geom_text(aes(x = international_reputation, y = index, label = avg_wage, color = international_reputation))

## wage by overall and potential 

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

## players that are GOAL KEEPER
p_gk = p_gen |>
  filter(club_position == "GK") |>
  select(9,12,73:78)|>
  na.omit()

set.seed(42)
train_gk = sample(c(TRUE,FALSE), nrow(p_gk), rep = TRUE)
test_gk = (!train_gk)
gk.train = p_gk[train_gk, ]
gk.test = p_gk[test_gk, ]

lm.fit_gk = lm(wage_eur~., data = gk.train)
lm.pred_gk = predict(lm.fit_gk, gk.test, type = "response")
summary(lm.fit_gk)

mean((lm.pred_gk - gk.test$wage_eur) ^ 2)

## players that are FORWARD
p_f = p_gen |>
  filter(club_position %in% c("ST", "CF", "RS", "LS", "RF", "LF", "RW", "LW")) |> 
  select(9, 12, 44:72) |> 
  na.omit()

set.seed(42)
train_f = sample(c(TRUE,FALSE), nrow(p_f), rep = TRUE)
test_f = (!train_f)
f.train = p_f[train_f, ]
f.test = p_f[test_f, ]

lm.fit_f = lm(wage_eur~., data = f.train)
lm.pred_f = predict(lm.fit_f, f.test, type = "response")
summary(lm.fit_f)

mean((lm.pred_f - f.test$wage_eur) ^ 2)

## players that are MIDFIELD
p_m = p_gen |> 
  filter(club_position %in% c("RCM", "CDM", "RDM", "LCM", "CAM", "LDM", "LM", "RM", "CM", "LAM", "RAM")) |> 
  select(9, 12, 44:72) |> 
  na.omit()

set.seed(42)
train_m = sample(c(TRUE,FALSE), nrow(p_m), rep = TRUE)
test_m = (!train_m)
m.train = p_m[train_m, ]
m.test = p_m[test_m, ]

lm.fit_m = lm(wage_eur~., data = m.train)
lm.pred_m = predict(lm.fit_m, m.test, type = "response")
summary(lm.fit_m)

mean((lm.pred_m - m.test$wage_eur) ^ 2)

## players that are DEFENDER
p_d = p_gen |> 
  filter(club_position %in% c("LCB", "RCB", "LB", "RB", "CB", "RWB", "LWB")) |> 
  select(9, 12, 44:72) |> 
  na.omit()

set.seed(42)
train_d = sample(c(TRUE,FALSE), nrow(p_d), rep = TRUE)
test_d = (!train_d)
d.train = p_d[train_d, ]
d.test = p_d[test_d, ]

lm.fit_d = lm(wage_eur~., data = d.train)
lm.pred_d = predict(lm.fit_d, d.test, type = "response")
summary(lm.fit_d)

mean((lm.pred_d - d.test$wage_eur) ^ 2)

## GK

train_gk.mat = model.matrix(wage_eur~., data = gk.train)
test_gk.mat = model.matrix(wage_eur~., data = gk.test)

set.seed(42)
cv.out_gk = cv.glmnet(train_gk.mat, gk.train$wage_eur, alpha = 1)
bestlam_gk = cv.out_gk$lambda.min
bestlam_gk

lasso.mod_gk = glmnet(train_gk.mat, gk.train$wage_eur, alpha = 1)
lasso.pred_gk = predict(lasso.mod_gk, s = bestlam_gk, newx = test_gk.mat)
mean((lasso.pred_gk - gk.test$wage_eur) ^ 2)

lasso.coef_gk = predict(lasso.mod_gk, type = "coefficients", s = bestlam_gk)
length(lasso.coef_gk[lasso.coef_gk != 0])
lasso.coef_gk

## F

train_f.mat = model.matrix(wage_eur~., data = f.train)
test_f.mat = model.matrix(wage_eur~., data = f.test)

set.seed(42)
cv.out_f = cv.glmnet(train_f.mat, f.train$wage_eur, alpha = 1)
bestlam_f = cv.out_f$lambda.min
bestlam_f

lasso.mod_f = glmnet(train_f.mat, f.train$wage_eur, alpha = 1)
lasso.pred_f = predict(lasso.mod_f, s = bestlam_f, newx = test_f.mat)
mean((lasso.pred_f - f.test$wage_eur) ^ 2)

lasso.coef_f = predict(lasso.mod_f, type = "coefficients", s = bestlam_f)
length(lasso.coef_f[lasso.coef_f != 0])
lasso.coef_f

## M

train_m.mat = model.matrix(wage_eur~., data = m.train)
test_m.mat = model.matrix(wage_eur~., data = m.test)

set.seed(42)
cv.out_m = cv.glmnet(train_m.mat, m.train$wage_eur, alpha = 1)
bestlam_m = cv.out_m$lambda.min
bestlam_m

lasso.mod_m = glmnet(train_m.mat, m.train$wage_eur, alpha = 1)
lasso.pred_m = predict(lasso.mod_m, s = bestlam_m, newx = test_m.mat)
mean((lasso.pred_m - m.test$wage_eur) ^ 2)

lasso.coef_m = predict(lasso.mod_m, type = "coefficients", s = bestlam_m)
length(lasso.coef_m[lasso.coef_m != 0])
lasso.coef_m

## D

train_d.mat = model.matrix(wage_eur~., data = d.train)
test_d.mat = model.matrix(wage_eur~., data = d.test)

set.seed(42)
cv.out_d = cv.glmnet(train_d.mat, d.train$wage_eur, alpha = 1)
bestlam_d = cv.out_d$lambda.min
bestlam_d

lasso.mod_d = glmnet(train_d.mat, d.train$wage_eur, alpha = 1)
lasso.pred_d = predict(lasso.mod_d, s = bestlam_d, newx = test_d.mat)
mean((lasso.pred_d - d.test$wage_eur) ^ 2)

lasso.coef_d = predict(lasso.mod_d, type = "coefficients", s = bestlam_d)
length(lasso.coef_d[lasso.coef_d != 0])
lasso.coef_d
