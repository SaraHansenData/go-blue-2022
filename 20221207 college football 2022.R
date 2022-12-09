#College Football 2022 (Big Ten)
##Sara Hansen
##Created December 7, 2022
##Last edited December 7, 2022

library(tidyverse)
install.packages("ggimage")
library(ggimage)

#Step 1: Get data
#Downloaded all 2022 data, regular season
##https://collegefootballdata.com/exporter/games?year=2022&seasonType=regular

#Step 2: Import data
setwd("C:/Users/saras/Desktop/Data Projects")
dat <- read.csv("collegefootballdata.csv", header = TRUE,
                quote = "", fileEncoding = "UTF-8")

#Step 3: Explore
glimpse(dat)

dat %>%
  count(home_conference, away_conference)
#Big Ten doesn't always just play Big Ten
dat %>%
  filter(home_conference == "Big Ten" | away_conference == "Big Ten") %>%
  count(home_team, home_conference, away_team, away_conference)

#Step 4: Filter and clean
#Just Big Ten (as of 2022 season) teams, just the points, just completed games
dat2 <- dat %>%
  filter(home_conference == "Big Ten" | away_conference == "Big Ten") %>%
  filter(completed == "true") %>%
  select(home_team, home_points, away_team, away_points,
         start_date, conference_game) %>%
  mutate(start_date = as.Date(start_date)) #simpler format
dat2 %>%
  count(home_points, away_points)

#Get University of Michigan data
um <- dat2 %>%
  filter(home_team == "Michigan" | away_team == "Michigan") 
um %>%
  count(start_date, home_team, away_team, conference_game)

#Step 5: Format data
#Don't need home and away conference, just conference_game
um2 <- um %>%
  pivot_longer(c(home_team, away_team),
               names_to = "team_type", values_to = "team_name") %>%
  pivot_longer(c(home_points, away_points),
               names_to = "points_type", values_to = "points") %>%
  filter((team_type == "home_team" & points_type == "home_points") |
           (team_type == "away_team" & points_type == "away_points")) %>%
  select(-points_type)

#Step 6: Visualize
#Graph University of Michigan
um2 %>%
  ggplot(aes(x = start_date, y = as.numeric(points), group = team_type)) +
  geom_point(aes(color = team_name)) +
  geom_line()

#Extra steps
#Make it so that the points are the team logos
teams <- dat2 %>%
  distinct(team_name = home_team) %>%
  mutate(image = 
           case_when(team_name == "Michigan" ~ 
                       "C:/Users/saras/Desktop/Data Projects/collegefootballlogos/U-M_Logo-Hex.png"))

#Graph with logo
um2 %>%
  left_join(teams, by = "team_name") %>%
  ggplot(aes(x = start_date, y = as.numeric(points), group = team_type)) +
  geom_point() +
  geom_line()
  geom_image(aes(image = image))
  #how to get it so the lines only go between the team of interest
