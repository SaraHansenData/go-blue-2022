## Go Blue 2022
## Sara E. Hansen, hanse2s
## modified: 2022-12-21

library(tidyverse)
library(ggimage)
library(grid)
library(gridExtra)
library(sportyR)

# Get data from CollegeFootballData.com
# Downloaded all 2022 data, regular season
## https://collegefootballdata.com/exporter/games?year=2022&seasonType=regular

# Import
setwd("C:/Users/saras/Desktop/Data Projects")
dat <- read.csv("collegefootballdata.csv", header = TRUE,
                quote = "", fileEncoding = "UTF-8")

# Explore
glimpse(dat)
dat %>%
  filter(home_conference == "Big Ten" | away_conference == "Big Ten") %>%
  count(home_team, home_conference, away_team, away_conference)
# Big Ten teams sometimes play teams outside the Big Ten conference

###############################################################################
########### Part 1: Show Wolverine Wins in 2022 ##########
# Prepare
# Just Big Ten (as of 2022 season) teams, just the points, just completed games
dat2 <- dat %>%
  filter(home_conference == "Big Ten" | away_conference == "Big Ten") %>%
  filter(completed == "true") %>%
  select(home_team, home_points, away_team, away_points,
         start_date, conference_game) %>%
  mutate(start_date = as.Date(start_date)) #simpler format
dat2 %>%
  count(home_points, away_points)

# Format for plotting
dat3 <- dat2 %>%
  mutate(record_type = 
           case_when(home_team != "Michigan" &
                       away_team != "Michigan" ~ "Other Big Ten Game",
                     TRUE ~ "Michigan Opponent")) %>%
  #Michigan will be "Michigan Opponent" but the points will get covered
  pivot_longer(c(home_team, away_team),
               names_to = "team_type", values_to = "team_name") %>%
  pivot_longer(c(home_points, away_points),
               names_to = "points_type", values_to = "points") %>%
  filter((team_type == "home_team" & points_type == "home_points") |
           (team_type == "away_team" & points_type == "away_points")) %>%
  select(-points_type) %>%
  mutate(image =
           case_when(team_name == "Michigan" ~  "wolverine_face_outlined.png"))

# Also need dataset of just UM points to connect the dots
um <- dat3 %>%
  filter(team_name == "Michigan") 

# And dataset of just UM games to show point difference
um_diff <- dat3 %>%
  filter(record_type == "Michigan Opponent")

# Plot
p1 <- dat3 %>%
  arrange(desc(record_type)) %>% #puts Michigan Opponent on top, alphabetically last
  ggplot(aes(x = start_date, y = as.numeric(points), group = record_type)) +
  geom_point(aes(shape = record_type, color = record_type, size = record_type)) +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_manual(values = c("#FFCB05", "#FFFFFF")) +
  scale_size_manual(values = c(2.5, 1.5)) + #note scale_ calls are still alphabetical
  annotate("text", x = as.Date("2022-11-26"), y = 51.5,
           label = str_wrap("Win 45-23 to Ohio State", 12),
           color = "#FFCB05", size = 2.7, fontface = "bold") +
  annotate("text", x = as.Date("2022-12-04"), y = 50.5,
           label = str_wrap("Big Ten Championship Win", 13),
           color = "#FFCB05", size = 2.7, fontface = "bold") +
  #annotate("text") is easier to customize than geom_text()
  geom_line(data = um_diff, aes(group = start_date),
            color = "#FFCB05", linewidth = 1.4) +
  geom_line(data = um, aes(x = start_date, y = as.numeric(points), group = team_name),
                        color = "#FFCB05", linetype = "dashed", linewidth = 1) +
  geom_image(data = um, aes(image = image), asp = 1.618) +
  #make it pretty
  labs(title = "Wolverine Wins in 2022",
       x = "Date", y = "Score") +
  theme(plot.margin = margin(5, 15, 15, 15),
        panel.background = element_rect(fill = "#00274C"),
        plot.background = element_rect(color = "#FFFFFF", fill = "#00274C", size = 3),
        legend.background = element_rect(fill = "#00274C"),
        legend.key = element_rect(fill = "#00274C"),
        legend.position = c(0.92, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#FFFFFF"),
        axis.ticks = element_line(color = "#FFFFFF"),
        plot.title = element_text(color = "#FFCB05", size = 18,
                                  hjust = 0.5, vjust = -10, face = "bold"),
        axis.title = element_text(color = "#FFCB05", size = 12),
        legend.title = element_blank(),
        axis.text = element_text(color = "#FFFFFF", size = 10),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.title.x = element_text(margin = margin(t = 7)),
        legend.text = element_text(color = "#FFFFFF", size = 10)) +
  scale_y_continuous(breaks = seq(from = 0, to = 80, by = 20)) +
  scale_x_date(date_labels = "%b")
 
###############################################################################
########## Part 2: Proportion of games won by each team ##########
# Same base data
# Add winner and loser
wl <- dat %>%
  filter(completed == "true" &
           (home_conference == "Big Ten" | away_conference == "Big Ten"))%>%
  mutate(winner =
           case_when(as.numeric(home_points) > as.numeric(away_points) ~ home_team,
                     as.numeric(away_points) > as.numeric(home_points) ~ away_team,
                     as.numeric(away_points) == as.numeric(home_points) ~ "tie"),
         loser =
           case_when(as.numeric(home_points) > as.numeric(away_points) ~ away_team,
                     as.numeric(away_points) > as.numeric(home_points) ~ home_team,
                     as.numeric(away_points) == as.numeric(home_points) ~ "tie"))

# Format for plotting
wl2 <- wl %>%
  select(winner, loser) %>%
  pivot_longer(everything(),
               names_to = "result",
               values_to = "team") %>%
  count(team, result) %>%
  pivot_wider(names_from = result, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(win_prop = winner/(winner+loser),
         bar_label = paste(team, "won",
                           paste(winner, (winner + loser), sep = "/"),
                           "games", sep = " "))

# Join to get Big Ten only
bigten <- dat %>%
  filter(home_conference == "Big Ten") %>%
  distinct(home_team)

wl3 <- wl2 %>%
  right_join(bigten, by = c("team" = "home_team")) %>%
  arrange(desc(win_prop))

# Define colors for bars and text
## https://usteamcolors.com/big-ten-conference/
# See order:
wl3 %>%
  select(team)

text_colors <- c("#FFCB05", #Michigan
                 "#B0B7BC", #Ohio State
                 "#002D62", #Penn State
                 "#13294B", #Illinois
                 "#862334", #Minnesota
                 "#000000", #Purdue
                 "#FCD116", #Iowa
                 "#CF102D", #Maryland
                 "#C4012F", #Wisconsin
                 "#18453B", #Michigan State
                 "#990000", #Indiana
                 "#E41C38", #Nebraska
                 "#CC0033", #Rutgers
                 "#4E2A84") #Northwestern

#Better to define bar colors within ggplot() to avoid changing order

# Plot
p2 <- wl3 %>%
  mutate(team = fct_relevel(team,
                            "Northwestern", "Rutgers", "Nebraska", "Indiana",
                            "Michigan State", "Wisconsin", "Maryland",
                            "Iowa", "Purdue", "Minnesota", "Illinois",
                            "Penn State", "Ohio State", "Michigan")) %>%
  ggplot(aes(x = fct_relevel(team,
                             "Northwestern", "Rutgers", "Nebraska", "Indiana",
                             "Michigan State", "Wisconsin", "Maryland",
                             "Iowa", "Purdue", "Minnesota", "Illinois",
                             "Penn State", "Ohio State", "Michigan"),
             y = win_prop)) +
  geom_bar(aes(fill = team),
               stat = "identity", width = 0.85, position = "dodge") +
  scale_fill_manual(values = c(
                              "#4E2A84", #Northwestern
                              "#CC0033", #Rutgers
                              "#FDF2D9", #Nebraska
                              "#990000", #Indiana
                              "#18453B", #Michigan State
                              "#C4012F", #Wisconsin
                              "#FFCD23", #Maryland
                              "#000000", #Iowa
                              "#CEB888", #Purdue
                              "#862334", #Minnesota
                              "#E84A27", #Illinois
                              "#002D62", #Penn State
                              "#CE0F3D", #Ohio State
                              "#00274C")) + #Michigan
  coord_polar(theta = "y") +
  geom_label(aes(x = team, y = 0, label = team),
            color = text_colors, size = 1.75, hjust = 1,
            fontface = "bold", label.padding = unit(0.12, "lines")) +
  labs(x = "Proportion of 2022 Games Won") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", color = "#00274C",
                                       size = 2),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(color = "#00274C", face = "bold",
                                    vjust = -10),
        axis.title.x = element_blank(), #no idea why axes labels get flipped here
        axis.text = element_blank())
#p2 is a temporary plot to be overlaid on the football field

# Footballs with text
df_football <- data.frame(x = 0.5, y = 0.5)

football1 <- df_football %>%
  ggplot(aes(x = x, y = y)) +
  geom_image(image = "American_football.png", asp = 1.618, size = 0.35) +
  annotate("text", x = 0.502, y = 0.497,
           label = str_wrap("How many Big Ten teams are undefeated in 2022?", 11),
           color = "#FFFFFF", size = 4.5, fontface = "bold") +
  scale_x_continuous(limits = c(0.45, 0.55)) +
  scale_y_continuous(limits = c(0.45, 0.55)) +
  theme_void()

football2 <- df_football %>%
  ggplot(aes(x = x, y = y)) +
  geom_image(image = "American_football_rotate.png", asp = 1.618, size = 0.35) +
  annotate("text", x = 0.496, y = 0.508, label = "One team:",
           color = "#FFFFFF", size = 4.5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.495, 
           label = str_wrap("The University of Michigan Wolverines", 15),
           color = "#FFFFFF", size = 4.5, fontface = "bold") +
  scale_x_continuous(limits = c(0.45, 0.55)) +
  scale_y_continuous(limits = c(0.45, 0.55)) +
  theme_void()

# Football field with footballs and bar plot
p3 <- geom_football("ncaa") +
  annotation_custom(ggplotGrob(p2),
                  xmin = -55, ymin = -25,
                  xmax = 55, ymax = 25) +
  annotation_custom(ggplotGrob(football1),
                    xmin = -85, ymin = -17,
                    xmax = -5, ymax = 32) +
  annotation_custom(ggplotGrob(football2),
                    xmin = 5, ymin = -37,
                    xmax = 85, ymax = 12)

# Attribution line for bottom of plot
text <- paste("Data by CollegeFootballData.com - - - Data Visualization by Sara E. Hansen")
attr <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, size = 4, color = "#00274C", label = text) +
  theme_void()

# Combine all plots
grid.arrange(p3, p1, attr,
             heights = c(5.45, 5.45, 0.3))


