# --------------------------------------------------------------
# How accurate are all umpires over the season?
# How accurate are individual umpires over the season?
#
# Accessing and Analyzing MLB Pitch Tracking Data in R
# Aaron R. Baggett, Ph.D.
# March 28, 2017
# --------------------------------------------------------------

# Load package libraries
library(dplyr)
library(repmis)

# Load *pitch* data.frame
repmis::source_data("https://github.com/aaronbaggett/austin_pitchrx/blob/master/data/atbat.Rda?raw=true")
repmis::source_data("https://github.com/aaronbaggett/austin_pitchrx/blob/master/data/pitch.Rda?raw=true")
repmis::source_data("https://github.com/aaronbaggett/austin_pitchrx/blob/master/data/umpire.Rda?raw=true")

# Filtering and manipulation operations
# *atbat* table
atbat <- atbat %>%
  select(num, stand, b_height, batter_name, gameday_link) %>%
  group_by(gameday_link)
atbat <- collect(atbat, n = Inf)

# *pitch* table
pitch <- pitch %>%
  select(call = des, sz_top, sz_bot, px, pz, zone, num, count, gameday_link) %>%
  group_by(gameday_link) %>% 
  filter(call == "Called Strike" | call == "Ball")
pitch <- collect(pitch, n = Inf)

# *umpire* table
umpire <- umpire %>%
  select(position, umpire = name, gameday_link) %>%
  filter(position == "home") %>%
  group_by(gameday_link)
umpire <- collect(umpire, n = Inf)

# Join *atbat*, *pitch*, and *umpire* by *gameday_link*
ps_abs <- left_join(pitch, atbat, by = c("num", "gameday_link"))
ps_abs_us <- left_join(ps_abs, umpire, by = "gameday_link", copy = TRUE)

# Convert *ps_abs_us* to data frame
pfx_16 <- tbl_df(as.data.frame(ps_abs_us, n = -1))

pfx_16$game_date <- substr(pfx_16$gameday_link, start = 5, stop = 14)

# Function for including only complete cases by column
# Source: http://stackoverflow.com/a/11258247/1656111
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Omit missing elements of *px* and *pz*
pfx_16 <- completeFun(pfx_16, c("px", "pz"))

### --- Add player sz limits by *b_height* --- ###
pfx_16 <- pfx_16 %>%
  group_by(b_height) %>%
  mutate(player_sz_bot = mean(sz_bot)) %>%
  mutate(player_sz_top = mean(sz_top))

$$ \frac { \frac { (1.57\times 2)+17 }{ 12 }  }{ 2 } =\frac { 1.678333 }{ 2 } = 0.8391667 $$
  
  # Create *u_test* variable for correct/incorrect decision
  # Based on ball radius = ((1.57*2 + 17) / 12) / 2
  pfx_16$u_test_adj <- with(pfx_16,
    ifelse(call == "Ball" & px < -0.8391667 | px > 0.8391667 | 
        pz < player_sz_bot | pz > player_sz_top, 1,
      ifelse(call == "Called Strike" & pz > player_sz_bot & pz < player_sz_top & 
          px >= -0.8391667 & px <= 0.8391667, 1,
        ifelse(call == "Ball" & pz > player_sz_bot & pz < player_sz_top & 
            px > -0.8391667 & px < 0.8391667, 0,
          ifelse(call == "Called Strike" & px < -0.8391667 | px > 0.8391667 | 
              pz < player_sz_bot | pz > player_sz_top, 0, 99)))))

with(pfx_16, mean(as.numeric(u_test_adj)))
table(pfx_16$u_test_adj)

# Umpire accuracy rates per game over season
cum_acc <- pfx_16 %>% 
  group_by(game_date) %>% 
  summarize(mean_acc = mean(u_test_adj)) %>% 
  mutate(day_num = 1:length(game_date))

# Line plot of cumulative accuracy across season
ggplot(data = cum_acc, mapping = aes(x = day_num, y = mean_acc)) + 
  geom_line(color = "dodgerblue3") + 
  geom_vline(xintercept = 181, color = "tomato") +
  scale_x_continuous(breaks = seq(0, 205, 10), 
    name = "\nDay Number of Season") +
  scale_y_continuous(breaks = seq(0, 1, .01), 
    name = "Mean Accuracy per Day\n") +
  theme_bw() + theme(panel.grid.minor = element_blank())

# Individual umpire accuracy rate for season
ind_acc <- pfx_16 %>% 
  group_by(umpire) %>% 
  summarize(mean_acc = mean(u_test_adj),
    sd_acc = sd(u_test_adj)) %>% 
  arrange(mean_acc)

# Relevel umpire names to prevent ggplot resorting
ind_acc$umpire <- factor(ind_acc$umpire, levels = ind_acc$umpire)

# Dot plot of individual umpire accuracy for season
ggplot(data = ind_acc, mapping = aes(x = mean_acc, y = umpire, group = 1)) + 
  geom_point(color = "dodgerblue3") + geom_line(color = "dodgerblue3") + 
  geom_vline(xintercept = 0.9357033, color = "tomato") +
  scale_x_continuous(breaks = seq(0, 1, .005), 
    name = "\nMean Accuracy Over Season") +
  scale_y_discrete(name = "Umpire\n") +
  theme_bw() + theme(panel.grid.minor = element_blank())
