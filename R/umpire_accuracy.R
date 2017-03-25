# --------------------------------------------------------------
# How accurate are umpires given pitch count and location?
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
atbats <- collect(atbats, n = Inf)

# *pitch* table
pitch <- pitch %>%
  select(call = des, sz_top, sz_bot, px, pz, zone, num, count, gameday_link) %>%
  group_by(gameday_link)
pitches <- collect(pitches, n = Inf)

# *umpire* table
umpire <- umpire %>%
  select(position, umpire = name, gameday_link) %>%
  filter(position == "home") %>%
  group_by(gameday_link)
umpires <- collect(umpires, n = Inf)

# Join *atbat*, *pitch*, and *umpire* by *gameday_link*
ps_abs <- left_join(pitch, atbat, by = c("num", "gameday_link"))
ps_abs_us <- left_join(ps_abs, umpire, by = "gameday_link", copy = TRUE)

pfx_16 <- tbl_df(as.data.frame(ps_abs_us, n = -1))
