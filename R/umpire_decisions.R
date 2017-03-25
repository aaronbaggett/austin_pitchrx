# --------------------------------------------------------------
# How many pitches do umpires see during games? 
# How many decisions do home plate umpires make per game?
#
# Accessing and Analyzing MLB Pitch Tracking Data in R
# Aaron R. Baggett, Ph.D.
# March 28, 2017
# --------------------------------------------------------------

# Load package libraries
library(dplyr)

# Load *pitch* data.frame
load("http://aaronbaggett.com/austin_pitchrx/data/pitch.Rda")

# Count total number of pitches in each 2016 game
pitch %>% 
  count(des) %>% 
  summarize(n_pitches = sum(n))

# Count total number of decisions in each 2016 game
pitch %>% 
  filter(des == "Called Strike" | des == "Ball") %>% 
  count(des) %>% 
  summarize(n_decisions = sum(n))

# Create data.frame of all pitches seen, grouped by game
observed <- pitch %>% 
  group_by(gameday_link) %>% 
  summarize(observed = n())

# Create data.frame of calls made, grouped by game
decisions <- pitch %>% 
  group_by(gameday_link) %>% 
  filter(des == "Called Strike" | des == "Ball") %>% 
  summarize(decisions = n())

# Join *decisions* and *observed*
pitches <- left_join(decisions, observed, by = "gameday_link")

# Convert *pitches* to data.frame
pitches <- tbl_df(pitches)

# Add proportion of decisions per game
pitches <- pitches %>% 
  mutate(prop = decisions/observed)

# Summarize *pitches* data
pitch_summs <- pitches %>% 
  summarize(m_pitches = mean(observed),
    sd_pitches = sd(observed),
    m_calls = mean(decisions),
    sd_calls = sd(decisions),
    m_prop = mean(prop),
    sd_prop = sd(prop))

xtable::xtable(pitch_summs)










# Filtering and manipulation operations
# *atbat* table
atbats <- atbat %>%
  select(num, stand, b_height, batter_name, gameday_link) %>%
  group_by(gameday_link)
atbats <- collect(atbats, n = Inf)

# *pitch* table
pitches <- pitch %>%
  select(call = des, sz_top, sz_bot, px, pz, zone, num, count, gameday_link) %>%
  group_by(gameday_link)
pitches <- collect(pitches, n = Inf)

# *umpire* table
umpires <- umpire %>%
  select(position, umpire = name, gameday_link) %>%
  filter(position == "home") %>%
  group_by(gameday_link)
umpires <- collect(umpires, n = Inf)

# Join *atbat*, *pitch*, and *umpire* by *gameday_link*
ps_abs <- left_join(pitch_decs, atbats, by = c("num", "gameday_link"))
ps_abs_us <- left_join(ps_abs, umpires, by = "gameday_link", copy = TRUE)

pfx_16 <- tbl_df(as.data.frame(ps_abs_us, n = -1))
save(pfx_16, file = "~/Dropbox/Research Projects/Austin_R_Meetup/data/pfx_16.Rda")

