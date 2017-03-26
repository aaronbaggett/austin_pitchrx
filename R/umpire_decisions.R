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
library(repmis)

# Load *pitch* data.frame
repmis::source_data("https://github.com/aaronbaggett/austin_pitchrx/blob/master/data/pitch.Rda?raw=true")

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
