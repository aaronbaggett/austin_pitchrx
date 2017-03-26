# --------------------------------------------------------------
# Create and setup PITCHf/x database
#
# Accessing and Analyzing MLB Pitch Tracking Data in R
# Aaron R. Baggett, Ph.D.
# March 28, 2017
# --------------------------------------------------------------

# Load package libraries
library(dplyr)
library(pitchRx)

# Set and create working directory
setwd("~/your/working/directory")

# Initialize SQLite database
# NOTE: You may replace `db` with year (e.g., pfx_16)
pfx_db <- src_sqlite("pfx_16.sqlite3", create = TRUE)

# Set XML files to collect
files <- c("inning/inning_all.xml", "players.xml", "miniscoreboard.xml")

# Scrape data
scrape(start = "YYY-MM-DD", end = "YYY-MM-DD", suffix = files, connect = pfx_db$con)

# Verify *pfx_db* tables
pfx_db

# Load *pfx_db* DB
pfx_db <- src_sqlite("~/your/working/directory/pfx_db.sqlite3")

# Convert *pfx_db* tables to standalone tables
action <- tbl(pfx_db, "action")
atbat <- tbl(pfx_db, "atbat")
coach <- tbl(pfx_db, "coach")
game <- tbl(pfx_db, "game")
media <- tbl(pfx_db, "media")
pitch <- tbl(pfx_db, "pitch")
player <- tbl(pfx_db, "player")
po <- tbl(pfx_db, "po")
runner <- tbl(pfx_db, "runner")
umpire <- tbl(pfx_db, "umpire")