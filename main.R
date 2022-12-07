############################################################################## #
### Main File for sports econ paper
### gtarcese@gmail.com - 10/5/2022
############################################################################## #  

# Packages
library(expp)
# This messes with view() function, and probably lots of others
as.data.frame <- base::as.data.frame
library(fastDummies)
library(htmltools)
library(httr)
library(leaflet)
library(lubridate)
library(magrittr)
library(openxlsx)
library(patchwork)
library(renv)
library(RCurl)
library(readxl)
library(rvest)
library(scales)
library(shiny)
library(spdep)
library(stargazer)
library(tidyselect)
library(tidyverse)
library(tigris)
library(totalcensus)
# Import "personal package"
source("personal_package.R", local = personal_package <- new.env())
personal_package$my_attach(personal_package) 

# Raw Data Collection ----------------------------------------------------------

# CBSA data (from totalcensus package)
data("dict_cbsa")
write_csv(dict_cbsa, "assets/cbsa_data/raw.cbsa.csv")

# Covid-19 data (NewYorkTimes)
nytRaw2021 <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2021.csv")
nytRaw2022 <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv")
write_csv(read_csv(nytRaw2021), "assets/covid_data/raw.covid2021.csv")
write_csv(read_csv(nytRaw2022), "assets/covid_data/raw.covid2022.csv")

# Import data scraping objects
attach_source("scrape.R", "data_scraping")

# 2015-16 season. 1231 observations -> 1230 games, 1 allstar game
raw.nba.2015 <- scrape_season(nbaSeasons[1] %>% reduce(c), "nba")
# 2016-17 season. 1232 observations -> 1230 games, 1 allstar game, 1/7/2017 blazers postponed.
raw.nba.2016 <- scrape_season(nbaSeasons[2] %>% reduce(c), "nba")
# 2017-18 season. 1232 observations -> 1230 games, 1 allstar game, USA game.
raw.nba.2017 <- scrape_season(nbaSeasons[3] %>% reduce(c), "nba")
# 2018-19 season. 1232 observations -> 1230 games, 1 allstar game, USA game.
raw.nba.2018 <- scrape_season(nbaSeasons[4] %>% reduce(c), "nba")
# NBA 2021-22 season. 1231 observations -> 1230 games, 1 allstar game.
raw.nba.2021 <- scrape_season(nbaSeasons[5] %>% reduce(c), "nba")

# 2015-16 season. 1235 observations -> 1230 games, 3 allstar games, 1/23/2016 islanders 
# postponed, 1/24/2016 capitals postponed. Missing 7 games from 10/8/2015, persistent 503.
raw.nhl.2015 <- scrape_season(nhlSeasons[1] %>% reduce(c), "nhl")
# 2016-17 season. 1234 observations -> 1230 games, 3 allstar games, 3/14/2017 devils postponed.
raw.nhl.2016 <- scrape_season(nhlSeasons[2] %>% reduce(c), "nhl")
# 2017-18 season. 1274 observations -> 1271 games, 3 allstar games. VGK joined this season.
raw.nhl.2017 <- scrape_season(nhlSeasons[3] %>% reduce(c), "nhl")
# 2018-19 season. 1274 observations -> 1271 games, 3 allstar games.
raw.nhl.2018 <- scrape_season(nhlSeasons[4] %>% reduce(c), "nhl")
# NHL 2021-22 season. 1413 observations -> 1312 games, 3 allstar games, 98 postponed. Kraken joined.
raw.nhl.2021 <- scrape_season(nhlSeasons[5] %>% reduce(c), "nhl")

# Write the data
mass_write("assets/game_data/nba/", game_data_names %includes% "nba")
mass_write("assets/game_data/nhl/", game_data_names %includes% "nhl")

# Betting data 
attach_source("bet.R", "betting_data")
write_bet("assets/betting_data/")

# # Collect Raw data, tigris package maybe maybe census idk
counties(cb = TRUE, resolution = "500k") %>% # 500k is best resolution
  saveRDS(file = "assets/leaflet/2020_fips_shapes.rds") # geometry doesn't save nicely as CSV, should probably do for other files as well

# FIP/CBSA Data ----------------------------------------------------------------

# assigns home team to cbsa
assign_home <- function(.cbsa) {
  case_when(
    .cbsa == "12060" ~ "Atlanta Hawks",
    .cbsa == "14460" ~ "Boston Bruins,Boston Celtics",
    .cbsa == "15380" ~ "Buffalo Sabres",
    .cbsa == "16740" ~ "Charlotte Hornets",
    .cbsa == "16980" ~ "Chicago Blackhawks,Chicago Bulls",
    .cbsa == "17460" ~ "Cleveland Cavaliers",
    .cbsa == "18140" ~ "Columbus Blue Jackets",
    .cbsa == "19100" ~ "Dallas Mavericks,Dallas Stars",
    .cbsa == "19740" ~ "Colorado Avalanche,Denver Nuggets",
    .cbsa == "19820" ~ "Detroit Pistons,Detroit Red Wings",
    .cbsa == "26420" ~ "Houston Rockets",
    .cbsa == "26900" ~ "Indiana Pacers",
    .cbsa == "29820" ~ "Vegas Golden Knights",
    .cbsa == "31080" ~ "Anaheim Ducks,Los Angeles Clippers,Los Angeles Kings,Los Angeles Lakers",
    .cbsa == "32820" ~ "Memphis Grizzlies",
    .cbsa == "33100" ~ "Florida Panthers,Miami Heat",
    .cbsa == "33340" ~ "Milwaukee Bucks",
    .cbsa == "33460" ~ "Minnesota Timberwolves,Minnesota Wild",
    .cbsa == "34980" ~ "Nashville Predators",
    .cbsa == "35380" ~ "New Orleans Pelicans", 
    .cbsa == "35620" ~ "Brooklyn Nets,New Jersey Devils,New York Islanders,New York Knicks,New York Rangers",
    .cbsa == "36420" ~ "Oklahoma City Thunder",
    .cbsa == "36740" ~ "Orlando Magic",
    .cbsa == "37980" ~ "Philadelphia 76ers,Philadelphia Flyers",
    .cbsa == "38060" ~ "Arizona Coyotes,Phoenix Suns",
    .cbsa == "38300" ~ "Pittsburgh Penguins",
    .cbsa == "38900" ~ "Portland Trail Blazers",
    .cbsa == "39580" ~ "Carolina Hurricanes",
    .cbsa == "40900" ~ "Sacramento Kings",
    .cbsa == "41180" ~ "St. Louis Blues", 
    .cbsa == "41620" ~ "Utah Jazz",
    .cbsa == "41700" ~ "San Antonio Spurs",
    .cbsa == "41860" ~ "Golden State Warriors",
    .cbsa == "41940" ~ "San Jose Sharks",
    .cbsa == "45300" ~ "Tampa Bay Lightning",
    .cbsa == "47900" ~ "Washington Capitals,Washington Wizards"
  )
}
# returns time zone based on cbsa code
cbsa_to_time <- function(.cbsa) {
  case_when(
    .cbsa == "12060" ~ "EST",
    .cbsa == "14460" ~ "EST",
    .cbsa == "15380" ~ "EST",
    .cbsa == "16740" ~ "EST",
    .cbsa == "16980" ~ "CST",
    .cbsa == "17460" ~ "EST",
    .cbsa == "18140" ~ "EST",
    .cbsa == "19100" ~ "CST",
    .cbsa == "19740" ~ "MST",
    .cbsa == "19820" ~ "EST",
    .cbsa == "26420" ~ "CST",
    .cbsa == "26900" ~ "EST",
    .cbsa == "29820" ~ "PST",
    .cbsa == "31080" ~ "PST",
    .cbsa == "32820" ~ "CST",
    .cbsa == "33100" ~ "EST",
    .cbsa == "33340" ~ "CST",
    .cbsa == "33460" ~ "CST",
    .cbsa == "34980" ~ "CST",
    .cbsa == "35380" ~ "CST", 
    .cbsa == "35620" ~ "EST",
    .cbsa == "36420" ~ "CST",
    .cbsa == "36740" ~ "EST",
    .cbsa == "37980" ~ "EST",
    .cbsa == "38060" ~ "MST",
    .cbsa == "38300" ~ "EST",
    .cbsa == "38900" ~ "PST",
    .cbsa == "39580" ~ "EST",
    .cbsa == "40900" ~ "PST",
    .cbsa == "41180" ~ "CST", 
    .cbsa == "41620" ~ "MST",
    .cbsa == "41700" ~ "CST",
    .cbsa == "41860" ~ "PST",
    .cbsa == "41940" ~ "PST",
    .cbsa == "45300" ~ "EST",
    .cbsa == "47900" ~ "EST"
  )
}

# get Neighboring counties
dat.neighboring <- counties() %>% # gets county geo-spatial data
  select(GEOID, geometry) %>% 
  set_colnames(c("fips", "geometry"))
dat.neighboring <- dat.neighboring %>% # run geo-spatial list into a function that automatically detects adjacencies
  pull(geometry) %>% 
  poly2nb(row.names = dat.neighboring %>% pull(fips)) %>% 
  neighborsDataFrame() %>% # expp library
  group_by(id) %>% 
  summarize(id_neigh = list(unique(id_neigh))) %>% 
  ungroup() %>% 
  set_colnames(c("fips", "fips_neighbors"))

# Clean CBSA data
dat.cbsa <- read_csv("assets/cbsa_data/raw.cbsa.csv", col_types = cols(.default = "c")) %>% 
  as_tibble() %>% 
  select(CBSA, CBSA_title, STATE, COUNTY, central_outlying) %>% 
  rename_with(~tolower(.x), everything()) %>% 
  filter(cbsa %in% (c(12060, 14460, 15380, 16740, 16980, 17460, 18140, 19100, 19740, 
                     19820, 26420, 26900, 29820, 31080, 32820, 33100, 33340, 33460,
                     34980, 35380, 35620, 36420, 36740, 37980, 38060, 38300, 38900, 
                     39580, 40900, 41180, 41620, 41700, 41860, 41940, 45300, 47900) %>% 
           as.character())) %>% # all 36 unique cbsas. 27 unique NBA, 21 unique NHL. 313 FIPS
  unite("fips", state:county, sep = "") %>% 
  separate(cbsa_title, into = c("cbsa_title", "cbsa_states"), sep = ", ") %>% 
  mutate(cbsa_states = map(cbsa_states, ~str_split(.x, "-")[[1]] %>% c())) %>% 
  mutate(home = assign_home(cbsa), .after = cbsa_title) %>% 
  mutate(home = map(home, ~str_split(.x, ",")[[1]] %>% c())) %>% 
  mutate(time_zone = cbsa_to_time(cbsa), .after = cbsa_states) %>% 
  group_by(cbsa) %>% 
  mutate(cbsa_fips = list(unique(fips)), .after = time_zone) %>% # all FIPs in a cbsa, in a list
  ungroup() %>% 
  left_join(dat.neighboring, by = "fips") %>% 
  mutate(cbsa_neighbors = map2(fips_neighbors, cbsa_fips, ~setdiff(.x, .y))) %>% # All fips neighbors, including those already in the CBSA
  left_join(unnest(., cbsa_neighbors) %>%
              distinct(cbsa, cbsa_neighbors) %>% 
              group_by(cbsa) %>% 
              summarize(cbsa_neighbors = list(unique(cbsa_neighbors))),
            by = "cbsa") %>% 
  select(-cbsa_neighbors.x) %>% 
  rename(cbsa_neighbors = cbsa_neighbors.y) %>% # now, cbsa_neighbors is at the CBSA level, and identifies all bordering to that CBSA
  relocate(cbsa_neighbors, .after = cbsa_fips) %>% 
  bind_rows(distinct(., cbsa, cbsa_neighbors) %>% 
              unnest(cbsa_neighbors) %>% 
              rename(fips = cbsa_neighbors) %>% 
              mutate(central_outlying = "Neighboring")
              ) %>% # adds 430 fips, 313 + 430 = 743, all the neighboring fips. Assign them as "neighboring". 
  filter(fips %notin% c("26021", "26159", "26005", "26139", "26121", "26127")) %>% # erraneously assigned neighbors
  relocate(fips_neighbors, .after = central_outlying) %>% 
  left_join(x = select(., -(cbsa_title:cbsa_neighbors)), # for some reason, specifying x and y was necessary
            y = select(., cbsa:cbsa_neighbors) %>% distinct(cbsa, .keep_all = T),
             by = "cbsa") %>% 
  relocate(fips:fips_neighbors, .after = last_col())
  # 313 unique CBSA-only fips, 407 unique neighbor-only fips.
  # 23 duplicates. 13 fips are doubled because they neighbors to two CBSAs. 10 are a neighbor to one cbsa, and in another cbsa.

# Covid-19 Data ----------------------------------------------------------------

# Clean Covid-19 data
dat.covid <- read_csv("assets/covid_data/raw.covid2021.csv") %>% 
  bind_rows(read_csv("assets/covid_data/raw.covid2022.csv")) %>% 
  as_tibble() %>% 
  relocate(fips, state, .before = county) %>% 
  # The 5 boroughs are listed as "New York City" in county var, and NA in fips. 
  # I made it so NYC all under Bronx fip. 309 unique fips now. So 720 -> 716 unique fips
  mutate(fips = case_when(county == "New York City" ~ "36005", T ~ fips)) %>% 
  filter(fips %in% (dat.cbsa %>% pull(fips))) %>% 
  # 2021-09-13 is the Monday 4 weeks before the first NHL games, 2022-05-29 is the 
  # Sunday 4 weeks after the last games. The NBA starts later and finishes earlier than the NHL.
  filter("2021-09-12" <= date & date <= "2022-05-29") %>% # keep a day before 2021-09-13 for lag calc
  left_join(dat.cbsa %>% select(cbsa, cbsa_title, fips, central_outlying), by = "fips") %>% 
  relocate(cbsa:cbsa_title, .after = date) %>% 
  relocate(central_outlying, .after = county) %>% 
  mutate(floor_monday = floor_date(date, "week", 1), .after = date) %>%  # week identifier
  group_by(fips, central_outlying) %>% # need central_outlying to adequately identify counties, since we have duplicates.
  mutate(across(cases:deaths, ~case_when( # create new cases and deaths
    row_number() == 1 ~ NA_real_, 
    T ~ .x - lag(.x)
  ), 
  .names = "n_{col}"
  )) %>%  
  ungroup() %>% 
  filter(date > "2021-09-12") %>% # we just needed this to calculate new cases for 2021-09-13
  # we now have 739 FIPs each with 259 observations. 191401 total, 23 duplicate FIPs.
  group_by(floor_monday, fips, central_outlying) %>% 
  mutate(across(n_cases:n_deaths, ~sum(.x), .names = "w_{col}")) %>% # weekly cases/deaths
  ungroup() %>% 
  # Code below is for the 'filled' cases/deaths vars.
  # fill takes c(NA, 1, NA, 3, NA) -> c(0, 1, 1, 3, 0). By filling in the boundary NAs, we could potentially mess up the data bad.
  # For example, c(0, 0, 1000, -1, 2000, ...) -> c(0, 0, 1000, 1500, 2000, ...), when perhaps we should only trust the first time we get a
  # trustworthy "Start" and "End" value. For low-pop fips, 0s likely are fine to use as boundaries, not the case for bigger ones.
  # We're gonna want to analyze the percent increase/decrease by week and make sure there aren't many outliers
  left_join(group_by(., fips, central_outlying) %>% 
              mutate(w_n_cases = case_when( # encodes 0 and negatives as NAs
                w_n_cases <= 0 ~ NA_real_,
                T ~ w_n_cases
              )) %>% # 191401
              mutate(w_n_deaths = case_when(
                w_n_deaths <= 0 ~ NA_real_, # Encoding 0s messes shit up because first observations are sometimes 0.
                T ~ w_n_deaths
              )) %>% 
              distinct(., fips, central_outlying, floor_monday, .keep_all = T) %>% 
              # group_by(fips, central_outlying) %>% 
              mutate(across(w_n_cases:w_n_deaths, ~floor(fill_average(.x)), .names = "f_{col}")) %>% 
              select(floor_monday, fips, central_outlying, f_w_n_cases:f_w_n_deaths),
            by = c("floor_monday", "fips", "central_outlying")) %>% 
  # filter(floor_monday != as.Date("2022-05-23")) %>% 
  # I used to remove the week above, as the were some 0s in the case/death counts in the last week.
  # Aggregate to CBSA level below.
  left_join(
    filter(., central_outlying != "Neighboring") %>% 
      distinct(date, cbsa, fips, .keep_all = T) %>%
      group_by(date, cbsa) %>% 
      mutate(across(n_cases:n_deaths, ~sum(.x), .names = "cbsa_{col}")) %>% 
      distinct(date, cbsa, .keep_all = T) %>% 
      ungroup() %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(cbsa_n_cases:cbsa_n_deaths, ~lag(.x), .names = "lag_{col}")) %>% 
      select(date, cbsa, cbsa_n_cases:lag_cbsa_n_deaths),
    by = c("date", "cbsa")) %>% 
  left_join(
    filter(., central_outlying == "Neighboring") %>% 
      distinct(date, cbsa, fips, .keep_all = T) %>%
      group_by(date, cbsa) %>% 
      mutate(across(n_cases:n_deaths, ~sum(.x), .names = "neigh_cbsa_{col}")) %>% 
      distinct(date, cbsa, .keep_all = T) %>% 
      ungroup() %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(neigh_cbsa_n_cases:neigh_cbsa_n_deaths, ~lag(.x), .names = "lag_{col}")) %>% 
      select(date, cbsa, neigh_cbsa_n_cases:lag_neigh_cbsa_n_deaths),
    by = c("date", "cbsa")) %>% 
  left_join(
    filter(., central_outlying != "Neighboring") %>% 
      distinct(floor_monday, cbsa, fips, .keep_all = T) %>%
      group_by(floor_monday, cbsa) %>% 
      mutate(across(w_n_cases:f_w_n_deaths, ~sum(.x), .names = "cbsa_{col}")) %>% 
      distinct(floor_monday, cbsa, .keep_all = T) %>% 
      ungroup() %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(cbsa_w_n_cases:cbsa_f_w_n_deaths, ~lag(.x), .names = "lag_{col}")) %>% 
      select(floor_monday, cbsa, cbsa_w_n_cases:lag_cbsa_f_w_n_deaths),
    by = c("floor_monday", "cbsa")) %>% 
  left_join(
    filter(., central_outlying == "Neighboring") %>% 
      distinct(floor_monday, cbsa, fips, .keep_all = T) %>%
      group_by(floor_monday, cbsa) %>% 
      mutate(across(w_n_cases:f_w_n_deaths, ~sum(.x), .names = "neigh_cbsa_{col}")) %>% 
      distinct(floor_monday, cbsa, .keep_all = T) %>% 
      ungroup() %>% 
      group_by(cbsa) %>% # add one week lag
      mutate(across(neigh_cbsa_w_n_cases:neigh_cbsa_f_w_n_deaths, ~lag(.x), .names = "lag_{col}")) %>% 
      select(floor_monday, cbsa, neigh_cbsa_w_n_cases:lag_neigh_cbsa_f_w_n_deaths),
    by = c("floor_monday", "cbsa"))

write_csv(dat.covid %>% mutate(across(everything(), ~as.character(.x) %>% paste0("?"))),
          "assets/cleaned/dat.covid.csv")
# not loading nicely? Maybe cause im doing it rn with a bunch of NAs? Shouldn't be different from last timne ...
# write.xlsx(dat.covid %>% mutate(across(everything(), ~as.character(.x))),
#            "assets/cleaned/dat.covid.xlsx")

# Calvert in washington
# theres a week in boston. can look at essex county
# gilpin county, fips 13149 good examples of differneces in the fill.
# old notes below VVVV
# examples:
# Manassas Park city -> very little covid activity, 7 day stretches of 0 cases
# Canadien county -> 1 figure the whole week
# Orange county, Florida -> 1 figure the whole week, example of 2 figures a week ...
# ... they also seemed to only be reporting deaths for a period of time

# Betting Data -----------------------------------------------------------------

# Load already cleaned betting data
attach_source("bet.R", "betting_data")
dat.bet <- load_clean_bet()

# Game Data Cleaning -----------------------------------------------------------

# Clean NBA data 
dat.nba <- mass_load("assets/game_data/nba/", 1, .bind = T) %>% # 6158 observations
  as_tibble() %>% 
  mutate(across(everything(), ~str_replace(.x, "\\?", ""))) %>% 
  filter(home_record != "") %>% # -7 -> remove allstar, USA games
  filter(home_score != "NA") %>% # -1 -> removes postponed game
  filter(home != "Toronto Raptors") %>% # -205, 41*5
  filter(stadium %notin% c("Arena Ciudad de Mexico", "Mexico City Arena", "O2 Arena (ENG)", 
  "UW-Milwaukee Panther Arena")) %>% # -12 -> this last stadium was only played in one time
  mutate(across(home:away, ~case_when( 
    .x == "LA Clippers" ~ "Los Angeles Clippers",
    T ~ .x
  ))) %>% 
  # Filling in missing attendance figures below. Missing from the actual ESPN site.
  mutate(attendance = case_when(
    date == "2021-11-03" & home == "Sacramento Kings" ~ "Attendance: 12,480", # https://www.nba.com/game/nop-vs-sac-0022100118?watch
    date == "2021-10-26" & home == "Oklahoma City Thunder" ~ "Attendance: 15,717", # https://www.nba.com/game/gsw-vs-okc-0022100051    
    date == "2019-04-09" & home == "Utah Jazz" ~ "Attendance: 18,306", # https://www.nba.com/game/den-vs-uta-0021801217?watch
    date == "2021-10-27" & home == "Boston Celtics" ~ "Attendance: 19,156", # https://www.nba.com/game/was-vs-bos-0022100056?watch
    date == "2017-11-08" & home == "Orlando Magic" ~ "Attendance: 18,803", # https://www.nba.com/game/nyk-vs-orl-0021700160
    T ~ attendance
  )) %>% 
  mutate(league = "NBA", .after = date)

# Clean NHL data 
dat.nhl <- mass_load("assets/game_data/nhl/", 1, .bind = T) %>% # 6423 observations
  as_tibble() %>% 
  mutate(across(everything(), ~str_replace(.x, "\\?", ""))) %>% 
  filter(home_record != "") %>% # -15 -> remove allstar games
  filter(home_score != "NA") %>% # -101 -> removes postponed game
  filter(home %notin% c("Calgary Flames", "Edmonton Oilers", "Montreal Canadiens", 
                        "Ottawa Senators", "Toronto Maple Leafs", "Vancouver Canucks", 
                        "Winnipeg Jets", "Seattle Kraken")) %>% # -1476, 41*7*5 + 41 (Kraken)
  filter(stadium %notin% c("Citi Field", "Ericsson Globe", "Hartwall Areena", 
                           "Lincoln Financial Field", "Navy-Marine Corps Memorial Stadium",
                           "Nissan Stadium", "Notre Dame Stadium", "Scandinavium",
                           "Target Field", "Tim Hortons Field", "Scotiabank Arena", 
                           "NA", "Rogers Place")) %>% # -23. NA just seems to be winter classics
  # Add in missing games from -> https://www.espn.com/nhl/boxscore/_/gameId/400814774
  # https://www.hockey-reference.com/boxscores/201510080BUF.html https://www.espn.com/nhl/game/_/gameId/400814776
  # https://www.hockey-reference.com/boxscores/201510080DAL.html https://www.espn.com/nhl/game/_/gameId/400814778
  # https://www.hockey-reference.com/boxscores/201510080STL.html https://www.espn.com/nhl/game/_/gameId/400814780
  bind_rows(tibble(date = rep("2015-10-08", 7),
                   home = c("Boston Bruins", "Buffalo Sabres", "Colorado Avalanche",
                            "Dallas Stars","Nashville Predators", "St. Louis Blues", 
                            "Tampa Bay Lightning"),
                   away = c("Winnipeg Jets", "Ottawa Senators", "Minnesota Wild", 
                            "Pittsburgh Penguins", "Carolina Hurricanes", "Edmonton Oilers",
                            "Philadelphia Flyers"),
                   home_record = c("0-1-0", "0-1-0", "0-1-0", "1-0-0", "1-0-0", "1-0-0",
                                   "1-0-0"),
                   away_record = c("1-0-0", "1-0-0", "1-0-0", "0-1-0", "0-1-0", "0-1-0", 
                                   "0-0-1"),
                   home_score = c(2, 1, 4, 3, 2, 3, 3) %>% as.character(),
                   away_score = c(6, 3, 5, 0, 1, 1, 2) %>% as.character(),
                   attendance = c("Attendance: 17,565", "Attendance: 19,070", "Attendance: 18,007",
                                  "Attendance: 18,532", "Attendance: 17,204", "Attendance: 19,327",
                                  "Attendance: 19,092"),
                   capacity = c("Capacity: 17,850", "Capacity: 19,070", "Capacity: 18,007",
                                "Capacity: 18,532", "Capacity: 17,113", "Capacity: 18,096", 
                                "Capacity: 19,092"),
                   stadium = c("TD Garden", "KeyBank Center", "Ball Arena", "American Airlines Center",
                               "Bridgestone Arena", "Enterprise Center", "Amalie Arena"),
                   game_time = c("7:00 PM, October 8, 2015", "7:00 PM, October 8, 2015", 
                                 "9:00 PM, October 8, 2015", "8:30 PM, October 8, 2015",
                                 "8:00 PM, October 8, 2015", "8:00 PM, October 8, 2015",
                                 "7:30 PM, October 8, 2015"))) %>% 
  arrange(date) %>% 
  mutate(league = "NHL", .after = date)

# Final Merge ------------------------------------------------------------------

# policy data function
policy_func <- function(.home, .date) {
  case_when(
    .home == "Anaheim Ducks" & .date > "2022-04-01" ~ "none",
    .home == "Anaheim Ducks" & .date > "2022-01-15" ~ "vaccine",
    .home == "Anaheim Ducks" & .date > "2021-12-15" ~ "both",
    .home == "Anaheim Ducks" & .date > "2021-10-01" ~ "vaccine", 
    .home == "Arizona Coyotes" & .date > "2021-10-01" ~ "none",
    .home == "Atlanta Hawks" & .date > "2021-10-01" ~ "none", 
    .home == "Boston Bruins" & .date >= "2022-03-05" ~ "none",  
    .home == "Boston Bruins" & .date >= "2022-02-21" ~ "mask",
    .home == "Boston Bruins" & .date >= "2021-10-01" ~ "both", 
    .home == "Boston Celtics" & .date >= "2022-03-05" ~ "none",  
    .home == "Boston Celtics" & .date >= "2022-02-21" ~ "mask", 
    .home == "Boston Celtics" & .date >= "2021-10-01" ~ "both", 
    .home == "Brooklyn Nets" & .date >= "2022-03-07" ~ "none", 
    .home == "Brooklyn Nets" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Buffalo Sabres" & .date > "2022-02-28" ~ "none", 
    .home == "Buffalo Sabres" & .date > "2021-10-01" ~ "vaccine", 
    .home == "Carolina Hurricanes" & .date > "2022-02-28" ~ "none", 
    .home == "Carolina Hurricanes" & .date > "2021-10-01" ~ "mask", 
    .home == "Columbus Blue Jackets" & .date > "2021-10-01" ~ "none", 
    .home == "Chicago Bulls" & .date >= "2022-03-22" ~ "none",
    .home == "Chicago Bulls" & .date >= "2022-03-04" ~ "vaccine", 
    .home == "Chicago Bulls" & .date >= "2021-10-01" ~ "both",
    .home == "Charlotte Hornets" & .date >= "2022-02-28" ~ "none",
    .home == "Charlotte Hornets" & .date >= "2021-10-01" ~ "mask",
    .home == "Chicago Blackhawks" & .date >= "2022-03-22" ~ "none",
    .home == "Chicago Blackhawks" & .date >= "2022-03-03" ~ "vaccine", 
    .home == "Chicago Blackhawks" & .date >= "2021-10-01" ~ "both",
    .home == "Chicago Bulls" & .date >= "2022-03-22" ~ "none",
    .home == "Chicago Bulls" & .date >= "2022-03-04" ~ "vaccine", 
    .home == "Chicago Bulls" & .date >= "2021-10-01" ~ "both",
    .home == "Cleveland Cavaliers" & .date >= "2022-02-01" ~ "none",
    .home == "Cleveland Cavaliers" & .date >= "2021-12-31" ~ "mask",
    .home == "Cleveland Cavaliers" & .date >= "2021-10-01" ~ "none",
    .home == "Colorado Avalanche" & .date >= "2022-03-12" ~ "none",
    .home == "Colorado Avalanche" & .date >= "2021-10-01" ~ "both",
    .home == "Dallas Mavericks" & .date >= "2022-03-03" ~ "none",
    .home == "Dallas Mavericks" & .date >= "2021-11-15" ~ "mask",
    .home == "Dallas Mavericks" & .date >= "2021-10-01" ~ "both",
    .home == "Dallas Stars" & .date >= "2022-03-03" ~ "none", # bad data potentially
    .home == "Dallas Stars" & .date >= "2021-10-01" ~ "mask",
    .home == "Denver Nuggets" & .date >= "2022-03-12" ~ "none",
    .home == "Denver Nuggets" & .date >= "2021-10-01" ~ "both",
    .home == "Detroit Pistons" & .date >= "2021-10-01" ~ "none",
    .home == "Detroit Red Wings" & .date >= "2021-10-01" ~ "none",
    .home == "Florida Panthers" & .date >= "2021-10-01" ~ "none",
    .home == "Golden State Warriors" & .date >= "2022-04-01" ~ "none",
    .home == "Golden State Warriors" & .date >= "2022-02-16" ~ "vaccine",
    .home == "Golden State Warriors" & .date >= "2021-10-01" ~ "both",
    .home == "Houston Rockets" & .date >= "2021-10-01" ~ "none",
    .home == "Indiana Pacers" & .date >= "2021-10-01" ~ "none",
    .home == "Los Angeles Clippers" & .date >= "2022-04-01" ~ "none",
    .home == "Los Angeles Clippers" & .date >= "2022-02-25" ~ "vaccine",
    .home == "Los Angeles Clippers" & .date >= "2021-10-01" ~ "both",
    .home == "Los Angeles Kings" & .date >= "2022-04-01" ~ "none",
    .home == "Los Angeles Kings" & .date >= "2022-02-25" ~ "vaccine",
    .home == "Los Angeles Kings" & .date >= "2021-10-01" ~ "both",
    .home == "Los Angeles Lakers" & .date >= "2022-04-01" ~ "none",
    .home == "Los Angeles Lakers" & .date >= "2022-02-25" ~ "vaccine",
    .home == "Los Angeles Lakers" & .date >= "2021-10-01" ~ "both",
    .home == "Minnesota Wild" & .date >= "2022-02-28" ~ "none",
    .home == "Minnesota Wild" & .date >= "2022-02-10" ~ "mask",
    .home == "Minnesota Wild" & .date >= "2022-01-06" ~ "both",
    .home == "Minnesota Wild" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Nashville Predators" & .date >= "2021-11-13" ~ "none",
    .home == "Nashville Predators" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Memphis Grizzlies" & .date >= "2021-11-24" ~ "none",
    .home == "Memphis Grizzlies" & .date >= "2021-10-01" ~ "both",
    .home == "New Jersey Devils" & .date >= "2022-03-02" ~ "none",
    .home == "New Jersey Devils" & .date >= "2022-01-10" ~ "both",
    .home == "New Jersey Devils" & .date >= "2022-12-22" ~ "mask",
    .home == "New Jersey Devils" & .date >= "2021-10-01" ~ "none",
    .home == "New York Islanders" & .date >= "2022-02-17" ~ "none",
    .home == "New York Islanders" & .date >= "2021-10-01" ~ "both",
    .home == "New York Rangers" & .date >= "2022-03-07" ~ "none",
    .home == "New York Rangers" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Pittsburgh Penguins" & .date >= "2021-10-01" ~ "none",
    .home == "San Jose Sharks" & .date >= "2021-03-28" ~ "none",
    .home == "San Jose Sharks" & .date >= "2022-03-02" ~ "vaccine",
    .home == "San Jose Sharks" & .date >= "2021-10-01" ~ "both",
    .home == "St. Louis Blues" & .date >= "2022-03-06" ~ "none",
    .home == "St. Louis Blues" & .date >= "2021-10-01" ~ "both",
    .home == "Tampa Bay Lightning" & .date >= "2021-10-01" ~ "none",
    .home == "Vegas Golden Knights" & .date >= "2022-02-10" ~ "none",
    .home == "Vegas Golden Knights" & .date >= "2021-10-01" ~ "mask",
    .home == "Washington Capitals" & .date >= "2022-03-01" ~ "none",
    .home == "Washington Capitals" & .date >= "2022-02-15" ~ "mask",
    .home == "Washington Capitals" & .date >= "2022-01-15" ~ "both",
    .home == "Washington Capitals" & .date >= "2021-10-01" ~ "mask",
    .home == "Miami Heat" & .date >= "2022-02-26" ~ "none",
    .home == "Miami Heat" & .date >= "2021-10-1" ~ "mask",
    .home == "Milwaukee Bucks" & .date >= "2022-03-02" ~ "none",
    .home == "Milwaukee Bucks" & .date >= "2022-01-01" ~ "mask",
    .home == "Milwaukee Bucks" & .date >= "2021-10-1" ~ "none",
    .home == "Minnesota Timberwolves" & .date >= "2022-02-28" ~ "none",
    .home == "Minnesota Timberwolves" & .date >= "2022-02-24" ~ "vaccine",
    .home == "Minnesota Timberwolves" & .date >= "2022-01-26" ~ "both",
    .home == "Minnesota Timberwolves" & .date >= "2022-01-16" ~ "mask",
    .home == "Minnesota Timberwolves" & .date >= "2021-10-01" ~ "none",
    .home == "New Orleans Pelicans" & .date >= "2022-03-22" ~ "none", 
    .home == "New Orleans Pelicans" & .date >= "2022-03-03" ~ "vaccine",
    .home == "New Orleans Pelicans" & .date >= "2021-10-01" ~ "both",
    .home == "New York Knicks" & .date >= "2022-03-07" ~ "none",
    .home == "New York Knicks" & .date >= "2021-10-01" ~ "both",
    .home == "Oklahoma City Thunder" & .date >= "2022-01-15" ~ "none",
    .home == "Oklahoma City Thunder" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Orlando Magic" & .date >= "2021-10-01" ~ "none",
    .home == "Philadelphia 76ers" & .date >= "2022-03-02" ~ "none",
    .home == "Philadelphia 76ers" & .date >= "2022-02-06" ~ "mask",
    .home == "Philadelphia 76ers" & .date >= "2022-01-03" ~ "both",
    .home == "Philadelphia 76ers" & .date >= "2021-10-01" ~ "mask",
    .home == "Philadelphia Flyers" & .date >= "2022-03-02" ~ "none",
    .home == "Philadelphia Flyers" & .date >= "2022-02-06" ~ "mask",
    .home == "Philadelphia Flyers" & .date >= "2022-01-03" ~ "both",
    .home == "Philadelphia Flyers" & .date >= "2021-10-01" ~ "mask",
    .home == "Phoenix Suns" & .date >= "2021-10-01" ~ "none",
    .home == "Portland Trail Blazers" & .date >= "2022-03-12" ~ "none",
    .home == "Portland Trail Blazers" & .date >= "2021-10-01" ~ "both",
    .home == "Sacramento Kings" & .date >= "2022-04-01" ~ "none",
    .home == "Sacramento Kings" & .date >= "2022-03-22" ~ "vaccine",
    .home == "Sacramento Kings" & .date >= "2021-10-01" ~ "both",
    .home == "San Antonio Spurs" & .date >= "2021-10-01" ~ "none",
    .home == "Utah Jazz" & .date >= "2022-02-25" ~ "none",
    .home == "Utah Jazz" & .date >= "2022-01-21" ~ "vaccine",
    .home == "Utah Jazz" & .date >= "2022-01-08" ~ "both",
    .home == "Utah Jazz" & .date >= "2021-10-01" ~ "vaccine",
    .home == "Washington Wizards" & .date >= "2022-03-01" ~ "none",
    .home == "Washington Wizards" & .date >= "2022-02-15" ~ "mask",
    .home == "Washington Wizards" & .date >= "2022-01-15" ~ "both",
    .home == "Washington Wizards" & .date >= "2021-10-01" ~ "mask",
    T ~ NA_character_
  )
}
  
# Merge everything together, prep for regression
dat.final <- dat.nba %>% 
  bind_rows(dat.nhl) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(across(home_score:away_score, ~as.numeric(.x))) %>% 
  mutate(across(attendance:capacity, ~str_extract(.x, "(?<=: )[\\s\\S]*") %>% 
                  str_replace(",", "") %>% 
                  as.numeric())) %>% 
  mutate(attendance_per = attendance/capacity) %>% 
  relocate(attendance_per, .after = capacity) %>% # up to here all just string cleaning
  mutate(across(c(home_record, away_record, game_time), ~str_extract(.x, "^[^,]*"))) %>% 
  left_join(dat.bet %>% select(-home_odds, -away_odds), by = c("date", "home")) %>% 
  relocate(adj_home_odds:adj_away_odds, .after = away_score) %>% 
  left_join(dat.cbsa %>% # get cbsa-level vars, 4 vars
              select(cbsa:time_zone) %>% 
              unnest(home) %>% 
              distinct(home, .keep_all = T),
            by = "home") %>% 
  relocate(cbsa:time_zone, .after = date) %>% 
  left_join(dat.covid %>% # merging covid data, 16 vars
              distinct(date, cbsa, .keep_all = T) %>% 
              select(date, cbsa, cbsa_n_cases:ncol(.)),
            by = c("date", "cbsa")) %>% 
  mutate(floor_monday = floor_date(date, "week", 1), .after = date) %>% 
  relocate(season, .after = floor_monday) %>% 
  mutate(game_time_num = case_when( # make 24 hour time. All games in PM
    time_zone == "EST" ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric(),
    time_zone == "CST" ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() - 1,
    time_zone == "MST" ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() - 2,
    T ~ str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() - 3 
  ) + # Adjust for time weirdness
    case_when(str_extract(game_time, "(\\d+|:)+") %>% str_replace(":", ".") %>% as.numeric() >= 12 ~ 0, T ~ 12), 
  .after = game_time) %>% 
  mutate(game_time = case_when( # Adjust time
    game_time_num < 12 ~ format(round(game_time_num, 2), nsmall = 2) %>% paste("AM") %>% str_replace("\\.", ":"),
    T ~ format(round(game_time_num, 2), nsmall = 2) %>% paste("PM") %>% str_replace("\\.", ":"),
  )) %>% 
  mutate(game_time_approx = case_when( # forward inclusive, like an 8:59pm game is still "Evening Game"
    game_time_num < 18 ~ "Early Game", # includes, for example, 5:15pm
    18 <= game_time_num | game_time_num <= 20 ~ "Evening Game",
    game_time_num > 20 ~ "Night Game" 
  ), .after = game_time_num) %>% 
  mutate(policy = policy_func(home, date), .after = stadium) 
  
write_csv(dat.final %>% mutate(across(everything(), ~as.character(.x) %>% paste0("?"))),
          "assets/cleaned/dat.final.csv")
# not loading nicely? Maybe cause im doing it rn with a bunch of NAs? Shouldn't be different from last timne ...
write.xlsx(dat.final %>% mutate(across(everything(), ~as.character(.x))),
          "assets/cleaned/dat.final.xlsx")

# Leaflet Data -----------------------------------------------------------------

# 200 & 710 observations
dat.leaflet <- readRDS("assets/leaflet/2020_fips_shapes.rds") %>% # 500k is best resolution
  select(GEOID, geometry) %>% 
  mutate(geometry = st_transform(geometry, '+proj=longlat +datum=WGS84')) %>% # prevents leaflet warnings
  rename(fips = GEOID) %>% 
  bind_rows(filter(., fips %in% c("36005", "36047", "36061", "36081", "36085")) %>% summarize(fips = "36005", geometry = st_union(geometry))) %>% 
  filter(row_number() != 471) %>% 
  right_join(dat.covid %>% 
               select(date, floor_monday, cbsa, cbsa_title, county, fips, central_outlying, w_n_cases, w_n_deaths, cbsa_w_n_cases, cbsa_w_n_deaths, neigh_cbsa_w_n_cases, neigh_cbsa_w_n_deaths), 
             by = "fips") %>% 
  filter("2021-10-12" <= date & date <= "2022-04-29") %>% # just the 2021-22 season
  left_join(dat.final %>% select(date, cbsa, season, home:game_time_approx), by = c("date", "cbsa")) %>% 
  relocate(season, home:game_time_approx, geometry, .after = central_outlying) %>% 
  group_by(fips) %>% 
  mutate(temp = (any(central_outlying != "Neighboring") & any(central_outlying == "Neighboring")), .after = fips) %>% 
  ungroup() %>% 
  filter(!(temp == T & central_outlying == "Neighboring")) %>% # removes cbsa-neighbor duplicates, keeps CBSA ones
  group_by(fips) %>% 
  mutate(temp = (first(cbsa) == cbsa), .after = fips) %>% 
  ungroup() %>% 
  filter(temp == T) %>% # Removes neighbor-neighbor overlap.
  select(-temp) %>% 
  left_join(filter(., date == "2021-11-16") %>% # could be any day
              mutate(neighboring = case_when(central_outlying == "Neighboring" ~ T, T ~ F)) %>% 
              group_by(cbsa, neighboring) %>% 
              mutate(cbsa_geometry = st_union(geometry)) %>% 
              ungroup() %>% 
              distinct(fips, cbsa, .keep_all = T) %>% 
              select(fips, cbsa, cbsa_geometry) %>% 
              st_drop_geometry(geometry),
            by = c("fips", "cbsa")
  ) %>% 
  relocate(cbsa_geometry, .after = cbsa_title) %>% 
  mutate(across(c(home_score:away_score, attendance), ~as.integer(.x))) %>% 
  mutate(policy = case_when(
    policy == "vaccine" ~ "Vaccine Mandate",
    policy == "mask" ~ "Mask Mandate",
    policy == "both" ~ "Mask & Vaccine Mandate",
    T ~ "No Policy"
  )) %>% 
  left_join(st_drop_geometry(., geometry, cbsa_geometry) %>% # group all game data into a single data-frame column
              select(floor_monday, fips, date, home:away_score, stadium, policy, attendance) %>% 
              filter(!is.na(home)) %>% 
              mutate(date = as.character(date)) %>% 
              set_colnames(c("floor_monday", "fips", "Date", "Home Team", "Away Team", "Home Record", "Away Record", "Home Score", "Away Score", "Stadium", "Covid-19 Policy", "Attendance")) %>% 
              nest(game_data_nest = Date:Attendance),
            by = c("floor_monday", "fips")
  ) %>% 
  select(-(home:game_time)) %>% # all in data frame column
  group_by(fips, floor_monday) %>% 
  mutate(temp = row_number()) %>% # remove duplicates caused by multiples games in same cbsa on same day, just keep first.
  ungroup() %>% 
  filter(temp == 1) %>% 
  select(-temp) %>% # 710 unique fips times 29 weeks
  mutate(fip_label = paste0(
    "<dt style=font-weight:bold;font-size:12pt;>CBSA-Stats</dt>
        <li style=margin-left:15px;>CBSA Title: ", cbsa_title, "</li>
        <li style=margin-left:15px;>Weekly New Cases: ", cbsa_w_n_cases, "</li>
        <li style=margin-left:15px;>Weekly New Deaths: ", cbsa_w_n_deaths, "</li>
    <dt style=font-weight:bold;font-size:12pt;>Neighboring County Stats</dt>
        <li style=margin-left:15px;>Weekly New Cases: ", neigh_cbsa_w_n_cases, "</li>
        <li style=margin-left:15px;>Weekly New Deaths: ", neigh_cbsa_w_n_deaths, "</li>
    <dt style=font-weight:bold;font-size:12pt;>County-Stats</dt>
        <li style=margin-left:15px;>County Title: ", county, "</li>
        <li style=margin-left:15px;>Weekly New Cases: ", w_n_cases, "</li>
        <li style=margin-left:15px;>Weekly New Deaths: ", w_n_deaths, "</li>"
  )) %>% 
  select(-game_time_num, -season, -game_time_approx) 
# 
dat.leaflet.fips <- dat.leaflet %>%
  select(floor_monday, cbsa, cbsa_title, game_data_nest, fips, county, fip_label, central_outlying, geometry, w_n_cases:neigh_cbsa_w_n_deaths)
# 
dat.leaflet.cbsa <- dat.leaflet %>% 
  st_drop_geometry() %>% 
  select(cbsa, cbsa_geometry) %>% 
  distinct(cbsa_geometry)

saveRDS(dat.leaflet.fips, "app/dat.leaflet.fips.rds")
saveRDS(dat.leaflet.cbsa, "app/dat.leaflet.cbsa.rds")



test <- dat.leaflet %>% filter(floor_monday == "2021-11-15")

leaflet() %>% 
  addProviderTiles(
    providers$CartoDB.VoyagerNoLabels,
    options = providerTileOptions(
      minZoom = 4, 
      maxZoom = 10
    )) %>% 
  addPolygons(
    data = test %>% filter(central_outlying == "Neighboring"),
    fillColor = ~pal.leaflet(central_outlying),
    fillOpacity = 0.5, 
    color = "#053f63", 
    weight = 0.5,
    label = ~lapply(fip_label, htmltools::HTML)
  ) %>%
  addPolygons(
    data = test %>% filter(central_outlying != "Neighboring"),
    fillColor = ~pal.leaflet(central_outlying),
    fillOpacity = 0.5, 
    color = "#80091f", 
    weight = 0.5,
    label = ~lapply(fip_label, htmltools::HTML)
  ) %>% 
  addPolylines(
    data = test %>% group_by(cbsa_geometry) %>% filter(row_number() == 1) %>% pull(cbsa_geometry),
    color = "#000000", 
    weight = 0.75
  ) %>% 
  setMaxBounds(lng1 = -45, lat1 = 8, lng2 = -142, lat2 = 61) # box you can't drag out of



