############################################################################## #
### Load Betting data From https://www.sportsbookreviewsonline.com/
############################################################################## #

# downloads and writes betting data to directory
write_bet <- function(.path) {
  # assemble urls to data. 
  years <- c("2015-16", "2016-17", "2017-18", "2018-19", "2021-22")
  baseUrlNba <- "https://www.sportsbookreviewsonline.com/scoresoddsarchives/nba/nba%20odds%20"
  baseUrlNhl <- "https://www.sportsbookreviewsonline.com/scoresoddsarchives/nhl/nhl%20odds%20"
  urls <- c(paste0(baseUrlNba, years, ".xlsx"), paste0(baseUrlNhl, years, ".xlsx"))
  for (url in urls) {
    name <- paste0("raw.", str_sub(url, 60, -27), ".", str_sub(url, 77, -9), ".bet")
    tmp <- tempfile(fileext = ".xlsx")
    download.file(url = url, destfile = tmp, mode = "wb")
    tmp <- read_xlsx(tmp)
    write_csv(tmp, paste0(.path, name, ".csv"))
  }
}
# helper team name function
team_name_help <- function(.team, .nba = T) {
  if (.nba) {
    case_when(
      .team == "Atlanta" ~ "Atlanta Hawks",
      .team == "Boston" ~ "Boston Celtics",
      .team == "Brooklyn" ~ "Brooklyn Nets",
      .team == "Charlotte" ~ "Charlotte Hornets",
      .team == "Chicago" ~ "Chicago Bulls",
      .team == "Cleveland" ~ "Cleveland Cavaliers",
      .team == "Dallas" ~ "Dallas Mavericks",
      .team == "Denver" ~ "Denver Nuggets",
      .team == "Detroit" ~ "Detroit Pistons",
      .team == "GoldenState" ~ "Golden State Warriors",
      .team == "Golden State" ~ "Golden State Warriors",
      .team == "Houston" ~ "Houston Rockets",
      .team == "Indiana" ~ "Indiana Pacers",
      .team == "LA Clippers" ~ "Los Angeles Clippers",
      .team == "LAClippers" ~ "Los Angeles Clippers",
      .team == "LALakers" ~ "Los Angeles Lakers",
      .team == "Memphis" ~ "Memphis Grizzlies",
      .team == "Miami" ~ "Miami Heat",
      .team == "Milwaukee" ~ "Milwaukee Bucks",
      .team == "Minnesota" ~ "Minnesota Timberwolves",
      .team == "NewOrleans" ~ "New Orleans Pelicans", 
      .team == "NewYork" ~ "New York Knicks",
      .team == "Oklahoma City" ~ "Oklahoma City Thunder",
      .team == "OklahomaCity" ~ "Oklahoma City Thunder",
      .team == "Orlando" ~ "Orlando Magic",
      .team == "Philadelphia" ~ "Philadelphia 76ers",
      .team == "Phoenix" ~ "Phoenix Suns",
      .team == "Portland" ~ "Portland Trail Blazers",
      .team == "Sacramento" ~ "Sacramento Kings",
      .team == "SanAntonio" ~ "San Antonio Spurs",
      .team == "Toronto" ~ "Toronto Raptors", 
      .team == "Utah" ~ "Utah Jazz",
      .team == "Washington" ~ "Washington Wizards",
      T ~ "Error"
    )
  }
  else {
    case_when(
      .team == "Anaheim" ~ "Anaheim Ducks",
      .team == "Arizona" ~ "Arizona Coyotes",
      .team == "Boston" ~ "Boston Bruins",
      .team == "Buffalo" ~ "Buffalo Sabres",
      .team == "Calgary" ~ "Calgary Flames",
      .team == "Carolina" ~ "Carolina Hurricanes",
      .team == "Chicago" ~ "Chicago Blackhawks",
      .team == "Colorado" ~ "Colorado Avalanche",
      .team == "Columbus" ~ "Columbus Blue Jackets",
      .team == "Dallas" ~ "Dallas Stars",
      .team == "Detroit" ~ "Detroit Red Wings",
      .team == "Edmonton" ~ "Edmonton Oilers",
      .team == "Florida" ~ "Florida Panthers",
      .team == "LosAngeles" ~ "Los Angeles Kings",
      .team == "Minnesota" ~ "Minnesota Wild",
      .team == "Montreal" ~ "Montreal Canadiens",
      .team == "Nashville" ~ "Nashville Predators",
      .team == "NewJersey" ~ "New Jersey Devils",
      .team == "NYIslanders" ~ "New York Islanders",
      .team == "NYRangers" ~ "New York Rangers", 
      .team == "Ottawa" ~ "Ottawa Senators",
      .team == "Philadelphia" ~ "Philadelphia Flyers",
      .team == "Pittsburgh" ~ "Pittsburgh Penguins",
      .team == "SanJose" ~ "San Jose Sharks",
      .team == "SeattleKraken" ~ "Seattle Kraken",
      .team == "St.Louis" ~ "St. Louis Blues",
      .team == "TampaBay" ~ "Tampa Bay Lightning",
      .team == "Toronto" ~ "Toronto Maple Leafs",
      .team == "Vancouver" ~ "Vancouver Canucks",
      .team == "Vegas" ~ "Vegas Golden Knights",
      .team == "Washington" ~ "Washington Capitals",
      .team == "Winnipeg" ~ "Winnipeg Jets", 
      T ~ "Error"
    )
  }
}
# clean data
load_clean_bet <- function() {
  rawEnvir <- new.env()
  mass_load("./assets/betting_data/", rawEnvir)
  cleanEnvir <- new.env()
  for (obj in ls(envir = rawEnvir)) {
    year <- str_sub(obj, 9L, 12L) %>% as.numeric()
    mlName <- if (str_detect(obj, "nba")) "ML" else "Close"
    get(obj, envir = rawEnvir) %>% 
      filter(VH != "N") %>% # must be like games in London/Mexico, it was only like 3 though.
      select(Date, VH, Team, mlName) %>% 
      set_colnames(c("date", "VH", "home", "home_odds")) %>% 
      mutate(home = team_name_help(home, str_detect(obj, "nba"))) %>% 
      left_join(mutate(., away_odds = lag(home_odds)) %>% 
                  filter(VH == "H") %>% 
                  select(date, home, away_odds), by = c("date", "home")) %>% 
      filter(VH == "H") %>%
      select(-VH) %>% 
      mutate(across(home_odds:away_odds, ~case_when(
        .x > 0 ~ 100/(100 + .x),
        .x < 0 ~ -.x/(100 - .x),
      ))) %>% 
      mutate(across(home_odds:away_odds, ~.x/(home_odds + away_odds), .names = "adj_{col}")) %>% # biggest difference is 4.7%
      # We had to assume the data had the visitor listed first then the home team on the next row. 
      # Just judging by the results not being too different, I'd say it's fine.
      mutate(date = case_when(
        str_length(date) == 4 ~ paste(year, str_sub(date, 1L, 2L), str_sub(date, 3L, 4L), sep = "-"),
        T ~ paste(year + 1, paste0("0", str_sub(date, 1L, 1L)), str_sub(date, 2L, 3L), sep = "-")
      )) %>%
      mutate(date = as.Date(date)) %>%
      mutate(season = paste0(as.character(year), "-", str_sub(as.character(year + 1), 3L, 4L)), .after = date) %>% 
      assign(str_replace(obj, "raw", "dat"), ., envir = cleanEnvir) 
  }
  dat.bet <- NULL
  for (dat in ls(envir = cleanEnvir)) {
    dat.bet <- bind_rows(dat.bet, get(dat, envir = cleanEnvir))
  }
  dat.bet
}


