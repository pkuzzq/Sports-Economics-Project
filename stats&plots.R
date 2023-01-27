############################################################################## #
### Plots and Tables for Sports Econ Paper
############################################################################## #  

# Attendance Tables ------------------------------------------------------------

# League-level Statistics
dat.final %>% 
  group_by(league) %>% 
  summary_stats(attendance_per) %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))
# League-level Home-level Statistics
dat.final %>% 
  group_by(league, home) %>% 
  summarize(home_var = pvar(attendance_per))%>% 
  ungroup() %>% 
  group_by(league) %>% 
  summary_stats("home_var") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))

# League-Season-level Statistics
dat.final %>% 
  group_by(league, season) %>% 
  summary_stats(attendance_per, yes_median = T) %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) 
# League-Season-level Home-team Statistics
dat.final %>% 
  group_by(league, season, home) %>% 
  summarize(home_var = pvar(attendance_per))%>% 
  ungroup() %>% 
  group_by(league, season) %>% 
  summary_stats("home_var") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3))))

# Home-level Statistics
dat.final %>% 
  group_by(home) %>% 
  summary_stats("attendance_per") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) 

# Season-Home-level Statistics
dat.final %>% 
  group_by(league, season, home) %>% 
  summary_stats("attendance_per") %>% 
  mutate(across(where(~is.numeric(.x)), ~sprintf("%.3f", round(.x, 3)))) 

# Attendance Plots -------------------------------------------------------------

# From Stack exchange
guide_squarekey <- function(...) {
  # Constructor just prepends a different class
  x <- guide_legend(...)
  class(x) <- c("squarekey", class(x))
  x
}
# From Stack exchange
guide_gengrob.squarekey <- function(guide, theme) {
  legend <- NextMethod()
  is_key <- startsWith(legend$layout$name, "key-")
  is_key_bg <- is_key & endsWith(legend$layout$name, "-bg")
  is_key <- is_key & !endsWith(legend$layout$name, "-bg")
  
  key_col <- unique(legend$layout$l[is_key])
  keywidth <- convertUnit(legend$widths[2], "mm", valueOnly = TRUE)
  
  legend$grobs[is_key] <- lapply(legend$grobs[is_key], function(key) {
    key$height <- unit(keywidth - 0.5, "mm")
    key
  })
  legend$grobs[is_key_bg] <- lapply(legend$grobs[is_key_bg], function(bg) {
    bg$height <- unit(keywidth, "mm")
    bg
  })
  legend
}

# Plot of attendance Distributions
plot.attendance <- dat.final %>% 
  mutate("Season(s)" = case_when(
    season == "2021-22" ~ "2021",
    T ~ "2015-2018"
    )) %>% 
  ggplot(aes(x = attendance_per, fill = `Season(s)`)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~league, nrow = 2, scale = "free_y") + 
  scale_x_continuous(
    breaks = seq(0.3, 1.1, length =  9), 
    labels = percent_format(accuracy = 1)
    ) + 
  geom_hline(yintercept = 0) + 
  labs(
    title = "Distribution of Attendance per Capcity",
    subtitle = "2015-2018 Seasons VS. 2021 Season",
    x = "Attendance per Capcity",
    y = "Density",
    ) 

# Covid-19 Tables --------------------------------------------------------------

# Covid-19 Statistics
dat.final %>% 
  filter(season == "2021-22") %>% 
  group_by(league) %>% 
  summary_stats(lag_cbsa_w_n_cases:lag_cbsa_f_w_n_deaths, yes_median = T) 

# Single Team
dat.final %>% 
  filter(season == "2021-22") %>% 
  filter(home == "Boston Celtics") %>% 
  summary_stats(lag_cbsa_w_n_cases:lag_cbsa_f_w_n_deaths) 
  
# Covid-19 Plots ---------------------------------------------------------------

# National Level
plot.covid.national <- dat.covid %>% 
  filter(date > "2021-10-11" & date < "2022-04-30") %>% 
  group_by(date, cbsa) %>% 
  summarize(
    floor_monday = first(floor_monday),
    lag_cbsa_w_n_cases = first(lag_cbsa_w_n_cases),
    lag_cbsa_w_n_deaths = first(lag_cbsa_w_n_deaths),
    lag_cbsa_f_w_n_cases = first(lag_cbsa_f_w_n_cases),
    lag_cbsa_f_w_n_deaths = first(lag_cbsa_f_w_n_deaths)
  ) %>% 
  ungroup() %>% 
  distinct(floor_monday, cbsa, .keep_all = T) %>% 
  group_by(floor_monday) %>% 
  summarize(
    floor_monday = first(floor_monday),
    lag_cbsa_w_n_cases = sum(lag_cbsa_w_n_cases),
    lag_cbsa_w_n_deaths = sum(lag_cbsa_w_n_deaths),
    lag_cbsa_f_w_n_cases = sum(lag_cbsa_f_w_n_cases),
    lag_cbsa_f_w_n_deaths = sum(lag_cbsa_f_w_n_deaths)
  ) %>% 
  ungroup() %>% 
  pivot_longer(lag_cbsa_f_w_n_cases:lag_cbsa_f_w_n_deaths) %>% 
  ggplot(aes(x = floor_monday, y = value, fill = name)) +
  geom_col() +
  facet_wrap(
    ~name, 
    nrow = 2,
    scales = "free_y", 
    labeller = as_labeller(
      c(
        `lag_cbsa_f_w_n_cases` = "Lag Weekly New Cases", 
        `lag_cbsa_f_w_n_deaths` = "Lag Weekly New Deaths")
      )
    ) +
  geom_hline(yintercept = 0) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = comma) + 
  scale_fill_discrete(guide = "none") + 
  labs(
    title = "Lag Weekly New Covid-19 Cases/Deaths",
    subtitle = "10/11/2021 to 4/25/2022, Aggregated Over Select CBSAs",
    x = "Date (2021-22)",
    y = "Count"
  ) 

# Per CBSA
cbsa_plot <- function(.home) {
  if (.home %in% (dat.final %>% filter(league == "NBA") %>% pull(home))) {
    dat.lower <- "2021-10-18"
    dat.upper <- "2022-04-11"
  }
  else {
    dat.lower <- "2021-10-11"
    dat.upper <- "2022-04-30"
  }
  dat.covid %>%
    unnest(home) %>%
    filter(home == .home) %>%
    filter(date > dat.lower & date < dat.upper) %>%
    select(floor_monday, lag_cbsa_w_n_cases:lag_cbsa_f_w_n_deaths) %>%
    distinct(floor_monday, .keep_all = T) %>%
    left_join(
      dat.final %>%
        filter(home == .home) %>%
        select(floor_monday, home, league),
      by = c("floor_monday")
    ) %>%
    group_by(floor_monday) %>%
    summarize(
      num_games = sum(!is.na(league), na.rm = T),
      lag_cbsa_w_n_cases = first(lag_cbsa_w_n_cases),
      lag_cbsa_w_n_deaths = first(lag_cbsa_w_n_deaths),
      lag_cbsa_f_w_n_cases = first(lag_cbsa_f_w_n_cases),
      lag_cbsa_f_w_n_deaths = first(lag_cbsa_f_w_n_deaths)
    ) %>%
    ungroup() %>%
    mutate(num_games = as.character(num_games)) %>%
    pivot_longer(lag_cbsa_w_n_cases:lag_cbsa_f_w_n_deaths) %>%
    mutate(name = factor(name, levels = c(
      "lag_cbsa_f_w_n_cases", "lag_cbsa_w_n_cases", "lag_cbsa_f_w_n_deaths", "lag_cbsa_w_n_deaths"
      ))) %>% 
    ggplot(aes(x = floor_monday, y = value, fill = name)) +
    geom_col(width = 5) +
    facet_wrap(
      ~name,
      nrow = 2,
      scales = "free_y",
      labeller = as_labeller(
        c(
          `lag_cbsa_f_w_n_cases` = "Lag Weekly New Cases*",
          `lag_cbsa_w_n_cases` = "Lag Weekly New Cases",
          `lag_cbsa_f_w_n_deaths` = "Lag Weekly New Deaths*",
          `lag_cbsa_w_n_deaths` = "Lag Weekly New Deaths"
          
          )
      )
    ) +
    geom_hline(yintercept = 0) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(labels = comma) +
    scale_fill_discrete(guide = "none") + 
    labs(
      title = "New Covid-19 Cases/Deaths Aggregated Over Target CBSAs",
      subtitle = "Every Week From 10/11/2021 to 4/25/2022",
      x = "Date (2021-22)",
      y = "Count"
      )
}

cbsa_plot(.home = "Boston Celtics")

# Policy Graph -----------------------------------------------------------------

plot.policies <- tibble(
  date = rep(seq(as.Date("2021-10-12"), as.Date("2022-04-29"), by = "days"), 53),
  leage_home = dat.final %>%
    mutate(league_home = paste(league, home, sep = ",")) %>% 
    pull(league_home) %>%
    unique() %>% 
    map(~rep(.x, 200)) %>% 
    unlist()
  ) %>% 
  separate(leage_home, into = c("league", "home"), extra = "merge") %>% 
  mutate(policy = policy_func(home, date)) %>% 
  filter(!(league == "NBA" & date < "2021-10-20")) %>% 
  mutate(policy = case_when(
    league == "NBA" & date < "2021-10-20" ~ "none",
    T ~ policy
  )) %>% 
  dummy_cols("policy", remove_selected_columns = T) %>% 
  rename_with(~str_sub(.x, 8L, -1L), 4:7) %>% 
  group_by(league, date) %>% 
  summarize(
    "none" = sum(none)/n(),
    "mask" = sum(mask)/n(),
    "vaccine" = sum(vaccine)/n(),
    "both" = sum(both)/n(),
  ) %>% 
  ungroup() %>% 
  pivot_longer(none:both, names_to = "policy", values_to = "values") %>%
  mutate(policy = factor(policy, levels = c("none", "mask", "vaccine", "both"))) %>% 
  ggplot(aes(x = date, y = values, color = policy)) + 
  geom_line() + 
  facet_wrap(~league) + 
  geom_hline(yintercept = 0) + 
  coord_cartesian(ylim = c(0, 0.5)) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_color_discrete(
    name = "Policy Type",
    labels = c("No Policy", "Mask Mandate", "Vaccine Mandate", "Mask & Vaccine\n      Mandate")
  ) +
  guides(color = guide_legend(keyheight = c(1, 1, 1, 1.5))) + 
  labs(
    title = "Covid-19 Policy Types in the NBA and NHL",
    subtitle = "Over the 2021-22 Season",
    x = "Date (2021-22)",
    y = "Percent Teams\nWith Policy"
  ) 








# plot.covid <- dat.covid %>% 
#   filter(date > "2021-10-11" & date < "2022-04-30") %>% 
#   group_by(date, cbsa) %>% 
#   summarize(
#     floor_monday = first(floor_monday),
#     lag_cbsa_w_n_cases = first(lag_cbsa_w_n_cases),
#     lag_cbsa_w_n_deaths = first(lag_cbsa_w_n_deaths),
#     lag_cbsa_f_w_n_cases = first(lag_cbsa_f_w_n_cases),
#     lag_cbsa_f_w_n_deaths = first(lag_cbsa_f_w_n_deaths)
#   ) %>% 
#   ungroup() %>% 
#   group_by(date) %>% 
#   summarize(
#     floor_monday = first(floor_monday),
#     lag_cbsa_w_n_cases = sum(lag_cbsa_w_n_cases),
#     lag_cbsa_w_n_deaths = sum(lag_cbsa_w_n_deaths),
#     lag_cbsa_f_w_n_cases = sum(lag_cbsa_f_w_n_cases),
#     lag_cbsa_f_w_n_deaths = sum(lag_cbsa_f_w_n_deaths)
#   ) %>% 
#   ungroup() %>% 
#   left_join(
#     dat.final %>% 
#       select(date, league),
#     by = c("date")
#   ) %>% 
#   group_by(floor_monday) %>% 
#   summarize(
#     num_games_nba = sum(league == "NBA", na.rm = T),
#     num_games_nhl = sum(league == "NHL", na.rm = T),
#     lag_cbsa_w_n_cases = first(lag_cbsa_w_n_cases),
#     lag_cbsa_w_n_deaths = first(lag_cbsa_w_n_deaths),
#     lag_cbsa_f_w_n_cases = first(lag_cbsa_f_w_n_cases),
#     lag_cbsa_f_w_n_deaths = first(lag_cbsa_f_w_n_deaths)
#   ) %>% 
#   ungroup() %>% 
  
  







