############################################################################## #
### Regressions
############################################################################## #  

ivreg(Y ~ X + W | W + Z, ... )

reg_generator <- function(var, .league = "NBA", .per = F, .policy = F, .vaccine = F, .cluster = F, out) {
  # set data
  dat <- dat.final %>% filter(league == .league) %>% 
    mutate(attendance_per = attendance_per*100) %>% 
    dummy_cols(., select_columns = "policy") %>% 
    select(-policy_NA) %>% 
    mutate(across(contains("policy"), ~as.logical(.x)))
  
  dat
  # vars
  var_list <- ifelse(.per, var ,paste(paste0(var, "_quart_2"), paste0(var, "_quart_3"), paste0(var, "_quart_4"), sep = " + "))
  # # policys
  # policy_list <- ifelse(.policy, "masks + vaccine + vaccine_masks", "masks_2 + vaccine_2 + vaccine_masks")
  # # Vaccine 
  # vaccine <- ifelse(.vaccine, "+ fully_vaxed_per", "")
  # cluster <- ifelse(.cluster, "home", "0")
  # # Coviariate Labels
  # if (.per & .vaccine) {
  #   cov_labels <- c("Log(Vaccine Percent + 1)*100", "per")
  # }
  # else if (!.per  & .vaccine) {
  #   cov_labels <- c("Log(Vaccine Percent + 1)*100", "quart 2", "quart 3", "quart 4")
  # }
  # else if (.per) {
  #   cov_labels <- c("per")
  # }
  # else {
  #   cov_labels <- c("quart 2", "quart 3", "quart 4")
  # }
  # # Regs
  # reg.1 <- felm(as.formula(paste("attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` + 
  #               home_rank + away_rank + implied_odds_scaled + covid_season", vaccine, "|0|0|", cluster)),
  #               data = dat)
  # 
  # reg.2 <- felm(as.formula(paste("attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +",
  #                                "home_rank + away_rank + implied_odds_scaled + covid_season", vaccine, "+",
  #                                var_list,  "|0|0|", cluster)),
  #               data = dat)
  # 
  # reg.3 <- felm(as.formula(paste("attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +",
  #                                "home_rank + away_rank + implied_odds_scaled + covid_season", vaccine, "+",
  #                                var_list, "+",
  #                                policy_list, "|0|0|", cluster)),
  #               data = dat)
  # 
  # reg.4 <- felm(as.formula(paste("attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +",
  #                                "home_rank + away_rank + implied_odds_scaled + covid_season", vaccine, "+",
  #                                var_list, "+",
  #                                policy_list, "+",
  #                                "Nov + Dec + Jan + Feb + Mar + Apr", "|0|0|", cluster)),
  #               data = dat)
  # 
  # reg.5 <- felm(as.formula(paste("attendance_per ~ party + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +",
  #                                "home_rank + away_rank + implied_odds_scaled + covid_season", vaccine, "+",
  #                                var_list, "+",
  #                                policy_list, "+",
  #                                "Nov + Dec + Jan + Feb + Mar + Apr + politic_democratic + politic_republican", "|0|0|", cluster)),
  #               data = dat)
  # #%%%%%%%%%%%%%%%%%%
  # stargazer(reg.1, reg.2, reg.3, reg.4, reg.5, 
  #           omit = c("home", "away", "weekday", "season_", "month", "implied", "party"), type = out,
  #           dep.var.labels=c("Attendance Per Capacity", "Attendance Per Capacity", "Attendance Per Capacity", "Attendance Per Capacity", "Attendance Per Capacity"),
  #           covariate.labels=c("Covid Season", cov_labels,
  #                              "Masks", "Vaccines", "Masks xxx Vaccines", "November xxx Covid Season", "December xxx Covid Season", "January xxx Covid Season", "February xxx Covid Season", 
  #                              "March xxx Covid Season", "April xxx Covid Season", "Democratic xxx Covid Season", "Republican xxx Covid Season"),
  #           add.lines=list(c('Home, away, weekday FE', 'Yes','Yes', "Yes", "Yes", "No home FE"),  
  #                          c('Season, month of season FE', 'Yes','Yes', "Yes", "Yes", "Yes"),
  #                          c("Political Party FE", "no", "no", "no", "no", "yes")),
  #           header=FALSE, single.row = TRUE, no.space = TRUE, column.sep.width = "-1pt", digits = 2, omit.stat = c("f", "ser", "rsq"))
  # 
}

reg_generator()

# 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reg.vax.nba <- felm(attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +
                      home_rank + away_rank + implied_odds_scaled + covid_season + 
                      WA_new_cases_L1_quart_2 + WA_new_cases_L1_quart_3 + WA_new_cases_L1_quart_4 +
                      masks + vaccine + vaccine_masks +  
                      Nov + Dec + Jan + Feb + Mar + Apr + fully_vaxed_per|0|0|home,
                    data = dat.final %>% filter(nba) %>% mutate(fully_vaxed_per = log(fully_vaxed_per + 1)*100) %>% mutate(attendance_per = attendance_per*100)
)
reg.vax.nhl <- felm(attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +
                      home_rank + away_rank + implied_odds_scaled + covid_season + 
                      WA_new_cases_L1_quart_2 + WA_new_cases_L1_quart_3 + WA_new_cases_L1_quart_4 +
                      masks + vaccine + vaccine_masks +  
                      Nov + Dec + Jan + Feb + Mar + Apr + fully_vaxed_per|0|0|home,
                    data = dat.final %>% filter(!nba) %>% mutate(fully_vaxed_per = log(fully_vaxed_per + 1)*100) %>% mutate(attendance_per = attendance_per*100)
)
reg.vax.nba.per <- felm(attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +
                          home_rank + away_rank + implied_odds_scaled + covid_season + 
                          WA_new_deaths_L1_quart_2 + WA_new_deaths_L1_quart_3 + WA_new_deaths_L1_quart_4 +
                          masks + vaccine + vaccine_masks +  
                          Nov + Dec + Jan + Feb + Mar + Apr + fully_vaxed_per|0|0|home,
                        data = dat.final %>% filter(nba) %>% mutate(fully_vaxed_per = log(fully_vaxed_per + 1)*100) %>% mutate(attendance_per = attendance_per*100) %>%  mutate(across(contains("hype"), ~ .x*100))
)
reg.vax.nhl.per <- felm(attendance_per ~ home + away + weekday + month + `season_2016-17` + `season_2017-18` + `season_2018-19` +
                          home_rank + away_rank + implied_odds_scaled + covid_season + 
                          WA_new_deaths_L1_quart_2 + WA_new_deaths_L1_quart_3 + WA_new_deaths_L1_quart_4 + 
                          masks + vaccine + vaccine_masks +  
                          Nov + Dec + Jan + Feb + Mar + Apr + fully_vaxed_per|0|0|home,
                        data = dat.final %>% filter(!nba) %>% mutate(fully_vaxed_per = log(fully_vaxed_per + 1)*100) %>% mutate(attendance_per = attendance_per*100) %>%  mutate(across(contains("hype"), ~ .x*100))
)
#%%%%%%%%%%%%%%%%%%
stargazer(reg.vax.nba, reg.vax.nhl, reg.vax.nba.per, reg.vax.nhl.per,
          omit = c("home", "away", "weekday", "season_", "month", "implied", "party"), type = "latex",
          dep.var.labels=c("Attendance Per Capacity", "Attendance Per Capacity", "Attendance Per Capacity", "Attendance Per Capacity", "Attendance Per Capacity"),
          covariate.labels=c("Covid Season", "Lag New Cases Quart 2", "Lag New Cases Quart 3", "Lag New Cases Quart 4",  "Lag New Deaths Quart 2", "Lag New Deaths Quart 3", "Lag New Deaths Quart 4",
                             "Masks", "Vaccines", "Masks xxx Vaccines", "November xxx Covid Season", "December xxx Covid Season", "January xxx Covid Season", "February xxx Covid Season", 
                             "March xxx Covid Season", "April xxx Covid Season", "Log Vaccine per Population"),
          add.lines=list(c('Home, away, weekday FE', 'Yes','Yes', "Yes", "Yes", "No home FE"),  
                         c('Season, month of season FE', 'Yes','Yes', "Yes", "Yes", "Yes"),
                         c("Political Party FE", "no", "no", "no", "no", "yes")),
          header=FALSE, single.row = TRUE, no.space = TRUE, column.sep.width = "-1pt", digits = 2, omit.stat = c("f", "ser", "rsq"))
