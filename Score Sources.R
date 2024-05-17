# required packages
library(tidyverse)
library(gt)

# change location as required
source("AFL Location Functions.R", local = T)

# change the round as required
current_round <- get_match_chains(season = 2024, round = 9)

score_sources <- current_round %>%
  #converts the indigenous team names back to regular team names
  mutate(homeTeam.teamName = case_when(homeTeam.teamName == "Kuwarna"  ~ "Adelaide Crows",
                                       homeTeam.teamName == "Walyalup"  ~ "Fremantle",
                                       homeTeam.teamName == "Yartapuulti"  ~ "Port Adelaide",
                                       homeTeam.teamName == "Waalitj Marawar"  ~ "West Coast Eagles",
                                       homeTeam.teamName == "Narrm"  ~ "Melbourne",
                                       homeTeam.teamName == "Euro-Yroke"  ~ "St Kilda",
                                       TRUE ~ homeTeam.teamName),
         awayTeam.teamName = case_when(awayTeam.teamName == "Kuwarna"  ~ "Adelaide Crows",
                                       awayTeam.teamName == "Walyalup"  ~ "Fremantle",
                                       awayTeam.teamName == "Yartapuulti"  ~ "Port Adelaide",
                                       awayTeam.teamName == "Waalitj Marawar"  ~ "West Coast Eagles",
                                       awayTeam.teamName == "Narrm"  ~ "Melbourne",
                                       awayTeam.teamName == "Euro-Yroke"  ~ "St Kilda",
                                       TRUE ~ awayTeam.teamName),
         team.teamName = case_when(team.teamName == "Kuwarna"  ~ "Adelaide Crows",
                                   team.teamName == "Walyalup"  ~ "Fremantle",
                                   team.teamName == "Yartapuulti"  ~ "Port Adelaide",
                                   team.teamName == "Waalitj Marawar"  ~ "West Coast Eagles",
                                   team.teamName == "Narrm"  ~ "Melbourne",
                                   team.teamName == "Euro-Yroke"  ~ "St Kilda",
                                   TRUE ~ team.teamName)) %>%
  group_by(season, roundNumber, homeTeam.teamName, awayTeam.teamName, chain_number, chainTeamName) %>%
  # filter for chains that include scores
  filter(finalState %in% c("goal", "behind", "rushed", "rushedOpp")) %>%
  slice(1) %>%
  # create variables for the different score sources
  mutate(homeGoalsFromTurnover = if_else(initialState == "possGain" & finalState == "goal" & homeTeam.teamName == chainTeamName, 1, 0),
         homeBehindsFromTurnover = case_when(initialState == "possGain" & finalState %in% c("behind", "rushed") & homeTeam.teamName == chainTeamName ~ 1,
                                             initialState == "possGain" & finalState == "rushedOpp" & awayTeam.teamName == chainTeamName ~ 1,
                                             TRUE ~ 0),
         awayGoalsFromTurnover = if_else(initialState == "possGain" & finalState == "goal" & awayTeam.teamName == chainTeamName, 1, 0),
         awayBehindsFromTurnover = case_when(initialState == "possGain" & finalState %in% c("behind", "rushed") & awayTeam.teamName == chainTeamName ~ 1,
                                             initialState == "possGain" & finalState == "rushedOpp" & homeTeam.teamName == chainTeamName ~ 1,
                                             TRUE ~ 0),
         homeGoalsFromStoppage = if_else(initialState %in% c("centreBounce", "throwIn", "ballUp") & finalState == "goal" & homeTeam.teamName == chainTeamName, 1, 0),
         homeBehindsFromStoppage = case_when(initialState %in% c("centreBounce", "throwIn", "ballUp") & finalState %in% c("behind", "rushed") & homeTeam.teamName == chainTeamName ~ 1,
                                             initialState %in% c("centreBounce", "throwIn", "ballUp") & finalState == "rushedOpp" & awayTeam.teamName == chainTeamName ~ 1,
                                             TRUE ~ 0),
         awayGoalsFromStoppage = if_else(initialState %in% c("centreBounce", "throwIn", "ballUp") & finalState == "goal" & awayTeam.teamName == chainTeamName, 1, 0),
         awayBehindsFromStoppage = case_when(initialState %in% c("centreBounce", "throwIn", "ballUp") & finalState %in% c("behind", "rushed") & awayTeam.teamName == chainTeamName ~ 1,
                                             initialState %in% c("centreBounce", "throwIn", "ballUp") & finalState == "rushedOpp" & homeTeam.teamName == chainTeamName ~ 1,
                                             TRUE ~ 0),
         homeGoalsDefHalf = if_else(x < 0 & finalState == "goal" & homeTeam.teamName == chainTeamName, 1, 0),
         homeBehindsDefHalf = case_when(x < 0 & finalState %in% c("behind", "rushed") & homeTeam.teamName == chainTeamName ~ 1,
                                        x < 0 & finalState == "rushedOpp" & awayTeam.teamName == chainTeamName ~ 1,
                                        TRUE ~ 0),
         awayGoalsDefHalf = if_else(x < 0 & finalState == "goal" & awayTeam.teamName == chainTeamName, 1, 0),
         awayBehindsDefHalf = case_when(x < 0 & finalState %in% c("behind", "rushed") & awayTeam.teamName == chainTeamName ~ 1,
                                        x < 0 & finalState == "rushedOpp" & homeTeam.teamName == chainTeamName ~ 1,
                                        TRUE ~ 0),
         homeGoalsForHalf = if_else(x >= 0 & finalState == "goal" & homeTeam.teamName == chainTeamName, 1, 0),
         homeBehindsForHalf = case_when(x >= 0 & finalState %in% c("behind", "rushed") & homeTeam.teamName == chainTeamName ~ 1,
                                        x >= 0 & finalState == "rushedOpp" & awayTeam.teamName == chainTeamName ~ 1,
                                        TRUE ~ 0),
         awayGoalsForHalf = if_else(x >= 0 & finalState == "goal" & awayTeam.teamName == chainTeamName, 1, 0),
         awayBehindsForHalf = case_when(x >= 0 & finalState %in% c("behind", "rushed") & awayTeam.teamName == chainTeamName ~ 1,
                                        x >= 0 & finalState == "rushedOpp" & homeTeam.teamName == chainTeamName ~ 1,
                                        TRUE ~ 0)) %>%
  group_by(season, roundNumber, homeTeam.teamName, awayTeam.teamName) %>%
  select(season, roundNumber, homeTeam.teamName, awayTeam.teamName, homeGoalsFromTurnover, homeBehindsFromTurnover, awayGoalsFromTurnover, awayBehindsFromTurnover, 
         homeGoalsFromStoppage, homeBehindsFromStoppage, awayGoalsFromStoppage, awayBehindsFromStoppage, 
         homeGoalsDefHalf, homeBehindsDefHalf, awayGoalsDefHalf, awayBehindsDefHalf,
         homeGoalsForHalf, homeBehindsForHalf, awayGoalsForHalf, awayBehindsForHalf) %>%
  # sum the score sources
  summarise(homeGoalsFromTurnover = sum(homeGoalsFromTurnover), 
            homeBehindsFromTurnover = sum(homeBehindsFromTurnover), 
            homeScoreFromTurnover = homeGoalsFromTurnover * 6 + homeBehindsFromTurnover,
            awayGoalsFromTurnover = sum(awayGoalsFromTurnover), 
            awayBehindsFromTurnover = sum(awayBehindsFromTurnover), 
            awayScoreFromTurnover = awayGoalsFromTurnover * 6 + awayBehindsFromTurnover,
            homeGoalsFromStoppage = sum(homeGoalsFromStoppage), 
            homeBehindsFromStoppage = sum(homeBehindsFromStoppage),
            homeScoreFromStoppage = homeGoalsFromStoppage * 6 + homeBehindsFromStoppage,
            awayGoalsFromStoppage = sum(awayGoalsFromStoppage), 
            awayBehindsFromStoppage = sum(awayBehindsFromStoppage), 
            awayScoreFromStoppage = awayGoalsFromStoppage * 6 + awayBehindsFromStoppage,
            homeGoalsDefHalf = sum(homeGoalsDefHalf), 
            homeBehindsDefHalf = sum(homeBehindsDefHalf), 
            homeScoreDefHalf = homeGoalsDefHalf * 6 + homeBehindsDefHalf,
            awayGoalsDefHalf = sum(awayGoalsDefHalf), 
            awayBehindsDefHalf = sum(awayBehindsDefHalf),
            awayScoreDefHalf = awayGoalsDefHalf * 6 + awayBehindsDefHalf,
            homeGoalsForHalf = sum(homeGoalsForHalf), 
            homeBehindsForHalf = sum(homeBehindsForHalf), 
            homeScoreForHalf = homeGoalsForHalf * 6 + homeBehindsForHalf,
            awayGoalsForHalf = sum(awayGoalsForHalf), 
            awayBehindsForHalf = sum(awayBehindsForHalf),
            awayScoreForHalf = awayGoalsForHalf * 6 + awayBehindsForHalf)

# create the score sources table
score_sources_tab <- tibble(Round = score_sources$roundNumber,
                            Game = paste(score_sources$homeTeam.teamName, "-", score_sources$awayTeam.teamName),
                            Turnover = paste(paste0(score_sources$homeGoalsFromTurnover, ".", score_sources$homeBehindsFromTurnover, ".", score_sources$homeScoreFromTurnover), "-",
                                             paste0(score_sources$awayGoalsFromTurnover, ".", score_sources$awayBehindsFromTurnover, ".", score_sources$awayScoreFromTurnover)),
                            Stoppage = paste(paste0(score_sources$homeGoalsFromStoppage, ".", score_sources$homeBehindsFromStoppage, ".", score_sources$homeScoreFromStoppage), "-",
                                             paste0(score_sources$awayGoalsFromStoppage, ".", score_sources$awayBehindsFromStoppage, ".", score_sources$awayScoreFromStoppage)),
                            `Defensive 1/2` = paste(paste0(score_sources$homeGoalsDefHalf, ".", score_sources$homeBehindsDefHalf, ".", score_sources$homeScoreDefHalf), "-",
                                              paste0(score_sources$awayGoalsDefHalf, ".", score_sources$awayBehindsDefHalf, ".", score_sources$awayScoreDefHalf)),
                            `Forward 1/2` = paste(paste0(score_sources$homeGoalsForHalf, ".", score_sources$homeBehindsForHalf, ".", score_sources$homeScoreForHalf), "-",
                                                    paste0(score_sources$awayGoalsForHalf, ".", score_sources$awayBehindsForHalf, ".", score_sources$awayScoreForHalf))) %>%
  gt() %>%
  tab_spanner(label = "Score Sources", columns = c(Turnover:`Forward 1/2`)) %>%
  tab_style(cell_borders(sides = "right", color = "#d9d9d9", weight = px(2)), locations = cells_body()) %>%
  cols_align(align = "center") %>%
  opt_row_striping()

# display the table
score_sources_tab
