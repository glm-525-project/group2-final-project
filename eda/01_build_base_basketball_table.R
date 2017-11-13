### CODE OWNERS: Matthew Hawthorne

### OBJECTIVE: Build a basketball dataset

### DEVELOPER NOTES: None

library(dplyr)
library(tidyr)
library(foreign)
library(ggplot2)

project_repo = file.path(Sys.getenv("USERPROFILE"), "repos\\group2-final-project", fsep="\\")

data_path = file.path(project_repo, "data\\basketball_data", fsep = "\\")

### LIBRARIES, LOCATIONS, LITERALS, ETC. GO ABOVE HERE

players_df = read.csv(file.path(data_path, "basketball_players.csv"))

agg_player_df = players_df %>% select(
  -note
) %>% mutate(
  playerID = as.character(playerID),
  nba_seasons = ifelse(year >= 1976, 1, 0)
) %>% group_by(
  playerID
) %>% mutate(
  min_year = min(year),
  max_year = max(year)
) %>% ungroup() %>% group_by(
  playerID,
  min_year,
  max_year
) %>% summarize_at(
  vars(GP:nba_seasons),
  sum
) %>% filter(
  nba_seasons > 0 & max_year <= 2010 & GP >= 82*5
) 

hof_df = read.csv(file.path(data_path, "basketball_hof.csv"))
reduce_hof_df = hof_df %>% filter(hofID != "" & category == "Player") %>% select(
  hofID
) %>% rename(
  playerID = hofID
) %>% mutate(
  playerID = as.character(playerID),
  hof_flag = 1
)

draft_df = read.csv(file.path(data_path, "basketball_draft.csv"))
draft_pos_df = draft_df %>% mutate(
  draft_pos_flag = case_when(
    .$draftOverall == 0 ~ "Undrafted",
    .$draftOverall > 0 & .$draftOverall <= 30 ~ "Approx First Round",
    .$draftOverall > 30 & .$draftOverall <= 60 ~ "Approx Second Round or Higher",
    .$draftOverall >= 61 ~ "Undrafted"
  )
) %>% filter(
  playerID != "" & lgID == "NBA"
) %>% select(
  playerID,
  draft_pos_flag
) %>% mutate(
  playerID = as.character(playerID)
)

basketball_awards_df = read.csv(file.path(data_path, "basketball_awards_players.csv"))
awards_df = basketball_awards_df %>% mutate(
  playerID = as.character(playerID),
  award = case_when(
    grepl("All-NBA", as.character(.$award)) ~ "all_nba_team",
    grepl("Most Valuable Player", as.character(.$award)) ~ "mvp",
    grepl("Finals MVP", as.character(.$award)) ~ "finals_mvp",
    TRUE ~ "other_awards"
  ),
  cnt_awards = 1
) %>% group_by(
  playerID,
  award
) %>% summarize_at(
  vars(cnt_awards),
  sum
) %>% spread(
  award,
  cnt_awards
) %>% mutate_if(
  is.numeric,
  coalesce,
  ... = 0
)


rebuild_players_df = agg_player_df %>% mutate(
  seasons_played = max_year - min_year,
  ppg = ifelse(GP == 0, 0, points/GP),
  fg_percentage = ifelse(fgAttempted == 0, 0, fgMade/fgAttempted),
  ft_percentage = ifelse(ftAttempted == 0, 0, ftMade/ftAttempted),
  three_percentage = ifelse(threeAttempted == 0, 0, threeMade/threeAttempted),
  assists_to_turnovers = ifelse(turnovers == 0, 0, assists/turnovers),
  post_ppg = ifelse(PostGP == 0, 0, PostPoints/PostGP),
  post_fg_percentage = ifelse(PostfgAttempted == 0, 0, PostfgMade/PostfgAttempted),
  post_ft_percentage = ifelse(PostftAttempted == 0, 0, PostftMade/PostftAttempted),
  post_three_percentage = ifelse(PostthreeAttempted == 0, 0, PostthreeMade/PostthreeAttempted),
  post_assists_to_turnovers = ifelse(PostTurnovers == 0, 0, PostAssists/PostTurnovers)
)

basketball_joins_df = rebuild_players_df %>% left_join(
  reduce_hof_df,
  by = "playerID"
) %>% left_join(
  draft_pos_df,
  by = "playerID"
) %>% left_join(
  awards_df,
  by = "playerID"
) %>% mutate(
  hof_flag = ifelse(is.na(hof_flag), 0, 1),
  draft_pos_flag = ifelse(is.na(draft_pos_flag), "Undrafted", draft_pos_flag),
  all_nba_team = ifelse(is.na(all_nba_team), 0, all_nba_team),
  mvp = ifelse(is.na(mvp), 0, mvp),
  finals_mvp = ifelse(is.na(finals_mvp), 0, finals_mvp),
  other_awards = ifelse(is.na(other_awards), 0, other_awards)
)

final_basketball_df = basketball_joins_df %>% select(
  hof_flag,
  playerID,
  seasons_played,
  GP,
  ppg,
  fg_percentage,
  fgAttempted,
  ft_percentage,
  ftAttempted,
  three_percentage,
  threeAttempted,
  assists_to_turnovers,
  assists,
  turnovers,
  steals,
  blocks,
  PF,
  oRebounds,
  dRebounds,
  PostGP,
  post_ppg,
  post_fg_percentage,
  PostfgAttempted,
  post_ft_percentage,
  PostftAttempted,
  post_three_percentage,
  PostthreeAttempted,
  post_assists_to_turnovers,
  PostAssists,
  PostTurnovers,
  PostSteals,
  PostBlocks,
  PostPF,
  PostoRebounds,
  PostdRebounds,
  draft_pos_flag,
  all_nba_team,
  mvp,
  finals_mvp,
  other_awards
)
