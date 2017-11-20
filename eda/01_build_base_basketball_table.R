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

agg_data = function(x, y) {
  return(ifelse(is.infinite(x/y), 0, x/y))
}

players_df = read.csv(file.path(data_path, "basketball_players.csv")) %>% select(
  -note
) %>% filter(
  !(playerID == "lucasjo01" & year == 2010)
)

playoffs_df = read.csv(
  file.path(data_path, "basketball_series_post.csv"), 
  stringsAsFactors = F
) %>% filter(
  round == "F"
) %>% select(
  year,
  tmIDWinner
) %>% mutate(
  champion = 1
)

attendance_df = read.csv(
  file.path(data_path, "basketball_attendance.csv"),
  stringsAsFactors = F
)

playoff_players_df = players_df %>% left_join(
  playoffs_df,
  by=c("tmID" = "tmIDWinner", "year" = "year")
) %>% mutate(
  champion = ifelse(PostGP > 0, coalesce(champion, 0), 0)
)

all_star_votes_df = read.csv(
  file.path(data_path, "basketball_allstar_votes.csv"),
  stringsAsFactors = FALSE
) %>% rename(playerID = playerid) %>% select(-name)

agg_player_df = playoff_players_df %>% left_join(
  attendance_df,
  by=c("tmID", "year")
) %>% left_join(
  all_star_votes_df,
  by=c("playerID", "year")
) %>% mutate(
  playerID = as.character(playerID),
  nba_seasons = ifelse(year >= 1976, 1, 0)
) %>% group_by(
  playerID
)  %>% mutate(
  min_year = min(year),
  max_year = max(year),
  per_rank_attend = mean(per_rank_attend, na.rm = T)
) %>% ungroup() %>% group_by(
  playerID,
  min_year,
  max_year,
  per_rank_attend
) %>% mutate(
  votes = ifelse(is.na(votes), 0, votes)
) %>% summarize_at(
  vars(GP:nba_seasons),
  sum
) %>% filter(
  nba_seasons > 0 & max_year <= 2010 & GP >= 82*5
) %>% ungroup()

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
) %>% group_by(
  playerID
) %>% mutate(
  draft_rank = rank(draft_pos_flag, ties.method = "first")
) %>% filter(
  draft_rank == 1
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

adv_stats_df = read.csv(
  file.path(data_path, "basketball_adv_stats.csv"),
  stringsAsFactors = FALSE
)

agg_adv_stats_df = adv_stats_df %>% group_by(
  playerID
) %>% summarise_at(
  vars(off_win_share:player_eff_rating),
  funs(mean(., na.rm = TRUE))
)

all_star_df = read.csv(
  file.path(data_path, "basketball_player_allstar.csv"), 
  stringsAsFactors = FALSE
)

agg_all_star_df = all_star_df %>% rename(
  playerID = player_id
) %>% group_by(
  playerID
) %>% summarise(
  all_star_app = n()
) %>% select(
  playerID,
  all_star_app
)


rebuild_players_df = agg_player_df %>% mutate(
  seasons_played = max_year - min_year,
  pts_per_36m = agg_data(points, minutes)*36,
  fg_percentage = agg_data(fgMade, fgAttempted),
  ft_percentage = agg_data(ftMade, ftAttempted),
  asts_per_36m = agg_data(assists, minutes)*36,
  stls_per_36m = agg_data(steals, minutes)*36,
  blks_per_36m = agg_data(blocks, minutes)*36,
  rbs_per_36m = agg_data(rebounds, minutes)*36
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
) %>% left_join(
  agg_adv_stats_df,
  by = "playerID"
) %>% left_join(
  agg_all_star_df,
  by = "playerID"
) %>% mutate(
  hof_flag = ifelse(is.na(hof_flag), 0, 1),
  draft_pos_flag = ifelse(is.na(draft_pos_flag), "Undrafted", draft_pos_flag),
  all_nba_team = ifelse(is.na(all_nba_team), 0, all_nba_team),
  mvp = ifelse(is.na(mvp), 0, mvp),
  finals_mvp = ifelse(is.na(finals_mvp), 0, finals_mvp),
  other_awards = ifelse(is.na(other_awards), 0, other_awards),
  all_star_games = ifelse(is.na(all_star_app), 0, all_star_app),
  all_star_votes = votes,
  champion = ifelse(champion > 0, "Champion", "Not Champion")
)


basketball_df = basketball_joins_df %>% mutate(
  total_awards = all_nba_team + mvp + finals_mvp + other_awards
) %>% select(
  hof_flag,
  pts_per_36m,
  fg_percentage,
  ft_percentage,
  asts_per_36m,
  stls_per_36m,
  blks_per_36m,
  rbs_per_36m,
  off_win_share,
  def_win_share,
  win_share,
  val_over_rep_player,
  plus_minus,
  player_eff_rating,
  per_rank_attend,
  all_star_games,
  all_star_votes,
  draft_pos_flag,
  total_awards,
  champion
)

write.csv(basketball_df, file.path(data_path, "basketball_glm_data.csv"), row.names = F)