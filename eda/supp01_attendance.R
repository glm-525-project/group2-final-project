data_copy = readClipboard()
season_data = grepl("\\d+", data_copy)

build_vector = function(x, bool_vec, data, str) {
  if (bool_vec[x]) {
    ifelse(grepl(str, data[x]) | data[x] == "", NA, data[x])
  } else(
    NA
  )
}

test_vec = seq(1, length(data_copy), 1)
test_poss = unlist(sapply(test_vec, function(x) build_vector(x, !season_data, data_copy, "Season")))

init_df = data.frame(
  team_data = as.character(data_copy), 
  team_names = as.character(test_poss)
) %>% fill(team_names)
filter_df = init_df[season_data, ]

split_vals_df = filter_df %>%  separate(
  team_data,
  c("season", "gp", "tot_attend", "avg_attend"),
  "\\s+"
) %>% separate(
  season,
  c("season", "drop_season"),
  "-"
) %>% mutate(
  season = as.numeric(season),
  gp = as.numeric(gp),
  tot_attend = as.numeric(sub(",", "", tot_attend)),
  avg_attend = as.numeric(gsub("[,.]", "", avg_attend))
) %>% select(
  team_names,
  season,
  avg_attend
) %>% filter(
  team_names != "[missing one game for Chicago" & team_names != "NBA LEAGUE TOTALS"
)

write.csv(split_vals_df, file.path(Sys.getenv("USERPROFILE"), "Downloads\\attend.csv"))

teams_df = read.csv(
  file.path(data_path, "basketball_teams.csv"),
  stringsAsFactors = FALSE
) %>% mutate(
  name = toupper(name)
) %>% select(
  name,
  year,
  tmID
) %>% unique()

updated_attend_df = read.csv(
  file.path(Sys.getenv("USERPROFILE"), "Downloads\\attend_update.csv"),
  stringsAsFactors = FALSE
)

join_team_id_df = updated_attend_df %>% left_join(
  teams_df,
  by = c("name", "year")
) %>% filter(
  year <= 2011
) %>% mutate(
  attend_per = ifelse(avg_attend/capacity > 1, 1, avg_attend/capacity)
) %>% arrange(
  year, desc(attend_per), desc(avg_attend)
) %>% group_by(
  year
) %>% mutate(
  rank_attend = row_number(),
) %>% mutate(
  per_rank_attend = percent_rank(-rank_attend)
) %>% select(
  tmID,
  year,
  per_rank_attend
)

write.csv(join_team_id_df, file.path(data_path, "basketball_attendance.csv"), row.names = FALSE)

check_df = players_df %>% left_join(
  join_team_id_df,
  by=c("tmID", "year")
)  %>% mutate(
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
) %>% summarize_at(
  vars(GP:nba_seasons),
  sum
) %>% filter(
  nba_seasons > 0 & max_year <= 2010 & GP >= 82*5
) 

