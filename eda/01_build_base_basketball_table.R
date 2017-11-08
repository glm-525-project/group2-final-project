### CODE OWNERS: Matthew Hawthorne

### OBJECTIVE: Build a basketball dataset

### DEVELOPER NOTES: None

library(dplyr)
library(foreign)
library(ggplot2)

project_repo = file.path(Sys.getenv("USERPROFILE"), "repos\\group2-final-project", fsep="\\")

data_path = file.path(project_repo, "data\\basketball_data", fsep = "\\")

### LIBRARIES, LOCATIONS, LITERALS, ETC. GO ABOVE HERE

players_df = read.csv(file.path(data_path, "basketball_players.csv"))

agg_player_df = players_df %>% 
  group_by(playerID) %>% 
  mutate(
    min_year = min(year),
    max_year = max(year)
  ) %>% ungroup() %>%
  group_by(playerID, min_year, max_year) %>%
  summarize_at(
    vars(GP:PostthreeMade),
    sum
  ) %>% 
  filter(
    min_year >= 1977 & max_year <= 2010 & GP >= 82*5
  )

hof_df = read.csv(file.path(data_path, "basketball_hof.csv"))
reduce_hof_df = hof_df %>% filter(hofID != "" & category == "Player")

draft_df = read.csv(file.path(data_path, "basketball_draft.csv"))
draft_pos_df = draft_df %>% mutate(
  draft_pos_flag = case_when(
    .$draftOverall == 0 ~ "Undrafted",
    .$draftOverall <= 30 ~ "Approx First Round",
    .$draftOverall > 30 ~ "Approx Second Round or Higher"
  )
)
