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

basketball_univariate_glm = function(x, data) {
  glm_formula = as.formula(paste("hof_flag ~", x))
  return(glm(glm_formula, data = data, family = binomial(link = "logit")))
}

mean_cut_groups = function(cuts) {
  cut_list = strsplit(gsub("[^.,0-9]", "", as.character(unlist(cuts))), ",")
  convert_cut_type = lapply(cut_list, as.numeric)
  mean_cut_list = lapply(convert_cut_type, mean)
  return(unlist(mean_cut_list))
}

check_continuous_vars = function(df, var_x, number_cuts) {
  skinny_df = df[,c("hof_flag", var_x)]
  skinny_df[,var_x] = cut(as.matrix(skinny_df[,var_x]), number_cuts)
  group_df = skinny_df %>% group_by_(var_x) %>% summarize(hof_flag = mean(hof_flag))
  group_df[, var_x] = mean_cut_groups(group_df[, var_x])
  return(ggplot(group_df, aes_string(x = var_x, y = "hof_flag")) + geom_point())
}

