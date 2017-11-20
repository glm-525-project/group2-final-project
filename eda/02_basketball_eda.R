### CODE OWNERS: Matthew Hawthorne

### OBJECTIVE: Build a basketball dataset

### DEVELOPER NOTES: None

library(dplyr)
library(tidyr)
library(foreign)
library(ggplot2)

project_repo = file.path(Sys.getenv("USERPROFILE"), "repos\\group2-final-project", fsep="\\")

data_path = file.path(project_repo, "data\\basketball_data", fsep = "\\")

describe_data <- function(x){
  obs = dim(x)[1]
  ones_vector = matrix(1, obs)
  sample_mean = t(ones_vector/obs)%*%x
  ind = diag(1, obs)
  J = ones_vector %*% t(ones_vector)
  sample_cov = (1/(obs-1))*(t(x)%*%(ind - (J/obs))%*%x)
  init_v = diag(sample_cov)^(-1/2)
  V = diag(init_v)
  sample_rho = V%*%sample_cov%*%V
  output = list(sample_mean = t(sample_mean), sample_cov = sample_cov, sample_rho = sample_rho)
  return(output)
}

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

df = read.csv(file.path(data_path, "basketball_glm_data.csv"))
attach(df)
# Categorical Data

# Championship Flag
champ_table = table(champion)
champ_prop = prop.table(champ_table)

champ_table
champ_prop

champ.fit = basketball_univariate_glm("champion", df)
summary(champ.fit)

#Draft Position Flag
draft_pos = table(draft_pos_flag)
draft_pos_prop = prop.table(draft_pos)

draft_pos
draft_pos_prop

draft_pos.fit = basketball_univariate_glm("draft_pos_flag", df)
summary(draft_pos.fit)

# Continuous Vars

reduce_set = c("champion", "draft_pos_flag", "hof_flag")
cont_df = df %>% select(-one_of(reduce_set))

cor_matrix = as.data.frame(
  describe_data(as.matrix(cont_df))$sample_rho
)
colnames(cor_matrix) <- colnames(cont_df)
rownames(cor_matrix) <- colnames(cont_df)

cont_vars = colnames(cont_df)

# Check Vars Plots

cont_var_list = sapply(cont_vars, function(x) check_continuous_vars(df, x, 10), simplify = FALSE, USE.NAMES = T)
cont_var_glm = sapply(cont_vars, function(x) summary(basketball_univariate_glm(x, df)), simplify = FALSE, USE.NAMES = T)



