#### Brexit Analysis at UKHLS level ####
#### Charlotte Haussmann & Tobias Ruettenauer ####
#### 2022/ 03 / 12 ####

rm(list = ls())

library(haven)
library(psych)
library(car)
library(DescTools)
library(margins)
library(stargazer)
library(texreg)

library(lme4)
library(lfe)
library(sandwich)
library(lmtest)
library(stargazer)

library(spdep)
library(sf)
library(stars)

library(ggplot2)
library(gridExtra)
library(grid)
library(ggeffects)
library(ggstatsplot)
library(gginnards)
library(viridisLite)

library(extrafont)
loadfonts()

library(effects)
library(interplot)
library(margins)



### Working Directory
setwd("C:/work/Forschung/Brexit-Deprivation/09_Replication/02_Data")

### Functions

# Mode
getmode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Repeat last function
repeat_last = function(x,
                       forward = TRUE,
                       maxgap = Inf,
                       na.rm = FALSE) {
  if (!forward)
    x = rev(x)           # reverse x twice if carrying backward
  ind = which(!is.na(x))             # get positions of nonmissing values
  if (is.na(x[1]) && !na.rm)
    # if it begins with NA
    ind = c(1, ind)                 # add first pos
  rep_times = diff(# diffing the indices + length yields how often
    c(ind, length(x) + 1))          # they need to be repeated
  if (maxgap < Inf) {
    exceed = rep_times - 1 > maxgap  # exceeding maxgap
    if (any(exceed)) {
      # any exceed?
      ind = sort(c(ind[exceed] + 1, ind))      # add NA in gaps
      rep_times = diff(c(ind, length(x) + 1)) # diff again
    }
  }
  x = rep(x[ind], times = rep_times) # repeat the values at these indices
  if (!forward)
    x = rev(x)           # second reversion
  x
}





#################
### Load data ###
#################

load("UKHLS_Brexit.RData")





#########################
### Data manipulation ###
#########################


data$london <- ifelse(data$gor_dv == 7, 1, 0)

data$migback_gen <- as.factor(data$migback_gen)


### Year - month combination
data$year_month <- paste(data$istrtdaty, data$istrtdatm, sep =  "_")
data$year_month <- as.factor(data$year_month)
data$year_month <- relevel(data$year_month, ref = "2016_6")


### Subset selection

# Born in the UK or with British citizenship
oo <- which(data$citzn1 == 1 | data$bornuk_dv == 1)
data_sub.df <- data[oo,]



###################
### Standardize ###
###################

vars <- c(
  "brexit_leave",
  "deprivation_comb",
  "w5_diff_income_score_r",
  "w5_diff_ind_income_score_r",
  "w5_moransi_income_score_r",
  "w10_diff_income_score_r",
  "w10_diff_ind_income_score_r",
  "w10_moransi_income_score_r",
  # "w20_diff_income_score_r",
  # "w20_diff_ind_income_score_r",
  # "w20_moransi_income_score_r",
  "index_dep_rank_r",
  "index_dep_score_r",
  "income_rank_r",
  "income_score_r",
  "population_dens",
  "median_age",
  "age_18_29",
  "age_65above",
  "educ_no",
  "educ_high",
  "foreign_perc",
  "ethmin_perc",
  "ethmin_perc",
  "manufacturingg_perc",
  "price_change_per",
  "w_index_dep_rank_r",
  "w_index_dep_score_r",
  "w_income_rank_r",
  "w_income_score_r",
  "w_population_dens",
  "w_median_age",
  "w_age_18_29",
  "w_age_65above",
  "w_educ_no",
  "w_educ_high",
  "w_foreign_perc",
  "w_ethmin_perc",
  "w_manufacturingg_perc",
  "w_price_change_per",
  "w2_index_dep_rank_r",
  "w2_index_dep_score_r",
  "w2_income_rank_r",
  "w2_income_score_r",
  "w2_population_dens",
  "w2_median_age",
  "w2_age_18_29",
  "w2_age_65above",
  "w2_educ_no",
  "w2_educ_high",
  "w2_foreign_perc",
  "w2_ethmin_perc",
  "w2_manufacturingg_perc",
  "w2_price_change_per",
  "w5_index_dep_rank_r",
  "w5_index_dep_score_r",
  "w5_income_rank_r",
  "w5_income_score_r",
  "w5_population_dens",
  "w5_median_age",
  "w5_age_18_29",
  "w5_age_65above",
  "w5_educ_no",
  "w5_educ_high",
  "w5_foreign_perc",
  "w5_ethmin_perc",
  "w5_manufacturingg_perc",
  "w5_price_change_per",
  "w10_index_dep_rank_r",
  "w10_index_dep_score_r",
  "w10_income_rank_r",
  "w10_income_score_r",
  "w10_population_dens",
  "w10_median_age",
  "w10_age_18_29",
  "w10_age_65above",
  "w10_educ_no",
  "w10_educ_high",
  "w10_foreign_perc",
  "w10_ethmin_perc",
  "w10_manufacturingg_perc",
  "w10_price_change_per",
  "w15_index_dep_rank_r",
  "w15_index_dep_score_r",
  "w15_income_rank_r",
  "w15_income_score_r",
  "w15_population_dens",
  "w15_median_age",
  "w15_age_18_29",
  "w15_age_65above",
  "w15_educ_no",
  "w15_educ_high",
  "w15_foreign_perc",
  "w15_ethmin_perc",
  "w15_manufacturingg_perc",
  "w15_price_change_per",
  "w20_index_dep_rank_r",
  "w20_index_dep_score_r",
  "w20_income_rank_r",
  "w20_income_score_r",
  "w20_population_dens",
  "w20_median_age",
  "w20_age_18_29",
  "w20_age_65above",
  "w20_educ_no",
  "w20_educ_high",
  "w20_foreign_perc",
  "w20_ethmin_perc",
  "w20_manufacturingg_perc",
  "w20_price_change_per",
  "w35_index_dep_rank_r",
  "w35_index_dep_score_r",
  "w35_income_rank_r",
  "w35_income_score_r",
  "w35_population_dens",
  "w35_median_age",
  "w35_age_18_29",
  "w35_age_65above",
  "w35_educ_no",
  "w35_educ_high",
  "w35_foreign_perc",
  "w35_ethmin_perc",
  "w35_manufacturingg_perc",
  "w35_price_change_per",
  "w50_index_dep_rank_r",
  "w50_index_dep_score_r",
  "w50_income_rank_r",
  "w50_income_score_r",
  "w50_population_dens",
  "w50_median_age",
  "w50_age_18_29",
  "w50_age_65above",
  "w50_educ_no",
  "w50_educ_high",
  "w50_foreign_perc",
  "w50_ethmin_perc",
  "w50_manufacturingg_perc",
  "w50_price_change_per",
  "w75_index_dep_rank_r",
  "w75_index_dep_score_r",
  "w75_income_rank_r",
  "w75_income_score_r",
  "w75_population_dens",
  "w75_median_age",
  "w75_age_18_29",
  "w75_age_65above",
  "w75_educ_no",
  "w75_educ_high",
  "w75_foreign_perc",
  "w75_ethmin_perc",
  "w75_manufacturingg_perc",
  "w75_price_change_per"
)

for (v in vars) {
  nv <- paste0("std_", v)
  data_sub.df[, nv] <- as.vector(scale(data_sub.df[, v]))
}






#################################
### Analysis final - stepwise ###
#################################

lm4_6 <-
  felm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_index_dep_score_r +
      std_w20_index_dep_score_r +
      std_index_dep_score_r * std_w20_index_dep_score_r +
      std_population_dens + # controls
      std_age_18_29 +
      std_age_65above +
      std_foreign_perc +
      #std_ethmin_perc +
      std_price_change_per +
      std_w20_population_dens + # w_controls
      std_w20_age_18_29 +
      std_w20_age_65above +
      std_w20_foreign_perc +
      #std_w20_ethmin_perc +
      std_w20_price_change_per
    | 0 | 0 | lsoa11,
    data = data_sub.df
  )


oo <- which(!rownames(data_sub.df) %in% names(lm4_6$na.action))

lm4_1 <-
  felm(
    brexit_leave ~ std_deprivation_comb + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv
    | 0 | 0 | lsoa11,
    data = data_sub.df[oo, ]
  )

lm4_2 <-
  felm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec
    | 0 | 0 | lsoa11,
    data = data_sub.df[oo, ]
  )

lm4_3 <-
  felm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_index_dep_score_r  +
      std_w20_index_dep_score_r +
      std_population_dens + # controls
      std_age_18_29 +
      std_age_65above +
      std_foreign_perc +
      #std_ethmin_perc +
      std_price_change_per +
      std_w20_population_dens + # w_controls
      std_w20_age_18_29 +
      std_w20_age_65above +
      std_w20_foreign_perc +
      #std_w20_ethmin_perc +
      std_w20_price_change_per
    | 0 | 0 | lsoa11,
    data = data_sub.df[oo, ]
  )


lm4_4 <-
  felm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_deprivation_comb * std_index_dep_score_r +
      std_deprivation_comb * std_w20_index_dep_score_r +
      std_deprivation_comb * std_population_dens + # controls
      std_deprivation_comb * std_age_18_29 +
      std_deprivation_comb * std_age_65above +
      std_deprivation_comb * std_foreign_perc +
      std_deprivation_comb * #std_ethmin_perc +
      std_deprivation_comb * std_price_change_per +
      std_deprivation_comb * std_w20_population_dens + # w_controls
      std_deprivation_comb * std_w20_age_18_29 +
      std_deprivation_comb * std_w20_age_65above +
      std_deprivation_comb * std_w20_foreign_perc +
      std_deprivation_comb * #std_w20_ethmin_perc +
      std_deprivation_comb * std_w20_price_change_per
    | 0 | 0 | lsoa11,
    data = data_sub.df[oo, ]
  )








screenreg(list(lm4_1, lm4_2, lm4_3, lm4_4, lm4_6), digits = 3)




####################
### Export table ###
####################


texreg(
  list(lm4_1, lm4_2, lm4_3, lm4_4, lm4_6),
  digits = 3,
  fontsize = "scriptsize",
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  caption = "LM. Individual and neighbourhood deprivation.",
  caption.above = TRUE,
  custom.model.names = paste0("(", c(1:5), ")"),
  custom.gof.rows = list(
    "Basic controls" = rep(c("yes"), times = 5),
    "Econ controls" = c("no", rep(c("yes"), times = 5 -
                                    1)),
    "Interaction controls" = c("no", "no", "no", "yes", "no")
  ),
  custom.coef.map = list(
    "std_deprivation_comb" = "Material deprivation",
    "std_index_dep_score_r" = "Area deprivation",
    "std_population_dens" = "Population density",
    "std_age_18_29" = "% age 18-29",
    "std_age_65above" = "% age 65 and above",
    "std_foreign_perc" = "% foreign nationals",
    "std_price_change_per"  = "$\\Delta$ house price",
    "std_w20_index_dep_score_r" = "$W_{20}$ Area deprivation",
    "std_w20_population_dens" = "$W_{20}$ Population density",
    "std_w20_age_18_29" = "$W_{20}$ Age 18-29",
    "std_w20_age_65above" = "$W_{20}$ Age 65 and above",
    "std_w20_foreign_perc" = "$W_{20}$ \\% foreign nationals",
    "std_w20_price_change_per"  = "$W_{20} \\Delta$ house price",
    "std_deprivation_comb:std_index_dep_score_r" = "Mat deprivation $\\times$ Area deprivation",
    "std_deprivation_comb:std_w20_index_dep_score_r" = "Mat deprivation $\\times$ $W_{20}$ Area deprivation",
    "std_index_dep_score_r:std_w20_index_dep_score_r" = "Area deprivation $\\times$ $W_{20}$ Area deprivation"
  ),
  groups = list(
    "Individual level" = 1,
    "Neighbourhood level" = 2:7,
    "Neighbourhood level 20km" = 8:13,
    "Interactions" = 14:16
  ),
  custom.note = c(
    "\\item %stars.
       Linear probability model. Covariates are in standard deviations and centered around their mean.
       Standard errors (in brackets) are calustered at the LSOA level.
                       \\item Basic controls: age (5-year bins), sex, migration background, ethnicity, highest education, marital status, interview month, deprivation battery eligibility.
                       \\item Econ controls: unemployed, household equivalence income.
                       \\item Interaction controls: individual material deprivation interacted with all neighbourhood variables shown above."
  ),
  file = "../03_Output/tab_final1.tex"
)











########################################
### Analysis final - nonlinear model ###
########################################

glm4_6 <-
  glm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_index_dep_score_r +
      std_w20_index_dep_score_r +
      std_index_dep_score_r * std_w20_index_dep_score_r +
      std_population_dens + # controls
      std_age_18_29 +
      std_age_65above +
      std_foreign_perc +
      #std_ethmin_perc +
      std_price_change_per +
      std_w20_population_dens + # w_controls
      std_w20_age_18_29 +
      std_w20_age_65above +
      std_w20_foreign_perc +
      #std_w20_ethmin_perc +
      std_w20_price_change_per,
    data = data_sub.df,
    family = binomial
  )
glm4_6_cl.vcov <-
  coeftest(glm4_6, vcov = vcovCL(glm4_6, cluster = data_sub.df$lsoa11[-glm4_6$na.action]))

oo <- which(!rownames(data_sub.df) %in% names(glm4_6$na.action))



glm4_1 <-
  glm(
    brexit_leave ~ std_deprivation_comb + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv,
    data = data_sub.df[oo, ],
    family = binomial
  )
glm4_1_cl.vcov <-
  coeftest(glm4_1, vcov = vcovCL(glm4_1, cluster = data_sub.df$lsoa11[oo]))


glm4_2 <-
  glm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec,
    data = data_sub.df[oo, ],
    family = binomial
  )
glm4_2_cl.vcov <-
  coeftest(glm4_2, vcov = vcovCL(glm4_2, cluster = data_sub.df$lsoa11[oo]))


glm4_3 <-
  glm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_index_dep_score_r  +
      std_w20_index_dep_score_r +
      std_population_dens + # controls
      std_age_18_29 +
      std_age_65above +
      std_foreign_perc +
      #std_ethmin_perc +
      std_price_change_per +
      std_w20_population_dens + # w_controls
      std_w20_age_18_29 +
      std_w20_age_65above +
      std_w20_foreign_perc +
      #std_w20_ethmin_perc +
      std_w20_price_change_per,
    data = data_sub.df[oo, ],
    family = binomial
  )
glm4_3_cl.vcov <-
  coeftest(glm4_3, vcov = vcovCL(glm4_3, cluster = data_sub.df$lsoa11[oo]))



glm4_4 <-
  glm(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_deprivation_comb * std_index_dep_score_r +
      std_deprivation_comb * std_w20_index_dep_score_r +
      std_deprivation_comb * std_population_dens + # controls
      std_deprivation_comb * std_age_18_29 +
      std_deprivation_comb * std_age_65above +
      std_deprivation_comb * std_foreign_perc +
      std_deprivation_comb * #std_ethmin_perc +
      std_deprivation_comb * std_price_change_per +
      std_deprivation_comb * std_w20_population_dens + # w_controls
      std_deprivation_comb * std_w20_age_18_29 +
      std_deprivation_comb * std_w20_age_65above +
      std_deprivation_comb * std_w20_foreign_perc +
      std_deprivation_comb * #std_w20_ethmin_perc +
      std_deprivation_comb * std_w20_price_change_per,
    data = data_sub.df[oo, ],
    family = binomial
  )
glm4_4_cl.vcov <-
  coeftest(glm4_4, vcov = vcovCL(glm4_4, cluster = data_sub.df$lsoa11[oo]))






screenreg(list(glm4_1, glm4_2, glm4_3, glm4_4, glm4_6), digits = 3)




####################
### Export table ###
####################


texreg(
  list(glm4_1, glm4_2, glm4_3, glm4_4, glm4_6),
  digits = 3,
  override.se = list(
    glm4_1_cl.vcov[, 2],
    glm4_2_cl.vcov[, 2],
    glm4_3_cl.vcov[, 2],
    glm4_4_cl.vcov[, 2],
    glm4_6_cl.vcov[, 2]
  ),
  override.pvalues = list(
    glm4_1_cl.vcov[, 4],
    glm4_2_cl.vcov[, 4],
    glm4_3_cl.vcov[, 4],
    glm4_4_cl.vcov[, 4],
    glm4_6_cl.vcov[, 4]
  ),
  fontsize = "scriptsize",
  dcolumn = TRUE,
  longtable = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  caption = "Logit. Individual and neighbourhood deprivation.",
  label = "tab:glm",
  caption.above = TRUE,
  custom.model.names = paste0("(", c(1:5), ")"),
  custom.gof.rows = list(
    "Basic controls" = rep(c("yes"), times = 5),
    "Econ controls" = c("no", rep(c("yes"), times = 5 -
                                    1)),
    "Interaction controls" = c("no", "no", "no", "yes", "no")
  ),
  custom.coef.map = list(
    "std_deprivation_comb" = "Material deprivation",
    "std_index_dep_score_r" = "Area deprivation",
    "std_population_dens" = "Population density",
    "std_age_18_29" = "% age 18-29",
    "std_age_65above" = "% age 65 and above",
    "std_foreign_perc" = "% foreign nationals",
    "std_price_change_per"  = "$\\Delta$ house price",
    "std_w20_index_dep_score_r" = "$W_{20}$ Area deprivation",
    "std_w20_population_dens" = "$W_{20}$ Population density",
    "std_w20_age_18_29" = "$W_{20}$ Age 18-29",
    "std_w20_age_65above" = "$W_{20}$ Age 65 and above",
    "std_w20_foreign_perc" = "$W_{20}$ \\% foreign nationals",
    "std_w20_price_change_per"  = "$W_{20} \\Delta$ house price",
    "std_deprivation_comb:std_index_dep_score_r" = "Mat deprivation $\\times$ Area deprivation",
    "std_deprivation_comb:std_w20_index_dep_score_r" = "Mat deprivation $\\times$ $W_{20}$ Area deprivation",
    "std_index_dep_score_r:std_w20_index_dep_score_r" = "Area deprivation $\\times$ $W_{20}$ Area deprivation"
  ),
  groups = list(
    "Individual level" = 1,
    "Neighbourhood level" = 2:7,
    "Neighbourhood level 20km" = 8:13,
    "Interactions" = 14:16
  ),
  custom.note = c(
    "\\item %stars.
       Logit model. Covariates are in standard deviations and centered around their mean.
       Standard errors (in brackets) are calustered at the LSOA level.
                       \\item Basic controls: age (5-year bins), sex, migration background, ethnicity, highest education, marital status, nterview month, deprivation battery eligibility.
                       \\item Econ controls: unemployed, household equivalence income.
                       \\item Interaction controls: individual material deprivation interacted with all neighbourhood variables shown above."
  ),
  file = "../03_Output/tab_final1_glm.tex"
)














###################################
### Analysis final - multilevel ###
###################################


mlm4_6 <-
  lmer(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_index_dep_score_r +
      std_w20_index_dep_score_r +
      std_index_dep_score_r * std_w20_index_dep_score_r +
      std_population_dens + # controls
      std_age_18_29 +
      std_age_65above +
      std_foreign_perc +
      #std_ethmin_perc +
      std_price_change_per +
      std_w20_population_dens + # w_controls
      std_w20_age_18_29 +
      std_w20_age_65above +
      std_w20_foreign_perc +
      #std_w20_ethmin_perc +
      std_w20_price_change_per
    + (1 + std_deprivation_comb | lsoa11),
    data = data_sub.df
  )

oo <- which(rownames(data_sub.df) %in% rownames(mlm4_6@frame))




mlm4_1 <-
  lmer(
    brexit_leave ~ std_deprivation_comb + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv
    + (1 + std_deprivation_comb | lsoa11),
    data = data_sub.df[oo, ]
  )

mlm4_2 <-
  lmer(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec
    + (1 + std_deprivation_comb | lsoa11),
    data = data_sub.df[oo, ]
  )

mlm4_3 <-
  lmer(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_index_dep_score_r  +
      std_w20_index_dep_score_r +
      std_population_dens + # controls
      std_age_18_29 +
      std_age_65above +
      std_foreign_perc +
      #std_ethmin_perc +
      std_price_change_per +
      std_w20_population_dens + # w_controls
      std_w20_age_18_29 +
      std_w20_age_65above +
      std_w20_foreign_perc +
      #std_w20_ethmin_perc +
      std_w20_price_change_per
    + (1 + std_deprivation_comb | lsoa11),
    data = data_sub.df[oo, ]
  )


mlm4_4 <-
  lmer(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec +
      std_deprivation_comb * std_index_dep_score_r +
      std_deprivation_comb * std_w20_index_dep_score_r +
      std_deprivation_comb * std_population_dens + # controls
      std_deprivation_comb * std_age_18_29 +
      std_deprivation_comb * std_age_65above +
      std_deprivation_comb * std_foreign_perc +
      # std_deprivation_comb * std_ethmin_perc +
      std_deprivation_comb * std_price_change_per +
      std_deprivation_comb * std_w20_population_dens + # w_controls
      std_deprivation_comb * std_w20_age_18_29 +
      std_deprivation_comb * std_w20_age_65above +
      std_deprivation_comb * std_w20_foreign_perc +
      # std_deprivation_comb * std_w20_ethmin_perc +
      std_deprivation_comb * std_w20_price_change_per
    + (1 + std_deprivation_comb | lsoa11),
    data = data_sub.df[oo, ]
  )
screenreg(mlm4_4, digits = 3)



# Test
mlm4_5 <-
  lmer(
    brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
      age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
      unemployed + hh_inc_oecd_dec + hh_jobclass_comb +
      std_deprivation_comb * std_index_dep_score_r +
      std_deprivation_comb * std_w20_index_dep_score_r +
      std_deprivation_comb * std_population_dens + # controls
      std_deprivation_comb * std_age_18_29 +
      std_deprivation_comb * std_age_65above +
      std_deprivation_comb * std_foreign_perc +
      # std_deprivation_comb * std_ethmin_perc +
      std_deprivation_comb * std_price_change_per +
      std_deprivation_comb * std_w20_population_dens + # w_controls
      std_deprivation_comb * std_w20_age_18_29 +
      std_deprivation_comb * std_w20_age_65above +
      std_deprivation_comb * std_w20_foreign_perc +
      # std_deprivation_comb * std_w20_ethmin_perc +
      std_deprivation_comb * std_w20_price_change_per
    + (1 + std_deprivation_comb | lsoa11),
    data = data_sub.df[oo, ]
  )






screenreg(list(mlm4_1, mlm4_2, mlm4_3, mlm4_4, mlm4_6, mlm4_5), digits = 3)





### Test social Class


mlm_test <- lmer(
  brexit_leave ~
    age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
    unemployed + hh_inc_oecd_dec + hh_jobclass_comb +
    hh_jobclass_comb * std_index_dep_score_r +
    hh_jobclass_comb * std_w20_index_dep_score_r +
    hh_jobclass_comb * std_population_dens + # controls
    hh_jobclass_comb * std_age_18_29 +
    hh_jobclass_comb * std_age_65above +
    hh_jobclass_comb * std_foreign_perc +
    # std_deprivation_comb * std_ethmin_perc +
    hh_jobclass_comb * std_price_change_per +
    hh_jobclass_comb * std_w20_population_dens + # w_controls
    hh_jobclass_comb * std_w20_age_18_29 +
    hh_jobclass_comb * std_w20_age_65above +
    hh_jobclass_comb * std_w20_foreign_perc +
    # std_deprivation_comb * std_w20_ethmin_perc +
    hh_jobclass_comb * std_w20_price_change_per
  + (1 + std_deprivation_comb | lsoa11),
  data = data_sub.df[oo, ]
)

screenreg(list(mlm_test), digits = 3)




####################
### Export table ###
####################


textab <-
  texreg(
    list(mlm4_1, mlm4_2, mlm4_3, mlm4_4, mlm4_6),
    digits = 3,
    stars = c(0.001, 0.01, 0.05, 0.1),
    symbol = '\\dagger',
    fontsize = "scriptsize",
    dcolumn = TRUE,
    longtable = TRUE,
    threeparttable = TRUE,
    use.packages = FALSE,
    caption = "Multilevel LM. Individual and neighbourhood deprivation.",
    label = "tab:lmer",
    caption.above = TRUE,
    custom.model.names = paste0("(", c(1:5), ")"),
    custom.gof.rows = list(
      "Basic controls" = rep(c("yes"), times = 5),
      "Econ controls" = c("no", rep(c("yes"), times = 5 -
                                      1)),
      "Interaction controls" = c("no", "no", "no", "yes", "no")
    ),
    custom.coef.map = list(
      "std_deprivation_comb" = "Material deprivation",
      "std_index_dep_score_r" = "Area deprivation",
      "std_population_dens" = "Population density",
      "std_age_18_29" = "% age 18-29",
      "std_age_65above" = "% age 65 and above",
      "std_foreign_perc" = "% foreign nationals",
      "std_price_change_per"  = "$\\Delta$ house price",
      "std_w20_index_dep_score_r" = "$W_{20}$ Area deprivation",
      "std_w20_population_dens" = "$W_{20}$ Population density",
      "std_w20_age_18_29" = "$W_{20}$ Age 18-29",
      "std_w20_age_65above" = "$W_{20}$ Age 65 and above",
      "std_w20_foreign_perc" = "$W_{20}$ \\% foreign nationals",
      "std_w20_price_change_per"  = "$W_{20} \\Delta$ house price",
      "std_deprivation_comb:std_index_dep_score_r" = "Mat deprivation $\\times$ Area deprivation",
      "std_deprivation_comb:std_w20_index_dep_score_r" = "Mat deprivation $\\times$ $W_{20}$ Area deprivation",
      "std_index_dep_score_r:std_w20_index_dep_score_r" = "Area deprivation $\\times$ $W_{20}$ Area deprivation"
    ),
    groups = list(
      "Individual level" = 1,
      "Neighbourhood level" = 2:7,
      "Neighbourhood level 20km" = 8:13,
      "Interactions" = 14:16
    ),
    custom.note = c(
      "\\item $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{\\dagger}p<0.1$.
       Multilevel Linear probability model. Random intercept and random slope for material deprivation at the LSOA level. Covariates are in standard deviations and centered around their mean.
       Standard errors (in brackets) are clustered at the LSOA level.
                       \\item Basic controls: age (5-year bins), sex, migration background, ethnicity, highest education, marital status, interview month, deprivation battery eligibility.
                       \\item Econ controls: unemployed, household equivalence income.
                       \\item Interaction controls: individual material deprivation interacted with all neighbourhood variables shown above."
    ),
    #file = "../03_Output/tab_final1_lmer.tex"
  )


textab <-
  gsub("D[{].[}][{].[}][{][[:digit:]]+\\.*[[:digit:]]*[}]",
       "D{.}{.}{4.5}",
       textab)

# Print
write.table(
  textab,
  file = "../03_Output/tab_final1_lmer.tex",
  col.names = FALSE,
  row.names = FALSE,
  quote = FALSE
)







#### ------------------------------------------ ####
####      Descriptives of estimation sample     ####
#### ------------------------------------------ ####

vv <- names(mlm4_6@frame)
vv <- vv[-length(vv)] # delete id

# replace standardization, and categorization
vv <- gsub("std_", "", vv)
vv <- gsub("age_cat", "age_dv", vv)
vv <- gsub("hh_inc_oecd_dec", "hh_inc_oecd", vv)

# get the right data
oo <- which(rownames(data_sub.df) %in% rownames(mlm4_6@frame))
data.desc <- data_sub.df[oo, vv]

# drop eligibility for either  deprivation measures
data.desc <-
  data.desc[, -which(names(data.desc) %in% c("dep_child_elg", "dep_pensioner_elg", "year_month"))]


# Rename
nl <- list(
  "brexit_leave" = "Vote intention Leave",
  "deprivation_comb" = "Material deprivation",
  "age_dv" = "Age",
  "sex_male" = "Sex",
  "migback_gen" = "Migration background",
  "ethn_dv_short" = "Ethnicity",
  "hiqual_dv" = "Highest Education",
  "child" = "Child(ren) in hh",
  "marstat_dv" = "Marital status",
  "unemployed" = "Unemployed",
  "hh_inc_oecd" = "Household equivalence income",
  "index_dep_score_r" = "Area deprivation",
  "population_dens" = "Population density",
  # "median_age" = "Median age",
  "age_18_29" = "% age 18-29",
  "age_65above" = "% age 65 and above",
  "foreign_perc" = "\\% foreign nationals",
  "price_change_per"  = "Delta house price",
  "w20_index_dep_score_r" = "$W_{20}$ Area deprivation",
  "w20_population_dens" = "$W_{20}$ Population density",
  # "w20_median_age" = "$W_{20}$ Median age",
  "w20_age_18_29" = "$W_{20}$ Age 18-29",
  "w20_age_65above" = "$W_{20}$ Age 65 and above",
  "w20_foreign_perc" = "$W_{20}$ \\% foreign nationals",
  "w20_price_change_per"  = "$W_{20}$ house price"
)
oo <- match(names(data.desc), names(nl))
nl <- nl[oo]



### Export Descpritives
stargazer(
  data.desc,
  out = "../03_Output/tab_descriptives.tex",
  style = "aer",
  type = "latex",
  align = FALSE,
  title = "Descriptive statistics",
  label = "tab:desc",
  #covariate.labels = unname(unlist(nl)),
  digits = 2,
  float = TRUE,
  font.size = "scriptsize"
)

out <-
  read.delim("../03_Output/tab_descriptives.tex", header = FALSE)

# Replace names
names(nl) <- gsub("_", "\\_", names(nl), fixed = TRUE)
for (i in 1:length(nl)) {
  out <-
    data.frame(sapply(
      out,
      FUN = function(x)
        gsub(names(nl)[i], nl[i], x, fixed = TRUE)
    ))
}
out <-
  sapply(
    out,
    FUN = function(x)
      gsub("w20\\_", "$W_{20}$ ", x, fixed = TRUE)
  )

# Save
cat(" ", file = "../03_Output/tab_descriptives.tex", sep = "\n")
lapply(
  out,
  FUN = function(x)
    cat(
      x,
      file = "../03_Output/tab_descriptives.tex",
      append = TRUE,
      sep = "\n"
    )
)







#### -------------------------- ####
####      Plot main effects     ####
#### -------------------------- ####


### Get data from model output

coeff.df <- ggcoefstats(mlm4_3, output = "tidy")$data


oo <-
  which(grepl("std_", coeff.df$term) & coeff.df$effect == "fixed")
eff.df <- coeff.df[oo,]

### Rename

eff.df$group <- 2
eff.df$group[grepl("std_w20", eff.df$term)] <- 3
eff.df$group[grepl("std_deprivation_comb", eff.df$term)] <- 1

eff.df$group <-
  factor(
    eff.df$group,
    levels = c(1:3),
    labels = c("Individual", "Neighbourhood", "Area 20km")
  )

eff.df$term <- as.character(eff.df$term)
rename.list <- list(
  std_deprivation_comb = "Material deprivation",
  std_index_dep_score_r = "Area deprivation",
  std_population_dens = "Population density",
  std_age_18_29 = "% age 18-29",
  std_age_65above = "% age 65 and above",
  std_foreign_perc = "% foreign nationals",
  std_price_change_per = "Delta House price",
  std_w20_index_dep_score_r = "Area deprivation",
  std_w20_population_dens = "Population density",
  std_w20_age_18_29 = "% age 18-29",
  std_w20_age_65above = "% age 65 and above",
  std_w20_foreign_perc = "% foreign nationals",
  std_w20_price_change_per  = "Delta House price"
)

eff.df$term2 <-
  factor(eff.df$term, levels = rev(names(rename.list)))
for (i in names(rename.list)) {
  levels(eff.df$term2)[levels(eff.df$term2) == i] <- rename.list[[i]]
}



# Coef Labels
eff.df$lab <-
  as.character(sprintf("%.3f", round(eff.df$estimate, 3)))

eff.df$lab[eff.df$p.value <= 0.1 &
             eff.df$p.value > 0.05] <-
  paste0(eff.df$lab[eff.df$p.value <= 0.1 &
                      eff.df$p.value > 0.05], expression("\u2020"))
eff.df$lab[eff.df$p.value <= 0.05 &
             eff.df$p.value > 0.01] <-
  paste0(eff.df$lab[eff.df$p.value <= 0.05 &
                      eff.df$p.value > 0.01], "*")
eff.df$lab[eff.df$p.value <= 0.01 &
             eff.df$p.value > 0.001] <-
  paste0(eff.df$lab[eff.df$p.value <= 0.01 &
                      eff.df$p.value > 0.001], "**")
eff.df$lab[eff.df$p.value <= 0.001] <-
  paste0(eff.df$lab[eff.df$p.value <= 0.001], "***")

eff.df$offset <- 0.5
eff.df$offset[eff.df$group == "Neighbourhood"] <- -1
eff.df$offset[eff.df$group == "Area 20km"] <- 1


### Plot
zp <- ggplot(eff.df)
zp <-
  zp + geom_hline(yintercept = 0,
                  colour = alpha("black", 0.3),
                  lty = 2)
zp <-
  zp + geom_pointrange(
    aes(
      x = term2,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      colour = group,
      shape = group
    ),
    lwd = 0.7,
    fill = "black",
    alpha = 1,
    position = position_dodge(width = 1 / 2)
  )
zp <-
  zp + geom_label(
    aes(
      label = lab,
      x = term2 ,
      y = estimate,
      colour = group
    ),
    size = 4,
    show.legend  = FALSE,
    nudge_x = eff.df$offset * 0.3
  )
zp <- zp + coord_flip() + theme_bw()
zp <- zp + theme(legend.title = element_blank())
zp <-
  zp + labs(y = "Coefficients of standardized variables", x = "")
#zp <- zp + scale_color_viridis_d(option = "D", begin = 0.1,  end = 0.85, direction = -1)
zp <-
  zp + theme(
    text = element_text(family = "Times New Roman", size = 20),
    # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.key = element_blank(),
    axis.text.y = element_text(colour =
                                 "black", size = 20),
    axis.title.x = element_text(colour =
                                  "black"),
    axis.text.x = element_text(colour =
                                 "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, colour = "black"),
    panel.spacing.x = unit(6, "mm"),
    plot.margin = unit(c(0, 6, 0, 0), "mm")
    #axis.text.x = element_text(size=16),
    #axis.text.y = element_text(size=16),
    #axis.title.x = element_text(size=16),
    #axis.title.y = element_text(size=16)
  )
zp <-
  zp + guides(colour = guide_legend(override.aes = list(linetype = 0, alpha = 1)))
zp <- zp + geom_vline(aes(xintercept = 6.5))
zp <- zp + ggtitle("Probabilty of voting `leave'")
zp

### Export

cairo_ps(
  file = paste("../03_Output/", "Figure2.eps", sep = ""),
  width = 10,
  height = 8,
  bg = "white",
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
zp
dev.off()

png(
  file = paste("../03_Output/", "Figure2.png", sep = ""),
  width = 10,
  height = 8,
  units = "in",
  res = 300,
  type = "cairo",
  bg = "white",
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
zp
dev.off()





#### ------------------------- ####
####      Plot interaction     ####
#### ------------------------- ####


cols3 <- viridis(8)


cols2 <- mako(8)


# Use multlevel model


#####################################
### INdividual * Area deprivation ###
#####################################




### 20 km
int_2.pl <-
  interplot(
    mlm4_4,
    var1 = "std_deprivation_comb",
    var2 = "std_w20_index_dep_score_r",
    ci = 0.95,
    hist = TRUE,
    rfill = cols2[6]
  )
int_2.pl <- int_2.pl + labs(y = element_blank(),
                            x = "Area deprivation (20km)")
int_2.pl <- int_2.pl + theme_bw()
int_2.pl <-
  int_2.pl + theme(
    text = element_text(family = "Times New Roman", size = 20),
    # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.key = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.title.x = element_text(colour = "black", margin = margin(2, 2, 2, 2, "pt")),
    axis.text.x = element_text(colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, colour = "black"),
    #axis.text.x = element_text(size=16),
    #axis.text.y = element_text(size=16),
    #axis.title.x = element_text(size=16),
    #axis.title.y = element_text(size=16)
  )
int_2.pl <-
  int_2.pl + geom_hline(aes(yintercept = 0), linetype = "dashed")

yrange <- layer_scales(int_2.pl)$y$range$range

# Move line up
int_2.pl <- move_layers(int_2.pl, "GeomLine", position = "top")
int_2.pl$layers[[4]]$aes_params$colour <- cols3[3]

# Change colour of Histogram
int_2.pl$layers
int_2.pl$layers[[1]]$aes_params$colour <- cols3[1]
int_2.pl$layers[[1]]$aes_params$fill <- cols3[8]
int_2.pl$layers[[1]]$aes_params$alpha <- 0.6
int_2.pl



### Direct
int_1.pl <-
  interplot(
    mlm4_4,
    var1 = "std_deprivation_comb",
    var2 = "std_index_dep_score_r",
    ci = 0.95,
    hist = FALSE
  )
int_1.pl <- int_1.pl + labs(y = element_blank(),
                            x = "Neighbourhood deprivation")
int_1.pl <- int_1.pl + theme_bw()
int_1.pl <-
  int_1.pl + theme(
    text = element_text(family = "Times New Roman", size = 20),
    # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.key = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.title.x = element_text(colour = "black", margin = margin(2, 2, 2, 2, "pt")),
    axis.text.x = element_text(colour = "black"),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, colour = "black"),
    #axis.text.x = element_text(size=16),
    #axis.text.y = element_text(size=16),
    #axis.title.x = element_text(size=16),
    #axis.title.y = element_text(size=16)
  )
#int_2.pl <- move_layers(int_2.pl, "GeomLine", position = "top")
int_1.pl <-
  int_1.pl + geom_hline(aes(yintercept = 0), linetype = "dashed")


### Add historgram manually
var2_dt <- mlm4_4@frame$std_index_dep_score_r

yrange2 <- c(min(ggplot_build(int_2.pl)$data[[2]]$ymax), yrange[2])

yrange <- yrange2
maxdiff <- (max(yrange2) - min(yrange2))
break_var2 <- length(unique(var2_dt))

if (break_var2 >= 100) {
  break_var2 <- 100
}
hist.out <- hist(var2_dt, breaks = break_var2, plot = FALSE)

n.hist <- length(hist.out$mids)
dist <- hist.out$mids[2] - hist.out$mids[1]
hist.max <- max(hist.out$counts)

histX <-
  data.frame(
    ymin = rep(min(yrange2) - maxdiff / 5, n.hist),
    ymax = hist.out$counts / hist.max * maxdiff / 5 + min(yrange2) - maxdiff /
      5,
    xmin = hist.out$mids - dist / 2,
    xmax = hist.out$mids + dist / 2
  )

esize = 0.5
ralpha = 0.5
rfill = cols2[6]
ercolor = NA
m <- int_1.pl$data

int_1.pl2 <- ggplot() +
  geom_rect(
    data = histX,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    colour = cols3[1],
    fill = cols3[8],
    alpha = 0.6,
    size = 0.5
  ) +
  geom_blank(data = data.frame(x = c(0, 0), y = yrange2), aes(x = x, y = y)) +
  geom_ribbon(
    data = m,
    aes_string(x = "fake", ymin = "lb", ymax = "ub"),
    alpha = ralpha,
    color = ercolor,
    fill = rfill
  ) +
  geom_line(data = m,
            aes_string(x = "fake", y = "coef1"),
            colour = cols3[3]) +
  labs(y = element_blank(),
       x = "Neighbourhood deprivation") +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman", size = 20),
    # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.key = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.title.x = element_text(colour = "black", margin = margin(2, 2, 10, 2, "pt")),
    axis.text.x = element_text(colour = "black", margin = margin(2, 2, 2, 2, "pt")),
    strip.background = element_blank(),
    strip.text = element_text(size = 20, colour = "black"),
    #axis.text.x = element_text(size=16),
    #axis.text.y = element_text(size=16),
    #axis.title.x = element_text(size=16),
    #axis.title.y = element_text(size=16)
  ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed")


### Export


cairo_ps(
  file = paste("../03_Output/", "Figure3.eps", sep = ""),
  width = 14,
  height = 7,
  bg = "white",
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
grid.arrange(
  int_1.pl2,
  int_2.pl,
  ncol = 2,
  left = textGrob(
    "Effect of individual deprivation",
    rot = 90,
    gp = gpar(fontsize = 20)
  )
)
dev.off()

png(
  file = paste("../03_Output/", "Figure3.png", sep = ""),
  width = 14,
  height = 7,
  units = "in",
  res = 300,
  type = "cairo",
  bg = "white",
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
grid.arrange(
  int_1.pl2,
  int_2.pl,
  ncol = 2,
  left = textGrob(
    "Effect of individual deprivation",
    rot = 90,
    gp = gpar(fontsize = 20)
  )
)
dev.off()








#### -------------------------------------------------- ####
####      Plot interaction with different distances     ####
#### -------------------------------------------------- ####

dists <- c("", "2", "5", "10", "20", "35", "50", "75")

res <- vector(mode = "list", length(dists))
hist.data <- vector(mode = "list", length(dists))


for (d in dists) {
  # create formula
  f1 <-
    "brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
                 age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
                 unemployed + hh_inc_oecd_dec +
                 std_deprivation_comb * std_index_dep_score_r +
                 std_deprivation_comb * std_population_dens +
                 std_deprivation_comb * std_age_18_29 +
                 std_deprivation_comb * std_age_65above +
                 std_deprivation_comb * std_foreign_perc +
                 std_deprivation_comb * std_price_change_per + "
  vs <-
    c(
      "std_index_dep_score_r",
      "std_population_dens",
      "std_age_18_29",
      "std_age_65above",
      "std_foreign_perc",
      "std_price_change_per"
    )
  vs <- gsub("std_", paste0("std_w", d, "_"), vs)
  f2 <-
    paste0(paste0("std_deprivation_comb * ", vs), collapse = " + ")
  f <- paste0(f1, f2, " + (1 + std_deprivation_comb | lsoa11)")
  f <- gsub("\n", "", f)
  
  #estimate model
  mlm <- lmer(f, data = data_sub.df)
  
  # plot interaction
  intv <- paste0("std_w", d, "_index_dep_score_r")
  int_tmp.pl <-
    interplot(
      mlm,
      var1 = "std_deprivation_comb",
      var2 = intv,
      ci = 0.95,
      hist = TRUE,
      rfill = cols2[6]
    )
  lab <- paste0("Area deprivation (", d, "km)")
  if (d == "") {
    lab <- paste0("Area deprivation (adjacent)")
  }
  
  int_tmp.pl <- int_tmp.pl + labs(y = element_blank(),
                                  x = lab)
  int_tmp.pl <- int_tmp.pl + theme_bw()
  int_tmp.pl <-
    int_tmp.pl + theme(
      text = element_text(family = "Times New Roman", size = 20),
      # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.key = element_blank(),
      axis.text.y = element_text(colour = "black"),
      axis.title.x = element_text(colour = "black", margin = margin(2, 2, 2, 2, "pt")),
      axis.text.x = element_text(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(size = 20, colour = "black"),
      #axis.text.x = element_text(size=16),
      #axis.text.y = element_text(size=16),
      #axis.title.x = element_text(size=16),
      #axis.title.y = element_text(size=16)
    )
  int_tmp.pl <-
    int_tmp.pl + geom_hline(aes(yintercept = 0), linetype = "dashed")
  
  if (d == dists[1]) {
    yrange <- layer_scales(int_tmp.pl)$y$range$range
    yrange2 <-
      c(min(ggplot_build(int_tmp.pl)$data[[3]]$ymax), yrange[2])
  } else{
    yrange[1] <-
      min(yrange[1], layer_scales(int_tmp.pl)$y$range$range[1])
    
    if (yrange[2] < layer_scales(int_tmp.pl)$y$range$range[2]) {
      yrange[2] <-
        max(yrange[2], layer_scales(int_tmp.pl)$y$range$range[2])
      yrange2 <-
        c(min(ggplot_build(int_tmp.pl)$data[[3]]$ymax), yrange[2])
    }
    
    
    
  }
  
  
  
  i <- which(dists == d)
  res[[i]] <- int_tmp.pl
  
  hist.data[[i]] <- mlm@frame[, intv]
  
  #clean up
  rm("mlm")
  gc()
  Sys.sleep(2)
}


# Bring on same scale and change design
# max.hist <- vector(mode = "numeric", length(dists))
# for(i in 1:length(dists)){
#   max.hist[i] <- ggplot_build(res[[i]])$data[[3]]$ymax
# }

# Bring on same scale and change design
res2 <- vector(mode = "list", length(dists))
for (i in 1:length(dists)) {
  #variable for hist
  var2_dt <- hist.data[[i]]
  
  # Range differences
  
  maxdiff <- (max(yrange2) - min(yrange2))
  break_var2 <- length(unique(var2_dt))
  
  if (break_var2 >= 100) {
    break_var2 <- 100
  }
  hist.out <- hist(var2_dt, breaks = break_var2, plot = FALSE)
  
  n.hist <- length(hist.out$mids)
  dist <- hist.out$mids[2] - hist.out$mids[1]
  hist.max <- max(hist.out$counts)
  
  histX <- data.frame(
    ymin = rep(min(yrange2) - maxdiff / 5, n.hist),
    ymax = hist.out$counts / hist.max * maxdiff / 5 + min(yrange2) - maxdiff /
      5,
    xmin = hist.out$mids - dist / 2,
    xmax = hist.out$mids + dist / 2
  )
  
  
  esize = 0.5
  ralpha = 0.5
  rfill = cols2[6]
  ercolor = NA
  m1 <- res[[i]]$layers[[3]]$data
  m2 <- res[[i]]$layers[[2]]$data
  
  res[[i]]$labels$x
  
  tmp.pl2 <- ggplot() +
    geom_rect(
      data = histX,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      colour = cols3[1],
      fill = cols3[8],
      alpha = 0.6,
      size = 0.5
    ) +
    geom_blank(data = data.frame(x = c(0, 0), y = yrange2), aes(x = x, y = y)) +
    geom_ribbon(
      data = m1,
      aes_string(x = "fake", ymin = "lb", ymax = "ub"),
      alpha = ralpha,
      color = ercolor,
      fill = rfill
    ) +
    geom_line(data = m2,
              aes_string(x = "fake", y = "coef1"),
              colour = cols3[3]) +
    labs(y = element_blank(),
         x = res[[i]]$labels$x) +
    theme_bw() +
    theme(
      text = element_text(family = "Times New Roman", size = 20),
      # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.key = element_blank(),
      axis.text.y = element_text(colour = "black"),
      axis.title.x = element_text(colour = "black", margin = margin(2, 2, 10, 2, "pt")),
      axis.text.x = element_text(colour = "black", margin = margin(2, 2, 2, 2, "pt")),
      strip.background = element_blank(),
      strip.text = element_text(size = 20, colour = "black"),
      #axis.text.x = element_text(size=16),
      #axis.text.y = element_text(size=16),
      #axis.title.x = element_text(size=16),
      #axis.title.y = element_text(size=16)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dashed")
  
  
  res2[[i]] <- tmp.pl2
  
}



# Export

cairo_ps(
  file = paste0("../03_Output/", "Figure4.eps"),
  width = 12,
  height = 14,
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
grid.arrange(
  grobs = res2,
  ncol = 2,
  left = textGrob(
    paste0("Effect of individual deprivation"),
    rot = 90,
    vjust = 1,
    gp = gpar(fontsize = 20, fontfamily = "Times New Roman")
  )
)

dev.off()


# Png
png(
  file = paste0("../03_Output/", "Figure4.png"),
  width = 12,
  height = 14,
  units = "in",
  res = 300
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
grid.arrange(
  grobs = res2,
  ncol = 2,
  left = textGrob(
    paste0("Effect of individual deprivation"),
    rot = 90,
    vjust = 1,
    gp = gpar(fontsize = 20, fontfamily = "Times New Roman")
  )
)

dev.off()





#### ---------------------------------------------------------------- ####
####      Plot neighbourhood interaction with different distances     ####
#### ---------------------------------------------------------------- ####

dists <- c("", "2", "5", "10", "20", "35", "50", "75")

res <- vector(mode = "list", length(dists))
hist.data <- vector(mode = "list", length(dists))


for (d in dists) {
  # create formula
  f1 <-
    "brexit_leave ~ std_deprivation_comb  + dep_child_elg + dep_pensioner_elg + year_month +
                 age_cat + sex_male + migback_gen + ethn_dv_short + hiqual_dv + child + marstat_dv +
                 unemployed + hh_inc_oecd_dec +
                 std_index_dep_score_r +
                 std_population_dens +
                 std_age_18_29 +
                 std_age_65above +
                 std_foreign_perc +
                 std_price_change_per + "
  vs <-
    c(
      "std_index_dep_score_r",
      "std_population_dens",
      "std_age_18_29",
      "std_age_65above",
      "std_foreign_perc",
      "std_price_change_per"
    )
  vs <- gsub("std_", paste0("std_w", d, "_"), vs)
  f2 <- paste0("std_index_dep_score_r * ", vs[1], " + ")
  f3 <- paste0(vs[-1], collapse = " + ")
  f <- paste0(f1, f2, f3, " + (1 + std_deprivation_comb | lsoa11)")
  f <- gsub("\n", "", f)
  
  #estimate model
  mlm <- lmer(f, data = data_sub.df)
  
  # plot interaction
  intv <- paste0("std_w", d, "_index_dep_score_r")
  int_tmp.pl <-
    interplot(
      mlm,
      var1 = "std_index_dep_score_r",
      var2 = intv,
      ci = 0.95,
      hist = TRUE,
      rfill = cols2[6]
    )
  lab <- paste0("Area deprivation (", d, "km)")
  if (d == "") {
    lab <- paste0("Area deprivation (adjacent)")
  }
  
  int_tmp.pl <- int_tmp.pl + labs(y = element_blank(),
                                  x = lab)
  int_tmp.pl <- int_tmp.pl + theme_bw()
  int_tmp.pl <-
    int_tmp.pl + theme(
      text = element_text(family = "Times New Roman", size = 20),
      # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.key = element_blank(),
      axis.text.y = element_text(colour = "black"),
      axis.title.x = element_text(colour =
                                    "black", margin = margin(2, 2, 2, 2, "pt")),
      axis.text.x = element_text(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(size = 20, colour = "black"),
      #axis.text.x = element_text(size=16),
      #axis.text.y = element_text(size=16),
      #axis.title.x = element_text(size=16),
      #axis.title.y = element_text(size=16)
    )
  int_tmp.pl <-
    int_tmp.pl + geom_hline(aes(yintercept = 0), linetype = "dashed")
  
  if (d == dists[1]) {
    yrange <- layer_scales(int_tmp.pl)$y$range$range
    yrange2 <-
      c(min(int_tmp.pl$layers[[1]]$data$ymax),
        max(int_tmp.pl$layers[[1]]$data$ymax))
  } else{
    yrange <-
      c(
        min(yrange[1], layer_scales(int_tmp.pl)$y$range$range[1]),
        max(yrange[2], layer_scales(int_tmp.pl)$y$range$range[2])
      )
    
    yrange2 <-
      c(min(yrange2[1], min(int_tmp.pl$layers[[1]]$data$ymax)),
        max(yrange[2], layer_scales(int_tmp.pl)$y$range$range[2]))
    
    
    
  }
  
  
  
  i <- which(dists == d)
  res[[i]] <- int_tmp.pl
  
  hist.data[[i]] <- mlm@frame[, intv]
  
  #clean up
  rm("mlm")
  gc()
  Sys.sleep(2)
}


# Bring on same scale and change design
# max.hist <- vector(mode = "numeric", length(dists))
# for(i in 1:length(dists)){
#   max.hist[i] <- ggplot_build(res[[i]])$data[[3]]$ymax
# }

# Bring on same scale and change design
yrange2[1] <- -0.063

res2 <- vector(mode = "list", length(dists))
for (i in 1:length(dists)) {
  #variable for hist
  var2_dt <- hist.data[[i]]
  
  # Range differences
  
  maxdiff <- (max(yrange2) - min(yrange2))
  break_var2 <- length(unique(var2_dt))
  
  if (break_var2 >= 100) {
    break_var2 <- 100
  }
  hist.out <- hist(var2_dt, breaks = break_var2, plot = FALSE)
  
  n.hist <- length(hist.out$mids)
  dist <- hist.out$mids[2] - hist.out$mids[1]
  hist.max <- max(hist.out$counts)
  
  histX <- data.frame(
    ymin = rep(min(yrange2) - maxdiff / 5, n.hist),
    ymax = hist.out$counts / hist.max * maxdiff / 5 + min(yrange2) - maxdiff /
      5,
    xmin = hist.out$mids - dist / 2,
    xmax = hist.out$mids + dist / 2
  )
  
  # if(i == 3){ # Manually correct (don't know why)
  #   histX[,c(1:2)] <- histX[,c(1:2)] -0.005
  # }
  
  esize = 0.5
  ralpha = 0.5
  rfill = cols2[6]
  ercolor = NA
  m1 <- res[[i]]$layers[[3]]$data
  m2 <- res[[i]]$layers[[2]]$data
  
  res[[i]]$labels$x
  
  tmp.pl2 <- ggplot() +
    geom_rect(
      data = histX,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      colour = cols3[1],
      fill = cols3[8],
      alpha = 0.6,
      size = 0.5
    ) +
    geom_blank(data = data.frame(x = c(0, 0), y = yrange2), aes(x = x, y = y)) +
    geom_ribbon(
      data = m1,
      aes_string(x = "fake", ymin = "lb", ymax = "ub"),
      alpha = ralpha,
      color = ercolor,
      fill = rfill
    ) +
    geom_line(data = m2,
              aes_string(x = "fake", y = "coef1"),
              colour = cols3[3]) +
    labs(y = element_blank(),
         x = res[[i]]$labels$x) +
    theme_bw() +
    theme(
      text = element_text(family = "Times New Roman", size = 20),
      # legend.position = c(0.95, 0.99), legend.justification = c(0.95,0.99),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black"),
      legend.key = element_blank(),
      axis.text.y = element_text(colour = "black"),
      axis.title.x = element_text(colour = "black", margin = margin(2, 2, 10, 2, "pt")),
      axis.text.x = element_text(colour = "black", margin = margin(2, 2, 2, 2, "pt")),
      strip.background = element_blank(),
      strip.text = element_text(size = 20, colour = "black"),
      #axis.text.x = element_text(size=16),
      #axis.text.y = element_text(size=16),
      #axis.title.x = element_text(size=16),
      #axis.title.y = element_text(size=16)
    ) +
    geom_hline(aes(yintercept = 0), linetype = "dashed")
  
  
  res2[[i]] <- tmp.pl2
  
}



# Export

cairo_ps(
  file = paste0("../03_Output/", "Figure5.eps"),
  width = 12,
  height = 14,
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
grid.arrange(
  grobs = res2,
  ncol = 2,
  left = textGrob(
    paste0("Effect of neighbourhood deprivation"),
    rot = 90,
    vjust = 1,
    gp = gpar(fontsize = 20, fontfamily = "Times New Roman")
  )
)

dev.off()



# Png
png(
  file = paste0("../03_Output/", "Figure5.png"),
  width = 12,
  height = 14,
  units = "in",
  res = 300
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
grid.arrange(
  grobs = res2,
  ncol = 2,
  left = textGrob(
    paste0("Effect of neighbourhood deprivation"),
    rot = 90,
    vjust = 1,
    gp = gpar(fontsize = 20, fontfamily = "Times New Roman")
  )
)

dev.off()




