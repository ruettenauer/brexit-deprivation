#### Brexit Prepare UKHLS Data ####
#### Charlotte Haussmann & Tobias Ruettenauer ####
#### 2022/ 03 / 12 ####

rm(list = ls())

library(haven)
library(psych)
library(car)
library(plm)
library(DescTools)
library(margins)
library(stargazer)



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



#############
### UKHLS ###
#############

#UKHLS from Stata
data_full <- foreign::read.dta("all_ukhls.dta")


### Order data
data_full <- data_full[order(data_full$pidp, data_full$wave),]


### Feed forward citizenship variable
data_full$citzn1 <- ave(
  data_full$citzn1,
  data_full$pidp,
  FUN = function(x)
    repeat_last(x)
)





#################################
### Reduce to England, wave 8 ###
#################################

### Order data
data_full <- data_full[order(data_full$pidp, data_full$wave),]


### Feed back wave 10 referendum variables
data_full$w10_euref <- ave(
  data_full$euref,
  data_full$pidp,
  FUN = function(x)
    mean(x, na.rm = TRUE)
)
data_full$w10_voteeuref <- ave(
  data_full$voteeuref,
  data_full$pidp,
  FUN = function(x)
    mean(x, na.rm = TRUE)
)


### Feed forward last job nssec
table(data_full$jlnssec8_dv, useNA = "ifany")
data_full$ff_jlnssec8_dv <- ave(
  data_full$jlnssec8_dv,
  data_full$pidp,
  FUN = function(x)
    repeat_last(x)
)
table(data_full$ff_jlnssec8_dv, useNA = "ifany")


### Reduce
data <- data_full[which(data_full$gor_dv <= 9),]
data <- data[which(data$wave == 26),]








############################################
### Merge with macro IMD and brexit vote ###
############################################

load("Makro_final_orig.RData")

data <- dplyr::left_join(data, macro.df, by = "lsoa11")




#########################
### Prepare variables ###
#########################




#### Dependent variable, Brexit
#Should UK remain a member of the EU
#eumem 1= remain 2= leave
prop.table(table(data$eumem, useNA = "ifany")) #42,85 leave
data$eumem[data$eumem == 1] <- 0
data$eumem[data$eumem == 2] <- 1
names(data)[names(data) == "eumem"] <- "brexit_leave"

# Retrospective vote
table(data$w10_euref, useNA = "ifany")
data$brexit_voted <- ifelse(data$w10_euref == 1, 1, 0)
table(data$brexit_voted, useNA = "ifany")

table(data$w10_voteeuref, useNA = "ifany")
data$brexit_voted_leave <- ifelse(data$w10_voteeuref == 2, 1, 0)
table(data$brexit_voted_leave, useNA = "ifany")



#### Covariates


### political voting
#vote7	voted in last general election 1= Yes 2 No
prop.table(table(data$vote7, useNA = "ifany"))
data$vote7[which(data$vote7 < 0)] <- NA
names(data)[names(data) == "vote7"] <- "vote_last"
data$vote_last <- as.factor(data$vote_last)
prop.table(table(data$vote_last))





### Age

table(data$age_dv)

# Cut into bins
cuts <-
  seq(min(data$age_dv, na.rm = T), max(data$age_dv, na.rm = T), 5)
cuts <- c(cuts[-which(cuts > 90)],  max(data$age_dv, na.rm = T))
data$age_cat <-
  cut(data$age_dv, breaks = cuts, include.lowest = TRUE)
table(data$age_cat, useNA = "ifany")

data$age_cat <- relevel(as.factor(data$age_cat),
                        ref = as.numeric(getmode(data$age_cat)))


### sex_dv sex 1= male 2=female
data$sex_dv[data$sex_dv == 1] <- 1
data$sex_dv[data$sex_dv == 2] <- 0
names(data)[names(data) == "sex_dv"] <- "sex_male"
prop.table(table(data$sex_male, useNA = "ifany"))


### Migration to UK
table(data$bornuk_dv, useNA = "ifany")
data$bornuk_dv[which(data$bornuk_dv < 0)] <- NA
data$migback <- data$bornuk_dv - 1


### Migration background generation
table(data$generation, useNA = "ifany")
data$generation[which(data$generation < 0)] <- NA
data$migback_gen <- data$generation
data$migback_gen[data$migback_gen %in% c(4:6)] <- 0
table(data$migback_gen)


### Ethnic group
table(data$ethn_dv, useNA = "ifany")
data$ethn_dv[which(data$ethn_dv < 0)] <- NA

data$ethn_dv <- relevel(as.factor(data$ethn_dv),
                        ref = as.numeric(getmode(data$ethn_dv)))


### Ethnic group short
data$ethn_dv_short <- as.numeric(data$ethn_dv)
data$ethn_dv_short[data$ethn_dv_short %in% c(1:4)] <- 0
data$ethn_dv_short[data$ethn_dv_short %in% c(5:8)] <- 1
data$ethn_dv_short[data$ethn_dv_short %in% c(9:13)] <- 2
data$ethn_dv_short[data$ethn_dv_short %in% c(14:16)] <- 3
data$ethn_dv_short[data$ethn_dv_short %in% c(17:97)] <- 4

data$ethn_dv_short <- as.factor(data$ethn_dv_short)

table(data$ethn_dv_short)


### Education
table(data$hiqual_dv, useNA = "ifany")

data$hiqual_dv <- relevel(as.factor(data$hiqual_dv),
                          ref = as.numeric(getmode(data$hiqual_dv)))


### Child(ren) in household
table(data$nkids_dv)
data$child <- data$nkids_dv
data$child[data$child > 0] <- 1
table(data$child)


### Marital status
table(data$marstat_dv, useNA = "ifany")

data$marstat_dv <- relevel(as.factor(data$marstat_dv),
                           ref = as.numeric(getmode(data$marstat_dv)))



### jbstat	Current labour force status
prop.table(table(data$jbstat))
names(data)[names(data) == "jbstat"] <- "jobstat"
data$jobstat <- relevel(as.factor(data$jobstat),
                        ref = as.numeric(getmode(data$jobstat)))
table(data$jobstat)

# Unemployed
data$unemployed <- ifelse(data$jobstat == 3, 1, 0)
table(data$unemployed)


### NSEC

# Current job only
table(data$jbnssec8_dv, useNA = "ifany")
data$jobclass <- data$jbnssec8_dv
table(data$jobclass, useNA = "ifany")

# Last jobstatus
table(data$ff_jlnssec8_dv, useNA = "ifany")
data$jobclass_comb <- data$jobclass
oo <- which(is.na(data$jobclass))
data$jobclass_comb[oo] <- data$ff_jlnssec8_dv[oo]
table(data$jobclass_comb, useNA = "ifany")



# Highest jobclass observed in household
data$hh_jobclass <- ave(
  data$jobclass,
  data$hidp,
  FUN = function(x)
    min(x, na.rm = TRUE)
)
data$hh_jobclass[which(is.infinite(data$hh_jobclass))] <- NA
table(data$hh_jobclass, useNA = "ifany")



# Highest combined jobclass observed in household
data$hh_jobclass_comb <- ave(
  data$jobclass_comb,
  data$hidp,
  FUN = function(x)
    min(x, na.rm = TRUE)
)
data$hh_jobclass_comb[which(is.infinite(data$hh_jobclass_comb))] <-
  NA
table(data$hh_jobclass_comb, useNA = "ifany")



# Relevel
data$jobclass <- relevel(as.factor(data$jobclass),
                         ref = as.numeric(getmode(data$jobclass)))
data$jobclass_comb <- relevel(as.factor(data$jobclass_comb),
                              ref = as.numeric(getmode(data$jobclass_comb)))
data$hh_jobclass <- relevel(as.factor(data$hh_jobclass),
                            ref = as.numeric(getmode(data$hh_jobclass)))
data$hh_jobclass_comb <- relevel(as.factor(data$hh_jobclass_comb),
                                 ref = as.numeric(getmode(data$hh_jobclass_comb)))



### Household income
summary(data$fihhmnnet1_dv)
summary(data$ieqmoecd_dv)

data$hh_inc_oecd <- data$fihhmnnet1_dv / data$ieqmoecd_dv
summary(data$hh_inc_oecd)

# Income deciles
breaks <-
  quantile(data$hh_inc_oecd,
           probs = seq(0, 1, by = 0.1),
           na.rm = TRUE)
data$hh_inc_oecd_dec <-
  cut(data$hh_inc_oecd,
      breaks = breaks,
      include.lowest = TRUE)
table(data$hh_inc_oecd_dec)
data$hh_inc_oecd_dec <- relevel(as.factor(data$hh_inc_oecd_dec),
                                ref = as.numeric(getmode(data$hh_inc_oecd_dec)))




### Individual labour income
summary(data$fimnlabnet_dv)
data$ind_lab_inc <- data$fimnlabnet_dv

(q <- quantile(data$ind_lab_inc, seq(0, 1, 0.1)))
table(cut(data$ind_lab_inc, c(
  min(data$ind_lab_inc),
  -1,
  1,
  seq(500, 5000, 500),
  max(data$ind_lab_inc)
)))

# Income deciles
breaks <-
  quantile(data$ind_lab_inc,
           probs = seq(0, 1, by = 0.1),
           na.rm = TRUE)
breaks[2] <- -1
breaks[3] <- 0
breaks <- breaks[-c(4, 5)]
breaks
data$ind_lab_inc_dec <-
  cut(data$ind_lab_inc,
      breaks = breaks,
      include.lowest = TRUE)
table(data$ind_lab_inc_dec)
data$ind_lab_inc_dec <- relevel(as.factor(data$ind_lab_inc_dec),
                                ref = as.numeric(getmode(data$ind_lab_inc_dec)))




### Home owner
table(data$tenure_dv, useNA = "always")

data$owner <- data$tenure_dv
data$owner[data$owner %in% c(1, 2)] <- 1
data$owner[data$owner %in% c(3:8)] <- 0
table(data$owner)



### urban_dv Urban or rural area, derived; 1= urban 2= rural
prop.table(table(data$urban_dv))
data$urban_dv[data$urban_dv < 0] <- NA
data$area_rural <- data$urban_dv - 1

prop.table(table(data$area_rural))




### Subjective financial situation
prop.table(table(data$finfut))
data$finfut[which(data$finfut < 0)] <- NA
names(data)[names(data) == "finfut"] <- "financial_diff_fut"
prop.table(table(data$financial_diff_fut))

data$financial_diff_fut <-
  relevel(as.factor(data$financial_diff_fut), ref = 3)


prop.table(table(data$finnow))
data$finnow[which(data$finnow < 0)] <- NA
names(data)[names(data) == "finnow"] <- "financial_diff_now"
prop.table(table(data$financial_diff_now))

data$financial_diff_now <-
  relevel(as.factor(data$financial_diff_now), ref = 1)



### Health
#health scsf1 1= Excellent 5=poor
table(data$scsf1, useNA = "ifany")
data$scsf1[data$scsf1 < 0] <- NA
names(data)[names(data) == "scsf1"] <- "health_problems"
table(data$health_problems, useNA = "ifany")

### Well-being
table(data$scghq1_dv, useNA = "ifany")
data$wellbeing <- 36 - data$scghq1_dv
table(data$wellbeing, useNA = "ifany")






############################
### Material deprivation ###
############################

### Determine eligibility

# if kids<15 in hh, eligible for child deprivation
data$dep_child_elg <- ifelse(data$nkids015 > 0, 1, 0)

# if NO kids<15 in hh and n pensioners > 0, eligible for pensioner deprivation
data$dep_pensioner_elg <-
  ifelse(data$nkids015 == 0 & data$npensioner > 0, 1, 0)

### General material deprivation
vars <-
  c(
    "matdepa",
    "matdepd",
    "matdepe",
    "matdepf",
    "matdepg",
    "matdeph",
    "matdepi",
    "matdepj"
  )
dep <-
  apply(
    data[, vars],
    2,
    FUN = function(x)
      ifelse(x == 2, 1, 0)
  ) # dont have
w <-
  apply(
    data[, vars],
    2,
    FUN = function(x)
      ifelse(x == 1, 1, 0)
  ) # have
w <-
  apply(
    w,
    2,
    FUN = function(x)
      ave(
        x,
        FUN = function(x)
          mean(x, na.rm = TRUE)
      )
  ) # weights (percent have)
depw <- dep * w
n <-
  apply(
    dep,
    1,
    FUN = function(x)
      sum(ifelse(!is.na(x), 1, 0))
  ) # n valid items
oo_na <- which(n < (ncol(depw) - 2)) # allow for two missing items
data$mat_deprivation <- rowSums(depw, na.rm = TRUE) / n
data$mat_deprivation[oo_na] <- NA
summary(data$mat_deprivation, useNA = "ifany")
hist(data$mat_deprivation)

# Filter check against pensioner deprivation
prop.table(table(ifelse(is.na(
  data$mat_deprivation
), 1, 0), data$age_cat), margin = 2)




### General + child material deprivation
vars <-
  c(
    "matdepa",
    "matdepd",
    "matdepe",
    "matdepf",
    "matdepg",
    "matdeph",
    "matdepi",
    "matdepj",
    "cdepdo2",
    "cdepdo3",
    "cdepdo4",
    "cdepdo5",
    "cdepdo6",
    "cdephave1",
    "cdephave2",
    "cdephave3",
    "cdephave4"
  )
dep <-
  apply(
    data[, vars],
    2,
    FUN = function(x)
      ifelse(x == 2, 1, 0)
  ) # dont have
w <-
  apply(
    data[, vars],
    2,
    FUN = function(x)
      ifelse(x == 1, 1, 0)
  ) # have
w <-
  apply(
    w,
    2,
    FUN = function(x)
      ave(
        x,
        FUN = function(x)
          mean(x, na.rm = TRUE)
      )
  ) # weights (percent have)
depw <- dep * w
n <-
  apply(
    dep,
    1,
    FUN = function(x)
      sum(ifelse(!is.na(x), 1, 0))
  ) # n valid items
oo_na <- which(n < (ncol(depw) - 2)) # allow for two missing items
data$kid_deprivation <- rowSums(depw, na.rm = TRUE) / n
data$kid_deprivation[oo_na] <- NA
summary(data$kid_deprivation, useNA = "ifany")
hist(data$kid_deprivation)

# Filter check against pensioner deprivation
prop.table(table(ifelse(is.na(
  data$kid_deprivation
), 1, 0), data$dep_child_elg), margin = 2)





### Pensioner deprivation
vars <-
  c(
    "pdepa2",
    "pdepb2",
    "pdepc2",
    "pdepd2",
    "pdepe2",
    "pdepf2",
    "pdepg2",
    "pdeph2",
    "pdepi2",
    "pdepk2",
    "pdepl2",
    "pdepm2",
    "pdepn2",
    "pdepo2"
  )
have <-
  c(
    "pdepa1",
    "pdepb1",
    "pdepc1",
    "pdepd1",
    "pdepe1",
    "pdepf1",
    "pdepg1",
    "pdeph1",
    "pdepi1",
    "pdepk1",
    "pdepl1",
    "pdepm1",
    "pdepn1",
    "pdepo1"
  )
dep <-
  apply(
    data[, vars],
    2,
    FUN = function(x)
      ifelse(x == 1, 1, 0)
  ) # dont have
w <-
  apply(
    data[, have],
    2,
    FUN = function(x)
      ifelse(x == 1, 1, 0)
  ) # have
w <-
  apply(
    w,
    2,
    FUN = function(x)
      ave(
        x,
        FUN = function(x)
          mean(x, na.rm = TRUE)
      )
  ) # weights (percent have)
depw <- dep * w
n <-
  apply(
    dep,
    1,
    FUN = function(x)
      sum(ifelse(!is.na(x), 1, 0))
  ) # n valid items
oo_na <- which(n < (ncol(depw) - 2)) # allow for two missing items
data$pen_deprivation <- rowSums(depw, na.rm = TRUE) / n
data$pen_deprivation[oo_na] <- NA
summary(data$pen_deprivation, useNA = "ifany")
hist(data$pen_deprivation)

# Check NAs against each other
prop.table(table(ifelse(is.na(
  data$mat_deprivation
), 1, 0), ifelse(is.na(
  data$pen_deprivation
), 1, 0)), margin = 2)





### Combine into common index

# Start with kid and general
data$deprivation_comb <- ifelse(data$nkids015 > 0,
                                data$kid_deprivation,
                                data$mat_deprivation)

# Replace with pensioner if NA
oo <- which(is.na(data$deprivation_comb))
data$deprivation_comb[oo] <- data$pen_deprivation[oo]

# Summary
summary(data$deprivation_comb, useNA = "ifany")
hist(data$deprivation_comb, breaks = "Scott")
hist(data$deprivation_comb[data$deprivation_comb > 0], breaks = "Scott")


### Problems paying bills
table(data$xphsdba, useNA = "ifany")
data$problems_bills <- ifelse(data$xphsdba > 1, 1, 0)
table(data$problems_bills, useNA = "ifany")

table(data$xphsdct, useNA = "ifany")
data$problems_counciltax <- ifelse(data$xphsdct == 1, 1, 0)
table(data$problems_counciltax, useNA = "ifany")



### Save
save(data, file = "UKHLS_Brexit.RData")


