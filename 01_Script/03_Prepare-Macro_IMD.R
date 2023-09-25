
#### Brexit Macro Data ####
#### Charlotte Haussmann & Tobias Ruettenauer ####
#### 2022/ 03 / 12 ####

rm(list=ls())

### Load packages
library(rgdal)
library(rgeos)
library(sf)
library(spdep)
library(seg)



### Working Directory
setwd("C:/work/Forschung/Brexit-Deprivation/09_Replication/02_Data")




##############################
### Load deprivaion scores ###
##############################


# Read deprivation index 2015 
DEP <-
  "File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv"

DEP <-
  read.csv(
    file = DEP,
    header = TRUE,
    sep = ",",
    dec = ".",
    stringsAsFactors = TRUE
  )
View(DEP)

# brexit results
BREX <- "EU_referendum_result_data.csv"
BREX <-
  read.csv(
    file = BREX,
    header = TRUE,
    sep = ",",
    dec = ".",
    stringsAsFactors = TRUE
  )
View(BREX)

# area.code an local athory code matchen
names(DEP)[names(DEP) == "Local.Authority.District.code..2013."] <-
  "Area_Code"

DF <- dplyr::left_join(DEP, BREX, by = "Area_Code")
DF <- dplyr::arrange(DF, id)
str(DF)



#################################
### Prepare deprivaion scores ###
#################################

### Overall

names(DF)[names(DF) == "Index.of.Multiple.Deprivation..IMD..Score"] <-
  "index_dep_score"
summary(DF$index_dep_score)
# Reverse Rank
DF$index_dep_score_r <- DF$index_dep_score
names(DF)[names(DF) == "Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived."] <-
  "index_dep_rank"
DF$index_dep_rank_r <-
  (max(DF$index_dep_rank) + 1 - DF$index_dep_rank) #/ sd(DF$index_dep_rank)
summary(DF$index_dep_rank_r)


### Income

names(DF)[names(DF) == "Income.Score..rate."] <- "income_score"
summary(DF$income_score)
# Reverse Rank
DF$income_score_r <- DF$income_score
summary(DF$income_score_r)
names(DF)[names(DF) == "Income.Rank..where.1.is.most.deprived."] <-
  "income_rank"
DF$income_rank_r <-
  (max(DF$income_rank) + 1 - DF$income_rank) #/ sd(DF$income_rank)
summary(DF$income_rank_r)


### Employment

names(DF)[names(DF) == "Employment.Score..rate."] <- "emp_score"
summary(DF$emp_score)
# Reverse Rank
DF$emp_score_r <- DF$emp_score
summary(DF$emp_score_r)
names(DF)[names(DF) == "Employment.Rank..where.1.is.most.deprived."] <-
  "emp_rank"
DF$emp_rank_r <-
  (max(DF$emp_rank) + 1 - DF$emp_rank) #/ sd(DF$emp_rank)
summary(DF$emp_rank_r)


### Education

names(DF)[names(DF) == "Education..Skills.and.Training.Score"] <-
  "educ_score"
summary(DF$educ_score)
# Reverse Rank
DF$educ_score_r <- DF$educ_score
summary(DF$educ_score_r)
names(DF)[names(DF) == "Education..Skills.and.Training.Rank..where.1.is.most.deprived."] <-
  "educ_rank"
DF$educ_rank_r <-
  (max(DF$educ_rank) + 1 - DF$educ_rank) #/ sd(DF$educ_rank)
summary(DF$educ_rank_r)


### Health

names(DF)[names(DF) == "Health.Deprivation.and.Disability.Score"] <-
  "health_score"
summary(DF$health_score)
# Reverse Rank
DF$health_score_r <- DF$health_score
summary(DF$health_score_r)
names(DF)[names(DF) == "Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived."] <-
  "health_rank"
DF$health_rank_r <-
  (max(DF$health_rank) + 1 - DF$health_rank) #/ sd(DF$health_rank)
summary(DF$health_rank_r)


### Crime

names(DF)[names(DF) == "Crime.Score"] <- "crime_score"
summary(DF$crime_score)
# Reverse Rank
DF$crime_score_r <- DF$crime_score
summary(DF$crime_score_r)
names(DF)[names(DF) == "Crime.Rank..where.1.is.most.deprived."] <-
  "crime_rank"
DF$crime_rank_r <-
  (max(DF$crime_rank) + 1 - DF$crime_rank) #/ sd(DF$crime_rank)
summary(DF$crime_rank_r)


### Housing

names(DF)[names(DF) == "Barriers.to.Housing.and.Services.Score"] <-
  "housing_score"
summary(DF$housing_score)
# Reverse Rank
DF$housing_score_r <- DF$housing_score
summary(DF$housing_score_r)
names(DF)[names(DF) == "Barriers.to.Housing.and.Services.Rank..where.1.is.most.deprived."] <-
  "housing_rank"
DF$housing_rank_r <-
  (max(DF$housing_rank) + 1 - DF$housing_rank) #/ sd(DF$housing_rank)
summary(DF$housing_rank_r)


### Environment

names(DF)[names(DF) == "Living.Environment.Score"] <- "enviro_score"
summary(DF$enviro_score)
# Reverse Rank
DF$enviro_score_r <- DF$enviro_score
summary(DF$enviro_score_r)
names(DF)[names(DF) == "Living.Environment.Rank..where.1.is.most.deprived."] <-
  "enviro_rank"
DF$enviro_rank_r <-
  (max(DF$enviro_rank) + 1 - DF$enviro_rank) #/ sd(DF$enviro_rank)
summary(DF$enviro_rank_r)

DF$lsoa_2011 <- DF$LSOA.code..2011.



#####################
### Young and old ###
#####################


### Overall

names(DF)[names(DF) == "Income.Deprivation.Affecting.Children.Index..IDACI..Score..rate."] <-
  "income_children_score"
summary(DF$income_children_score)
# Wertebereich standardisieren
DF$income_children_score_r <- DF$income_children_score
summary(DF$income_children_score_r)

names(DF)[names(DF) == "Income.Deprivation.Affecting.Children.Index..IDACI..Rank..where.1.is.most.deprived."] <-
  "income_children_rank"
DF$income_children_rank_r <-
  (max(DF$income_children_rank) + 1 - DF$income_children_rank) #/ sd(DF$income_children_rank)
summary(DF$income_children_rank_r)


### Income

names(DF)[names(DF) == "Income.Deprivation.Affecting.Older.People..IDAOPI..Score..rate."] <-
  "income_older_score"
summary(DF$income_older_score)
# Wertebereich standardisieren
DF$income_older_score_r <- DF$income_older_score
summary(DF$income_older_score_r)

names(DF)[names(DF) == "Income.Deprivation.Affecting.Older.People..IDAOPI..Rank..where.1.is.most.deprived."] <-
  "income_older_rank"
DF$income_older_rank_r <-
  (max(DF$income_older_rank) + 1 - DF$income_older_rank) #/ sd(DF$income_older_rank)
summary(DF$income_older_rank_r)





#### ---- Transform into percentile ranks ---- ####

vars_rank <- names(DF)[grepl("rank_r", names(DF))]

for (v in vars_rank) {
  cuts <- quantile(DF[, v], probs = seq(0, 1, 0.01), na.rm = TRUE)
  DF[, v] <-
    cut(DF[, v], cuts, labels = FALSE, include.lowest = TRUE)
  
}




##################################################
### Keep only relevent variables in Macro data ###
##################################################

names(DF)[1] <- "lsoa11"

vars <- c("lsoa11",
          "Electorate"              ,                                                                          
          "ExpectedBallots"         ,                                                                          
          "VerifiedBallotPapers"    ,                                                                          
          "Pct_Turnout"             ,                                                                          
          "Votes_Cast"              ,                                                                          
          "Valid_Votes"             ,                                                                          
          "Remain"                  ,                                                                          
          "Leave"                   ,                                                                          
          "Rejected_Ballots"        ,                                                                          
          "No_official_mark"        ,                                                                          
          "Voting_for_both_answers" ,                                                                          
          "Writing_or_mark"         ,                                                                          
          "Unmarked_or_void"        ,                                                                          
          "Pct_Remain"              ,                                                                          
          "Pct_Leave"               ,                                                                          
          "Pct_Rejected"            ,                                                                          
          "index_dep_score_r"       ,                                                                          
          #"min"                     ,                                                                          
          #"max"                     ,                                                                          
          "index_dep_rank_r"        ,                                                                          
          "income_score_r"          ,                                                                          
          "income_rank_r"           ,      
          "income_children_score_r"          ,                                                                          
          "income_children_rank_r"           ,  
          "income_older_score_r"          ,                                                                          
          "income_older_rank_r"           ,  
          "emp_score_r"             ,                                                                          
          "emp_rank_r"              ,                                                                          
          "educ_score_r"            ,                                                                          
          "educ_rank_r"             ,                                                                          
          "health_score_r"          ,                                                                          
          "health_rank_r"           ,                                                                          
          "crime_score_r"           ,                                                                          
          "crime_rank_r"            ,                                                                          
          "housing_score_r"         ,                                                                          
          "housing_rank_r"          ,                                                                          
          "enviro_score_r"          ,                                                                          
          "enviro_rank_r"           ,                                                                          
          #"lsoa_2011"               ,                                                                          
          #"leave_lagmean"           ,                                                                          
          #"moran_loc"               ,
          "Area_Code"               ,
          "Local.Authority.District.name..2013."
          )

DF <- DF[, vars]





########################################
### Compute averages in larger areas ###
########################################


en_imd_new_res.df <- DF

# Read shape 
en.sp <- st_read(dsn = "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3-shp",
                 layer = "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3")

names(en.sp)[2] <- "lsoa11"

# Only England
en.sp <- en.sp[which(substr(en.sp$lsoa11, 1, 1) == "E"), ]

# Merge DF
en.spdf <- merge(en.sp, en_imd_new_res.df, by = "lsoa11")

# Geographic centroids
coords <- st_geometry(st_centroid(en.spdf))


### W Queens nbs
q_nb <- poly2nb(en.spdf, queen = TRUE, row.names = rownames(en.spdf))
summary(q_nb)

# lw object, allow null nbs
q_lw <- nb2listw(q_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_q <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(q_lw, x, zero.policy = TRUE))

# name prefix
colnames(w_q) <- paste0("w_", colnames(w_q))




### W in 2km
km2_nb <- dnearneigh(coords, d1 = 0, d2 = 2000, row.names = rownames(en.spdf))
summary(km2_nb)

# lw object, allow null nbs
nn2 <- which(unlist(lapply(km2_nb, FUN = function(x) x[1])) == 0)
km2_lw <- nb2listw(km2_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_2km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km2_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_2km)){
  w_2km[nn2, i] <- NA
}

# name prefix
colnames(w_2km) <- paste0("w2_", colnames(w_2km))




### W in 5km
km5_nb <- dnearneigh(coords, d1 = 0, d2 = 5000, row.names = rownames(en.spdf))
summary(km5_nb)

# lw object, allow null nbs
nn5 <- which(unlist(lapply(km5_nb, FUN = function(x) x[1])) == 0)
km5_lw <- nb2listw(km5_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_5km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km5_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_5km)){
  w_5km[nn5, i] <- NA
}

# name prefix
colnames(w_5km) <- paste0("w5_", colnames(w_5km))




### W in 10km
km10_nb <- dnearneigh(coords, d1 = 0, d2 = 10000, row.names = rownames(en.spdf))
summary(km10_nb)

# lw object, allow null nbs
nn10 <- which(unlist(lapply(km10_nb, FUN = function(x) x[1])) == 0)
km10_lw <- nb2listw(km10_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_10km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km10_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_10km)){
  w_10km[nn10, i] <- NA
}

# name prefix
colnames(w_10km) <- paste0("w10_", colnames(w_10km))




### W in 15km
km15_nb <- dnearneigh(coords, d1 = 0, d2 = 15000, row.names = rownames(en.spdf))
summary(km15_nb)

# lw object, allow null nbs
nn15 <- which(unlist(lapply(km15_nb, FUN = function(x) x[1])) == 0)
km15_lw <- nb2listw(km15_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_15km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km15_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_15km)){
  w_15km[nn15, i] <- NA
}

# name prefix
colnames(w_15km) <- paste0("w15_", colnames(w_15km))





### W in 20km
km20_nb <- dnearneigh(coords, d1 = 0, d2 = 20000, row.names = rownames(en.spdf))
summary(km20_nb)

# lw object, allow null nbs
nn20 <- which(unlist(lapply(km20_nb, FUN = function(x) x[1])) == 0)
km20_lw <- nb2listw(km20_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_20km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km20_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_20km)){
  w_20km[nn20, i] <- NA
}

# name prefix
colnames(w_20km) <- paste0("w20_", colnames(w_20km))





### W in 35km
km35_nb <- dnearneigh(coords, d1 = 0, d2 = 35000, row.names = rownames(en.spdf))
summary(km35_nb)

# lw object, allow null nbs
nn35 <- which(unlist(lapply(km35_nb, FUN = function(x) x[1])) == 0)
km35_lw <- nb2listw(km35_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_35km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km35_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_35km)){
  w_35km[nn35, i] <- NA
}

# name prefix
colnames(w_35km) <- paste0("w35_", colnames(w_35km))




### W in 50km
km50_nb <- dnearneigh(coords, d1 = 0, d2 = 50000, row.names = rownames(en.spdf))
summary(km50_nb)

# lw object, allow null nbs
nn50 <- which(unlist(lapply(km50_nb, FUN = function(x) x[1])) == 0)
km50_lw <- nb2listw(km50_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_50km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km50_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_50km)){
  w_50km[nn50, i] <- NA
}

# name prefix
colnames(w_50km) <- paste0("w50_", colnames(w_50km))





### W in 75km
km75_nb <- dnearneigh(coords, d1 = 0, d2 = 75000, row.names = rownames(en.spdf))
summary(km75_nb)

# lw object, allow null nbs
nn75 <- which(unlist(lapply(km75_nb, FUN = function(x) x[1])) == 0)
km75_lw <- nb2listw(km75_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_75km <- apply(st_drop_geometry(en.spdf[, c(11:46)]), 2, FUN = function(x) lag.listw(km75_lw, x, zero.policy = TRUE))

# Set NA for empty nb sets
for(i in colnames(w_75km)){
  w_75km[nn75, i] <- NA
}

# name prefix
colnames(w_75km) <- paste0("w75_", colnames(w_75km))





### Bind together

macro.df <- data.frame(st_drop_geometry(en.spdf), w_q, w_2km, w_5km, 
                       w_10km, w_15km, w_20km, w_35km, w_50km, w_75km)






##########################################
### Compute deviations in larger areas ###
##########################################

# load("Census_rescaled.RData")

load("Census2011.RData")
load("Census2011_Codebook.RData")
macro_census_res.df <- census2011.df[, c("lsoa11", "KS101EW0001")]
names(macro_census_res.df)[2] <- "population"

en.spdf <- merge(en.spdf, macro_census_res.df[, c("lsoa11", "population")], 
                 by = "lsoa11", all.x = TRUE, all.y = FALSE)


### 5km
sd_5km <- st_drop_geometry(en.spdf)[, c("lsoa11"), drop = FALSE]
for(i in 1:length(km5_nb)){
  wids <- km5_nb[[i]]
  tmp <- en.spdf[c(i, wids), ]
  
  diff <- max(tmp$income_score_r) - min(tmp$income_score_r)
  diff_ind <- en.spdf$income_score_r[i] - min(tmp$income_score_r)
  
  sd_5km[i, "w5_diff_income_score_r"] <- diff
  sd_5km[i, "w5_diff_ind_income_score_r"] <- diff_ind

}

local.m5 <- localmoran(en.spdf$income_score_r, listw = km5_lw, 
                       alternative = "two.sided", zero.policy = TRUE, na.action = na.omit)

sd_5km$w5_moransi_income_score_r <- data.frame(local.m5)$Ii



### 10 km
sd_10km <- st_drop_geometry(en.spdf)[, c("lsoa11"), drop = FALSE]
for(i in 1:length(km10_nb)){
  wids <- km10_nb[[i]]
  tmp <- en.spdf[c(i, wids), ]
  
  diff <- max(tmp$income_score_r) - min(tmp$income_score_r)
  diff_ind <- en.spdf$income_score_r[i] - min(tmp$income_score_r)
  
  
  sd_10km[i, "w10_diff_income_score_r"] <- diff
  sd_10km[i, "w10_diff_ind_income_score_r"] <- diff_ind
  # sd_10km[i, "w10_segD500_income_score_r"] <- segD
  
}

local.m10 <- localmoran(en.spdf$income_score_r, listw = km10_lw, 
                       alternative = "two.sided", zero.policy = TRUE, na.action = na.omit)

sd_10km$w10_moransi_income_score_r <- data.frame(local.m10)$Ii



### 20 km
sd_20km <- st_drop_geometry(en.spdf)[, c("lsoa11"), drop = FALSE]
for(i in 1:length(km20_nb)){
  wids <- km20_nb[[i]]
  tmp <- en.spdf[c(i, wids), ]
  
  diff <- max(tmp$income_score_r) - min(tmp$income_score_r)
  diff_ind <- en.spdf$income_score_r[i] - min(tmp$income_score_r)
  
  
  sd_20km[i, "w20_diff_income_score_r"] <- diff
  sd_20km[i, "w20_diff_ind_income_score_r"] <- diff_ind
  # sd_20km[i, "w20_segD500_income_score_r"] <- segD
  
}

local.m20 <- localmoran(en.spdf$income_score_r, listw = km20_lw, 
                        alternative = "two.sided", zero.policy = TRUE, na.action = na.omit)

sd_20km$w20_moransi_income_score_r <- data.frame(local.m20)$Ii


### Bind together

macro.df <- merge(macro.df, sd_5km, by = "lsoa11")
macro.df <- merge(macro.df, sd_10km, by = "lsoa11")
macro.df <- merge(macro.df, sd_20km, by = "lsoa11")




### Save transformed data
# macro.df <- macro.df[, -c(2:6)]
save(macro.df, file = "Makro_final_orig.RData")


