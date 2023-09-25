#### Brexit Prepare Census Data ####
#### Charlotte Haussmann & Tobias Ruettenauer ####
#### 2022/ 03 / 12 ####

rm(list = ls())

### Load packages
library(rgdal)
library(rgeos)
library(spdep)
library(sf)
library(readxl)
library(nomisr)



### Working Directory
setwd("C:/work/Forschung/Brexit-Deprivation/09_Replication/02_Data")


### Load data

load("Makro_final_orig.RData")
load("Census2011.RData")
load("Census2011_Codebook.RData")


###########################
### Prepare census data ###
###########################

macro_census.df <- census2011.df[, 1:2]

macro_census.df$population <- census2011.df$KS101EW0001
macro_census.df$hectar <- census2011.df$KS101EW0007
macro_census.df$population_dens <- census2011.df$KS101EW_7

macro_census.df$median_age <- census2011.df$KS102EW0019

macro_census.df$age_18_29 <-
  (census2011.df$KS102EW0008 + census2011.df$KS102EW0009 + census2011.df$KS102EW0010) / census2011.df$KS102EW0001

macro_census.df$age_65above <-
  (
    census2011.df$KS102EW0014 + census2011.df$KS102EW0015 +
      census2011.df$KS102EW0016 + census2011.df$KS102EW0017
  ) / census2011.df$KS102EW0001

macro_census.df$educ_no <-
  (census2011.df$KS501EW0002) / census2011.df$KS501EW0001

macro_census.df$educ_high <-
  (census2011.df$KS501EW0007) / census2011.df$KS501EW0001

macro_census.df$foreign_perc <-
  (census2011.df$KS204EW0001 - census2011.df$KS204EW_100) / census2011.df$KS204EW0001 * 100

macro_census.df$ethmin_perc <-
  (census2011.df$KS201EW0001 - census2011.df$KS201EW_100) / census2011.df$KS201EW0001 * 100

macro_census.df$nonchrist_perc <-
  (census2011.df$KS209EW_100 - census2011.df$KS209EW0002) / census2011.df$KS209EW_100 * 100


macro_census.df$manufacturingg_perc <-
  (census2011.df$KS605EW0002 + census2011.df$KS605EW0003 + census2011.df$KS605EW0004) / census2011.df$KS605EW0001 * 100




######################################
### Add increase in housing prices ###
######################################

hp.df <-
  read_xls(
    "hpssadataset46medianpricepaidforresidentialpropertiesbylsoa.xls",
    sheet = "Data",
    skip = 5,
    na = ":"
  )

names(hp.df) <- gsub("Year ending", "hprice", names(hp.df))
names(hp.df) <- tolower(names(hp.df))
names(hp.df) <- make.names(names(hp.df))

names(hp.df)[3] <- "lsoa11"


# Mean 2016
hp.df$price_2016 <-
  rowMeans(hp.df[, c("hprice.mar.2016",
                     "hprice.jun.2016",
                     "hprice.sep.2016",
                     "hprice.dec.2016")],
           na.rm = TRUE)
# Mean 2006
hp.df$price_2006 <-
  rowMeans(hp.df[, c("hprice.mar.2006",
                     "hprice.jun.2006",
                     "hprice.sep.2006",
                     "hprice.dec.2006")],
           na.rm = TRUE)

### Impute if missing

# Mean 2015
hp.df$price_2015 <-
  rowMeans(hp.df[, c(
    "hprice.mar.2015",
    "hprice.jun.2015",
    "hprice.sep.2015",
    "hprice.dec.2015",
    "hprice.mar.2014",
    "hprice.jun.2014",
    "hprice.sep.2014",
    "hprice.dec.2014"
  )],
  na.rm = TRUE)
# Mean 2005
hp.df$price_2005 <-
  rowMeans(hp.df[, c(
    "hprice.mar.2005",
    "hprice.jun.2005",
    "hprice.sep.2005",
    "hprice.dec.2005",
    "hprice.mar.2004",
    "hprice.jun.2004",
    "hprice.sep.2004",
    "hprice.dec.2004"
  )],
  na.rm = TRUE)

# Impute with previous year
oo <- which(is.na(hp.df$price_2016))
hp.df$price_2016[oo] <- hp.df$price_2015[oo]

oo <- which(is.na(hp.df$price_2006))
hp.df$price_2006[oo] <- hp.df$price_2005[oo]


# Impute with area mean
hp.df$price_2016_mean <- ave(
  hp.df$price_2016,
  hp.df$local.authority.code,
  FUN = function(x)
    mean(x, na.rm = TRUE)
)
hp.df$price_2006_mean <- ave(
  hp.df$price_2006,
  hp.df$local.authority.code,
  FUN = function(x)
    mean(x, na.rm = TRUE)
)

oo <- which(is.na(hp.df$price_2016))
hp.df$price_2016[oo] <- hp.df$price_2016_mean[oo]

oo <- which(is.na(hp.df$price_2006))
hp.df$price_2006[oo] <- hp.df$price_2006_mean[oo]




### Change
hp.df$price_change <- hp.df$price_2016 - hp.df$price_2006
hp.df$price_change_per <-
  hp.df$price_change / hp.df$price_2006 * 100

summary(hp.df$price_change)
summary(hp.df$price_change_per)


hp.df <-
  hp.df[, c("lsoa11", "price_2016", "price_change", "price_change_per")]


macro_census.df <- merge(
  macro_census.df,
  hp.df,
  by = "lsoa11",
  all.x = TRUE,
  all.y = FALSE
)


save(macro_census.df, file = "Census_origscale.RData")




########################################
### Compute averages in larger areas ###
########################################

# Read shape
en.sp <-
  st_read(dsn = "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3-shp",
          layer = "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3")

names(en.sp)[2] <- "lsoa11"

# Only England
en.sp <- en.sp[which(substr(en.sp$lsoa11, 1, 1) == "E"),]

# Merge DF
en.spdf <- merge(en.sp, macro_census.df, by = "lsoa11")

# Geographic centroids
coords <- st_geometry(st_centroid(en.spdf))

last <- which(names(en.spdf) == "price_change_per")
first <- which(names(en.spdf) == "population")

### W Queens nbs
q_nb <-
  poly2nb(en.spdf, queen = TRUE, row.names = rownames(en.spdf))
summary(q_nb)

# lw object, allow null nbs
q_lw <- nb2listw(q_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_q <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(q_lw, x, zero.policy = TRUE)
  )

# name prefix
colnames(w_q) <- paste0("w_", colnames(w_q))




### W in 2km
km2_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 2000,
             row.names = rownames(en.spdf))
summary(km2_nb)

# lw object, allow null nbs
nn2 <- which(unlist(lapply(
  km2_nb,
  FUN = function(x)
    x[1]
)) == 0)
km2_lw <- nb2listw(km2_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_2km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km2_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_2km)) {
  w_2km[nn2, i] <- NA
}

# name prefix
colnames(w_2km) <- paste0("w2_", colnames(w_2km))




### W in 5km
km5_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 5000,
             row.names = rownames(en.spdf))
summary(km5_nb)

# lw object, allow null nbs
nn5 <- which(unlist(lapply(
  km5_nb,
  FUN = function(x)
    x[1]
)) == 0)
km5_lw <- nb2listw(km5_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_5km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km5_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_5km)) {
  w_5km[nn5, i] <- NA
}

# name prefix
colnames(w_5km) <- paste0("w5_", colnames(w_5km))





### W in 10km
km10_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 10000,
             row.names = rownames(en.spdf))
summary(km10_nb)

# lw object, allow null nbs
nn10 <- which(unlist(lapply(
  km10_nb,
  FUN = function(x)
    x[1]
)) == 0)
km10_lw <- nb2listw(km10_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_10km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km10_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_10km)) {
  w_10km[nn10, i] <- NA
}

# name prefix
colnames(w_10km) <- paste0("w10_", colnames(w_10km))





### W in 15km
km15_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 15000,
             row.names = rownames(en.spdf))
summary(km15_nb)

# lw object, allow null nbs
nn15 <- which(unlist(lapply(
  km15_nb,
  FUN = function(x)
    x[1]
)) == 0)
km15_lw <- nb2listw(km15_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_15km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km15_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_15km)) {
  w_15km[nn15, i] <- NA
}

# name prefix
colnames(w_15km) <- paste0("w15_", colnames(w_15km))





### W in 20km
km20_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 20000,
             row.names = rownames(en.spdf))
summary(km20_nb)

# lw object, allow null nbs
nn20 <- which(unlist(lapply(
  km20_nb,
  FUN = function(x)
    x[1]
)) == 0)
km20_lw <- nb2listw(km20_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_20km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km20_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_20km)) {
  w_20km[nn20, i] <- NA
}

# name prefix
colnames(w_20km) <- paste0("w20_", colnames(w_20km))





### W in 35km
km35_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 35000,
             row.names = rownames(en.spdf))
summary(km35_nb)

# lw object, allow null nbs
nn35 <- which(unlist(lapply(
  km35_nb,
  FUN = function(x)
    x[1]
)) == 0)
km35_lw <- nb2listw(km35_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_35km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km35_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_35km)) {
  w_35km[nn35, i] <- NA
}

# name prefix
colnames(w_35km) <- paste0("w35_", colnames(w_35km))





### W in 50km
km50_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 50000,
             row.names = rownames(en.spdf))
summary(km50_nb)

# lw object, allow null nbs
nn50 <- which(unlist(lapply(
  km50_nb,
  FUN = function(x)
    x[1]
)) == 0)
km50_lw <- nb2listw(km50_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_50km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km50_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_50km)) {
  w_50km[nn50, i] <- NA
}

# name prefix
colnames(w_50km) <- paste0("w50_", colnames(w_50km))





### W in 75km
km75_nb <-
  dnearneigh(coords,
             d1 = 0,
             d2 = 75000,
             row.names = rownames(en.spdf))
summary(km75_nb)

# lw object, allow null nbs
nn75 <- which(unlist(lapply(
  km75_nb,
  FUN = function(x)
    x[1]
)) == 0)
km75_lw <- nb2listw(km75_nb, style = "W", zero.policy = TRUE)

# Lag listwise
w_75km <-
  apply(
    st_drop_geometry(en.spdf[, c(first:last)]),
    2,
    FUN = function(x)
      lag.listw(km75_lw, x, zero.policy = TRUE)
  )

# Set NA for empty nb sets
for (i in colnames(w_75km)) {
  w_75km[nn75, i] <- NA
}

# name prefix
colnames(w_75km) <- paste0("w75_", colnames(w_75km))




#####################
### Bind together ###
#####################

macro_census.df <-
  data.frame(
    st_drop_geometry(en.spdf[, -c(2:6)]),
    w_q,
    w_2km,
    w_5km,
    w_10km,
    w_15km,
    w_20km,
    w_35km,
    w_50km,
    w_75km
  )

macro.df <- merge(macro.df, macro_census.df, by = "lsoa11")

### Save orig data
save(macro.df, file = "Makro_final_orig.RData")
