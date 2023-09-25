#### Brexit Decriptive Statistics ####
#### Charlotte Haussmann & Tobias Ruettenauer ####
#### 2022/ 03 / 12 ####

rm(list = ls())

library(plyr)
library(haven)
library(psych)
library(car)
library(plm)
library(DescTools)
library(margins)
library(stargazer)
library(texreg)

library(sf)
library(spdep)
library(rmapshaper)
library(mapview)
library(tmap)
library(tmaptools)
library(maps)
library(viridisLite)
library(extrafont)
loadfonts()

library(lfe)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggeffects)
library(ggstatsplot)


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
  "w10_foreign_perc",
  "w10_ethmin_perc",
  "w10_manufacturingg_perc",
  "w10_price_change_per",
  "w20_index_dep_rank_r",
  "w20_index_dep_score_r",
  "w20_income_rank_r",
  "w20_income_score_r",
  "w20_population_dens",
  "w20_median_age",
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
  "w75_foreign_perc",
  "w75_ethmin_perc",
  "w75_manufacturingg_perc",
  "w75_price_change_per"
)


### Standardise vars
for (v in vars) {
  nv <- paste0("std_", v)
  data[, nv] <- as.vector(scale(data[, v]))
}


### Final model
lm4_6 <- felm(
  brexit_leave ~ std_deprivation_comb  +
    age_cat + sex_male + migback + ethn_dv_short + hiqual_dv + child + marstat_dv +
    unemployed + hh_inc_oecd_dec +
    std_index_dep_score_r +
    std_w20_index_dep_score_r +
    std_index_dep_score_r * std_w20_index_dep_score_r +
    std_population_dens + # controls
    std_median_age +
    std_foreign_perc +
    #std_ethmin_perc +
    std_price_change_per +
    std_w20_population_dens + # w_controls
    std_w20_median_age +
    std_w20_foreign_perc +
    #std_w20_ethmin_perc +
    std_w20_price_change_per
  | 0 | 0 | lsoa11,
  data = data
)

oo <- which(!rownames(data) %in% names(lm4_6$na.action))


### Subset selection
data_sub.df <- data[oo, ]





####################
### Descriptives ###
####################


data_sub.df$brexit_leave <- factor(
  data_sub.df$brexit_leave,
  levels = c("0", "1"),
  labels = c("Remain", "Leave")
)


#### Plot the distributions of Deprivation ####

### INdividual Deprivation
oo <- which(data_sub.df$deprivation_comb > 0)

mu <- ddply(data_sub.df[oo,],
            "brexit_leave",
            summarise,
            grp.mean = mean(deprivation_comb, na.rm = TRUE))
head(mu)

zp_ind <- ggplot(data_sub.df, aes(x = deprivation_comb)) +
  geom_histogram(
    aes(y = ..density..),
    position = "identity",
    alpha = 0.5,
    color = "#E69F00",
    fill = "#E69F00"
  ) +
  # geom_density(alpha = 0.6)+
  geom_vline(
    xintercept = mean(data_sub.df$deprivation_comb, na.rm = TRUE),
    color = "#E69F00",
    linetype = "dashed"
  ) +
  scale_color_manual(values = c("#E69F00")) +
  scale_fill_manual(values = c("#E69F00")) +
  labs(title = "Individual deprivation", x = "Standardized deprivation score", y = "Density") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 18))

zp_ind 



### Neighbourhood Deprivation
mu <- ddply(data_sub.df,
            "brexit_leave",
            summarise,
            grp.mean = mean(index_dep_score_r, na.rm = TRUE))
head(mu)

zp_nbh <- ggplot(data_sub.df, aes(x = index_dep_score_r / 100)) +
  geom_histogram(
    aes(y = ..density..),
    position = "identity",
    alpha = 0.5,
    color = "#56B4E9",
    fill = "#56B4E9"
  ) +
  #geom_density(alpha = 0.6)+
  geom_vline(
    xintercept = mean(data_sub.df$index_dep_score_r / 100, na.rm = TRUE),
    color = "#56B4E9",
    linetype = "dashed"
  ) +
  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  labs(title = "Neighbourhood deprivation", x = "Standardized deprivation score", y = "Density") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 18))

zp_nbh 



### Export 
cairo_pdf(file = paste("../03_Output/", "Hist_deprivation.pdf", sep=""), width = 13, height = 8, 
          bg = "white", family = "Times New Roman")
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
grid.arrange(zp_ind, zp_nbh, ncol = 2)
dev.off()

png(file = paste("../03_Output/", "Hist_deprivation.png", sep=""), width = 13, height = 8, 
    units = "in", res = 300, type = "cairo",
    bg = "white", family = "Times New Roman")
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
grid.arrange(zp_ind, zp_nbh, ncol = 2)
dev.off()




######################
### Commuting time ###
######################

# Load census 2011
load("C:/work/Forschung/Umweltgerichtigkeit XII - UK2/02_Data/Census2011_EW.RData")

### Individual Deprivation
mu <- weighted.mean(census2011.df$QS702EW0013,
                    census2011.df$KS101EW0001, na.rm = TRUE)

zp_com <- ggplot(census2011.df, aes(x = QS702EW0013)) +
  geom_histogram(
    aes(y = ..density.., weight = KS101EW0001),
    position = "identity",
    alpha = 0.5,
    color = "#56B4E9",
    fill = "#56B4E9"
  ) +
  # geom_density(alpha = 0.6)+
  geom_vline(xintercept = mu,
             color = "#E69F00",
             linetype = "dashed") +
  annotate(
    "text",
    x = mu + 4,
    y = 0.09,
    label = paste0("Mean: ", round(mu, 2)),
    color = "#E69F00",
    size = 6
  ) +
  scale_color_manual(values = c("#56B4E9")) +
  scale_fill_manual(values = c("#56B4E9")) +
  labs(title = "Census 2011 - Commuting distance \n Population weighted", x =
         "Commuting distance (km)", y = "Density") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman", size = 18))

zp_com


cairo_pdf(
  file = paste("../03_Output/", "Hist_commute.pdf", sep = ""),
  width = 8,
  height = 6,
  bg = "white",
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
zp_com
dev.off()

png(
  file = paste("../03_Output/", "Hist_commute.png", sep = ""),
  width = 8,
  height = 6,
  units = "in",
  res = 300,
  type = "cairo",
  bg = "white",
  family = "Times New Roman"
)
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
zp_com
dev.off()





############################
### Maps Macro variables ###
############################

### NUTS1 borders and cities
nuts1.sp <- st_read(dsn = "NUTS_Level_1_(January_2018)_Boundaries",
                    layer = "NUTS_Level_1_(January_2018)_Boundaries")

nuts1.il <-  ms_innerlines(nuts1.sp[1:9,])

data("world.cities")

en_cities <- world.cities[world.cities$country.etc == "UK",]
en_cities <- en_cities[en_cities$pop > 4e5,]
en_cities <-
  st_as_sf(en_cities, coords = c("long", "lat"), crs = 4326)
en_cities <- st_transform(en_cities, crs = st_crs(nuts1.sp))
en_cities <- en_cities[nuts1.sp[1:9,],]


### Deprivation Map

# Read shape
en.sp <- st_read(dsn = "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3-shp",
                 layer = "Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3")

names(en.sp)[2] <- "lsoa11"

# Only England
en.sp <- en.sp[which(substr(en.sp$lsoa11, 1, 1) == "E"),]

# Add data
load("Makro_final_orig.RData")
en.sp <- en.sp[, "lsoa11"]
en.spdf <- merge(en.sp, macro.df, by = "lsoa11")



### Overal deprivation, Map on LSOA level
mp1 <- tm_shape(en.spdf) +
  tm_fill(
    col = c("index_dep_score_r"),
    #style = "cont",
    style = "fisher",
    n = 8,
    title = c("Deprivation Score"),
    palette = viridis(
      n = 8,
      direction = -1,
      option = "G"
    ),
    legend.hist = FALSE
  ) +
  # tm_facets("var") +
  tm_layout(
    main.title = "Index of Multiple Deprivation",
    main.title.position = "center",
    title.snap.to.legend = TRUE,
    fontfamily = "Times New Roman",
    scale = 1.3
  ) +
  tm_shape(nuts1.il) +
  tm_lines(lwd  = 1, col = "#ffffff")
# tm_shape(en_cities) +
# tm_text(text = "name")

mp1 



### Income Deprivation Map

# Map on LSOA level
en.spdf$income_score_r <- en.spdf$income_score_r * 100
mp2 <- tm_shape(en.spdf) +
  tm_fill(
    col = c("income_score_r"),
    #style = "cont",
    style = "fisher",
    n = 8,
    title = c("Score (% deprived)"),
    palette = viridis(
      n = 8,
      direction = -1,
      option = "C"
    ),
    legend.hist = FALSE
  ) +
  # tm_facets("var") +
  tm_layout(
    main.title = "Income Deprivation",
    main.title.position = "center",
    title.snap.to.legend = TRUE,
    fontfamily = "Times New Roman",
    scale = 1.3
  ) +
  tm_shape(nuts1.il) +
  tm_lines(lwd  = 1, col = "#ffffff")
# tm_shape(en_cities) +
# tm_text(text = "name")

mp2




### Brexit Voteshare on LSOA

mp3 <- tm_shape(en.spdf) +
  tm_fill(
    col = c("Pct_Leave"),
    #style = "cont",
    style = "fisher",
    n = 8,
    title = c("Vote Share in %"),
    palette = viridis(
      n = 8,
      direction = -1,
      option = "C"
    ),
    legend.hist = FALSE
  ) +
  # tm_facets("var") +
  tm_layout(
    main.title = "Leave Vote",
    main.title.position = "center",
    title.snap.to.legend = TRUE,
    fontfamily = "Times New Roman",
    scale = 1.3
  ) +
  tm_shape(nuts1.il) +
  tm_lines(lwd  = 1, col = "#ffffff")
# tm_shape(en_cities) +
# tm_text(text = "name")





### Combine ###

# cairo_ps(file = paste("../03_Output/Figure1.eps", sep=""), 
#          width = 14, height = 10, bg = "white", family = "Times New Roman")
# par(mar=c(0,0,0,0))
# par(mfrow=c(1,1),oma=c(0,0,0,0))
# tmap_arrange(mp1, mp3, ncol = 2)
# dev.off()

jpeg(file = paste("../03_Output/Figure1.jpeg", sep=""), units = "in", res = 300,
     width = 14, height = 9, bg = "white", family = "Times New Roman", type = "cairo")
par(mar=c(0,0,0,0))
par(mfrow=c(1,1),oma=c(0,0,0,0))
tmap_arrange(mp1, mp3, ncol = 2)
dev.off()




