#### Brexit get Census Data ####
#### Charlotte Haussmann & Tobias Ruettenauer ####
#### 2022/ 03 / 12 ####

rm(list=ls())

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





#######################
### Census 2011 API ###
#######################

Sys.setenv(NOMIS_API_KEY = "Add_your_key_here")
nomis_api_key(check_env = TRUE)

x <- nomis_data_info()

### Get key statistics ids
oo <-
  which(grepl("KS\\d+EW", x$name.value) &
          !grepl("- Males|- Females", x$name.value)) # exclude separation by sex
ksids <- x$id[oo]

q <- nomis_overview(ksids[1])
a <-
  nomis_get_metadata(id = ksids[1],
                     concept = "GEOGRAPHY",
                     type = "type")

for (i in ksids) {
  nd <- nomis_get_metadata(id = i)
  if ("RURAL_URBAN" %in% nd$conceptref) {
    UR <- TRUE
  } else{
    UR <- FALSE
  }
  if ("C_SEX" %in% nd$conceptref) {
    SEX <- TRUE
  } else{
    SEX <- FALSE
  }
  
  if (UR == TRUE) {
    if (SEX == TRUE) {
      # Split the API requests into countries
      # tmp_sc <- nomis_get_data(id = i, time = "2011", geography = "TYPE231",
      #                          measures = 20100, RURAL_URBAN = 0, C_SEX = 0)
      tmp_en <-
        nomis_get_data(
          id = i,
          time = "2011",
          geography = "TYPE298",
          measures = 20100,
          RURAL_URBAN = 0,
          C_SEX = 0
        )
      # tmp_ni <- nomis_get_data(id = i, time = "2011", geography = "TYPE258",
      #                          measures = 20100, RURAL_URBAN = 0, C_SEX = 0)
      #
    } else{
      # Split the API requests into countries
      # tmp_sc <- nomis_get_data(id = i, time = "2011", geography = "TYPE231",
      #                          measures = 20100, RURAL_URBAN = 0)
      tmp_en <-
        nomis_get_data(
          id = i,
          time = "2011",
          geography = "TYPE298",
          measures = 20100,
          RURAL_URBAN = 0
        )
      # tmp_ni <- nomis_get_data(id = i, time = "2011", geography = "TYPE258",
      #                          measures = 20100, RURAL_URBAN = 0)
    }
  } else{
    if (SEX == TRUE) {
      # Split the API requests into countries
      # tmp_sc <- nomis_get_data(id = i, time = "2011", geography = "TYPE231",
      #                          measures = 20100, C_SEX = 0)
      tmp_en <-
        nomis_get_data(
          id = i,
          time = "2011",
          geography = "TYPE298",
          measures = 20100,
          C_SEX = 0
        )
      # tmp_ni <- nomis_get_data(id = i, time = "2011", geography = "TYPE258",
      #                          measures = 20100, C_SEX = 0)
    } else{
      # Split the API requests into countries
      # tmp_sc <- nomis_get_data(id = i, time = "2011", geography = "TYPE231",
      #                          measures = 20100)
      tmp_en <-
        nomis_get_data(
          id = i,
          time = "2011",
          geography = "TYPE298",
          measures = 20100
        )
      # tmp_ni <- nomis_get_data(id = i, time = "2011", geography = "TYPE258",
      #                          measures = 20100)
    }
    
  }
  
  
  
  # Append
  # ks_tmp <- rbind(tmp_en, tmp_sc, tmp_ni)
  ks_tmp <- tmp_en
  
  
  
  # Make lower case names
  names(ks_tmp) <- tolower(names(ks_tmp))
  names(ks_tmp)[names(ks_tmp) == "geography_code"] <- "lsoa11"
  names(ks_tmp)[names(ks_tmp) == "geography_name"] <- "name"
  
  # # Truncate cell name
  # # ks_tmp$cell_name <- substr(ks_tmp$cell_name, 1, 30)
  # ks_tmp$cell_name <- gsub(".", "_", make.names(ks_tmp$cell_name), fixed = TRUE)
  # ks_tmp$cell_name <- gsub("__", "_", ks_tmp$cell_name)
  # ks_tmp$cell_name <- gsub("_$", "", ks_tmp$cell_name)
  # ks_tmp$cell_name <- tolower(ks_tmp$cell_name)
  
  # replace weird cell codes
  onlynum <- which(grepl("^[[:digit:]]+$", ks_tmp$cell_code))
  if (length(onlynum) != 0) {
    code <- substr(ks_tmp$cell_code[-onlynum][1], 1, 7)
    ks_tmp$cell_code[onlynum] <-
      paste0(code, "_", ks_tmp$cell_code[onlynum])
  }
  
  # save codebook
  ks_cb <-
    unique(ks_tmp[, c("date", "cell_type", "cell", "cell_code", "cell_name")])
  
  ### Reshape
  ks_res <-
    tidyr::pivot_wider(
      ks_tmp,
      id_cols = c("lsoa11", "name"),
      names_from = "cell_code",
      values_from = "obs_value"
    )
  
  ### Merge
  if (i == ksids[1]) {
    census_keystat.df <- ks_res
    census_keystat_cb.df <- ks_cb
  } else{
    census_keystat.df <-
      merge(census_keystat.df,
            ks_res,
            by = c("lsoa11", "name"),
            all = TRUE)
    census_keystat_cb.df <- rbind(census_keystat_cb.df, ks_cb)
  }
  
}

# ### Get some quick statistics ids
# quickdf <- c("QS203UK", "QS611UK", "QS302UK", "QS417UK")
#
# oo <- which(grepl(paste(quickdf, collapse = "|"), x$name.value))
# qsids <- x$id[oo]
#
# # q <- nomis_overview(qsids[1])
# # a <- nomis_get_metadata(id = qsids[1], concept = "GEOGRAPHY", type = "type")
#
# for(i in qsids){
#
#   # Split the API requests into countries
#   tmp_sc <- nomis_get_data(id = i, time = "2011", geography = "TYPE231",
#                            measures = 20100)
#   tmp_en <- nomis_get_data(id = i, time = "2011", geography = "TYPE298",
#                            measures = 20100)
#   tmp_ni <- nomis_get_data(id = i, time = "2011", geography = "TYPE258",
#                            measures = 20100)
#
#   # Append
#   ks_tmp <- rbind(tmp_en, tmp_sc, tmp_ni)
#
#   # Make lower case names
#   names(ks_tmp) <- tolower(names(ks_tmp))
#   names(ks_tmp)[names(ks_tmp) == "geography_code"] <- "lsoa11"
#   names(ks_tmp)[names(ks_tmp) == "geography_name"] <- "name"
#
#   # # Truncate cell name
#   # # ks_tmp$cell_name <- substr(ks_tmp$cell_name, 1, 30)
#   # ks_tmp$cell_name <- gsub(".", "_", make.names(ks_tmp$cell_name), fixed = TRUE)
#   # ks_tmp$cell_name <- gsub("__", "_", ks_tmp$cell_name)
#   # ks_tmp$cell_name <- gsub("_$", "", ks_tmp$cell_name)
#   # ks_tmp$cell_name <- tolower(ks_tmp$cell_name)
#
#   # Correct non-standard naming
#   if(!any(names(ks_tmp) %in% "cell_code")){
#     nn <- grep("^c_.*_code$", names(ks_tmp))
#     repl <- gsub("^(c_.*)_code$", "\\1", names(ks_tmp)[nn])
#     names(ks_tmp) <- gsub(repl, "cell", names(ks_tmp))
#   }
#
#   # replace weird cell codes
#   onlynum <- which(grepl("^[[:digit:]]+$", ks_tmp$cell_code))
#   if(length(onlynum) != 0){
#     code <- substr(ks_tmp$cell_code[-onlynum][1], 1, 7)
#     ks_tmp$cell_code[onlynum] <- paste0(code, "_", ks_tmp$cell_code[onlynum])
#   }
#
#   # save codebook
#   ks_cb <- unique(ks_tmp[, c("date", "cell_type", "cell", "cell_code", "cell_name")])
#
#   ### Reshape
#   ks_res <- tidyr::pivot_wider(ks_tmp, id_cols = c("lsoa11", "name"),
#                                names_from = "cell_code",
#                                values_from = "obs_value")
#
#   ### Merge
#   if(i == qsids[1]){
#     census_quickstat.df <- ks_res
#     census_quickstat_cb.df <- ks_cb
#   }else{
#     census_quickstat.df <- merge(census_quickstat.df, ks_res, by = c("lsoa11", "name"), all = TRUE)
#     census_quickstat_cb.df <- rbind(census_quickstat_cb.df, ks_cb)
#   }
#
# }
#
# save(census_quickstat.df, file = "Census_Quickstats.RData")
#
#
#
#
# ### Combine to one df and one codebook
# census2011.df <- merge(census_keystat.df, census_quickstat.df,
#                        by = c("lsoa11", "name"), all = TRUE)
#
# census2011_cb.df <- rbind(census_keystat_cb.df, census_quickstat_cb.df)




census2011.df <- census_keystat.df
census2011_cb.df <- census_keystat_cb.df


# Save
save(census2011.df, file = "Census2011.RData")
save(census2011_cb.df, file = "Census2011_Codebook.RData")
