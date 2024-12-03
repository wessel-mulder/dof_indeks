#############################
##### Clean up raw data #####
#############################
# incl. make a final species list

library(tidyverse)
library(chron)


######## clean up species list #########

# load IOC for spp and sub species levels
ioc <- read.csv("./data/other/IOC_species_and_ssp_only.csv")
names(ioc)[6] <- "latin"
ioc$latin <- gsub("_", " ", ioc$latin)

# load observations
observations <- as.data.frame(read_delim("./data/DOF_data_to_2020/pktobs.txt"))
head(observations)

# Load spp (species) data frame
spp <- read.delim("./data/DOF_data_to_2020/arter.txt",as.is=T)
str(spp)
spp <- spp %>% select(id, artnr, overartnr, latin, english, arttype, iucn, ibaantal)
names(spp) <- c("dof_id", "euring_id", "species_euring_id", "latin", "common", "type", "iucn", "iba_amnt")
spp$species_euring_id <- gsub("^$|^ $",NA, spp$species_euring_id) # remove blanks in sp.eur.id

# remove non target spp. # mammals, butterflies, amphibs, reptiles
spp <- spp[!grepl(paste(c("MA","AM","LE","RE"), collapse = '|'), spp$euring),] 
summary(spp)

# clean up type column
summary(as.factor(spp$type))
spp <- spp %>% mutate(type = case_when(type == "art" ~ "species",
                                       type == "underart" ~ "subspecies",
                                       type == "hybrid" ~ "hybrid",
                                       type == "ubestemt" ~ "undefined"))

# show undefined subspecies - just one
undefined_ssp <- spp[grepl("ssp", spp$common) & spp$type == "undefined",]


### match names to IOC - international ornithological committee ###
# try match Danish species level to IOC - all 552 directly matched
species_match <- inner_join( spp[spp$type == "species",], ioc[ioc$rank == "Species", -c(1,3,4)], by = "latin")

# try match Danish subspecies level to IOC - 88/89 directly matched
subspecies_match <- inner_join(spp[spp$type == "subspecies",], ioc[ioc$rank == "ssp", -c(1,3,4)], by = "latin")
not_found_subspecies <- setdiff(spp[spp$type == "subspecies","latin"], subspecies_match$latin)
not_found_subspecies # actually undefined ssp 
undefined_ssp <- rbind(undefined_ssp, spp[spp$latin == not_found_subspecies,]) # add to previous undef. ssp

# add type = undefined ssp
spp$type[spp$dof_id %in% c(undefined_ssp$dof_id)] <- "undefined_ssp"
summary(as.factor(spp$type))


# preliminary times seen (will be recalculated later after aggregating subspecies and cleaning observations data)
spp$times_seen <- sapply(spp$euring_id, function(x){sum(observations$artnr == x, na.rm = T)})
sum(spp$times_seen > 0) # 362 seen more than once

# remove species never seen
spp <- spp[spp$times_seen >= 1, ]

# exclude hybrids and undefined species
final_species_list <- spp[!is.na(spp$species_euring_id),]


# load danish birds appendix from Heldbjerg, H., A.D. Fox and T. Vikstrøm 2018: How can we improve citizen science based bird monitoring in Denmark? - Dansk Orn. Foren. Tidsskr. 112: 90-104
heldbjerg_2018 <- read_csv("./data/other/Heldbjerg_et_al_2018/Heldbjerg_Fox_and_Vikstrom_2018_appendix1.csv")
# add breeding status to species list
final_species_list$breeding_status <- heldbjerg_2018$Status[match(final_species_list$latin, heldbjerg_2018$Latin)]
# add last confirmed breeding
final_species_list$last_confirmed_breeding <- heldbjerg_2018$Last_confirmed_breeding[match(final_species_list$latin, heldbjerg_2018$Latin)]
# add invasive?
final_species_list$invasive <- heldbjerg_2018$Invasive[match(final_species_list$latin, heldbjerg_2018$Latin)]


# load danish birds appendix from Heldbjerg, H., D.P. Eskildsen, T. Vikstrøm & N.Y. Ali 2020: Udbredelse af danske ynglefugle for 100 år siden og i dag. DOFT 114: 127-140.
heldbjerg_2020 <- read_csv("./data/other/Heldbjerg_et_al_2020/DOFT 114_2020_Appendix2_Heldbjerg_et_al_2020.csv")
# add number of breeding (probable or confirmed) squares in atlas III to species list
final_species_list$Atlas_III_squares_with_breeding <- heldbjerg_2020$In_Atlas_III_squares_with_probable_or_confirmed_breeding[match(as.numeric(final_species_list$euring_id), heldbjerg_2020$Euring)]
# 0 in this column means they were in a previous atlas but not this one


######## clean up routes data #########

routes <- read.delim("./data/DOF_data_to_2020/pktrute.txt", as.is=T)
str(routes)

# # make factors
routes$rid <- as.factor(routes$rid) # Route ID as a factor
routes$oid <- as.factor(routes$oid) # observer ID as factor

# clean season column
routes$season[routes$season == "v"] <- "V" # some seasons with lower case: convert all to uppercase
routes$season[routes$season == "NULL"] <- NA # some routes have NULL for season (convert to NA)
routes$season <- droplevels(as.factor(routes$season)) # convert season to factor and drop unused levels

# clean date and time columns
routes$firstdate <- as.POSIXct(strptime(routes$firstdate, tz= "", format = "%Y-%m-%d")) # convert first date to R format
routes$firsttime <- chron(times. = paste0(routes$firsttime, ":00"), format = c(times = "h:m:s")) # convert first time to R format
routes <- add_column(routes, first.date.time = as.POSIXct(strptime(paste0(routes$firstdate, routes$firsttime), tz= "", format = "%Y-%m-%d %H:%M")), .after = "firsttime")
# routes$lastyear <- as.factor(routes$lastyear) # last year as factor

# make others factors
# routes$map <- as.factor(routes$map) # map as factor
# routes$aktiv <- as.factor(routes$aktiv) # active? as factor

# clean up other cols
routes$lon <- gsub(",", ".", routes$lon)
routes$lat <- gsub(",", ".", routes$lat)
routes$lon <- as.numeric(routes$lon) # longitude as numeric
routes$lat <- as.numeric(routes$lat) # latitde as numeric
routes$pkt <- as.integer(routes$pkt) # number of points as integer
str(routes)

# get essentials for locations
# routes.locs <- routes[,c("rid", "navn", "season","lon","lat")]
# str(routes.locs)

# check which are missing location info
summary(routes$lon)
summary(routes$lat)
r1 <- which(routes$lon==0) # 286
r2 <- which(routes$lat==0) # 286 missing
identical(r1,r2) # lat and lon are missing from the same routes
rm(r1, r2)

# clean up those missing location info (set NA to lat and long)
routes[routes$lat == 0, c("lat", "lon")] <- NA # replaces missing location info with NA (rather than 0)
length(routes$lat[is.na(routes$lat)]) / length(unique(routes$rid))*100 # 7.79 % missing route location data (257 routes)

# subset columns
routes <- routes %>% select(rid, oid, season, firstdate, firsttime, lastyear, lon, lat)

# save cleaned routes file
summary(routes)
write_csv(routes, "./data/clean_DOF_data/route_df_cleaned.csv")




######## clean up points data #########

points <- read.delim("./data/DOF_data_to_2020/pktpunkt.txt", as.is = T)
str(points)

# make some cols factors
points[c("pid","pktnr","rid")] <- lapply(points[c("pid","pktnr","rid")], factor) # convert point ID, point no. (in transect), and route ID to factors
summary(points)

# get essentials
# points.locs <- points[,c("pid","lon","lat","rid")]
# str(points.locs)

# check location info
summary(points$lon)
summary(points$lat)

# identify the zeros for each
p1 <- which(points$lon==0)
p2 <- which(points$lat==0) # lots of points missing lattitude (30963)
identical(p1,p2) # lat and lon are not missing from the same points
intersect(p1,p2) # all points missing lon are also missing lat
rm(p1, p2)

# make lat and long NA for these
points[points$lat == 0, c("lat", "lon")] <- NA

# clean location info
points$lon <- as.numeric(gsub(",", ".", points$lon))
points$lat <- as.numeric(gsub(",", ".", points$lat))

# save point locations
points <- distinct(points[,c(1,6:8)])
write_csv(points, "./data/clean_DOF_data/point_locations_cleaned.csv")






########## Clean up survey data ##########

surveys <- read.delim("./data/DOF_data_to_2020/pkttaelling.txt", as.is = T)

# surveys (sort out)
str(surveys)

# change class of some columns to factors
surveys[c("tid","rid","skydaekke","snow","indtastning_bekraeftet")] <- lapply(surveys[c("tid","rid","skydaekke","snow","indtastning_bekraeftet")], factor)

# clean dates and times
surveys$tdate <- as.POSIXct(strptime(surveys$tdate, tz= "", format = "%Y-%m-%d")) # convert survey date to R format
surveys$tyear <- as.numeric(surveys$tyear) # year of survey to factor
surveys$time1 <- chron(times. = paste0(surveys$time1, ":00"), format = c(times = "h:m:s")) # convert survey start time to R format
surveys$time2 <- chron(times. = paste0(surveys$time2, ":00"), format = c(times = "h:m:s")) # convert survey end time to R format

# add start date & time column
surveys <- add_column(surveys, start.date.time= as.POSIXct(strptime(paste0(surveys$tdate, surveys$time1), tz= "", format = "%Y-%m-%d %H:%M")), .after = "time2")
# add end date & time column
surveys <- add_column(surveys, end.date.time= as.POSIXct(strptime(paste0(surveys$tdate, surveys$time2), tz= "", format = "%Y-%m-%d %H:%M")), .after = "start.date.time")

# rename things
surveys <- surveys %>% rename(start_time = time1, end_time = time2, year = tyear, date = tdate, birds_seen = antalobs, points_surveyed = antalpktobs)

# check
summary(surveys)
colnames(surveys)


# subset columns
surveys <- surveys %>% select(tid, rid, date, year, start_time, end_time, birds_seen, points_surveyed)
# save
write_csv(surveys, "./data/clean_DOF_data/survey_df_cleaned.csv")





######## Make joined data frame #######
# by adding surveys and points dfs 
joined_points <- left_join(points, surveys, by = "rid") # One row for each point each year surveyed
length(which(is.na(joined_points$year)))
joined_points <- joined_points[-which(is.na(joined_points$year)),] # 334 points have NA for year (and all survey info)
summary(joined_points$year)
n_distinct(joined_points$year) # 46 years
table(joined_points$year)
hist(joined_points$year, breaks = 43, main = "Number of points by year", xlab = "Year")
abline(v = 1987-0.5, col = 2, lwd = 2)

#add on some route info
joined_points <- left_join(joined_points, routes[,!colnames(routes) %in% c("lat","lon")], by = "rid")

# filter out unecessary columns
colnames(joined_points)
joined_points <- joined_points %>% select(pid, rid, tid, oid, year, date, season, start_time, end_time, lat, lon)

# save cleaned and joined df
write_csv(joined_points, "./data/clean_DOF_data/cleaned_point_survey_route_data.csv")






####### Clean up observations ########

observations <- as.data.frame(read_delim("./data/DOF_data_to_2020/pktobs.txt"))
head(observations)
observations$antal_afrundet <- NULL # remove rounded counts
names(observations)[4:5] <- c("euring_id","number")

obs <- observations
summary(obs)
colSums(is.na(obs))

# subset to just species we are sure of
obs <- observations[which(observations$euring_id %in% final_species_list$euring_id),]
n_distinct(obs$euring_id) # 315 unique spp / ssp

# aggregate subspecies
nrow(obs[obs$euring_id == "00721",]) # cormorant ssp. seen 5802 times
obs$euring_id <- final_species_list$species_euring_id[match(obs$euring_id, final_species_list$euring_id)] # change subsp to species level euring ID
# obs <- obs[!is.na(obs$obsid),]
nrow(obs[obs$euring_id == "00721",]) # cormorant ssp. seen x times
n_distinct(obs$euring_id) # now 281 unique full species

# add abundance for subspecies that are now pooled to species level (could be counted separately)
head(obs)
aggr_obs <- aggregate(number ~ tid + pid + euring_id, data = obs, FUN = sum)
head(aggr_obs)


# now finish editing species list
# aggregate subspecies to species level if species present (then remove subspeciess)
final_species_list$species_euring_id[final_species_list$type == "subspecies"] %in% final_species_list$euring_id # all subsp full species are represented
final_species_list <- final_species_list[!final_species_list$type == "subspecies", ] # remove subspecies

# add actual number of times seen from cleaned obs data to species list
final_species_list$times_seen <- sapply(final_species_list$euring_id, function(x){sum(aggr_obs$euring_id == x, na.rm = T)})
hist(final_species_list$times_seen)

# birds that were breeding in one of the atlas studies (have a number, >0, in atlas III column)
atlas_III_birds <- final_species_list[which(final_species_list$Atlas_III_squares_with_breeding > 0), ]
# not probable or likely breeding in atlas III
leftover <- final_species_list[which(!final_species_list$dof_id %in% atlas_III_birds$dof_id), ] %>% arrange(breeding_status, Atlas_III_squares_with_breeding, -times_seen) 
# some were however breeding prior to atlas III
to_keep <- c("Fringilla montifringilla", "Circus cyaneus", "Lyrurus tetrix", "Ciconia nigra", "Columba livia") # based on manual check of those leftover
# identify species kept for analysis
final_species_list$use_in_analysis <- ifelse(final_species_list$latin %in% c(atlas_III_birds$latin, to_keep), 1, 0)

# save list of all species
write_csv(final_species_list,  "./data/clean_DOF_data/full_spp_list_w_nonbreeders.csv")

# remove non-breeding species
ultimate_spp_list <- final_species_list[final_species_list$use_in_analysis == 1, -c(10:15)]
#ultimate_spp_list <- ulti

# observations of non breedin species
nobreed_obs <- aggr_obs[aggr_obs$euring_id %in% final_species_list$euring_id[final_species_list$use_in_analysis == 0],]
nobreed_obs$date <- surveys$date[match(nobreed_obs$tid, surveys$tid)]
nobreed_obs$year <-  as.integer(word(as.character(nobreed_obs$date), 1, 1, sep = "-"))

# plot number seen over time compared to survey number
par(mfrow = c(2,1), mar = c(2.5,3,2,0))
plot(table(surveys$year) / max(table(surveys$year)), main = "non-breeder abundance per year", ylab = "relative index")
points(nobreed_obs$number / max(nobreed_obs$number) ~ nobreed_obs$year, pch = 1, cex = 1, col = "red")
# plot times seen over time compared to survey number
plot(table(surveys$year) / max(table(surveys$year)), main = "non-breeders occupancy per year", ylab = "relative index")
points(1975:2021, as.numeric(table(nobreed_obs$year) / max(table(nobreed_obs$year))), pch = 1, cex = 1, col = "red")

# remove non-breeding species from observations too
aggr_obs <- aggr_obs[aggr_obs$euring_id %in% ultimate_spp_list$euring_id,]

# check that the species list and observation data contain just the exact same species
identical(sort(unique(final_species_list$euring_id)),
          sort(unique(obs$euring_id)))

# save final species list for study
write_csv(ultimate_spp_list, "./data/clean_DOF_data/cleaned_spp_list.csv")

# save
write_csv(aggr_obs, "./data/clean_DOF_data/cleaned_observations.csv")





###### Clean up habitats #######

habitats <- as.data.frame(read_delim("./data/DOF_data_to_2020/pkthabitat.txt"))
head(habitats)

# changes
habitats <- habitats[-8]
names(habitats)[4:7] <- c("hab_code_1", "hab_code_2", "hab_code_3", "hab_code_3")
summary(habitats)
str(habitats)

# save
write_csv(habitats, "./data/clean_DOF_data/cleaned_habitats.csv")


















###### GY #######

# 
# # keep only species level (i.e. aggregate subspecies into their higher species level)
# 
# sapply(final_species_list$latin[final_species_list$type == "subspecies"], \(x){
#   
# })
# 
# # subspecies which have the higher level species also present - substitute with that info (except times seen column)
# final_species_list[final_species_list$type == "subspecies" & final_species_list$species_euring_id %in% final_species_list$euring_id, ]
# 
# 
# 
# final_species_list <- final_species_list[final_species_list$euring_id %in% final_species_list$species_euring_id,]
# 
# x <- final_species_list[!final_species_list$euring_id %in% final_species_list$species_euring_id,]
# 
# x$common[!x$species_euring_id %in% final_species_list$euring_id]
# 
# str(final_species_list)

# where same species has different obsid at same point
# sub <- obs[1:5000,]
# x <- distinct(rbind(data.frame(obsid = 3728292, tid = 1, pid = 24321, euring_id = "01730", number = 1), sub))
# head(x)
# x[]

# # aggregate subspecies to species level if species present (then remove subsepcies)
# final_species_list$species_euring_id[final_species_list$type == "subspecies"] %in% final_species_list$euring_id # all subsp full species are represented
# final_species_list <- final_species_list[!final_species_list$type == "subspecies", ] # remove subspecies


# load bird spp list from 2015 (agreed w/ Carsten for lidar chapter)
# breeding_2015 <- read_csv("./data/other/2014-16_species_list.csv")

# see which are on this list
# final_species_list$on_2015_list <- ifelse(word(final_species_list$latin, 1, 2) %in% gsub("_", " ", breeding_2015$scientific_name), 1, 0)

# separate those which are not (but leave in for now)
# not_on_2015_list <- final_species_list[final_species_list$on_2015_list == 0,]


