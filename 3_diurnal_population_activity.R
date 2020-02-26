### INPUT section

### population grid from Urban Atlas 2012 (as created with preprocessor)
pop <- "/gpfs/work/ramacher/storage/EXPOSURE/population/UA_Population_grid_100m_HH.tif"

### commuter for transport and work environments in age group 15-64
### if an age group without commuting shall be calculated, set to 0
commuters <- 223500
#commuters <- 0

### set value for age distribution
#age_dist <- 0.135 ### 00 to 14 years
#age_dist <- 0.678 ### 15 to 64 years
#age_dist <- 0.187 ### 65 to 99 years
age_dist <- 1

### set name for age distribution
#age_group <- "00_14"
age_group <- "15_64"
#age_group <- "65_99"

### age group specific diurnal (24h) activity profiles:
### for weekdays and weekends &
### microenvironments HOME, WORK, TRANSPORT, OTHER
weekday_activity_profiles <- "/gpfs/work/ramacher/storage/EXPOSURE/activity_profiles/weekday_15_64.csv"
weekend_activity_profiles <- "/gpfs/work/ramacher/storage/EXPOSURE/activity_profiles/weekend_15_64.csv"

### output folder
output <- "/gpfs/work/ramacher/storage/EXPOSURE/activity_profiles/"

######
library(raster)
### read preprocessed population grid
pop <- raster(pop)
#plot(pop)
pop_sum <- sum(getValues(pop))

### apply age distribution
pop_sum <- round(pop_sum*age_dist)

### read activity profiles
weekday_activity_profiles <- read.csv(weekday_activity_profiles)
weekend_activity_profiles <- read.csv(weekend_activity_profiles)

### create diurnal activity profiles for weekdays and weekend
### for each microenvironment separately

### HOME Environment
## Diurnal weekDAY population sums
home_weekday <- round(rep(pop_sum,24)*weekday_activity_profiles$HOME)
## Diurnal weekEND population sums
home_weekend <- round(rep(pop_sum,24)*weekend_activity_profiles$HOME)

### OTHER Environment
## Diurnal weekDAY population sums
other_weekday <- round(rep(pop_sum,24)*weekday_activity_profiles$OTHER)
## Diurnal weekEND population sums
other_weekend <- round(rep(pop_sum,24)*weekend_activity_profiles$OTHER)

### WORK Environment
### only for working population (age 15 to 64)
### + commuting people between 9 a.m and  5 p.m.
commuter <- c(rep(0,8),0.5*commuters,rep(commuters,7),0.5*commuters,rep(0,7))
work_weekday <- round(rep(pop_sum,24)*weekday_activity_profiles$WORK)+commuter
work_weekend <- round(rep(pop_sum,24)*weekend_activity_profiles$WORK)+commuter

### TRANSPORT Environment
### + commuting people 7-9 a.m. and 5-7 p.m.
commuter <- c(rep(0,6),commuters,commuters,0.5*commuters,
              rep(0,7),0.5*commuters,commuters,commuters,rep(0,5))
transport_weekday <- round(rep(pop_sum,24)*weekday_activity_profiles$TRANSPORT)+commuter
transport_weekend <- round(rep(pop_sum,24)*weekend_activity_profiles$TRANSPORT)+commuter


### WRITE diurnal population for all microenvironments to output directory

write.csv(cbind(home_weekday,work_weekday,other_weekday,transport_weekday,
                home_weekend,work_weekend,other_weekend,transport_weekend),
          paste0(output,"diurnal_pop_",age_group,".csv"), quote = F, row.names = F)

### plot & check weekday
plot(home_weekday+work_weekday+other_weekday+transport_weekday, ylim=c(0,pop_sum+commuters))
lines(home_weekday)
lines(work_weekday)
lines(transport_weekday)
lines(other_weekday)
### plot & check weekend
#plot(home_weekend+work_weekend+other_weekend+transport_weekend, ylim=c(0,pop_sum+commuters))
#lines(home_weekend)
#lines(work_weekend)
#lines(transport_weekend)
#lines(other_weekend)