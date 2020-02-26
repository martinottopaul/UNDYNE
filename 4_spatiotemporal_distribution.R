spatiotemporal_distribution <- function(OSM)
{
  ###### spatiotemporal distribution
  ### ncdf output for diurnal cycle at weekdays and weekends for all microenvironments
  
  ### directory with prepared UrbanAtlas microenvironment grids/raster files from
  ### 2a_UrbanAtlas_microenvironments.R
  ua_me_home <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/ua_landuse/me_home_ua2012.tif"
  ua_me_work <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/ua_landuse/me_work_ua2012.tif"
  ua_me_other <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/ua_landuse/me_other_ua2012.tif"
  ua_me_transport <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/ua_landuse/me_transport_ua2012.tif"
  
  ### USE UrbanAtlas2012 Transport Environment or OSM Transport Microenvironments?
  OSM <- OSM
  
  ### directory with prepared OSM microenvironment grids/raster files from
  ### 2a_UrbanAtlas_microenvironments.R
  if(OSM == TRUE)
  {
    osm_me_foot <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_foot_osm.grd"
    osm_me_bike <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_bike_osm.tif"
    #osm_me_car <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_car_osm_secondary.tif"
    osm_me_car <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_car_osm_tertiary.tif"
    #osm_me_car <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_car_osm_residential.tif"
    osm_me_bus <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_bus_osm.tif"
    osm_me_subway <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_subway_osm.tif"
    osm_me_suburban <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_suburbantrains_osm.tif"
    osm_me_regional <- "/gpfs/work/ramacher/storage/EXPOSURE/microenvironments/osm/me_regionaltrains_osm.tif"
    
    ### Modal Splits for OSM Transport Environments
    # Modal split --> by foot, by bike, by car, by public transport
    modal_split <- c(0.27,0.15,0.36,0.22)
    # Public Transportation split --> bus, subway, suburban railway, regional railway
    public_split <- c(0.3625, 0.3225, 0.2525, 0.0625)
  }
  
  ### prepared with 3_diurnal_population_activity.R
  diurnal_population <- "/gpfs/work/ramacher/storage/EXPOSURE/activity_profiles/diurnal_pop_15_64.csv"
  
  ### do you want to write ncdf output?
  nc_out <- FALSE
  ### where?
  output_folder <- "/gpfs/work/ramacher/storage/EXPOSURE/population/"
  
  ### functions and libraries
  require(raster)
  require(ncdf4)
  source("/gpfs/work/ramacher/storage/EXPOSURE/utility_functions/diurnal_population_to_me_distribution.R")
  
  ### read diurnal population profiles
  diurnal_population <- read.csv(diurnal_population)
  
  ### spatiotemporal distribution of me specific diurnal population to microenvironments
  me_home_weekday <- dynamo(ua_me_home, diurnal_population$home_weekday)
  me_home_weekend <- dynamo(ua_me_home, diurnal_population$home_weekend)
  
  me_work_weekday <- dynamo(ua_me_work, diurnal_population$work_weekday)
  me_work_weekend <- dynamo(ua_me_work, diurnal_population$work_weekend)
  
  me_other_weekday <- dynamo(ua_me_other, diurnal_population$other_weekday)
  me_other_weekend <- dynamo(ua_me_other, diurnal_population$other_weekend)
  
  if(OSM == FALSE)
  {
    me_transport_weekday <- dynamo(ua_me_transport, diurnal_population$transport_weekday)
    me_transport_weekend <- dynamo(ua_me_transport, diurnal_population$transport_weekend)
    
    me_total_weekday <- me_home_weekday+me_work_weekday+me_other_weekday+me_transport_weekday
    me_total_weekend <- me_home_weekend+me_work_weekend+me_other_weekend+me_transport_weekend
    
    weekday <- list(me_home_weekday,me_work_weekday,me_other_weekday,me_transport_weekday)
    weekend <- list(me_home_weekend,me_work_weekend,me_other_weekend,me_transport_weekend)
    return(list(weekday,weekend))
    
  }
  
  if(OSM == TRUE)
  {
    me_foot_weekday <- dynamo(osm_me_foot,diurnal_population$transport_weekday*modal_split[1])
    me_foot_weekend <- dynamo(osm_me_foot,diurnal_population$transport_weekend*modal_split[1])
    
    me_bike_weekday <- dynamo(osm_me_bike,diurnal_population$transport_weekday*modal_split[2])
    me_bike_weekend <- dynamo(osm_me_bike,diurnal_population$transport_weekend*modal_split[2])
    
    me_car_weekday <- dynamo(osm_me_car,diurnal_population$transport_weekday*modal_split[3])
    me_car_weekend <- dynamo(osm_me_car,diurnal_population$transport_weekend*modal_split[3])
    
    me_bus_weekday <- dynamo(osm_me_bus,diurnal_population$transport_weekday*modal_split[4]*public_split[1])
    me_bus_weekend <- dynamo(osm_me_bus,diurnal_population$transport_weekend*modal_split[4]*public_split[1])
    
    me_subway_weekday <- dynamo(osm_me_subway,diurnal_population$transport_weekday*modal_split[4]*public_split[2])
    me_subway_weekend <- dynamo(osm_me_subway,diurnal_population$transport_weekend*modal_split[4]*public_split[2])
    
    me_suburban_weekday <- dynamo(osm_me_suburban,diurnal_population$transport_weekday*modal_split[4]*public_split[3])
    me_suburban_weekend <- dynamo(osm_me_suburban,diurnal_population$transport_weekend*modal_split[4]*public_split[3])
    
    me_regional_weekday <- dynamo(osm_me_regional,diurnal_population$transport_weekday*modal_split[4]*public_split[4])
    me_regional_weekend <- dynamo(osm_me_regional,diurnal_population$transport_weekend*modal_split[4]*public_split[4])
    
    me_transport_weekday <- me_foot_weekday+me_bike_weekday+me_car_weekday+me_bus_weekday+me_suburban_weekday+me_suburban_weekday+me_regional_weekday
    me_transport_weekend <- me_foot_weekend+me_bike_weekend+me_car_weekend+me_bus_weekend+me_suburban_weekend+me_suburban_weekend+me_regional_weekend
    
    me_total_weekday <- me_home_weekday+me_work_weekday+me_other_weekday+me_transport_weekday
    me_total_weekend <- me_home_weekend+me_work_weekend+me_other_weekend+me_transport_weekend
    
    weekday <- list(me_home_weekday,me_work_weekday,me_other_weekday,me_foot_weekday,
                    me_bike_weekday,me_car_weekday,me_bus_weekday,me_subway_weekday,
                    me_suburban_weekday,me_regional_weekday,me_transport_weekday)
    weekend <- list(me_home_weekend,me_work_weekend,me_other_weekend,me_foot_weekend,
                    me_bike_weekend,me_car_weekend,me_bus_weekend,me_subway_weekend,
                    me_suburban_weekend,me_regional_weekend,me_transport_weekend)
    
    return(list(weekday,weekend))
  }
}