library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(sf)
library(ggplot2)
#code from #https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/

moveAKHI <- function(us){

  # convert it to Albers equal area
  us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  us_aea@data$id <- rownames(us_aea@data)
  
  # extract, then rotate, shrink & move alaska (and reset projection)
  # need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
  alaska <- us_aea[us_aea$STATEFP=="02",]
  alaska <- elide(alaska, rotate=-50)
  alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
  alaska <- elide(alaska, shift=c(-2100000, -2500000))
  proj4string(alaska) <- proj4string(us_aea)
  
  # extract, then rotate & shift hawaii
  hawaii <- us_aea[us_aea$STATEFP=="15",]
  hawaii <- elide(hawaii, rotate=-35)
  hawaii <- elide(hawaii, shift=c(5400000, -1400000))
  proj4string(hawaii) <- proj4string(us_aea)
  
  # extract, then rotate & shift hawaii
  PR <- us_aea[us_aea$STATEFP=="72",]
  PR <- elide(PR, rotate=24)
  PR <- elide(PR, scale=max(apply(bbox(PR), 1, diff)) * 3)
  PR <- elide(PR, shift=c(-1000000, -2600000))
  proj4string(PR) <- proj4string(us_aea)
  
  # remove old states and put new ones back in; note the different order

  us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
  us_aea <- rbind(us_aea, alaska, hawaii, PR)
  us_states <- as(us_aea, "sf")
}


shp <- readOGR(dsn="data/cb_2018_us_county_20m/")
shp <- moveAKHI(shp)
saveRDS(shp,"data/us_counties_transformed.rds")

shp <- readOGR(dsn="data/cb_2018_us_state_20m/")
shp <- moveAKHI(shp)
saveRDS(shp,"data/us_states_transformed.rds")


ggplot(data=shp) +
  geom_sf()


