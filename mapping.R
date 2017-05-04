#### Holly Self #####
## MSc AMFE Univeristy of Aberdeen ##
## ZO5901 Research Project ##

#### Mapping trials ####

#### googlevis- making maps/GIS tools ####

## Explore locations

library("googleVis")

tracker.clean$location <- paste(tracker.clean$Latitude, tracker.clean$Longitude,  sep = ":")

## Practise plotting subset of sightings

# make subset
tracker.sample <- tracker.clean[1:20,]


# plot subset
sightings.sub <- gvisMap(tracker.sample, "location", "Species", 
                     options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                                  mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(sightings.sub)

#### Spinners ####

tracker.spin <- tracker.clean[tracker.clean$Species == "SPN Spinner Dolphin",]
tracker.spin <- tracker.spin[1:100,]

sightings.spin <- gvisMap(tracker.spin, "location", "Species", 
                     options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                                  mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(sightings.spin)

#### Bottlenose ####

tracker.bnd <- tracker.clean[tracker.clean$Species == "BND Bottlenose Dolphin",]
tracker.bnd <- tracker.bnd[2:200,]

sightings.bnd.2 <- gvisMap(tracker.bnd, "location", "Species", 
                     options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                                  mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(sightings.bnd.2)

sightings.bnd <- gvisMap(tracker.spin, "location", "Species", 
                     options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                                  mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(sightings.bnd)

#### FKW ####

tracker.fkw <- tracker.clean[tracker.clean$Species == "FKW False Killer Whale",]

sightings.fkw <- gvisMap(tracker.fkw, "location", "Species", 
                           options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                                        mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(sightings.fkw)

#### shp files ####

library("rgdal")
# read in downloaded coastline 
coastline <- readOGR(dsn = "C:\\Users\\Holly\\Google Drive\\Masters\\Thesis\\Data", layer = "coast_n83")
# plot coastline
plot(coastline)
# create shp file from point data 

sightings.shp <- SpatialPoints(coords =  sightings)
plot(sightings.shp)
sightings.clipped <- sightings[coastline,]


#### geoR- analysis ####

# For the examples included in this document we use the data set s100 included in the geoR distribution.
data("s100")

#### Exploratory tools ####

summary(s100)

#### Plotting data locations and values ####

# The function plot.geodata shows a 2 × 2 display with data locations 
# (top plots) and data versus coordinates (bottom plots).
# For an object of the class geodata the command plot() gives the same result

plot.geodata(s100)
