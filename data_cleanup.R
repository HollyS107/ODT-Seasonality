#### Holly Self #####
## MSc AMFE Univeristy of Aberdeen ##
## ZO5901 Research Project ##

#### Data cleanup ####
## opportunistic sightings 
## data provided by the Pacific Whale Foundation
## sebject to an MOU. 

#### Set Up ####
load("C:\\Users\\Holly\\Documents\\.RData")

wd <- "C:\\Users\\Holly\\Google Drive\\Masters\\Thesis"
setwd(wd)

# Packages

library("reshape") 
library("geoR")

#### Read in raw data ####

tracker.raw <- read.csv("Data\\tracker.csv", header = TRUE)

#### Assess Data ####
raw.summary <- capture.output(str(tracker.raw))

####### Issues reported by PWF #########

# This data set does not account for duplicate sightings, which happens when multiple vessels report the same group of dolphins. 
# The latitude and longitude data have not been verified and I expect you will find quite a few sightings on land as well as outside the probable study area (leeward waters of Maui). 
# The data will be variable by year, with the initial 2013  season having a lot of missed days and data collection improving thereafter.

############# Found Issues #############
# Some levels are factors which is not ideal (trip ID, sighting notes, date, time of 
# encounter, notes, departure time)

# some factors have levels repeated as typos, eg;
# Track.Status "" "Complete" "COmplete" "Edited"   "Edited "  "None"     "Unusable"
# (Track status, sighting status, Trip type, staff name, activity, calves present, inter-species, multiple groups)
# This is especially prevalent in staff name, which has capitslisation, trailing whitespace and spelling errors

# time has imported incorrectly 

######### Start Cleanup ##########

## create table to log progress 

data.prep <- data.frame(c("Trip ID", "Sighting.Notes", "Notes"))
data.prep$issue <- c("incorrect field (factor)", "incorrect field (factor)", "incorrect field (factor)")
data.prep$corrected <- c("Y", "Y", "Y")
data.prep$correction.date <- c("Thur Apr 20", "Thur Apr 20", "Thur Apr 20")
data.prep$correction.notes <- c("used as.character","used as.character", "used as.character")

colnames(data.prep) <- c("Field", "Issue", "Corrected?", "Correction Date", "Correction Notes")

data.prep$Field <- as.character(data.prep$Field)

data.prep <- rbind(data.prep[1:3,], c("Track Status", "Duplicate/unlabled levels", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:4,], c("Sighting Status", "Duplicate/unlabled levels", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:5,], c("Trip Type", "Duplicate/unlabled levels", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:6,], c("Calves Present", "Duplicate/unlabled levels", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:7,], c("Inter-species", "Duplicate/unlabled levels", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:8,], c("Multiple Groups", "Duplicate/unlabled levels", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:9,], c("Staff Name", "Capitalisation inconsistency", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:10,], c("Staff Name", "Trailing whitespace", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:11,], c("Staff Name", "Spelling Errors", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:12,], c("Staff Name", "Missing surname", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:13,], c("Activity code", "long level names", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:14,], c("Time", "Incorrect field type", "Y", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:15,], c("DateTime", "single field needed", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:16,], c("Latitude/Longitude", "erroneous (land) entries", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:17,], c("Latitude/Longitude", "autocorrelation- duplicate entries", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:18,], c("Multiple", "Sightings 22030,22080 & 22111 missing multiple pieces of info", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:19,], c("Sighting notes", "- rather than NAs present", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:20,], c("Time of encounter", "midnight times (incorrect) ", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:21,], c("Time of encounter", "03:19 time (incorrect) ", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:22,], c("Animal count", "Animal count /= adults+juveniles+calves", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:23,], c("Calf count", "obviously erroneous entires (9999)", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:24,], c("Calf count", "High entries- require verification", "N", "N/A", "N/A"))
data.prep <- rbind(data.prep[1:25,], c("Calves present", "yes/no does not line up with calf count entry", "N", "N/A", "N/A"))


#Write new data frame 

tracker.clean <- tracker.raw

#### Convert Collumn Types ####

tracker.clean$Trip.ID <- as.character(tracker.clean$Trip.ID)
tracker.clean$Sighting.Notes <- as.character(tracker.clean$Sighting.Notes)
tracker.clean$Notes <- as.character(tracker.clean$Notes)

data.prep$`Corrected?`[1:3] <- "Y"
data.prep$`Correction Date` [1:3] <- "Thur Apr 20"
data.prep$`Correction Notes` [1:3] <- "Used as.character()"

#### Cleanup Levels ####

####  Simple Fixes ####

# Capitalisation errors, few levels 
# Levels listed before and after 
# unknown used to replace blank entries 

### Track status ###
# Before: ""         "Complete" "COmplete" "Edited"   "Edited "  "None"     "Unusable"
levels(tracker.clean$Track.Status) <- list(complete=c("Complete", "COmplete"), edited=c("Edited", "Edited "), none = "None", unusable = "Unusable")
# After: "complete" "edited"   "none"     "unusable"


### Sighting status ###
# Before: ""  "No GPS track" "No GPS Track" "None" "Unusable" "Usable" "Usable "
levels(tracker.clean$Sighting.Status) <- list(no.track = c("No GPS track", "No GPS Track"), usable = c("Usable","Usable ") , unusable = "Unusable", unknown = "")
# After: "no.track" "usable"   "unusable" "unknown" 


### Trip Type ###
# Before: ""  "Dinner Cruise" "Dolphin-watch" "None" "Other""Snorkel trip" "Snorkel Trip"  "Whale-watch" 
levels(tracker.clean$Trip.type) <- list(Dinner.cruise = "Dinner Cruise", Dolphin.watch = "Dolphin-watch", None = "None", Other = "Other", Snorkel.trip = c("Snorkel trip", "Snorkel Trip"), Whale.watch = "Whale-watch")
# "Dinner.cruise" "Dolphin.watch" "None" "Other" "Snorkel.trip"  "Whale.watch" 


# Calves Present
# Before: ""               "Calves present" "no"             "No"             "YES" 
levels(tracker.clean$Calves.present) <- list(no = c("No", "no"), yes = c("YES", "Calves present"), unknown = "")
# After: "no"      "yes"     "unknown"


# Inter Species
# Before: ""  "Inter-species Interaction" "No" "off" "Yes"  
levels(tracker.clean$Inter.species) <- list(no = "No", yes = c("Yes", "Inter-species Interaction"), unknown = c("", "off"))
# After: "no"      "yes"     "unknown"



# Multiple groups
# Before: ""    "no"  "No"  "Yes"
levels(tracker.clean$Multiple.groups) <- list(no = c("no", "No"), yes = "Yes", unknown = "")
# After: "no"      "yes"     "unknown"

data.prep$`Corrected?`[4:9] <- "Y"
data.prep$`Correction Date` [4:9] <- "Fri Apr 21"
data.prep$`Correction Notes` [4:9] <- "Levels overwitten with list"

#### Staff name ####

#### Capitalisation errors ####

# convert all staff names to lower case to remove capitilisation errors
tracker.clean$Staff.name <- sapply(tracker.clean$Staff.name, tolower)
# convert from character back to factor
tracker.clean$Staff.name <- factor(tracker.clean$Staff.name)
# This has reduced levels from 265 to 198 
# Next error is trailing whitespace 

data.prep$`Corrected?`[10] <- "Y"
data.prep$`Correction Date` [10] <- "Fri Apr 21"
data.prep$`Correction Notes` [10] <- "overwritten in lowercase with apply(tolower)"

#### Trailing whitespace errors ####

# trim trailing whitespace function from stack overflow
# "http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r"
trim.trailing <- function (x) sub("\\s+$", "", x)
# Trim whitespace
tracker.clean$Staff.name <- trim.trailing(tracker.clean$Staff.name)
# Reconvert to factor 
tracker.clean$Staff.name <- factor(tracker.clean$Staff.name)
# This has reduced the staff names to 176, however spelling errors remain

data.prep$`Corrected?`[11] <- "Y"
data.prep$`Correction Date` [11] <- "Fri Apr 21"
data.prep$`Correction Notes` [11] <- "custom function from stackoverflow"

####### Obvious typos #######

# The typo is listed next to (=) the correct spelling. Where hypens or other special
# characters exist these have been replaced eg i replaces í

### new collumn made for ease of error correction 
tracker.clean$Staff.name2 <- tracker.clean$Staff.name

#### [27] "brian t" = "brian totman" 

## Before
tracker.clean[tracker.clean$Staff.name2 == "brian t", 1]
# [1] "20131220_OL_0700"
tracker.clean[tracker.clean$Staff.name2 == "brian totman", 1]
# [1] "20150226_OO_1400" "20140924_OO_0700" "20140518_OO_0700" "20140407_OO_0700" "20131224_OO_0830"
# [6] "20131224_OO_1600" "20131207_OO_0830"

levels(tracker.clean$Staff.name2)[match("brian t",levels(tracker.clean$Staff.name2))] <- "brian totman"


## After 
tracker.clean[tracker.clean$Staff.name2 == "brian t", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20131220_OL_0700",31]
# [1] brian totman

#### [31] "brittany guent" = "brittany guenther"

## Before
tracker.clean[tracker.clean$Staff.name2 == "brittany guent", 1]
# [1] "20150227_OS_0830"

tracker.clean[tracker.clean$Staff.name2 == "brittany guenther", 1]
# [1] "20170105_OE_0645" "20170224_OD_0630" "20170228_OS_1000" "20170320_OD_0630" "20170329_OD_0630"
# [71] "20151208_OQ_0800" "20151210_OQ_0800" "20151215_OQ_0800" "20151217_OQ_0800"

levels(tracker.clean$Staff.name2)[match("brittany guent",levels(tracker.clean$Staff.name2))] <- "brittany guenther"

## After 
tracker.clean[tracker.clean$Staff.name2 == "brittany guent", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20150227_OS_0830",31]
# [1] brittany guenther

### "caitlin o\\'brien" = "caitlin obrien"

## Before 
tracker.clean[tracker.clean$Staff.name2 == "caitlin o\\'brien", 1]
# [1] "20140429_OQ_0800" "20140217_OQ_0800"
tracker.clean[tracker.clean$Staff.name2 == "caitlin obrien", 1]
# [1] "20140108_OQ_0800" "20140123_OQ_0800" "20131213_OQ_1430" "20131225_OQ_0800" "20131225_OQ_0800"

levels(tracker.clean$Staff.name2)[match("caitlin o\\'brien",levels(tracker.clean$Staff.name2))] <- "caitlin obrien"

## After
tracker.clean[tracker.clean$Staff.name2 == "caitlin o\\'brien", 1]
# character(0)      
tracker.clean[tracker.clean$Trip.ID == "20140429_OQ_0800",31]
# [1] caitlin obrien
tracker.clean[tracker.clean$Trip.ID == "20140217_OQ_0800",31]
# [1] caitlin obrien

### "catherine r" = "catherine rapanotti" 

## Before 
tracker.clean[tracker.clean$Staff.name2 == "catherine r", 1]
# [1] "20130809_OA_0700"
tracker.clean[tracker.clean$Staff.name2 == "catherine rapanotti", 1]
# [1] "20150128_OQ_0800" "20150201_OD_0900" "20131105_OA_0730" "20131105_OA_0730" "20130107_OQ_0800"
# [6] "20130109_OD_0900" "20130721_OA_0700" "20130804_OQ_0800" "20130804_OQ_0800"

levels(tracker.clean$Staff.name2)[match("catherine r",levels(tracker.clean$Staff.name2))] <- "catherine rapanotti"

## After
tracker.clean[tracker.clean$Staff.name2 == "catherine r", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20130809_OA_0700",31]
# [1] catherine rapanotti

### "christy \\\"the koz\\\" kozama" & "christykozama" = "christy kozama"

## Before
tracker.clean[tracker.clean$Staff.name2 == "christy \\\"the koz\\\" kozama", 1]
# [1] "20130802_OA_0730"
tracker.clean[tracker.clean$Staff.name2 == "christykozama", 1]
# "20151204_OD_0900"
tracker.clean[tracker.clean$Staff.name2 == "christy kozama", 1]
#   [1] "20160106_OD_1130" "20160108_OD_0900" "20160118_OQ_0800" "20160303_OA_1415" "20160303_OA_1630"
# [151] "20130823_OA_0730" "20130829_OD_0800" "20130829_OD_0800" "20130830_OA_0730" "20131129_OA_0730"

levels(tracker.clean$Staff.name2)[match("christy \\\"the koz\\\" kozama",levels(tracker.clean$Staff.name2))] <- "christy kozama"
levels(tracker.clean$Staff.name2)[match("christykozama",levels(tracker.clean$Staff.name2))] <- "christy kozama"


## After
tracker.clean[tracker.clean$Staff.name2 == "christy \\\"the koz\\\" kozama", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20130802_OA_0730",31]
# [1] christy kozama
tracker.clean[tracker.clean$Staff.name2 == "christykozama", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20151204_OD_0900",31]
# [1] christy kozama

### "kerr smith" = "keri smith"

## Before
tracker.clean[tracker.clean$Staff.name2 == "kerr smith", 1]
# [1] "20150110_OS_1130" "20150113_OS_1100"
tracker.clean[tracker.clean$Staff.name2 == "keri smith", 1]
# [1] "20160212_OS_1000" "20160213_OS_1230" "20160313_OS_1000" "20150119_OQ_0800" "20150519_OS_0700"

levels(tracker.clean$Staff.name2)[match("kerr smith",levels(tracker.clean$Staff.name2))] <- "keri smith"

## After
tracker.clean[tracker.clean$Staff.name2 == "kerr smith", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20150110_OS_1130",31]
# [1] keri smith

### "megan o\\'hara" = megan o'hara
## Before
tracker.clean[tracker.clean$Staff.name2 == "megan o\\'hara", 1]
# [1] "20170112_OI_0830" "20170119_OE_1415" "20170217_OI_1045" "20170310_OV_0630" "20170317_OV_1700"
# [6] "20160311_OO_1230" "20160409_OO_1400" "20160705_OV_0800" "20161110_OL_0730" "20161119_OL_0730"
# [11] "20161221_OV_0900" "20161221_OV_1400" "20150617_OV_0830" "20150619_OV_0830" "20150623_OV_0830"
# [16] "20150725_OV_0830" "20151125_OO_1230" "20151127_OI_1130"
tracker.clean[tracker.clean$Staff.name2 == "megan o'hara", 1]
# character(0) (encoding error, not a typo)

levels(tracker.clean$Staff.name2)[match("megan o\\'hara",levels(tracker.clean$Staff.name2))] <- "megan o'hara"

## After
tracker.clean[tracker.clean$Staff.name2 == "megan o\\'hara", 1]
# character(0)
tracker.clean[tracker.clean$Staff.name2 == "megan o'hara", 1]
# [1] "20170112_OI_0830" "20170119_OE_1415" "20170217_OI_1045" "20170310_OV_0630" "20170317_OV_1700"
# [6] "20160311_OO_1230" "20160409_OO_1400" "20160705_OV_0800" "20161110_OL_0730" "20161119_OL_0730"
# [11] "20161221_OV_0900" "20161221_OV_1400" "20150617_OV_0830" "20150619_OV_0830" "20150623_OV_0830"
# [16] "20150725_OV_0830" "20151125_OO_1230" "20151127_OI_1130"

### "mikhail reedy" = "mikhala reedy"
## Before
tracker.clean[tracker.clean$Staff.name2 == "mikhail reedy", 1]
# [1] "20161213_OQ_0800"
tracker.clean[tracker.clean$Staff.name2 == "mikhala reedy", 1]
# [1] "20170102_OQ_0800" "20170114_OQ_0800" "20170114_OQ_0800" "20170124_OQ_0800" "20170124_OQ_0800"
# [6] "20170124_OQ_0800" "20170222_OQ_0800" "20170313_OQ_0800" "20170319_OQ_0800" "20170326_OQ_0800"
# [11] "20161224_OQ_1430" "20161224_OQ_1430"

levels(tracker.clean$Staff.name2)[match("mikhail reedy",levels(tracker.clean$Staff.name2))] <- "mikhala reedy"

## After
tracker.clean[tracker.clean$Staff.name2 == "mikhail reedy", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20161213_OQ_0800",31]
# [1] mikhala reedy

### savannah harte"  & "savannah sharte" & "savannahs harte" = "savanah harte"  
## Before
tracker.clean[tracker.clean$Staff.name2 == "savannah sharte", 1]
# [1] "20150727_OS_0700" ""  
tracker.clean[tracker.clean$Staff.name2 == "savannahs harte", 1]
# [1] "20150321_OS_1600"
tracker.clean[tracker.clean$Staff.name2 == "savannah harte", 1]
# [1] "20150530_OS_0700"
tracker.clean[tracker.clean$Staff.name2 == "savanah harte", 1]
# [1] "20150111_OA_0800" "20150302_OQ_0800" "20150329_OQ_0800" "20150413_OQ_0800" "20140101_OA_0915"
# [6] "20140129_OA_1415" "20140210_OQ_1430" "20140314_OF_1530" "20140321_OA_1145" "20140322_OD_1400"
# [11] "20140326_OA_1630" "20140404_OA_0645" "20140404_OA_0915" "20140403_OA_0645" "20140101_OA_0915"
# [16] "20131105_OD_0800" "20131105_OD_0800" "20131105_OD_0800" "20131116_OD_1400" "20131127_OD_0800"
# [21] "20131110_OD_0800" "20131129_OQ_0800" "20131214_OD_1130" "20131226_OQ_0800" "20131106_OD_1430"
# [26] "20130817_OD_0900"

levels(tracker.clean$Staff.name2)[match("savannah sharte",levels(tracker.clean$Staff.name2))] <- "savanah harte"
levels(tracker.clean$Staff.name2)[match("savannahs harte",levels(tracker.clean$Staff.name2))] <- "savanah harte"
levels(tracker.clean$Staff.name2)[match("savannah harte",levels(tracker.clean$Staff.name2))] <- "savanah harte"

## After
tracker.clean[tracker.clean$Staff.name2 == "savannah sharte", 1]
# character(0)
tracker.clean[tracker.clean$Staff.name2 == "savannahs harte", 1]
# character(0)
tracker.clean[tracker.clean$Staff.name2 == "savannah harte", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20150727_OS_0700",31]
# [1] savanah harte

### [153] "serena neff" = "serena neff sultan" 
## Before
tracker.clean[tracker.clean$Staff.name2 == "serena neff", 1]
# [1] "20130901_OQ_0800" "20130930_OD_0800" "20131127_OF_0900" "20131212_OF_1415" "20131105_OA_1400"
# [6] "20130514_OD_1100" "20130621_OQ_0800" "20130622_OD_0700" "20130708_OD_0800" "20130709_OQ_0800"
# [11] "414996_OQ_0800" 
tracker.clean[tracker.clean$Staff.name2 == "serena neff sultan", 1]
# [1] "20140119_OD_1130" "20140323_OD_1130" "20140331_OD_0900" "20140331_OD_1400" "20140405_OD_1130"
# [6] "20140407_OD_0900" "20140319_OD_1130"

levels(tracker.clean$Staff.name2)[match("serena neff",levels(tracker.clean$Staff.name2))] <- "serena neff sultan"

## After
tracker.clean[tracker.clean$Staff.name2 == "serena neff", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20130901_OQ_0800",31]
# [1] serena neff sultan

### "sierra frye-keele" = "sierra frye keele"
## Before
tracker.clean[tracker.clean$Staff.name2 == "sierra frye-keele", 1]
# [1] "20131110_OQ_0700" "20131116_OQ_1230" "414998_OQ_0800"   "414998_OQ_0800"   "20131123_OQ_0700"
tracker.clean[tracker.clean$Staff.name2 == "sierra frye keele", 1]
# [1] "20150118_OS_1600" "20150303_OS_0830" "20150413_OS_0830"

levels(tracker.clean$Staff.name2)[match("sierra frye-keele",levels(tracker.clean$Staff.name2))] <- "sierra frye keele"

## After
tracker.clean[tracker.clean$Staff.name2 == "sierra frye-keele", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20131110_OQ_0700",31]
# [1] sierra frye keele

### "tizoc garcía" = "tizoc garcia"
## Before
tracker.clean[tracker.clean$Staff.name2 == "tizoc garcía", 1]
# [1] "20170308_OV_1400" "20170317_OV_1700"
tracker.clean[tracker.clean$Staff.name2 == "tizoc garcia", 1]
# [1] "20170102_OS_1230" "20170303_OI_0900" "20170315_OO_1230" "20160103_OV_1130" "20160113_OV_0900"
# [26] "20151231_OV_1130"

levels(tracker.clean$Staff.name2)[match("tizoc garcía",levels(tracker.clean$Staff.name2))] <- "tizoc garcia"

## After
tracker.clean[tracker.clean$Staff.name2 == "tizoc garcía", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20170308_OV_1400",31]
# [1] tizoc garcia

# "torsten drukan" = "torsten durkan" 
## Before
tracker.clean[tracker.clean$Staff.name2 == "torsten drukan", 1]
# [1] "20140813_OV_0830"
tracker.clean[tracker.clean$Staff.name2 == "torsten durkan", 1]
# [1] "20160307_OV_0630" "20150101_OV_0830" "20150101_OV_1630" "20150320_OV_0830" "20150421_OV_0830"
# [6] "20150515_OV_0830" "20150702_OV_0830" "20150704_OV_0830" "20150716_OV_0830" "20140829_OV_0830"
# [11] "20140920_OV_0900"

levels(tracker.clean$Staff.name2)[match("torsten drukan",levels(tracker.clean$Staff.name2))] <- "torsten durkan"

## After
tracker.clean[tracker.clean$Staff.name2 == "torsten drukan", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20140813_OV_0830",31]
# [1] torsten durkan

###### Missing surname- only one staff member with that forname 

### "allee" = "allee jimenez" 
##Before
tracker.clean[tracker.clean$Staff.name2 == "allee", 1]
#[1] "20161013_OD_1700"
tracker.clean[tracker.clean$Staff.name2 == "allee jimenez", 1]
#  [1] "20170126_OS_1000" "20170218_OD_0630" "20170314_OD_0630" "20170315_OQ_0800" "20160615_OQ_0800"
# [21] "20161229_OS_1230" "20161231_OQ_0800"

levels(tracker.clean$Staff.name2)[match("allee",levels(tracker.clean$Staff.name2))] <- "allee jimenez"

## After
tracker.clean[tracker.clean$Staff.name2 == "allee", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20161013_OD_1700",31]
# [1] allee jimenez

# [29] "brigid" = "brigid magee" 
##Before
tracker.clean[tracker.clean$Staff.name2 == "brigid", 1]
#[1] "20150713_OV_0830"
tracker.clean[tracker.clean$Staff.name2 == "brigid magee", 1]
#  [1] "20170108_OO_1400" "20170319_OV_1230" "20170321_OV_1400" "20160211_OV_1400" "20160427_OO_0700"
# [11] "20150822_OV_0700" "20151206_OO_1230" "20151208_OO_0700"

levels(tracker.clean$Staff.name2)[match("brigid",levels(tracker.clean$Staff.name2))] <- "brigid magee"

## After
tracker.clean[tracker.clean$Staff.name2 == "brigid", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20150713_OV_0830",31]
# [1] brigid magee
 
# "lauren"  = "lauren fritz"
##Before
tracker.clean[tracker.clean$Staff.name2 == "lauren", 1]
# [1] "20150718_OL_0800"
tracker.clean[tracker.clean$Staff.name2 == "lauren fritz", 1]
#  [1] "20170124_OV_0900" "20170128_OV_0630" "20170128_OV_1130" "20170329_OO_1230" "20170330_OI_0900"
# [11] "20151219_OI_0800" "20151220_OV_0900" "20151220_OV_1400" "20151221_OI_0900"

levels(tracker.clean$Staff.name2)[match("lauren",levels(tracker.clean$Staff.name2))] <- "lauren fritz"

## After
tracker.clean[tracker.clean$Staff.name2 == "lauren", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20150718_OL_0800",31]
# [1] lauren fritz

### "mike" = "mike donohoe"
##Before
tracker.clean[tracker.clean$Staff.name2 == "mike", 1]
#[1] "20150327_OE_0730"
tracker.clean[tracker.clean$Staff.name2 == "mike donohoe", 1]
#  [1] "20160423_OE_0730" "20150102_OE_0730" "20150107_OE_0730" "20150108_OE_0730" "20150114_OE_0730"
# [51] "20140320_OE_0730" "20140324_OE_0730" "20140331_OE_0730" "20140520_OE_0730" "20140527_OE_0730"

levels(tracker.clean$Staff.name2)[match("mike",levels(tracker.clean$Staff.name2))] <- "mike donohoe"

## After
tracker.clean[tracker.clean$Staff.name2 == "mike", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20150327_OE_0730",31]
# [1] mike donohoe

### "savanah" = "savanah harte"
##Before
tracker.clean[tracker.clean$Staff.name2 == "savanah", 1]
# [1] "20130626_OQ_0800" "20130630_OQ_0800" "20130630_OQ_0800"
tracker.clean[tracker.clean$Staff.name2 == "savanah harte", 1]
# [1] "20150111_OA_0800" "20150302_OQ_0800" "20150321_OS_1600" "20150329_OQ_0800" "20150413_OQ_0800"
# [26] "20131129_OQ_0800" "20131214_OD_1130" "20131226_OQ_0800" "20131106_OD_1430" "20130817_OD_0900"

levels(tracker.clean$Staff.name2)[match("savanah",levels(tracker.clean$Staff.name2))] <- "savanah harte"

## After
tracker.clean[tracker.clean$Staff.name2 == "savanah", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20150111_OA_0800",31]
# savanah harte

### "sergio" = "sergio burgos"
##Before
tracker.clean[tracker.clean$Staff.name2 == "sergio", 1]
# [1] "414999_OF_0700"
tracker.clean[tracker.clean$Staff.name2 == "sergio burgos", 1]
#  [1] "20160403_OA_0815" "20160423_OD_1130" "20160514_OD_0800" "20160709_OS_1300" "20160709_OS_1300"
# [31] "20130820_OQ_0800" "20130326_OD_1400" "20130416_OD_1400"

# levels(tracker.clean$Staff.name2)[match("sergio",levels(tracker.clean$Staff.name2))] <- "sergio burgos"

## After
tracker.clean[tracker.clean$Staff.name2 == "sergio", 1]
tracker.clean[tracker.clean$Trip.ID == "414999",31]

### "tim"  "tim barker" 
##Before
tracker.clean[tracker.clean$Staff.name2 == "tim", 1]
# [1] "20130910_OE_0730" "20130911_OE_0730"
tracker.clean[tracker.clean$Staff.name2 == "tim barker", 1]
# [1] "20131120_OE_0730" "20130929_OE_0730" "20131028_OE_0730" "20131105_OE_0730" "20130903_OE_0730"
# [6] "20130901_OE_0730"

levels(tracker.clean$Staff.name2)[match("tim",levels(tracker.clean$Staff.name2))] <- "tim barker"

## After
tracker.clean[tracker.clean$Staff.name2 == "tim", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20130910_OE_0730",31]
# [1] tim barker

# "dav" "dav y" "dav yuan" 
##Before
tracker.clean[tracker.clean$Staff.name2 == "dav", 1]
# [1] "20130814_OE_0730" "20130827_OE_0730" "20130826_OE_0730"
tracker.clean[tracker.clean$Staff.name2 == "dav y", 1]
# [1] "20130810_OE_0730" "20130811_OE_0730"
tracker.clean[tracker.clean$Staff.name2 == "dav yuan", 1]
#  [1] "20140111_OE_0730" "20140130_OE_1400" "20140208_OE_0730" "20140211_OE_0730" "20140212_OE_0730"
# [21] "20131127_OE_0730"

levels(tracker.clean$Staff.name2)[match("dav",levels(tracker.clean$Staff.name2))] <- "dav yuan"
levels(tracker.clean$Staff.name2)[match("dav y",levels(tracker.clean$Staff.name2))] <- "dav yuan"

## After
tracker.clean[tracker.clean$Staff.name2 == "dav", 1]
# character(0)
tracker.clean[tracker.clean$Staff.name2 == "dav y", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20130814_OE_0730",31]
# [1] dav yuan

data.prep$`Corrected?`[12] <- "Y"
data.prep$`Correction Date` [12] <- "Mon Apr 24"
data.prep$`Correction Notes` [12] <- "Levels overwitten with match"

###### Missing surname- verification of individual required 



### "annie" - [19] "annie donahoe", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "annie", 1]

##Before
tracker.clean[tracker.clean$Staff.name2 == "annie", 1]
# [1] 20140130_OD_1400
tracker.clean[tracker.clean$Staff.name2 == "annie donahoe", 1]
#[1] 20150224_OS_0830 20150317_OS_1600

levels(tracker.clean$Staff.name2)[match("annie",levels(tracker.clean$Staff.name2))] <- "annie donahoe"

## After
tracker.clean[tracker.clean$Staff.name2 == "annie", 1]
tracker.clean[tracker.clean$Trip.ID == "20140130_OD_1400",31]
# [1] annie donahoe

### "chelsea" =  "chelsea aydelott", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "chelsea", 1]
# [1] 20131128_OI_1130
tracker.clean[tracker.clean$Staff.name2 == "chelsea aydelott", 1]
# [1] 20140204_OV_0830 20140204_OV_0830 20140424_OV_0830 20140307_OV_0700 20130820_OV_0700
# [21] 20131021_OQ_0700 20131021_OQ_0700 20131023_OV_0700

levels(tracker.clean$Staff.name2)[match("chelsea",levels(tracker.clean$Staff.name2))] <- "chelsea aydelott"

## After
tracker.clean[tracker.clean$Staff.name2 == "chelsea", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20131128_OI_1130",31]
# [1] chelsea aydelott

### "jackson" = "jackson kowalski", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "jackson", 1]
# [1] 20130723_OF_0700
tracker.clean[tracker.clean$Staff.name2 == "jackson kowalski", 1]
#   [1] 20170108_OQ_1730 20170114_OE_1630 20170130_OS_1230 20170225_OS_1630 20170225_OS_1230
# [136] 20130817_OQ_0800 20130817_OQ_0800 20130905_OF_0900 20130905_OF_0900 20131022_OA_0730

levels(tracker.clean$Staff.name2)[match("jackson",levels(tracker.clean$Staff.name2))] <- "jackson kowalski"

## After
tracker.clean[tracker.clean$Staff.name2 == "jackson", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20130723_OF_0700",31]
# [1] chelsea aydelott

### "saanah" = "sarah sinn-white", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "saanah", 1]
#[1] 20130630_OQ_0800
tracker.clean[tracker.clean$Staff.name2 == "sarah sinn-white", 1]
# [1] 20130119_OQ_0800

levels(tracker.clean$Staff.name2)[match("saanah",levels(tracker.clean$Staff.name2))] <- "sarah sinn-white"

## After
tracker.clean[tracker.clean$Staff.name2 == "saanah", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20130630_OQ_0800",31]
# [1] sarah sinn-white

### "v" = "victoria persson", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "v", 1]
# [1] 20130708_OV_0900
tracker.clean[tracker.clean$Staff.name2 == "victoria persson", 1]
#  [1] 20131012_OV_0700 20130811_OV_0700 20130829_OV_0830 20130913_OV_0700 20130913_OV_0700
# [11] 20131005_OV_0700 20130112_OI_0630 20130613_OV_0700

levels(tracker.clean$Staff.name2)[match("v",levels(tracker.clean$Staff.name2))] <- "victoria persson"

## After
tracker.clean[tracker.clean$Staff.name2 == "v", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20130708_OV_0900",31]
# [1] victoria persson

## Update data prep

data.prep$`Corrected?`[13] <- "Y"
data.prep$`Correction Date` [13] <- "Tue May 02"
data.prep$`Correction Notes` [13] <- "PWF confirmed names added"

#### Create unknown staff table to send to PWF ####

unknown.staff <- data.frame(c("annie", "chelsea", "jackson", "saanah", "v"))
colnames(unknown.staff) <- "entered name"
unknown.staff["Trip.ID"] <-  c("20140130_OD_1400", "20131128_OI_1130", "20130723_OF_0700", "20130630_OQ_0800", "20130708_OV_0900")
unknown.staff[1,2] <- c("20140130_OD_1400 20140317_OQ_0800 20140425_OQ_0800 20140429_OD_0900 20140508_OD_1430
                         20140109_OQ_1430 20140212_OD_1400 20140224_OD_0900 20140311_OQ_1430 20140326_OD_1400
                         20140326_OD_1400 20140531_OL_0800 20140211_OQ_0800 20140417_OD_1400 20140605_OQ_0800
                         20131205_OQ_1430 20131224_OD_0630 20131226_OD_0630 20131213_OA_1145 20131213_OA_1415
                         20131223_OQ_0800")
unknown.staff["option.1"] <-  c("annie goodenough", "chelsea anglin","jackson dowd", "sarah bonneson", "vanessa hernandez")
unknown.staff["option.2"] <-  c("annie donahoe", "chelsea anglin","jackson kowalski", "sarah mccullagh", "victoria persson")
unknown.staff["option.3"] <-  c("annie hillier", "N/A","N/A", "sarah sinn-white", "N/A")
unknown.staff["option.4"] <-  c("N/A", "N/A","N/A", "sarah updegraff", "N/A")

data.prep$`Corrected?` [13] <- "Pending"
data.prep$`Correction Notes` [13] <- "Information requested from PWF 21/4/17"

#### Activity ####

levels(tracker.clean$Activity)
# this will be used to create a new collumn containing the code only for ease 

tracker.clean$Activity.code <- tracker.clean$Activity
levels(tracker.clean$Activity.code)
# Current levels to be changed;
# [1] ""                                
# [2] "BOW Bow riding"                                                                # [3] "FOR Foraging, Feeding"                                                         # [4] "FS Fast Swimming (&gt;8 mph)"                                                  # [5] "FS Fast Swimming (&gt;8 mph), MA Milling Active - Surface behaviors 
# breaching, pec slapping etc"                  
# [6] "MA Milling Active - Surface behaviors breaching, pec slapping etc"             # [7] "MILLING - moving about but not in any one particular direction"                # [8] "MR Milling Resting - No active behaviors, lying quietly on surface, diving 
# after a few blows"                     
# [9] "MS Medium Swimming (3-8 mph)"                                                  # [10] "SA Surface Active - typical for comp pods, travel in clear direction while
# breaching, fluke or pec slapping, etc."
# [11] "SG Singing - HBW song is heard" 
# [12] "SOCIALIZING - primarily interacting with each other; mating, caressing,
# playing, etc."                            
# [13] "SS Slow Swimming (&lt;2-3 mph)" 

# Replace current levels with code only or unknown where entry is blank

levels(tracker.clean$Activity.code)[match("",levels(tracker.clean$Activity.code))] <- "unknown"
levels(tracker.clean$Activity.code)[match("BOW Bow riding",levels(tracker.clean$Activity.code))] <- "BOW"
levels(tracker.clean$Activity.code)[match("FOR Foraging, Feeding",levels(tracker.clean$Activity.code))] <- "FOR"
levels(tracker.clean$Activity.code)[match("FS Fast Swimming (&gt;8 mph)",levels(tracker.clean$Activity.code))] <- "FS"
levels(tracker.clean$Activity.code)[match("FS Fast Swimming (&gt;8 mph), MA Milling Active - Surface behaviors breaching, pec slapping etc",levels(tracker.clean$Activity.code))] <- "FS/MA"
levels(tracker.clean$Activity.code)[match("MA Milling Active - Surface behaviors breaching, pec slapping etc",levels(tracker.clean$Activity.code))] <- "MA"
# levels(tracker.clean$Activity.code)[match("MILLING - moving about but not in any one particular direction",levels(tracker.clean$Activity.code))] <- "MIL"
levels(tracker.clean$Activity.code)[match("MR Milling Resting - No active behaviors, lying quietly on surface, diving after a few blows",levels(tracker.clean$Activity.code))] <- "MR"
levels(tracker.clean$Activity.code)[match("MS Medium Swimming (3-8 mph)",levels(tracker.clean$Activity.code))] <- "MS"
levels(tracker.clean$Activity.code)[match("SA Surface Active - typical for comp pods, travel in clear direction while breaching, fluke or pec slapping, etc.",levels(tracker.clean$Activity.code))] <- "SA"
levels(tracker.clean$Activity.code)[match("SG Singing - HBW song is heard",levels(tracker.clean$Activity.code))] <- "SG"
# levels(tracker.clean$Activity.code)[match("SOCIALIZING - primarily interacting with each other; mating, caressing, playing, etc.",levels(tracker.clean$Activity.code))] <- "SOCIALIZING"
levels(tracker.clean$Activity.code)[match("SS Slow Swimming (&lt;2-3 mph)",levels(tracker.clean$Activity.code))] <- "SS"

# above method does not work for milling or socialising methods - there must be some 
# kind of invisible special character in use from the web app
# Solution- use alternative method to call that level 
levels(tracker.clean$Activity.code)[match(unique(tracker.clean$Activity.code)[13],levels(tracker.clean$Activity.code))] <- "SOC"
levels(tracker.clean$Activity.code)[match(unique(tracker.clean$Activity.code)[12],levels(tracker.clean$Activity.code))] <- "MIL"


# Check levels 

levels(tracker.clean$Activity.code)
unique(tracker.clean$Activity.code)

data.prep$`Corrected?`[14] <- "Y"
data.prep$`Correction Date` [14] <- "Fri Apr 28"
data.prep$`Correction Notes` [14] <- "Levels overwitten with list, milling and socialising contain some kind of unseen special character so cannot be called with level name"

#### Incorrect Sighting Times ####

# entry 03:19 - possible it should be 13:19 or 15:19?
# Check vessel departure time to check if this kind of typo is likely

tracker.clean[tracker.clean$Time.of.encounter == "03:19",]
# [1] "20131213_OA_1415"

# Departure is 1430 so this is most likely supposed to be 15:19

# Overwrite 03:19 with 15:19
levels(tracker.clean$Time.of.encounter)[match("03:19",levels(tracker.clean$Time.of.encounter))] <- "15:19"
# Check that worked correctly using Trip ID of that sighting
tracker.clean[tracker.clean$Trip.ID == "20131213_OA_1415",9]
# [1] 15:19

# Update data.prep
data.prep$`Corrected?`[22] <- "Y"
data.prep$`Correction Date` [22] <- "Wed Apr 26"
data.prep$`Correction Notes` [22] <- "corrected to 15:19 using match()"

# Midnight entries- Need confirmation of sighting time by PWF 
# Information requested from PWF 27/4/14

tracker.clean[tracker.clean$Time.of.encounter == "00:00",1]
# [1] "20150122_OQ_0800" "20150726_OD_0900" "20151003_OS_0700" "20151003_OE_0730" "20151005_OS_0900"
# [6] "20140728_OA_0730" "20131018_OD_0800"

# Are there tracks to get this info from?
tracker.clean[tracker.clean$Time.of.encounter == "00:00",3]
# [1] complete none     edited   none     complete complete edited  

data.prep$`Corrected?`[21] <- "Pending"
data.prep$`Correction Notes` [21] <- "Information requested from PWF 27/4/14"

#### Time Format ####

# two time collumns (not currently read correctly due to excel time formatting); 
tracker.clean$Time.of.encounter
tracker.clean$Departure.Time

# only the encounter time is needed 

# convert to HH:MM:SS format
# method: http://stackoverflow.com/questions/12863841/converting-char-to-date-time
tracker.clean$encounter.time <- format(tracker.clean$Time.of.encounter, format="%H:%M")
# change from character to factor
tracker.clean$encounter.time <- as.character(tracker.clean$encounter.time)

data.prep$`Corrected?`[15] <- "Y"
data.prep$`Correction Date` [15] <- "Tue Apr 25"
data.prep$`Correction Notes` [15] <- "corrected with format() and strptime ()"


#### create combined datetime column ####

as.character(tracker.clean$Date)
# create combine collumn
tracker.clean$encounter.datetime <- paste(tracker.clean$Date, tracker.clean$encounter.time)
# remove trailing whitespace
trim.trailing(tracker.clean$encounter.datetime)
#convert to time format
tracker.clean$encounter.datetime <- strptime(tracker.clean$encounter.datetime, format = "%Y-%m-%d %H:%M", tz = "HST") 

data.prep$`Corrected?`[16] <- "Y"
data.prep$`Correction Date` [16] <- "Tue Apr 25"
data.prep$`Correction Notes` [16] <- "time and date combined with paste() and formatted with strptime"

#### Sighting location ####



#### Remove sightings on land ####



#### Sighting Notes ####

# - present instead of NA

# Locate - entries and note trip ID for verification
tracker.clean[tracker.clean$Sighting.Notes == "-",1]
# [1] 20170101_OI_0900 20170102_OQ_0800 20170102_OS_1230

# replace - with NA
tracker.clean[tracker.clean$Sighting.Notes == "-",5] <- NA

# Check process has completed correctly 
tracker.clean[tracker.clean$Trip.ID == "20170101_OI_0900",5]
#[1] <NA>

# update data.prep 

data.prep$`Corrected?`[20] <- "Y"
data.prep$`Correction Date` [20] <- "Wed Apr 26"
data.prep$`Correction Notes` [20] <- "entries called with [] and overwritten"


#### Animal Count ####

tracker.clean$correct.count <- tracker.clean$Animal.count == tracker.clean$Adult.count + tracker.clean$Juvenile.count + tracker.clean$Calf.count

unique(tracker.clean[,1][tracker.clean$correct.count == FALSE])
# [1] NA                 "20160311_OO_1230" "20160409_OO_1400" "20160722_OV_0800" "20160804_OQ_0800"
# [6] "20160814_OQ_0800" "20160825_OQ_0800" "20161119_OQ_0800" "20150409_OQ_0800" "20150623_OV_0830"
# [11] "20150705_OS_0700" "20150706_OS_0700" "20150706_OQ_0800" "20150707_OA_0730" "20150708_OS_0700"
# [16] "20150708_OA_0730" "20150714_OS_0700" "20150716_OA_0730" "20150720_OA_0730" "20150721_OA_0730"
# [21] "20150725_OV_0830" ""                 "20150808_OS_0700" "20150808_OA_0730" "20150812_OA_0730"
# [26] "20150813_OA_0730" "20150813_OS_0900" "20131202_OL_0800" "20130121_OQ_0800" "414999_OO_0830" 

data.prep$`Corrected?`[23] <- "Not possible"
data.prep$`Correction Date`[23] <- "Fri Apr 28"
data.prep$`Correction Notes` [23] <- "To be used as best estimates - error location unclear"

#### Calf Count ####

unique(tracker.clean$Calf.count)
# [1]   NA    0    3    1    5    4    2    8   15   10   20    6   30 9999   25
# -9999 is used as a null value by  PWF so replace with NA
tracker.clean$Calf.count[tracker.clean$Calf.count == 9999] <- NA

data.prep$`Corrected?`[24] <- "Y"
data.prep$`Correction Date` [24] <- "Thu Apr 27"
data.prep$`Correction Notes` [24] <- "9999 is PWF NA- replaced with NA"

# High counts require verification: 8,10,15,20,25,30,
# Collate information about high entries in an object
calfcount.errors <- subset(tracker.clean, Calf.count >= 8)
nrow(calfcount.errors)
# [1] 27
# 27 entries with suggestive calf counts 
# Potentially a select number of observers?

# Check which staff member reported these numbers and how often
aggregate(data.frame(count = calfcount.errors$Staff.name2), list(value = calfcount.errors$Staff.name2), length)
#       aaron bement     1
#  brittany guenther     3
#     callen miracle     1
#     chelsea anglin     2
#      chris cilfone     9 # high number- check other calf counts for this observer
#    colleen farrell     1
#         dan greene     1
#         dan kraver     1
#     hannah pittore     3
#        jenny heard     1
#      jenny roberts     1
#         lilli mack     2
#       nik spurgeon     1

# check chris cilfone's calf counts
table(tracker.clean[tracker.clean$Staff.name2 == "chris cilfone", 20])
# calf count  0  1  2  3  4  5  6 10 25 
# instances   1  4  4  3  2 14  1  8  1


# Check calf counts against animal counts to check feasibility 
calfcount.errors[calfcount.errors$Calf.count == 8, 22]
# [1]  88  98  80 100 103 150 100  75 198  78

calfcount.errors[calfcount.errors$Calf.count == 10, 22]
# [1]  90 160  70 110 130 185 160 100 110 110 100 100 100

calfcount.errors[calfcount.errors$Calf.count == 15, 22]
# [1] 95

calfcount.errors[calfcount.errors$Calf.count == 20, 22]
# [1] 120

calfcount.errors[calfcount.errors$Calf.count == 25, 22]
# [1] 150

data.prep$`Corrected?`[25] <- "Y"
data.prep$`Correction Date` [25] <- "Thu Apr 27"
data.prep$`Correction Notes` [25] <- "entries line up with large pod sizes- supervisor advice required"


#### Calves Present ####

# Check if entries with calves marked as present have a calf count 
unique(tracker.clean[tracker.clean$Calves.present == "yes",20])
#  [1]   NA    1    5   30   10    0    4 9999    2    3   25    6
unique(tracker.clean[tracker.clean$Calves.present == "no",20])
# [1] NA  0  2  1  3  5

# check calves present entry when calf count is positive
unique(tracker.clean[tracker.clean$Calf.count >= 1,23])
# [1] <NA>    unknown yes     no
unique(tracker.clean[tracker.clean$Calf.count == 0,23])
# [1] <NA>    unknown no      yes 

# Collumn is not correct- likely easier to disregard and create new collumn if necessary
# confirmed by PWF- default is 'no', whereas calfcount default is NA, so count is the more reliable collumn
data.prep$`Corrected?` [26] <- "Y"
data.prep$`Correction Date` [26] <- "Tue May 02"
data.prep$`Correction Notes` [26] <- "Disregard- default is 'no', calfcount is more reliable'"

#### Missing Information ####

# Sightings missing information (sighting ID listed with missing info)

# 22030; missing: Trip.ID Boat.Initials Date ;
# Notes say 'Entered by RD 20150727' - suggests this is this date is date of entry not date of sighting
# 22080; missing;Trip.ID Boat.Initials Date
# 22111; missing;Trip.ID Boat.Initials Date
# Notes say 'Entered by RD 20150811' - suggests this is this date is date of entry not date of sighting

data.prep$`Corrected?`[19] <- "Pending"
data.prep$`Correction Notes` [19] <- "Information requested from PWF 27/4/17"

#### Check for Duplicate sightings ####

## Find potential duplicates ####
# create single species and date collumn
tracker.clean$specdate <- paste(tracker.clean$Species, tracker.clean$Date)
# create data frame of potential duplicate sightings 
species.date <- aggregate(data.frame(count = tracker.clean$specdate), list(value = tracker.clean$specdate), length)
# count number of days with potentially duplicate sightings 
species.date[species.date$count >1,]
# 1189 instances where the same species was sighted on more than one occasion on the same day
sum(species.date$count[species.date$count >1])
# [1] 1792 

# leave only duplicate sightings in this list
species.date <- species.date[species.date$count >1,]

#### Investigate if actual duplicate ####

### Create potential duplicate? collumn ###

# Check if pod sizes & sighting times are similar
tracker.clean$potdup <- "no"

for (i in 2:1189){
   w <- species.date[i,1]
   tracker.clean[tracker.clean$specdate == w,38] <- "yes"
}

unique(tracker.clean$potdup)
aggregate(data.frame(count = tracker.clean$potdup), list(value = tracker.clean$potdup), length)
#   value count
# 1    no  1063
# 2   yes  1789

# somehow we've gone from 1792 duplicates to 1789... 3 missing 
# are they incorrectly labelled no?
# create list of specdate where sightings are labelled no
no.dup <- tracker.clean[tracker.clean$potdup == "no",37]
no.dup <- as.data.frame(no.dup)

# find values that are potential duplicates that occure in the list of labelled no's
incorrect.no <- no.dup$no.dup %in% species.date$value

# count how many are TRUE
aggregate(data.frame(count = incorrect.no), list(value = incorrect.no), length)
#    value count
# 1 FALSE  1060
# 2  TRUE     3

# there's the 3 missing values 
# label them all 
no.dup$incorrect <- no.dup$no.dup %in% species.date$value
# look up the TRUE values to find the ones incorrectly labelled no
no.dup[no.dup$incorrect == TRUE,]
#                                no.dup correct incorrect
# 897 BND Bottlenose Dolphin 2013-01-01    TRUE      TRUE
# 898 BND Bottlenose Dolphin 2013-01-01    TRUE      TRUE
# 899 BND Bottlenose Dolphin 2013-01-01    TRUE      TRUE

# lookup that specdate in species.date
species.date[species.date$value == "BND Bottlenose Dolphin 2013-01-01",]
#                               value count
# 2 BND Bottlenose Dolphin 2013-01-01     3 

# it occurs 3 times there too, so the numbers line up... why has it not reballed potdup???
# do it manually 
tracker.clean[tracker.clean$specdate == "BND Bottlenose Dolphin 2013-01-01",38] <- "yes"

#### analyse similarity of sightings ####

## Pod Size

unique(tracker.clean$Animal.count[tracker.clean$potdup == "yes"])

data.prep$`Corrected?`[18] <- "Labelled"
data.prep$`Correction Date`[18] <- "Fri Apr 28"
data.prep$`Correction Notes` [18] <- "To be dealt with at modelling level"


#### Update summary ####

clean.summary <- capture.output(str(tracker.clean))

tracker.summary <- data.frame("Fields" = c(colnames(tracker.clean)), "Levels" = "NA", "Examples" = "NA", "Description" = "NA")
tracker.summary$Levels <- as.character(tracker.summary$Levels)
tracker.summary$Examples <- as.character(tracker.summary$Examples)
tracker.summary$Description <- as.character(tracker.summary$Description)

for(i in 1:nrow(tracker.clean)) 
  {
  x <- nrow(unique(tracker.clean[i]))
  tracker.summary[i,2] <- x
  y <- unique(tracker.clean[i])
  if( nrow(y) < 10) {
    z <- nrow(y)
    } else{
    (z <- 10)
      }
  tracker.summary[i,3] <- toString(y[1:z,])
}

# Fill in details of each field 

# [1] Trip.ID            Boat.Initials      Track.Status       Sighting.Status    Sighting.Notes    
# [6] Sighting.ID        Trip.type          Date               Time.of.encounter  Staff.name        
# [11] Depth..ft.         Sea.state          Water.temp         Latitude           Longitude         
# [16] Species            Activity           Adult.count        Juvenile.count     Calf.count        
# [21] Unknown.age.count  Animal.count       Calves.present     Inter.species      Multiple.groups   
# [26] Notes              Departure.Time     X                  X.1                X.2               
# [31] Staff.name2        encounter.time     encounter.datetime Activity.code      location          
# 35 Levels: Activity Activity.code Adult.count Animal.count Boat.Initials Calf.count Calves.present ... 

tracker.summary$Description[1] <- "unique ID comprised of date_boat initials_departure time"
tracker.summary$Description[2] <- "Initials of the PWF vessel sighting was made on eg OV for Ocean Voyager"
tracker.summary$Description[3] <- "Status of the GPS track for that vessel trip"
tracker.summary$Description[4] <- "Status of sighting (PWF field)"
tracker.summary$Description[5] <- "Notes regarding GPS track or sighting"
tracker.summary$Description[6] <- "Unique ID for sighting (generated by Tracker)"
tracker.summary$Description[7] <- "Type of tourism trip vessel undertaking at time of sighting"
tracker.summary$Description[8] <- "Date trip took place"
tracker.summary$Description[9] <- "Time encounter logged into Tracker (may be different from true begining of sighting due to delay in entry)"
tracker.summary$Description[10] <- "Name of staff member entering sighting into Tracker"
tracker.summary$Description[11] <- "Water depth at location of sighting"
tracker.summary$Description[12] <- "Beaufort sea state at time & location of sighting"
tracker.summary$Description[13] <- "Water temperature at sighting location"
tracker.summary$Description[14] <- "Latitude of location when sighting was logged (may be different from true begining of sighting due to delay in entry)"
tracker.summary$Description[15] <- "Longitude of location when sighting was logged (may be different from true begining of sighting due to delay in entry)"
tracker.summary$Description[16] <- "Species sighted"
tracker.summary$Description[17] <- "Behaviour (categorical) of groupat beginning of sighting"
tracker.summary$Description[18] <- "Number of adults counted in the group"
tracker.summary$Description[19] <- "Number of confirmed juveniles counted in the group"
tracker.summary$Description[20] <- "Number of confirmed calves counted in the group"
tracker.summary$Description[21] <- "Number of of individuals of unconfirmed age counted in the group"
tracker.summary$Description[22] <- "Total number of individuals"
tracker.summary$Description[23] <- "If calves were present in the group"
tracker.summary$Description[24] <- "If an inter-species interaction was observed (not just being present in a same vicinity)"
tracker.summary$Description[25] <- "If individuals were in more than one group when sighted"
tracker.summary$Description[26] <- "Notes from observer"
tracker.summary$Description[27] <- "Scheduled (not actual) time of departure from harbour"
tracker.summary$Description[28] <- "Unknown - PWF field"
tracker.summary$Description[29] <- "Unknown - PWF field"
tracker.summary$Description[30] <- "Unknown - PWF field"
tracker.summary$Description[31] <- "Edited & corrected staff name (HS field)"
tracker.summary$Description[32] <- "Reformatted encounter time (HS field)"
tracker.summary$Description[33] <- "Formatted cmbined date & time of sighting (HS field)"
tracker.summary$Description[34] <- "Code only of acitvity (HS field)"
tracker.summary$Description[35] <- "Latitude : Longitude of sighting (HS field)"
tracker.summary$Description[36] <- "True/False calculcation of if adults+juveniles+calves = animal count"

#### Data exploration ####
# Done in a meeting with David Lusseau
# "Wed May 03 2017"

aggregate(data.frame(count = tracker.clean$Species), list(value = tracker.clean$Species), length)

tracker.clean.bnd <- tracker.clean[tracker.clean$Species == "BND Bottlenose Dolphin",]

hist(tracker.clean.bnd$Adult.count)
plot(density(log10(tracker.clean.bnd$Adult.count)))
unique(tracker.clean.bnd$Animal.count)

tracker.clean.bnd <- tracker.clean.bnd[tracker.clean.bnd$Animal.count != 999,]
tracker.clean.bnd <- tracker.clean.bnd[!is.na(tracker.clean.bnd$Animal.count),]
