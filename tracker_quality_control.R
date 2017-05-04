#####################################
########### Holly Self ##############
## MSc AMFE Univeristy of Aberdeen ##
###### ZO5901 Research Project ######
#####################################

#### DATA DESCRIPTION ####
# opportunistic sightings from Whale & Dolphin Tracker
# data provided by the Pacific Whale Foundation
# subject to an MOU.

#### CODE DESCRIPTION ####
# Data quality control 
# Data profiling to discover inconsistencies and other anomalies in the data, 
# as well as performing data cleansing activities (e.g. removing outliers, 
# missing data interpolation) to improve the data quality.

#### DISTRIBUTION ####
# This code was written by Holly Self, and shared with the PWF
# subject to an MOU agreed by the PWF, Holly Self, and the University of Aberdeen
# It is not to be distributed beyond the PWF 

######################
#### Data cleanup ####
######################

################
#### Set Up ####
################
# assign working directory address for ease of adjustment 
wd <- "C:\\Users\\Holly\\Google Drive\\Masters\\Thesis"
# set working directory
setwd(wd)

##########################
#### Read in raw data ####
##########################

tracker.raw <- read.csv("Data\\tracker.csv", header = TRUE)

#####################
#### Assess Data ####
#####################

# write structure to object for ease of re-analysis
raw.summary <- capture.output(str(tracker.raw))

####### Issues reported by PWF #########

# Issues reported by Jens Currie on delivery of data;

# This data set does not account for duplicate sightings, which happens when multiple vessels report the same group of dolphins. 
# The latitude and longitude data have not been verified and I expect you will find quite a few sightings on land as well as outside the probable study area (leeward waters of Maui). 
# The data will be variable by year, with the initial 2013  season having a lot of missed days and data collection improving thereafter.

############# Found Issues #############

#               Field                                                         Issue
#1             Trip ID                                      incorrect field (factor)
#2      Sighting.Notes                                      incorrect field (factor)
#3               Notes                                      incorrect field (factor)
#4        Track Status                                     Duplicate/unlabled levels
#5     Sighting Status                                     Duplicate/unlabled levels
#6           Trip Type                                     Duplicate/unlabled levels
#7      Calves Present                                     Duplicate/unlabled levels
#8       Inter-species                                     Duplicate/unlabled levels
#9     Multiple Groups                                     Duplicate/unlabled levels
#10         Staff Name                                  Capitalisation inconsistency
#11         Staff Name                                           Trailing whitespace
#12         Staff Name                                               Spelling Errors
#13         Staff Name                                               Missing surname
#14      Activity code                                              long level names
#15               Time                                          Incorrect field type
#16           DateTime                                           single field needed
#17 Latitude/Longitude                                      erroneous (land) entries
#18 Latitude/Longitude                            autocorrelation- duplicate entries
#19           Multiple Sightings 22030,22080 & 22111 missing multiple pieces of info
#20     Sighting notes                                     - rather than NAs present
#21  Time of encounter                                   midnight times (incorrect) 
#22  Time of encounter                                       03:19 time (incorrect) 
#23       Animal count                       Animal count /= adults+juveniles+calves
#24         Calf count                         obviously erroneous entires (9999,30)
#25         Calf count                            High entries- require verification
#26     Calves present                 yes/no does not line up with calf count entry

# note: 'land' sightings will be tackled at a later stage 

##################################
######### Start Cleanup ##########
##################################

#Write new data frame to preserve raw data

tracker.clean <- tracker.raw

#### Convert Collumn Types ####

# the following fields need to be 'character'
tracker.clean$Trip.ID <- as.character(tracker.clean$Trip.ID)
tracker.clean$Sighting.Notes <- as.character(tracker.clean$Sighting.Notes)
tracker.clean$Notes <- as.character(tracker.clean$Notes)

#### Cleanup Levels ####

####  Simple Fixes ####

# The following fields have capitalisation errors and only a few levels,
# so fixes are done manually - eg two levels being 'no' and 'No' 
# Levels listed before and after 
# 'unknown' used to replace blank entries 

# NOTE: other meothods used further on in the process, such as trimtrailing and toolower, 
# may be of use here, but this was done previous to that process and so is conducted 
# differently 

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

#### Staff name ####

#### Capitalisation errors ####

# convert all staff names to lower case to remove capitilisation errors
tracker.clean$Staff.name <- sapply(tracker.clean$Staff.name, tolower)
# convert from character back to factor for ease of analysis
tracker.clean$Staff.name <- factor(tracker.clean$Staff.name)
# This has reduced levels from 265 to 198 

#### Trailing whitespace errors ####

# trim trailing whitespace function from stack overflow
# "http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r"
trim.trailing <- function (x) sub("\\s+$", "", x)
# Trim whitespace
tracker.clean$Staff.name <- trim.trailing(tracker.clean$Staff.name)
# Reconvert to factor 
tracker.clean$Staff.name <- factor(tracker.clean$Staff.name)

####### Obvious typos #######

# The typo is listed next to (=) the correct spelling. Where hypens or other special
# characters exist these have been replaced eg i replaces í

### new collumn made for ease of error correction 
tracker.clean$Staff.name2 <- tracker.clean$Staff.name

#### "brian t" = "brian totman" 

## Before
tracker.clean[tracker.clean$Staff.name2 == "brian t", 1]
# [1] "20131220_OL_0700"

levels(tracker.clean$Staff.name2)[match("brian t",levels(tracker.clean$Staff.name2))] <- "brian totman"

## After 
tracker.clean[tracker.clean$Staff.name2 == "brian t", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20131220_OL_0700",31]
# [1] brian totman

#### "brittany guent" = "brittany guenther"

## Before
tracker.clean[tracker.clean$Staff.name2 == "brittany guent", 1]
# [1] "20150227_OS_0830"

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
# [16] "20150725_OV_0830" "20151125_OO_1230" "20151127_OI_1130"

levels(tracker.clean$Staff.name2)[match("megan o\\'hara",levels(tracker.clean$Staff.name2))] <- "megan o'hara"

## After
tracker.clean[tracker.clean$Staff.name2 == "megan o\\'hara", 1]
# character(0)
tracker.clean[tracker.clean$Staff.name2 == "megan o'hara", 1]
# [1] "20170112_OI_0830" "20170119_OE_1415" "20170217_OI_1045" "20170310_OV_0630" "20170317_OV_1700"
# [16] "20150725_OV_0830" "20151125_OO_1230" "20151127_OI_1130"

### "mikhail reedy" = "mikhala reedy"
## Before
tracker.clean[tracker.clean$Staff.name2 == "mikhail reedy", 1]
# [1] "20161213_OQ_0800"

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

levels(tracker.clean$Staff.name2)[match("allee",levels(tracker.clean$Staff.name2))] <- "allee jimenez"

## After
tracker.clean[tracker.clean$Staff.name2 == "allee", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20161013_OD_1700",31]
# [1] allee jimenez

# "brigid" = "brigid magee" 
##Before
tracker.clean[tracker.clean$Staff.name2 == "brigid", 1]
#[1] "20150713_OV_0830"

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

# levels(tracker.clean$Staff.name2)[match("sergio",levels(tracker.clean$Staff.name2))] <- "sergio burgos"

## After
tracker.clean[tracker.clean$Staff.name2 == "sergio", 1]
tracker.clean[tracker.clean$Trip.ID == "414999",31]

### "tim"  "tim barker" 
##Before
tracker.clean[tracker.clean$Staff.name2 == "tim", 1]
# [1] "20130910_OE_0730" "20130911_OE_0730"

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

levels(tracker.clean$Staff.name2)[match("dav",levels(tracker.clean$Staff.name2))] <- "dav yuan"
levels(tracker.clean$Staff.name2)[match("dav y",levels(tracker.clean$Staff.name2))] <- "dav yuan"

## After
tracker.clean[tracker.clean$Staff.name2 == "dav", 1]
# character(0)
tracker.clean[tracker.clean$Staff.name2 == "dav y", 1]
# character(0)
tracker.clean[tracker.clean$Trip.ID == "20130814_OE_0730",31]
# [1] dav yuan

###### Missing surname- verification of individual required 

### "annie" - [19] "annie donahoe", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "annie", 1]

##Before
tracker.clean[tracker.clean$Staff.name2 == "annie", 1]
# [1] 20140130_OD_1400

levels(tracker.clean$Staff.name2)[match("annie",levels(tracker.clean$Staff.name2))] <- "annie donahoe"

## After
tracker.clean[tracker.clean$Staff.name2 == "annie", 1]
tracker.clean[tracker.clean$Trip.ID == "20140130_OD_1400",31]
# [1] annie donahoe

### "chelsea" =  "chelsea aydelott", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "chelsea", 1]
# [1] 20131128_OI_1130

levels(tracker.clean$Staff.name2)[match("chelsea",levels(tracker.clean$Staff.name2))] <- "chelsea aydelott"

## After
tracker.clean[tracker.clean$Staff.name2 == "chelsea", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20131128_OI_1130",31]
# [1] chelsea aydelott

### "jackson" = "jackson kowalski", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "jackson", 1]
# [1] 20130723_OF_0700

levels(tracker.clean$Staff.name2)[match("jackson",levels(tracker.clean$Staff.name2))] <- "jackson kowalski"

## After
tracker.clean[tracker.clean$Staff.name2 == "jackson", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20130723_OF_0700",31]
# [1] chelsea aydelott

### "saanah" = "sarah sinn-white", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "saanah", 1]
#[1] 20130630_OQ_0800

levels(tracker.clean$Staff.name2)[match("saanah",levels(tracker.clean$Staff.name2))] <- "sarah sinn-white"

## After
tracker.clean[tracker.clean$Staff.name2 == "saanah", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20130630_OQ_0800",31]
# [1] sarah sinn-white

### "v" = "victoria persson", confirmed by PWF
tracker.clean[tracker.clean$Staff.name2 == "v", 1]
# [1] 20130708_OV_0900

levels(tracker.clean$Staff.name2)[match("v",levels(tracker.clean$Staff.name2))] <- "victoria persson"

## After
tracker.clean[tracker.clean$Staff.name2 == "v", 1]
# factor(0)
tracker.clean[tracker.clean$Trip.ID == "20130708_OV_0900",31]
# [1] victoria persson

#### Activity ####

levels(tracker.clean$Activity)
# this will be used to create a new collumn containing the code only for ease 
# as full description is not needed 

tracker.clean$Activity.code <- tracker.clean$Activity

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

# Midnight entries- Need confirmation of sighting time by PWF 
# Information requested from PWF 27/4/14

tracker.clean[tracker.clean$Time.of.encounter == "00:00",1]
# [1] "20150122_OQ_0800" "20150726_OD_0900" "20151003_OS_0700" "20151003_OE_0730" "20151005_OS_0900"
# [6] "20140728_OA_0730" "20131018_OD_0800"

# Are there tracks to get this info from?
tracker.clean[tracker.clean$Time.of.encounter == "00:00",3]
# [1] complete none     edited   none     complete complete edited  

#### Time Format ####

# two time collumns (not currently read correctly due to excel time formatting); 
tracker.clean$Time.of.encounter
tracker.clean$Departure.Time

# only the encounter time is needed for analysis

# convert to HH:MM:SS format
tracker.clean$encounter.time <- format(tracker.clean$Time.of.encounter, format="%H:%M")
# change from factor to character
tracker.clean$encounter.time <- as.character(tracker.clean$encounter.time)

#### create combined datetime column ####

# create combined datetime collumn
tracker.clean$encounter.datetime <- paste(tracker.clean$Date, tracker.clean$encounter.time)
# remove trailing whitespace
tracker.clean$encounter.datetime <- trim.trailing(tracker.clean$encounter.datetime)
#convert to time format
tracker.clean$encounter.datetime <- strptime(tracker.clean$encounter.datetime, format = "%Y-%m-%d %H:%M", tz = "HST") 

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

#### Animal Count ####

tracker.clean$correct.count <- tracker.clean$Animal.count == tracker.clean$Adult.count + tracker.clean$Juvenile.count + tracker.clean$Calf.count

unique(tracker.clean[,1][tracker.clean$correct.count == FALSE])
# [1] NA                 "20160311_OO_1230" "20160409_OO_1400" "20160722_OV_0800" "20160804_OQ_0800"
# [6] "20160814_OQ_0800" "20160825_OQ_0800" "20161119_OQ_0800" "20150409_OQ_0800" "20150623_OV_0830"
# [11] "20150705_OS_0700" "20150706_OS_0700" "20150706_OQ_0800" "20150707_OA_0730" "20150708_OS_0700"
# [16] "20150708_OA_0730" "20150714_OS_0700" "20150716_OA_0730" "20150720_OA_0730" "20150721_OA_0730"
# [21] "20150725_OV_0830" ""                 "20150808_OS_0700" "20150808_OA_0730" "20150812_OA_0730"
# [26] "20150813_OA_0730" "20150813_OS_0900" "20131202_OL_0800" "20130121_OQ_0800" "414999_OO_0830" 

# This error is going to be left, and numbers will be used as 'best estimates' during analysis
# due to uncertainties around the origin of the error/mismatch

#### Calf Count ####

unique(tracker.clean$Calf.count)
# [1]   NA    0    3    1    5    4    2    8   15   10   20    6   30 9999   25
# -9999 is used as a null value by  PWF so replace with NA
tracker.clean$Calf.count[tracker.clean$Calf.count == 9999] <- NA

# High counts require verification: 8,10,15,20,25,30,
# Collate information about high entries in an object
calfcount.errors <- subset(tracker.clean, Calf.count >= 8)
nrow(calfcount.errors)
# [1] 27
# 27 entries with suggestive calf counts 

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

# High calf estimates line up with large pod sizes, so they will be used as best estimates

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
# therefor Calves.Present will be disregarded 

#### Missing Information ####

# Sightings missing information (sighting ID listed with missing info)

# 22030; missing: Trip.ID Boat.Initials Date ;
# Notes say 'Entered by RD 20150727' - suggests this is this date is date of entry not date of sighting
# 22080; missing;Trip.ID Boat.Initials Date
# 22111; missing;Trip.ID Boat.Initials Date
# Notes say 'Entered by RD 20150811' - suggests this is this date is date of entry not date of sighting

#### Check for Duplicate sightings ####

## Find potential duplicates ####
# create single species and date collumn (specdate) for ease of comparison
tracker.clean$specdate <- paste(tracker.clean$Species, tracker.clean$Date)
# create data frame listing the number of instances of each specdate combination
species.date <- aggregate(data.frame(count = tracker.clean$specdate), list(value = tracker.clean$specdate), length)
# count number of days with potentially duplicate sightings (where count is >1)
species.date[species.date$count >1,]
# 1189 instances (days) where the same species was sighted on more than one occasion on the same day
# count number of sightings that are potential duplucates (not days)
sum(species.date$count[species.date$count >1])
# [1] 1792 

# leave only potentially duplicate sightings in this list
species.date <- species.date[species.date$count >1,]

### Create 'potential duplicate?' collumn in tracker.clean ###

# create potdup (potenial duplicate) collumn, default entry is no
tracker.clean$potdup <- "no"

# iterate through the list of potential duplicates (species.date from above)
# and label any specdates present in both tracker.clean and species.date 'yes'

for (i in 2:1189){
  w <- species.date[i,1]
  tracker.clean[tracker.clean$specdate == w,38] <- "yes"
}

# check if labelling worked correctly 
aggregate(data.frame(count = tracker.clean$potdup), list(value = tracker.clean$potdup), length)
#   value count
# 1    no  1063
# 2   yes  1789

# somehow we've gone from 1792 duplicates to 1789... 3 missing 
# are they incorrectly labelled no, ie the loop missed them?
# create list of specdate where sightings are labelled no
no.dup <- tracker.clean[tracker.clean$potdup == "no",37]
no.dup <- as.data.frame(no.dup)

# find values that ARE potential duplicates that occur in the list of labelled no's
# ie find entries incorrectly labelled 'no' in potdup (will be labelled TRUE,
# as they DO appear in both the list of potential duplicates and the list labelled
# 'no' as a potential duplicate)

# search species.date for no.dup entries
incorrect.no <- no.dup$no.dup %in% species.date$value

# count how many are TRUE 
aggregate(data.frame(count = incorrect.no), list(value = incorrect.no), length)
#    value count
# 1 FALSE  1060
# 2  TRUE     3

# there's the 3 missing values (they are incorrectly labelled no)
# label them in no.dup to indicate this
no.dup$incorrect <- incorrect.no
# look up this TRUE value in no.dup to find which sightings are incorrectly labelled no
no.dup[no.dup$incorrect == TRUE,]
#                                no.dup incorrect
# 897 BND Bottlenose Dolphin 2013-01-01      TRUE     
# 898 BND Bottlenose Dolphin 2013-01-01      TRUE     
# 899 BND Bottlenose Dolphin 2013-01-01      TRUE     

# lookup that specdate in species.date
species.date[species.date$value == "BND Bottlenose Dolphin 2013-01-01",]
#                               value count
# 2 BND Bottlenose Dolphin 2013-01-01     3 

# it occurs 3 times there too, so the numbers line up... why has it not reballed potdup???
# do it manually 
tracker.clean[tracker.clean$specdate == "BND Bottlenose Dolphin 2013-01-01",38] <- "yes"