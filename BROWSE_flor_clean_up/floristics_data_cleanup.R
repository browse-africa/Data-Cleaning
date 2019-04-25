#load tidyverse library
library(tidyverse)

#load raw data (2013)
raw_data <- read.csv("data_raw/knp_floristics_raw_2013.csv", dec=".", header = TRUE, sep = ",", na.strings=c("","N/A","NA"))

#change column names
colnames(raw_data)
raw_data <- raw_data[,-c(2,3,7,8,36)]
raw_data <- raw_data %>%
  rename("date" = "Date") %>%
  rename("grid" = "Grid") %>%
  rename("plot" = "Plot") %>%
  rename("section_code" = "Section.code") %>%
  rename("species" = "species..correctly.spelled..") %>%
  rename("height" = "Height..cm.") %>%
  rename("num_stems" = "X..stems") %>%
  rename("stem_1_diam" = "stem.1.diameter..cm.") %>%
  rename("stem_2_diam" = "stem.2.diameter..cm.") %>%
  rename("stem_3_diam" = "stem.3.diameter..cm.") %>%
  rename("stem_4_diam" = "stem.4.diameter..cm.") %>%
  rename("stem_5_diam" = "stem.5.diameter..cm.") %>%
  rename("canopy_breakage" = "X.break") %>%
  rename("breakage_age" = "age..N.O.N.A.") %>%
  rename("breakage_resprout" = "sprout..Y..N.") %>%
  rename("circum" = "X..circum") %>%
  rename("add_circum" = "X..Additive") %>%
  rename("bark_stripped" = "X....3m") %>%
  rename("strip_age" = "age..n.o.N.A.") %>%
  rename("bark_regrow" = "X..regro") %>%
  rename("borer" = "X..borer") %>%
  rename("fire" = "X..fire") %>%
  rename("toppled" = "topple..y.n.") %>%
  rename("toppled_resprout" = "resprout..") %>%
  rename("elephant_damage" = "ele...y.n.N.A.") %>%
  rename("alive" = "alive..y.n.") %>%
  rename("notes" = "Notes") %>%
  rename("data_collector" = "Data.Collector") %>%
  rename("scribe" = "Data.Enterer") %>%
  rename("total_basal_area" = "total.basal.area")

#separate species column into genus and species
raw_data <- separate(raw_data, species, c("genus", "species"), sep = " ", remove = TRUE, extra = "merge")

#separate section_code into side, zone, fixed_area
raw_data <- separate(raw_data, section_code, c("zone", "side", "fixed_area"), sep = c(1,2), remove = TRUE, extra = "merge")

#create season column
raw_data$season <- NA
raw_data$season[grepl("May",raw_data$date)] <- "mj13"
raw_data$season[grepl("Jun",raw_data$date)] <- "mj13"
raw_data$season[grepl("Nov",raw_data$date)] <- "nd13"
raw_data$season[grepl("Dec",raw_data$date)] <- "nd13"

#remove extra spaces in breakage_age, breakage_resprout, alive
raw_data$breakage_age <- as.factor(trimws(raw_data$breakage_age))
raw_data$breakage_resprout <- as.factor(trimws(raw_data$breakage_resprout))
raw_data$alive <- as.factor(trimws(raw_data$alive))

#reformat breakage_resprout levels
levels(raw_data$breakage_resprout)
raw_data$breakage_resprout[raw_data$breakage_resprout == "n"] <- "N"
raw_data$breakage_resprout<-droplevels(raw_data$breakage_resprout)
levels(raw_data$breakage_resprout)

#reformat elephant_damage levels
levels(raw_data$elephant_damage)
raw_data$elephant_damage[raw_data$elephant_damage == "U"] <- NA
raw_data$elephant_damage<-droplevels(raw_data$elephant_damage)
levels(raw_data$elephant_damage)

#change fixed_area levels
raw_data$fixed_area <- as.character(raw_data$fixed_area)
raw_data$fixed_area[raw_data$fixed_area == "02"] <- "<2"
raw_data$fixed_area[raw_data$fixed_area == "10"] <- "2-10"
raw_data$fixed_area[raw_data$fixed_area == "25"] <- "10-25"
raw_data$fixed_area <- as.factor(raw_data$fixed_area)
raw_data$fixed_area <- droplevels(raw_data$fixed_area)
levels(raw_data$fixed_area)

#reformat date (standard ISO yyyy-mm-dd)
raw_data$date <- as.Date(raw_data$date, "%d-%b-%Y")
raw_data$date <- format(raw_data$date, "%d-%b-%Y")

#reformat stem diameter levels
raw_data$stem_2_diam[raw_data$stem_2_diam == "-"] <- NA
raw_data$stem_2_diam[raw_data$stem_2_diam == "0"] <- NA
raw_data$stem_3_diam[raw_data$stem_3_diam == "-"] <- NA
raw_data$stem_3_diam[raw_data$stem_3_diam == "0"] <- NA
raw_data$stem_4_diam[raw_data$stem_4_diam == "-"] <- NA
raw_data$stem_4_diam[raw_data$stem_4_diam == "0"] <- NA
raw_data$stem_5_diam[raw_data$stem_5_diam == "-"] <- NA
raw_data$stem_5_diam[raw_data$stem_5_diam == "0"] <- NA



#load raw data (2016)
raw_data_16 <- read.csv("data_raw/knp_floristics_raw_2016.csv", dec=".", header = TRUE, sep = ",", na.strings=c("","NA","N/A"))

#change column names
colnames(raw_data_16)
raw_data_16 <- raw_data_16[,-c(17,18,19)]
raw_data_16 <- raw_data_16 %>%
  rename("ref" = "index") %>%
  rename("grid" = "Grid") %>%
  rename("plot" = "Plot") %>%
  rename("way_point" = "Way.Point") %>%
  rename("gps" = "GPS.used") %>%
  rename("tag" = "Tag..") %>%
  rename("species" = "Species") %>%
  rename("height" = "Ht..cm.") %>%
  rename("stem_1_diam" = "Diameter.stem.1") %>%
  rename("stem_2_diam" = "Diameter.stem.2") %>%
  rename("stem_3_diam" = "Diameter.stem.3") %>%
  rename("stem_4_diam" = "Diameter.stem.4") %>%
  rename("stem_5_diam" = "Diameter.stem.5") %>%
  rename("num_stems" = "X..of.stems") %>%
  rename("canopy_breakage" = "X.") %>%
  rename("breakage_age" = "Age") %>%
  rename("breakage_resprout" = "Respr") %>%
  rename("circum" = "X..circum") %>%
  rename("add_circum" = "Add.circum") %>%
  rename("bark_stripped" = "X..3m..") %>%
  rename("strip_age" = "Age.1") %>%
  rename("bark_regrow" = "X..Regrow") %>%
  rename("borer" = "Borers..") %>%
  rename("fire" = "Fire..y.n.") %>%
  rename("toppled" = "Toppled..y.n.") %>%
  rename("toppled_resprout" = "Respr.1") %>%
  rename("elephant_damage" = "Elephant..y.n.") %>%
  rename("alive" = "Alive..y.n.") %>%
  rename("scribe" = "Data.entered.by")
  
#separate species column into genus and species
raw_data_16 <- separate(raw_data_16, species, c("genus", "species"), sep = " ", remove = TRUE, extra = "merge")

#change date format
raw_data_16$date <- as.Date(raw_data_16$date, "%d-%b-%y")
raw_data_16$date <- format(raw_data_16$date, "%d-%b-%Y")

#change location levels
levels(raw_data_16$location)
raw_data_16$location <- as.character(raw_data_16$location)
raw_data_16$location[raw_data_16$location == "Kruger National Park"] <- "K"
raw_data_16$location <- as.factor(raw_data_16$location)
raw_data_16$location <- droplevels(raw_data_16$location)

#change location2 levels
levels(raw_data_16$location2)
raw_data_16$location2 <- as.character(raw_data_16$location2)
raw_data_16$location2[raw_data_16$location2 == "Lower sabie"] <- "LS"
raw_data_16$location2[raw_data_16$location2 == "Lower Sabie"] <- "LS"
raw_data_16$location2 <- as.factor(raw_data_16$location2)
raw_data_16$location2 <- droplevels(raw_data_16$location2)

#add season column
raw_data_16$season <- NA
raw_data_16$season[grepl("May",raw_data_16$date)] <- "mj16"
raw_data_16$season[grepl("Jun",raw_data_16$date)] <- "mj16"
raw_data_16$season[grepl("Nov",raw_data_16$date)] <- "nd16"
raw_data_16$season[grepl("Dec",raw_data_16$date)] <- "nd16"

#reformat stem diameter levels
raw_data_16$stem_2_diam[raw_data_16$stem_2_diam == "-"] <- NA
raw_data_16$stem_2_diam[raw_data_16$stem_2_diam == "0"] <- NA
raw_data_16$stem_3_diam[raw_data_16$stem_3_diam == "-"] <- NA
raw_data_16$stem_3_diam[raw_data_16$stem_3_diam == "0"] <- NA
raw_data_16$stem_4_diam[raw_data_16$stem_4_diam == "-"] <- NA
raw_data_16$stem_4_diam[raw_data_16$stem_4_diam == "0"] <- NA
raw_data_16$stem_5_diam[raw_data_16$stem_5_diam == "-"] <- NA
raw_data_16$stem_5_diam[raw_data_16$stem_5_diam == "0"] <- NA



#load raw data (2017)
raw_data_17 <- read.csv("data_raw/knp_floristics_raw_2017.csv", dec=".", header = TRUE, sep = ",", na.strings=c("","NA","N/A","na"))

#change column names
colnames(raw_data_17)
raw_data_17 <- raw_data_17 %>%
  rename("ref" = "index") %>%
  rename("location" = "Location") %>%
  rename("location2" = "Location.2") %>%
  rename("date" = "Date") %>%
  rename("grid" = "Grid") %>%
  rename("plot" = "Plot") %>%
  rename("way_point" = "Way.Point") %>%
  rename("species" = "Species") %>%
  rename("height" = "Ht..cm.") %>%
  rename("stem_1_diam" = "Diameter.stem.1") %>%
  rename("stem_2_diam" = "Diameter.stem.2") %>%
  rename("stem_3_diam" = "Diameter.stem.3") %>%
  rename("stem_4_diam" = "Diameter.stem.4") %>%
  rename("num_stems" = "X..of.stems") %>%
  rename("canopy_breakage" = "Breaking..") %>%
  rename("breakage_age" = "Age") %>%
  rename("breakage_resprout" = "Respr") %>%
  rename("circum" = "Stripping...circum") %>%
  rename("add_circum" = "Add.circum") %>%
  rename("bark_stripped" = "X..3m..") %>%
  rename("strip_age" = "Age.1") %>%
  rename("bark_regrow" = "X..Regrow") %>%
  rename("borer" = "Borers..") %>%
  rename("fire" = "Fire..Yes.No.") %>%
  rename("toppled" = "Toppled..Yes.No.") %>%
  rename("toppled_resprout" = "Respr.1") %>%
  rename("elephant_damage" = "ElephaNot..Yes.No.") %>%
  rename("alive" = "Alive..Yes.No.") %>%
  rename("gps" = "GPS.used") %>%
  rename("scribe" = "data.entered.by")

#separate species column into genus and species
raw_data_17 <- separate(raw_data_17, species, c("genus", "species"), sep = " ", remove = TRUE, extra = "merge")

#change date format
raw_data_17$date <- as.Date(raw_data_17$date, "%d-%b-%y")
raw_data_17$date <- format(raw_data_17$date, "%d-%b-%Y")

#add season column
raw_data_17$season <- NA
raw_data_17$season[grepl("May",raw_data_17$date)] <- "mj17"
raw_data_17$season[grepl("Jun",raw_data_17$date)] <- "mj17"
raw_data_17$season[grepl("Nov",raw_data_17$date)] <- "nd17"
raw_data_17$season[grepl("Dec",raw_data_17$date)] <- "nd17"

#change location levels
levels(raw_data_17$location)
raw_data_17$location <- as.character(raw_data_17$location)
raw_data_17$location[raw_data_17$location == "Kruger Park"] <- "K"
raw_data_17$location <- as.factor(raw_data_17$location)
raw_data_17$location <- droplevels(raw_data_17$location)

#change location2 levels
levels(raw_data_17$location2)
raw_data_17$location2 <- as.character(raw_data_17$location2)
raw_data_17$location2[raw_data_17$location2 == "Lower Sabie"] <- "LS"
raw_data_17$location2 <- as.factor(raw_data_17$location2)
raw_data_17$location2 <- droplevels(raw_data_17$location2)

#change breakage_age levels
levels(raw_data_17$breakage_age)
raw_data_17$breakage_age <- as.character(raw_data_17$breakage_age)
raw_data_17$breakage_age[raw_data_17$breakage_age == "New"] <- "N"
raw_data_17$breakage_age[raw_data_17$breakage_age == "Old"] <- "O"
raw_data_17$breakage_age <- as.factor(raw_data_17$breakage_age)
raw_data_17$breakage_age <- droplevels(raw_data_17$breakage_age)

#change breakage_resprout levels
levels(raw_data_17$breakage_resprout)
raw_data_17$breakage_resprout <- as.character(raw_data_17$breakage_resprout)
raw_data_17$breakage_resprout[raw_data_17$breakage_resprout == "No"] <- "N"
raw_data_17$breakage_resprout[raw_data_17$breakage_resprout == "Yes"] <- "Y"
raw_data_17$breakage_resprout <- as.factor(raw_data_17$breakage_resprout)
raw_data_17$breakage_resprout <- droplevels(raw_data_17$breakage_resprout)

#change strip_age levels
levels(raw_data_17$strip_age)
raw_data_17$strip_age <- as.character(raw_data_17$strip_age)
raw_data_17$strip_age[raw_data_17$strip_age == "New"] <- "N"
raw_data_17$strip_age[raw_data_17$strip_age == "old"] <- "O"
raw_data_17$strip_age[raw_data_17$strip_age == "Old"] <- "O"
raw_data_17$strip_age <- as.factor(raw_data_17$strip_age)
raw_data_17$strip_age <- droplevels(raw_data_17$strip_age)

#change fire levels
levels(raw_data_17$fire)
raw_data_17$fire <- as.character(raw_data_17$fire)
raw_data_17$fire[raw_data_17$fire == "No"] <- "N"
raw_data_17$fire[raw_data_17$fire == "Yes"] <- "Y"
raw_data_17$fire <- as.factor(raw_data_17$fire)
raw_data_17$fire <- droplevels(raw_data_17$fire)

#change toppled levels
levels(raw_data_17$toppled)
raw_data_17$toppled <- as.character(raw_data_17$toppled)
raw_data_17$toppled[raw_data_17$toppled == "No"] <- "N"
raw_data_17$toppled[raw_data_17$toppled == "Yes"] <- "Y"
raw_data_17$toppled <- as.factor(raw_data_17$toppled)
raw_data_17$toppled <- droplevels(raw_data_17$toppled)

#change toppled_resprout levels
levels(raw_data_17$toppled_resprout)
raw_data_17$toppled_resprout <- as.character(raw_data_17$toppled_resprout)
raw_data_17$toppled_resprout[raw_data_17$toppled_resprout == "No"] <- "N"
raw_data_17$toppled_resprout[raw_data_17$toppled_resprout == "Yes"] <- "Y"
raw_data_17$toppled_resprout <- as.factor(raw_data_17$toppled_resprout)
raw_data_17$toppled_resprout <- droplevels(raw_data_17$toppled_resprout)

#change elephant_damage levels
levels(raw_data_17$elephant_damage)
raw_data_17$elephant_damage <- as.character(raw_data_17$elephant_damage)
raw_data_17$elephant_damage[raw_data_17$elephant_damage == "No"] <- "N"
raw_data_17$elephant_damage[raw_data_17$elephant_damage == "Yes"] <- "Y"
raw_data_17$elephant_damage <- as.factor(raw_data_17$elephant_damage)
raw_data_17$elephant_damage <- droplevels(raw_data_17$elephant_damage)

#change alive levels
levels(raw_data_17$alive)
raw_data_17$alive <- as.character(raw_data_17$alive)
raw_data_17$alive[raw_data_17$alive == "alive"] <- "Y"
raw_data_17$alive[raw_data_17$alive == "No"] <- "N"
raw_data_17$alive[raw_data_17$alive == "Yes"] <- "Y"
raw_data_17$alive <- as.factor(raw_data_17$alive)
raw_data_17$alive <- droplevels(raw_data_17$alive)

#reformat stem diameter levels
raw_data_17$stem_2_diam[raw_data_17$stem_2_diam == "-"] <- NA
raw_data_17$stem_2_diam[raw_data_17$stem_2_diam == "0"] <- NA
raw_data_17$stem_3_diam[raw_data_17$stem_3_diam == "-"] <- NA
raw_data_17$stem_3_diam[raw_data_17$stem_3_diam == "0"] <- NA
raw_data_17$stem_4_diam[raw_data_17$stem_4_diam == "-"] <- NA
raw_data_17$stem_4_diam[raw_data_17$stem_4_diam == "0"] <- NA
raw_data_17$stem_5_diam[raw_data_17$stem_5_diam == "-"] <- NA
raw_data_17$stem_5_diam[raw_data_17$stem_5_diam == "0"] <- NA



#load raw data (2018)
raw_data_18 <- read.csv("data_raw/knp_floristics_raw_2018.csv", dec=".", header = TRUE, sep = ",", na.strings=c("","NA","N/A"))

#change column names
colnames(raw_data_18)
raw_data_18 <- raw_data_18[,-c(2,7)]
raw_data_18 <- raw_data_18 %>%
  rename("ref" = "index") %>%
  rename("date" = "formatted.date") %>%
  rename("tag" = "Tree.tag") %>%
  rename("gps" = "gps.used") %>%
  rename("fixed_area" = "fixed.area") %>%
  rename("species" = "Species") %>%
  rename("height" = "fixed.ht.cm") %>%
  rename("num_stems" = "X..Stems") %>%
  rename("stem_1_diam" = "Diameter.1") %>%
  rename("stem_2_diam" = "Diameter.2") %>%
  rename("stem_3_diam" = "Diameter.3") %>%
  rename("stem_4_diam" = "Diameter.4") %>%
  rename("stem_5_diam" = "Diameter.5") %>%
  rename("canopy_breakage" = "Breaking") %>%
  rename("breakage_age" = "Age") %>%
  rename("breakage_resprout" = "Resprout") %>%
  rename("circum" = "strip.Circumference") %>%
  rename("add_circum" = "Additive.circumferance") %>%
  rename("bark_stripped" = "area..3m.stripped") %>%
  rename("strip_age" = "Age.strip") %>%
  rename("bark_regrow" = "Bark.Regrowth") %>%
  rename("borer" = "Borers") %>%
  rename("fire" = "Fire") %>%
  rename("toppled" = "Toppled") %>%
  rename("toppled_resprout" = "Resprout.1") %>%
  rename("elephant_damage" = "Ele.") %>%
  rename("alive" = "Alive") %>%
  rename("scribe" = "data.entered.bY")

#separate species column into genus and species
raw_data_18 <- separate(raw_data_18, species, c("genus", "species"), sep = " ", remove = TRUE, extra = "merge")

#reformat dates as dd-MMM-yyyy
raw_data_18$date <- as.Date(raw_data_18$date, "%d-%b-%y")
raw_data_18$date <- format(raw_data_18$date, "%d-%b-%Y")

#add season column
raw_data_18$season <- NA
raw_data_18$season[grepl("May",raw_data_18$date)] <- "mj18"
raw_data_18$season[grepl("Jun",raw_data_18$date)] <- "mj18"
raw_data_18$season[grepl("Nov",raw_data_18$date)] <- "nd18"
raw_data_18$season[grepl("Dec",raw_data_18$date)] <- "nd18"

#remove extra spaces in side
raw_data_18$side <- as.factor(trimws(raw_data_18$side))

#reformat strip_age levels
levels(raw_data_18$strip_age)
raw_data_18$strip_age<-as.character(raw_data_18$strip_age)
raw_data_18$strip_age[raw_data_18$strip_age == "Old"] <- "O"
raw_data_18$strip_age[raw_data_18$strip_age == "od"] <- "O"
raw_data_18$strip_age[raw_data_18$strip_age == "New"] <- "N"
raw_data_18$strip_age<-as.factor(raw_data_18$strip_age)
raw_data_18$strip_age<-droplevels(raw_data_18$strip_age)
levels(raw_data_18$strip_age)

#reformat breakage_age levels
levels(raw_data_18$breakage_age)
raw_data_18$breakage_age<-as.character(raw_data_18$breakage_age)
raw_data_18$breakage_age[raw_data_18$breakage_age == "Old"] <- "O"
raw_data_18$breakage_age[raw_data_18$breakage_age == "Old "] <- "O"
raw_data_18$breakage_age[raw_data_18$breakage_age == "New"] <- "N"
raw_data_18$breakage_age<-as.factor(raw_data_18$breakage_age)
raw_data_18$breakage_age<-droplevels(raw_data_18$breakage_age)
levels(raw_data_18$breakage_age)

#reformat alive levels
levels(raw_data_18$alive)
raw_data_18$alive[raw_data_18$alive == "n"] <- "N"
raw_data_18$alive<-droplevels(raw_data_18$alive)

#change fixed_area levels
levels(raw_data_18$fixed_area)
raw_data_18$fixed_area <- as.character(raw_data_18$fixed_area)
raw_data_18$fixed_area[raw_data_18$fixed_area == "0 to 2"] <- "<2"
raw_data_18$fixed_area[raw_data_18$fixed_area == "2 to 10"] <- "2-10"
raw_data_18$fixed_area[raw_data_18$fixed_area == "0 to 25"] <- "10-25"
raw_data_18$fixed_area <- as.factor(raw_data_18$fixed_area)
raw_data_18$fixed_area <- droplevels(raw_data_18$fixed_area)
levels(raw_data_18$fixed_area)

#reformat stem diameter levels
raw_data_18$stem_2_diam[raw_data_18$stem_2_diam == "-"] <- NA
raw_data_18$stem_2_diam[raw_data_18$stem_2_diam == "0"] <- NA
raw_data_18$stem_3_diam[raw_data_18$stem_3_diam == "-"] <- NA
raw_data_18$stem_3_diam[raw_data_18$stem_3_diam == "0"] <- NA
raw_data_18$stem_4_diam[raw_data_18$stem_4_diam == "-"] <- NA
raw_data_18$stem_4_diam[raw_data_18$stem_4_diam == "0"] <- NA
raw_data_18$stem_5_diam[raw_data_18$stem_5_diam == "-"] <- NA
raw_data_18$stem_5_diam[raw_data_18$stem_5_diam == "0"] <- NA



#combine files
colnames(raw_data) 
colnames(raw_data_16) 
colnames(raw_data_17) 
colnames(raw_data_18) 

raw_data$location <- "K"
raw_data$location2 <- "LS"
raw_data$way_point <- NA
raw_data$tag <- NA
raw_data$gps <- NA

raw_data_16$zone <- NA
raw_data_16$side <- NA
raw_data_16$fixed_area <- NA
raw_data_16$data_collector <- NA
raw_data_16$total_basal_area <- NA

raw_data_17$zone <- NA
raw_data_17$side <- NA
raw_data_17$fixed_area <- NA
raw_data_17$data_collector <- NA
raw_data_17$total_basal_area <- NA
raw_data_17$tag <- NA
raw_data_17$stem_5_diam <- NA

raw_data_18$location <- "K"
raw_data_18$location2 <- "LS"
raw_data_18$way_point <- NA
raw_data_18$notes <- NA
raw_data_18$data_collector <- NA
raw_data_18$total_basal_area <- NA

combined_raw_data <- rbind(raw_data, raw_data_16, raw_data_17, raw_data_18)

#remove rows with NA in all columns
combined_raw_data <- combined_raw_data[rowSums(is.na(combined_raw_data)) != ncol(combined_raw_data), ]

#remove 4 empty rows at end of file
combined_raw_data <- combined_raw_data[-c(3441:3444),]

#create species codes
combined_raw_data$sp2 <- substr(combined_raw_data$species, 0, 2)
combined_raw_data$gn2 <- substr(combined_raw_data$genus, 0, 2)
combined_raw_data$spcode <- paste(combined_raw_data$gn2, combined_raw_data$sp2, sep = "")
combined_raw_data$spcode <- toupper(combined_raw_data$spcode)
combined_raw_data <- subset(combined_raw_data, select = -c(sp2, gn2))

#create new dataframe for species codes
woody_species <- data.frame(combined_raw_data$spcode, combined_raw_data$genus, combined_raw_data$species)
woody_species <- woody_species[!duplicated(woody_species[1:3]),]

#rename woody_species columns
woody_species <- woody_species %>%
  rename("spcode" = "combined_raw_data.spcode") %>%
  rename("genus" = "combined_raw_data.genus") %>%
  rename("species" = "combined_raw_data.species")

#remove species and genus columns from floristics data
combined_raw_data <- subset(combined_raw_data, select = -c(species, genus))

#remove unnecessary columns from floristics data--
combined_raw_data <- subset(combined_raw_data, select = -c(ref, location2, way_point, gps))

#save clean data
write_csv(combined_raw_data,"./data_clean/CLEAN_kruger_floristics_MJ.csv")

#save species codes file
write_csv(woody_species,"./data_clean/kruger_woody_spp_codes_MJ.csv")
