## Theil Index
## March 17, 2020
## Nina Masters
## Aim 3

#install.packages("segregation")
library(segregation)

################ Example of data using pre-loaded schools00 dataset
mutual_total(schools00, "race", "school", weight = "n")
View(schools00)

#Standard errors in all functions can be estimated via boostrapping:
mutual_total(schools00, "race", "school", weight = "n", se = TRUE)

#Decompose segregation into a between-state and a within-state term (the sum of these equals total segregation):
# between states
mutual_total(schools00, "race", "state", weight = "n")

# within states
mutual_total(schools00, "race", "school", within = "state", weight = "n")

################################################################################################
##################     Now repeat this analysis but for MI vaccination data      ###############
################################################################################################

## DATA PREPARATION / ORGANIZATION
# setwd("/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /R Code")
# 
# #now read in full vaccination data 
# vax <- read.csv("/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Final Linked Vaccine and Census Data/MI full data table with spatial join and vaccination data.csv") 
# 
# drops <- c("X.1", "Score", "X", "Y", "City",  "DisplayX", "DisplayY", "SCHOOL_TYPE", "coords.x1", "coords.y1", "optional", "REVISED_SCHOOL_NAME", "SCHOOL_DISTRICT")
# vax <- vax[ , !(names(vax) %in% drops)]
# 
# #pull in file to link to county
# mdhhs_kindergarten <- read.csv("/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Final Cleaned Data/MDHHS_KG_FINAL_DEDUP.csv") 
# mdhhs_kindergarten <- dplyr::select(mdhhs_kindergarten, c("NAME_FOR_ADDRESS_MATCH", "MATCH_COUNTY"))
# names(mdhhs_kindergarten) <- c("NAME_ADDRESS_MATCH", "COUNTY")
# 
# #remove duplicates for different years
# mdhhs_kindergarten <- mdhhs_kindergarten[!duplicated(mdhhs_kindergarten$NAME_ADDRESS_MATCH), ]
# 
# #now merge in so we have all levels of analysis
# vax <- merge(vax, mdhhs_kindergarten, by = "NAME_ADDRESS_MATCH")
# 
# #reorganize columns
# vax <- vax[, c(1,3,4,5,2,117, 6:116)]

vax <- read.csv("/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Theil_Index_Vax_File.csv")
drops <- c("X")
vax <- vax[ , !(names(vax) %in% drops)]

#copy and create file of original data
original_data <- vax

#now create duplicate dataset nonvax - this will have #s of waivers
nonvax <- vax
nonvax$vax_status <- "non-vaccinated"
nonvax <- dplyr::select(nonvax, c(1, 118, 2:72))

# now reorganize the dataset and create second dataset to turn this into rows for "vax" and "nonvax" as categories
#first create row for vaccination status

vax$vax_status <- "vaccinated"
# #reorganize columns
vax <- dplyr::select(vax, c(1, 118, 2:72))
vax[,c(30:73)] <- 0 #no waiver numbers for the vaccinated people

#now set student quantity #s to be those who are complete among vaccinated students
vax$STUDENT_QY_2009 <- vax$COMPLETE_QY_2009
vax$STUDENT_QY_2010 <- vax$COMPLETE_QY_2010
vax$STUDENT_QY_2011 <- vax$COMPLETE_QY_2011
vax$STUDENT_QY_2012 <- vax$COMPLETE_QY_2012
vax$STUDENT_QY_2013 <- vax$COMPLETE_QY_2013
vax$STUDENT_QY_2014 <- vax$COMPLETE_QY_2014
vax$STUDENT_QY_2015 <- vax$COMPLETE_QY_2015
vax$STUDENT_QY_2016 <- vax$COMPLETE_QY_2016
vax$STUDENT_QY_2017 <- vax$COMPLETE_QY_2017
vax$STUDENT_QY_2018 <- vax$COMPLETE_QY_2018
vax$STUDENT_QY_2019 <- vax$COMPLETE_QY_2019

# now start working on the nonvax total student data 

# Complete QY in the nonvax dataset: these should be 0
nonvax[,c(19:29)] <- 0 #no complete numbers for the vaccinated people

#student_QY: this should be equal to total waivers
nonvax$STUDENT_QY_2009 <- nonvax$WAIV_TOT_QY_2009
nonvax$STUDENT_QY_2010 <- nonvax$WAIV_TOT_QY_2010
nonvax$STUDENT_QY_2011 <- nonvax$WAIV_TOT_QY_2011
nonvax$STUDENT_QY_2012 <- nonvax$WAIV_TOT_QY_2012
nonvax$STUDENT_QY_2013 <- nonvax$WAIV_TOT_QY_2013
nonvax$STUDENT_QY_2014 <- nonvax$WAIV_TOT_QY_2014
nonvax$STUDENT_QY_2015 <- nonvax$WAIV_TOT_QY_2015
nonvax$STUDENT_QY_2016 <- nonvax$WAIV_TOT_QY_2016
nonvax$STUDENT_QY_2017 <- nonvax$WAIV_TOT_QY_2017
nonvax$STUDENT_QY_2018 <- nonvax$WAIV_TOT_QY_2018
nonvax$STUDENT_QY_2019 <- nonvax$WAIV_TOT_QY_2019

#leave other waiver counts un-changed

#Now we want to rbind these two datasets and then sort by school so that the vax + nonvax counts will be on top of each other
total_vax <- rbind(vax, nonvax)

#now sort by school
total_vax <-total_vax[order(total_vax$NAME_ADDRESS_MATCH),]


#now manipulate values to reflect vaccinated individuals
#########################################################################################
#############       Now run the entropy measures and see what happens      ##############
#########################################################################################

#make holder dataframe
Theil <- data.frame(YEAR=2008:2018, Block_Group =rep(0,11),  Tract =rep(0,11), School_District =rep(0,11), City =rep(0,11), County =rep(0,11))

# Start with Block Group Level:
#for 2009 data - block group level
Theil$Block_Group[1] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2009")[2,2]
# H = 0.962

#for 2010 data - block group level
Theil$Block_Group[2] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2010")[2,2]
# H = 0.964

#for 2011 data - block group level
Theil$Block_Group[3] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2011")[2,2]
# H = 0.967

#for 2012 data - block group level
Theil$Block_Group[4] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2012")[2,2]
# H = 0.968

#for 2013 data - block group level
Theil$Block_Group[5] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2013")[2,2]
# H = 0.968

#for 2014 data - block group level
Theil$Block_Group[6] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2014")[2,2]
# H = 0.967

#for 2015 data - block group level
Theil$Block_Group[7] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2015")[2,2]
# H = 0.964

#for 2016 data - block group level
Theil$Block_Group[8] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2016")[2,2]
# H = 0.963

#for 2017 data - block group level
Theil$Block_Group[9] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2017")[2,2]
# H = 0.962

#for 2018 data - block group level
Theil$Block_Group[10] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2018")[2,2]
# H = 0.961

#for 2019 data - block group level
Theil$Block_Group[11] <-mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2019")[2,2]
# H = 0.962

##################################################################
# Now calculate for tract level:
#for 2009 data - tract level
Theil$Tract[1] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2009")[2,2]
# H = 0.894

#for 2010 data - tract level
Theil$Tract[2] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2010")[2,2]
# H = 0.897

#for 2011 data - tract level
Theil$Tract[3] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2011")[2,2]
# H = 0.902

#for 2012 data - tract level
Theil$Tract[4] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2012")[2,2]
# H = 0.906

#for 2013 data - tract level
Theil$Tract[5] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2013")[2,2]
# H = 0.907

#for 2014 data - tract level
Theil$Tract[6] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2014")[2,2]
# H = 0.903

#for 2015 data - tract level
Theil$Tract[7] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2015")[2,2]
# H = 0.900

#for 2016 data - tract level
Theil$Tract[8] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2016")[2,2]
# H = 0.887

#for 2017 data - tract level
Theil$Tract[9] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2017")[2,2]
# H = 0.887

#for 2018 data - tract level
Theil$Tract[10] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2018")[2,2]
# H = 0.889

#for 2019 data - tract level
Theil$Tract[11] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2019")[2,2]
# H = 0.892

##################################################################
# Now calculate for school district level:
#for 2009 data - school district level
Theil$School_District[1] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2009")[2,2]
# H = 0.537

#for 2010 data - school district level
Theil$School_District[2] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2010")[2,2]
# H = 0.528

#for 2011 data - school district level
Theil$School_District[3] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2011")[2,2]
# H = 0.552

#for 2012 data - school district level
Theil$School_District[4] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2012")[2,2]
# H = 0.546

#for 2013 data - school district level
Theil$School_District[5] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2013")[2,2]
# H = 0.546

#for 2014 data - school district level
Theil$School_District[6] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2014")[2,2]
# H = 0.548

#for 2015 data - school district level
Theil$School_District[7] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2015")[2,2]
# H = 0.543

#for 2016 data - school district level
Theil$School_District[8] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2016")[2,2]
# H = 0.526

#for 2017 data - school district level
Theil$School_District[9] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2017")[2,2]
# H = 0.515

#for 2018 data - school district level
Theil$School_District[10] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2018")[2,2]
# H = 0.530

#for 2019 data - school district level
Theil$School_District[11] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2019")[2,2]
# H = 0.529

##################################################################
# Now calculate for City level:
#for 2009 data - City level
Theil$City[1] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2009")[2,2]
# H = 0.538

#for 2010 data - City level
Theil$City[2] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2010")[2,2]
# H = 0.526

#for 2011 data - City level
Theil$City[3] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2011")[2,2]
# H = 0.543

#for 2012 data - City level
Theil$City[4] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2012")[2,2]
# H = 0.533

#for 2013 data - City level
Theil$City[5] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2013")[2,2]
# H = 0.546

#for 2014 data - City level
Theil$City[6] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2014")[2,2]
# H = 0.541

#for 2015 data - City level
Theil$City[7] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2015")[2,2]
# H = 0.538

#for 2016 data - City level
Theil$City[8] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2016")[2,2]
# H = 0.523

#for 2017 data - City level
Theil$City[9] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2017")[2,2]
# H = 0.516

#for 2018 data - City level
Theil$City[10] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2018")[2,2]
# H = 0.523

#for 2019 data - City level
Theil$City[11] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2019")[2,2]
# H = 0.518

##################################################################
# Now calculate for COUNTY level:
#for 2009 data - COUNTY level
Theil$County[1] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2009")[2,2]
# H = 0.180

#for 2010 data - COUNTY level
Theil$County[2] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2010")[2,2]
# H = 0.170

#for 2011 data - COUNTY level
Theil$County[3] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2011")[2,2]
# H = 0.184

#for 2012 data - COUNTY level
Theil$County[4] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2012")[2,2]
# H = 0.182

#for 2013 data - COUNTY level
Theil$County[5] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2013")[2,2]
# H = 0.181

#for 2014 data - COUNTY level
Theil$County[6] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2014")[2,2]
# H = 0.172

#for 2015 data - COUNTY level
Theil$County[7] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2015")[2,2]
# H = 0.167

#for 2016 data - COUNTY level
Theil$County[8] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2016")[2,2]
# H = 0.152

#for 2017 data - COUNTY level
Theil$County[9] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2017")[2,2]
# H = 0.149

#for 2018 data - COUNTY level
Theil$County[10] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2018")[2,2]
# H = 0.160

#for 2019 data - COUNTY level
Theil$County[11] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2019")[2,2]
# H = 0.161


Theil = data.frame(lapply(Theil, as.character), stringsAsFactors=FALSE)
write.csv(Theil, "/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Theil_Index_Results.csv")

#################################################################################################
#####################     Now decompose segregation into b/t within     #########################
#################################################################################################
#####################        Start with B/t Within County level         #########################
#################################################################################################
#make holder dataframe
Theil_Decomposition_County <- data.frame(YEAR=2008:2018, Between_County =rep(0,11),  Within_County =rep(0,11))

#Decompose segregation into a between-state and a within-county term (the sum of these equals total segregation):
# between counties
Theil_Decomposition_County$Between_County[1] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_County$Between_County[2] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_County$Between_County[3] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_County$Between_County[4] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_County$Between_County[5] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_County$Between_County[6] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_County$Between_County[7] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_County$Between_County[8] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_County$Between_County[9] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_County$Between_County[10] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_County$Between_County[11] <- mutual_total(total_vax, "vax_status", "COUNTY", weight = "STUDENT_QY_2019")[2,2]

# within counties 2019
Theil_Decomposition_County$Within_County[1] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_County$Within_County[2] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_County$Within_County[3] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_County$Within_County[4] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_County$Within_County[5] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_County$Within_County[6] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_County$Within_County[7] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_County$Within_County[8] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_County$Within_County[9] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_County$Within_County[10] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_County$Within_County[11] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "COUNTY", weight = "STUDENT_QY_2019")[2,2]

Theil_Decomposition_County$Total_Segregation <- as.numeric(Theil_Decomposition_County$Between_County) + as.numeric(Theil_Decomposition_County$Within_County)

Theil_Decomposition_County$Between_County <- as.numeric(Theil_Decomposition_County$Between_County)
Theil_Decomposition_County$Within_County <- as.numeric(Theil_Decomposition_County$Within_County)

write.csv(Theil_Decomposition_County, "/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Theil_Index_Decomposition_County.csv")

library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

ggplot(Theil_Decomposition_County) + geom_point(aes(x = YEAR, y = Between_County), col = "blue", size = 3) + 
  geom_point(aes(x = YEAR, y = Within_County), col = "red", size = 3) + theme_clean() + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

#reshape to turn into bar plot
library(reshape2)

Theil_Decomposition_County <- dplyr::select(Theil_Decomposition_County, c("YEAR", "Between_County", "Within_County"))
theil_county <- melt(Theil_Decomposition_County, id.vars = "YEAR", variable.name = "Decomp", value.name = "Theil")

ggplot(theil_county) + geom_bar(aes(x = YEAR, y = Theil, fill = Decomp), stat = "identity", position = "stack") + 
  theme_clean() + scale_fill_viridis(option  = "C", discrete = T) + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))


#################################################################################################
#####################     Now decompose segregation into b/t within     #########################
#################################################################################################
#####################                 B/t Within City level             #########################
#################################################################################################
#make holder dataframe
Theil_Decomposition_City <- data.frame(YEAR=2008:2018, Between_City =rep(0,11),  Within_City =rep(0,11))

#Decompose segregation into a between-state and a within-county term (the sum of these equals total segregation):
# between counties
Theil_Decomposition_City$Between_City[1] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_City$Between_City[2] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_City$Between_City[3] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_City$Between_City[4] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_City$Between_City[5] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_City$Between_City[6] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_City$Between_City[7] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_City$Between_City[8] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_City$Between_City[9] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_City$Between_City[10] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_City$Between_City[11] <- mutual_total(total_vax, "vax_status", "City_Caps", weight = "STUDENT_QY_2019")[2,2]

# within counties 2019
Theil_Decomposition_City$Within_City[1] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_City$Within_City[2] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_City$Within_City[3] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_City$Within_City[4] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_City$Within_City[5] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_City$Within_City[6] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_City$Within_City[7] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_City$Within_City[8] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_City$Within_City[9] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_City$Within_City[10] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_City$Within_City[11] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "City_Caps", weight = "STUDENT_QY_2019")[2,2]

Theil_Decomposition_City$Total_Segregation <- as.numeric(Theil_Decomposition_City$Between_City) + as.numeric(Theil_Decomposition_City$Within_City)

Theil_Decomposition_City$Between_City <- as.numeric(Theil_Decomposition_City$Between_City)
Theil_Decomposition_City$Within_City <- as.numeric(Theil_Decomposition_City$Within_City)

write.csv(Theil_Decomposition_City, "/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Theil_Index_Decomposition_City.csv")

library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

ggplot(Theil_Decomposition_City) + geom_point(aes(x = YEAR, y = Between_City), col = "blue", size = 3) + 
  geom_point(aes(x = YEAR, y = Within_City), col = "red", size = 3) + theme_clean() + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

#reshape to turn into bar plot
library(reshape2)

Theil_Decomposition_City <- dplyr::select(Theil_Decomposition_City, c("YEAR", "Between_City", "Within_City"))
theil_City <- melt(Theil_Decomposition_City, id.vars = "YEAR", variable.name = "Decomp", value.name = "Theil")

ggplot(theil_City) + geom_bar(aes(x = YEAR, y = Theil, fill = Decomp), stat = "identity", position = "stack") + 
  theme_clean() + scale_fill_viridis(option  = "C", discrete = T) + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))


#################################################################################################
#####################     Now decompose segregation into b/t within     #########################
#################################################################################################
#####################          B/t Within School Dist level             #########################
#################################################################################################
#make holder dataframe
Theil_Decomposition_School_District <- data.frame(YEAR=2008:2018, Between_School_District =rep(0,11),  Within_School_District =rep(0,11))

#Decompose segregation into a between-state and a within-county term (the sum of these equals total segregation):
# between counties
Theil_Decomposition_School_District$Between_School_District[1] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_School_District$Between_School_District[2] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_School_District$Between_School_District[3] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_School_District$Between_School_District[4] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_School_District$Between_School_District[5] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_School_District$Between_School_District[6] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_School_District$Between_School_District[7] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_School_District$Between_School_District[8] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_School_District$Between_School_District[9] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_School_District$Between_School_District[10] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_School_District$Between_School_District[11] <- mutual_total(total_vax, "vax_status", "DCODE", weight = "STUDENT_QY_2019")[2,2]

# within counties 2019
Theil_Decomposition_School_District$Within_School_District[1] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_School_District$Within_School_District[2] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_School_District$Within_School_District[3] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_School_District$Within_School_District[4] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_School_District$Within_School_District[5] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_School_District$Within_School_District[6] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_School_District$Within_School_District[7] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_School_District$Within_School_District[8] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_School_District$Within_School_District[9] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_School_District$Within_School_District[10] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_School_District$Within_School_District[11] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "DCODE", weight = "STUDENT_QY_2019")[2,2]

Theil_Decomposition_School_District$Total_Segregation <- as.numeric(Theil_Decomposition_School_District$Between_School_District) + as.numeric(Theil_Decomposition_School_District$Within_School_District)

Theil_Decomposition_School_District$Between_School_District <- as.numeric(Theil_Decomposition_School_District$Between_School_District)
Theil_Decomposition_School_District$Within_School_District <- as.numeric(Theil_Decomposition_School_District$Within_School_District)

write.csv(Theil_Decomposition_School_District, "/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Theil_Index_Decomposition_School_District.csv")

library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

ggplot(Theil_Decomposition_School_District) + geom_point(aes(x = YEAR, y = Between_School_District), col = "blue", size = 3) + 
  geom_point(aes(x = YEAR, y = Within_School_District), col = "red", size = 3) + theme_clean() + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

#reshape to turn into bar plot
library(reshape2)

Theil_Decomposition_School_District <- dplyr::select(Theil_Decomposition_School_District, c("YEAR", "Between_School_District", "Within_School_District"))
theil_School_District <- melt(Theil_Decomposition_School_District, id.vars = "YEAR", variable.name = "Decomp", value.name = "Theil")

ggplot(theil_School_District) + geom_bar(aes(x = YEAR, y = Theil, fill = Decomp), stat = "identity", position = "stack") + 
  theme_clean() + scale_fill_viridis(option  = "C", discrete = T) + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) +


library(dplyr)
library(tidyverse)

#################################################################################################
#####################     Now decompose segregation into b/t within     #########################
#################################################################################################
#####################          B/t Within Census Tract level             #########################
#################################################################################################
#make holder dataframe
Theil_Decomposition_Tract <- data.frame(YEAR=2008:2018, Between_Tract =rep(0,11),  Within_Tract =rep(0,11))

#Decompose segregation into a between-state and a within-county term (the sum of these equals total segregation):
# between tracts
Theil_Decomposition_Tract$Between_Tract[1] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_Tract$Between_Tract[2] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_Tract$Between_Tract[3] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_Tract$Between_Tract[4] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_Tract$Between_Tract[5] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_Tract$Between_Tract[6] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_Tract$Between_Tract[7] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_Tract$Between_Tract[8] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_Tract$Between_Tract[9] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_Tract$Between_Tract[10] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_Tract$Between_Tract[11] <- mutual_total(total_vax, "vax_status", "LINK_TRACT", weight = "STUDENT_QY_2019")[2,2]

# within tracts 2019
Theil_Decomposition_Tract$Within_Tract[1] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_Tract$Within_Tract[2] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_Tract$Within_Tract[3] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_Tract$Within_Tract[4] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_Tract$Within_Tract[5] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_Tract$Within_Tract[6] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_Tract$Within_Tract[7] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_Tract$Within_Tract[8] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_Tract$Within_Tract[9] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_Tract$Within_Tract[10] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_Tract$Within_Tract[11] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_TRACT", weight = "STUDENT_QY_2019")[2,2]

Theil_Decomposition_Tract$Total_Segregation <- as.numeric(Theil_Decomposition_Tract$Between_Tract) + as.numeric(Theil_Decomposition_Tract$Within_Tract)

Theil_Decomposition_Tract$Between_Tract <- as.numeric(Theil_Decomposition_Tract$Between_Tract)
Theil_Decomposition_Tract$Within_Tract <- as.numeric(Theil_Decomposition_Tract$Within_Tract)

write.csv(Theil_Decomposition_Tract, "/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Theil_Index_Decomposition_Tract.csv")

library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

ggplot(Theil_Decomposition_Tract) + geom_point(aes(x = YEAR, y = Between_Tract), col = "blue", size = 3) + 
  geom_point(aes(x = YEAR, y = Within_Tract), col = "red", size = 3) + theme_clean() + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

#reshape to turn into bar plot
library(reshape2)

Theil_Decomposition_Tract <- dplyr::select(Theil_Decomposition_Tract, c("YEAR", "Between_Tract", "Within_Tract"))
theil_Tract <- melt(Theil_Decomposition_Tract, id.vars = "YEAR", variable.name = "Decomp", value.name = "Theil")

ggplot(theil_Tract) + geom_bar(aes(x = YEAR, y = Theil, fill = Decomp), stat = "identity", position = "stack") + 
  theme_clean() + scale_fill_viridis(option  = "C", discrete = T) + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))
  

#################################################################################################
#####################     Now decompose segregation into b/t within     #########################
#################################################################################################
#####################          B/t Within Census BGlevel             #########################
#################################################################################################
#make holder dataframe
Theil_Decomposition_BG <- data.frame(YEAR=2008:2018, Between_BG =rep(0,11),  Within_BG =rep(0,11))

#Decompose segregation into a between-state and a within-county term (the sum of these equals total segregation):
# between bgs
Theil_Decomposition_BG$Between_BG[1] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_BG$Between_BG[2] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_BG$Between_BG[3] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_BG$Between_BG[4] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_BG$Between_BG[5] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_BG$Between_BG[6] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_BG$Between_BG[7] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_BG$Between_BG[8] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_BG$Between_BG[9] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_BG$Between_BG[10] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_BG$Between_BG[11] <- mutual_total(total_vax, "vax_status", "LINK_BG", weight = "STUDENT_QY_2019")[2,2]

# within bgs
Theil_Decomposition_BG$Within_BG[1] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2009")[2,2]
Theil_Decomposition_BG$Within_BG[2] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2010")[2,2]
Theil_Decomposition_BG$Within_BG[3] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2011")[2,2]
Theil_Decomposition_BG$Within_BG[4] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2012")[2,2]
Theil_Decomposition_BG$Within_BG[5] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2013")[2,2]
Theil_Decomposition_BG$Within_BG[6] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2014")[2,2]
Theil_Decomposition_BG$Within_BG[7] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2015")[2,2]
Theil_Decomposition_BG$Within_BG[8] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2016")[2,2]
Theil_Decomposition_BG$Within_BG[9] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2017")[2,2]
Theil_Decomposition_BG$Within_BG[10] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2018")[2,2]
Theil_Decomposition_BG$Within_BG[11] <- mutual_total(total_vax, "vax_status", "NAME_ADDRESS_MATCH", within = "LINK_BG", weight = "STUDENT_QY_2019")[2,2]

Theil_Decomposition_BG$Total_Segregation <- as.numeric(Theil_Decomposition_BG$Between_BG) + as.numeric(Theil_Decomposition_BG$Within_BG)

Theil_Decomposition_BG$Between_BG <- as.numeric(Theil_Decomposition_BG$Between_BG)
Theil_Decomposition_BG$Within_BG <- as.numeric(Theil_Decomposition_BG$Within_BG)

write.csv(Theil_Decomposition_BG, "/Users/ninamasters/Desktop/Dissertation/Aim 3 - MDHHS /Theil_Index_Decomposition_BG.csv")

library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

ggplot(Theil_Decomposition_BG) + geom_point(aes(x = YEAR, y = Between_BG), col = "blue", size = 3) + 
  geom_point(aes(x = YEAR, y = Within_BG), col = "red", size = 3) + theme_clean() + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

#reshape to turn into bar plot
library(reshape2)

Theil_Decomposition_BG <- dplyr::select(Theil_Decomposition_BG, c("YEAR", "Between_BG", "Within_BG"))
theil_BG <- melt(Theil_Decomposition_BG, id.vars = "YEAR", variable.name = "Decomp", value.name = "Theil")

ggplot(theil_BG) + geom_bar(aes(x = YEAR, y = Theil, fill = Decomp), stat = "identity", position = "stack") + 
  theme_clean() + scale_fill_viridis(option  = "C", discrete = T) + scale_x_discrete(lim = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

