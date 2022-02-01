library(learnr)
library(sf)
require(dplyr)
library(tigris)
library(stringr)
library(tidyverse)

##Read in vax data, filter out states that are unknown or noncontiguous (Alaska, Hawaii, territories); pick a date (currently November 1, 2021)
vax <- read.csv("~/Downloads/COVID-19_Vaccinations_in_the_United_States_County.csv") %>%
  dplyr::select("FIPS", "Date", "Series_Complete_Pop_Pct", "Recip_County") %>%
  filter(Date == "11/01/2021" & FIPS != "UNK" & FIPS != "25001" & FIPS != "25007" & FIPS != "25019" & FIPS != "06003"& FIPS != "06091"& FIPS != "06049"& FIPS != "06051"& FIPS != "06105"& FIPS != "06043"& FIPS != "06027"& FIPS != "06063")

##Load county shapefile, remove states that are noncontiguous
counties <- tigris::counties() %>%
  filter(STATEFP != "02" & STATEFP != "15" & STATEFP != "60" & STATEFP != "66"& STATEFP != "69"& STATEFP != "72"& STATEFP != "78")

##Join the vax data to the shapefile, remove any NAs, make the variable into the UNVAXED
data <- left_join(counties, vax, by = c("GEOID" = "FIPS"))  %>%
  dplyr::select("complete" =Series_Complete_Pop_Pct, geometry, GEOID , STATEFP) %>%
  drop_na(complete) %>%
  mutate(unvax = 100 - complete)


###Global and Local Moran's I
#Make the neighborhood matrix (default is queen's neighbors)
w <- nb2listw(poly2nb(data))
globalmoran <- moran.test(data$unvax,w)

##Make LISA
lisa <- localmoran(data$unvax,w)  #calculate the local moran's I

data$scaled_unvax <- scale(data$unvax)[,1]  #scaled unvax
data$lag_scaled_unvax <-  lag.listw(w, data$scaled_unvax) #lagged variable

##Get significance
data$p <- lisa[,5]

###Figure out what type of LISA it is if significant
data$lisa_sig <- NA
data$lisa_sig[(data$scaled_unvax >= 0 & data$lag_scaled_unvax >= 0) & (data$p <= 0.05)] <- 1
data$lisa_sig[(data$scaled_unvax <= 0 & data$lag_scaled_unvax <= 0) & (data$p <= 0.05)] <- 2
data$lisa_sig[(data$scaled_unvax >= 0 & data$lag_scaled_unvax <= 0) & (data$p <= 0.05)] <- 3
data$lisa_sig[(data$scaled_unvax <= 0 & data$lag_scaled_unvax >= 0) & (data$p <= 0.05)] <- 4
data$lisa_sig[ (data$p > 0.05)] <- 5  

labels <- c("1"="High-high","2"= "Low-low","3"= "High-low","4"= "Low-High","5" = "Not significant")
cols <- c("1"= "red","2"= "blue","3"= "pink","4"= "lightblue","5"= "lightgrey")

ggplot()+
  geom_sf(data = data, aes(fill = as.factor(lisa_sig)), lwd = 0.001)+
  theme_void()+
  scale_fill_manual(values = cols, labels = labels, na.value = "grey") +
  labs(fill ="Significant LISA values\nfor % Unvaccinated\n11/1/21")
