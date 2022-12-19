#These are the packages you need for this script use install.packages() if you don't have them

library(NADA)
library(NADA2)
library(EnvStats)
library(dplyr)
library(ggplot2)

wd <- getwd()
dir.data <- file.path(wd, "data")
dir.output <- file.path(wd, "output")

# for working with censored data, I need the "Thiaminase Activity" values in
# my spreadsheet to be either 1) the actual measured value, if it is above
# the calculated detection limit, or 2) the calculated detection limit if the 
# observed value was below that limit, so I am saving a second "metadata" csv
# file where I replace all values below the detection limit, with the detection 
# limit

meta <- read.csv(file = file.path(dir.data, "Thiaminase.Meta.2022.detection.limit.csv"),
                 stringsAsFactors = TRUE,
                 blank.lines.skip = TRUE)


meta <- meta[-c(198:334),] #removing empty rows
meta <- meta[-c(1:9),] #removing samples that were not analysed

#what is the % of nondetects observed for each species?
meta %>% 
  group_by(Species) %>% 
  summarise(n = n(), Pct_Nondetect = ((sum(No_detect == TRUE))/n()) * 100,
            nDetect = n() - sum(No_detect))

# Don't bother with due to small sample size and high incidence of nondetects:
  # Greenland turbot, Humpy shrimp, Longhead dab, Pacific cod (age 0), 
  # Threespine stickelback
# Fourhorn Sculpin have fewer than 3 observations above the detection limit so I
# am removing them too

#removing the above species in a new dataframe
meta_trimmed_species <- subset(meta, !(Species %in% c("Greenland turbot",
                                                      "Humpy shrimp", 
                                                      "Longhead Dab",
                                                      "Pacific cod (age 0)",
                                                      "Threespine stickleback",
                                                      "Fourhorn Sculpin")))
# remove unused factor levels in Species
meta_trimmed_species$Species <- droplevels(meta_trimmed_species$Species)

meta_trimmed_species %>% 
  group_by(Species) %>% 
  summarise(n = n(), Pct_Nondetect = ((sum(No_detect == TRUE))/n()) * 100)

#need to separate the species
#make a list with the species separated
separate_species <- meta %>% 
  group_split(Species)

#save each species as its own dataframe, find out % of nondetects in each group
separate_species[[19]]$Species

ArcticCisco <- separate_species[[1]] # n = 9
ArcticCod <- separate_species[[2]] # n = 10
BroadWhitefish <- separate_species[[3]] # n = 9
Capelin <- separate_species[[4]] # n = 11
#FourhornSculpin <- separate_species[[5]] # n = 10
GonatusSquid <- separate_species[[6]] # n = 6
PacificHerring <- separate_species[[11]] # n = 28
PacificHerringAge0 <- separate_species[[12]] # n = 8

RainbowSmelt <- separate_species[[13]] # n = 22
RainbowSmelt$Survey <- droplevels(RainbowSmelt$Survey) #remove unused factor levels

SaffronCod <- separate_species[[14]] # n = 14
SandLance <- separate_species[[15]] # n = 18
SurfSmelt <- separate_species[[16]] # n = 11
WalleyePollockAge0 <- separate_species[[18]] # n = 18

# explore some overall relationships
names(meta_trimmed_species)

ggplot(data = meta_trimmed_species, aes(x = Mass..g., y = Thiaminase_Activity)) +
  geom_point(aes(color = Species)) +
  facet_wrap(~Species, scales = "free") +
  guides(color = "none")
  


?cboxplot
cboxplot(meta_trimmed_species$Thiaminase_Activity, meta_trimmed_species$No_detect,
         xgroup = meta_trimmed_species$Species, show = TRUE)

cboxplot(Capelin$Thiaminase_Activity, Capelin$No_detect, xgroup = Capelin$Survey)

xx = cboxplot(RainbowSmelt$Thiaminase_Activity, RainbowSmelt$No_detect, xgroup = RainbowSmelt$Survey)

censtats(RainbowSmelt$Thiaminase_Activity, RainbowSmelt$No_detect)
?censtats
censtats(SandLance$Thiaminase_Activity, SandLance$No_detect)
?cenros


unique(RainbowSmelt$Survey)
SandLanceNBS <- SandLance[which(SandLance$Survey == c("NBS2021", "NBS2022")),]

RainbowSmeltNBS <- RainbowSmelt[which(RainbowSmelt$Survey == c("NBS2021", "NBS2022")),]
RainbowSmeltNBS <- RainbowSmelt[which(RainbowSmelt$Survey == c("NBS2021", "NBS2022")),]

censtats(SandLanceNBS2021$Thiaminase_Activity, San)

?which
