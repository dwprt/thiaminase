#These are the packages you need for this script use install.packages() if you don't have them

library(NADA)
library(NADA2)
library(EnvStats)
library(dplyr)
library(ggplot2)
library(lubridate)
library(writexl)

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


#meta <- meta[-c(224:334),] #removing empty rows
meta <- meta[-c(1:9),] #removing samples that were not analysed
#meta <- meta[-c(233:334),] #removing samples that were not analysed
#meta <- meta[-c(233:241),] #removing samples that were not analysed

meta$Catch.Date <- mdy(meta$Catch.Date)
    ##  Converts date column in spreadsheet to date format in R
    ## Using the lubridate package

levels(meta$Area) <- c("Northern Bering Sea", "Southeast Alaska", "Southern Bering Sea", "Arctic")
    ## Can use this line to rename the areas factor in the dataframe

meta$Reg_Date <- paste(meta$Area, " ", year(meta$Catch.Date), sep = "")
    ## Making a new factor that includes the year and the region


#Making a summary table---------------------------------------------------------
meta_summary <- meta %>% 
  group_by(Species, Reg_Date) %>% 
  summarise(n = n(), numNoDetect = sum(No_detect == TRUE), numDetect = sum(No_detect == FALSE),
            PctNoDetect = (sum(No_detect == TRUE)/n())*100, detect3 = sum(No_detect == FALSE) >= 3)
meta_summary2 <- meta %>% 
  filter(No_detect == FALSE) %>% 
  group_by(Species, Reg_Date) %>% 
  summarise(mean_DetectedThiaminaseActivity = mean(Thiaminase_Activity),
            sd_DetectedThiaminaseActivity = sd(Thiaminase_Activity),
            median_DetectedThiaminaseActivity = median(Thiaminase_Activity))
meta_summary3 <- inner_join(meta_summary, meta_summary2, by = c("Species", "Reg_Date"))
meta_summary3 <- as.data.frame(meta_summary3)

    ## This objects shows the Species per Region Per Year and how many non detects,
    ## detects, the $ of non detects, and do I have <=3 detects so that I can do ROS

#write_xlsx(meta_summary3, path = file.path(dir.output, "META_SUMMARY.xlsx"))
    
    ## The above line saves the metasummary output as a spreadsheet
#####################################################################################################


#how many of each species do I have from each region? how many have detectable thiaminase
meta_tab <- meta %>% 
  group_by(Species) %>% 
  summarise(Total_Samples = n(), #Pct_Nondetect = ((sum(No_detect == TRUE))/n()) * 100,
            Arctic_Samples = (sum(Survey == "VVB")),
            Arctic_detects = (sum(Survey == "VVB" & No_detect == FALSE)),
            Bering_Sea_Samples = (sum(Survey == "NBS2021") +  sum(Survey == "NBS2022")),
            Bering_Sea_detects = (sum(Survey == "NBS2021" & No_detect == FALSE) 
                                     +  sum(Survey == "NBS2022" & No_detect == FALSE)),
            Southeast_Samples = (sum(Survey == "SECM2022")),
            Southeast_detects = (sum(Survey == "SECM2022" & No_detect == FALSE))) 

#write.csv(meta_tab, file = paste(dir.output, "/", "meta_tab.csv", sep = ""))

#OK so what I want to do is make a boxplot for each region:
  # Arctic = Vanessa Von Biela Samples
  # Bering Sea = NBS2021 and NBS2022
  # Southeast = SECM2022
  # Start with Bering Sea because it will be bigges
    # Species with >=3 detects in bering sea: Capelin, Gonatus Squid, Pacific Herring,
    # Pacific Herring (age 0), Rainbow Smelt, Saffron Cod, Sand Lance, Walleye Pollock (age 0)

    #First thing to do is extract all the Bering Sea individuals from the meta data sheet

BeringSea <- meta[which(meta$Area == "Northern Bering Sea"),]

#Ok we have all the Bering Sea Samples, now I just want the species with =>3 detects

BeringSea <- BeringSea[BeringSea$Species %in% c("Capelin", "Gonatus Squid",
                                                    "Pacific Herring",
                                                    "Pacific Herring (age 0)",
                                                    "Rainbow Smelt", "Saffron Cod",
                                                    "Sand Lance", "Walleye Pollock (age 0)"),]

# Now make sure we only have factor levels for the species we are actually using

BeringSea$Species <- droplevels(BeringSea$Species)
levels(BeringSea$Species)

# Now make a boxplot of Thiaminase Activity Levels in the Bering Sea by Species
# Using ROS methods to handle censored observations

cboxplot(BeringSea$Thiaminase_Activity, BeringSea$No_detect, xgroup = BeringSea$Species,
         show = FALSE, printstat = TRUE, Title = "NBS2021 and NBS2022",
         Xlab = "Species", Ylab = expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"})))

#---------------------------------Arctic----------------------------------------

    ## I want to plot my Arctic data

Arctic <- meta[which(meta$Area == "Arctic"),]

    ## Separate all the species from the Arctic must have run lines 22:40
    ## those lines create the meta object

names(Arctic)

Arctic %>% 
  group_by(Species) %>% 
  summarise(Arctic_detects = (sum(Survey == "VVB" & No_detect == FALSE)))

    ## Shows how many detects I have from each species, I will make an ros
    ## model for the species I have <=3 detects for: Arctic Cisco, Arctic Cod
    ## Broad Whitefish, Rainbow Smelt

Arctic %>% 
  group_by(Species) %>% 
summarise(n = n(),
          sum(No_detect == TRUE))

ArcticSpeciesSep <- Arctic %>% 
  group_split(by = Species)

    ## Separate Species out by group, in order they are:
    ## 1) Arctic Cisco,     n = 7, make ROS model
    ## 2) Arctic Cod,       n = 4, make ROS model
    ## 3) Broad Whitefish,  n = 6, make ROS model
    ## 4) Fourhorn Sculpin  n = 2
    ## 5) Rainbow Smelt     n = 10, make ROS model
    ## 6) Saffron Cod       n = 1

#---------------------------------Arctic ARCI ROS-------------------------------

ArcticARCIros <- ros(ArcticSpeciesSep[[1]]$Thiaminase_Activity, ArcticSpeciesSep[[1]]$No_detect)

    ## ROS model for Arctic Cisco (n = 9)

ArcticARCIrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                            c("species", "region", "mean_thia_act", 
                                              "median_thia_act", "sd_thia_act",
                                              "LT_thia_act", "UT_thia_act"))

ArcticARCIrosSummary[,1] <- "Arctic Cisco \n n = 9, n.d. = 2"
ArcticARCIrosSummary[,2] <- ArcticSpeciesSep[[1]][1,6]
ArcticARCIrosSummary[,3] <- mean(ArcticARCIros)
ArcticARCIrosSummary[,4] <- median(ArcticARCIros)
ArcticARCIrosSummary[,5] <- sd(ArcticARCIros)
ArcticARCIrosSummary[,6] <- mean(ArcticARCIros) - sd(ArcticARCIros)
ArcticARCIrosSummary[,7] <- mean(ArcticARCIros) + sd(ArcticARCIros)

#---------------------------------Arctic ARCO ROS-------------------------------

ArcticARCOros <- ros(ArcticSpeciesSep[[2]]$Thiaminase_Activity, ArcticSpeciesSep[[2]]$No_detect)

## ROS model for Arctic Cod (n = 4)

ArcticARCOrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                 c("species", "region", "mean_thia_act", 
                                   "median_thia_act", "sd_thia_act",
                                   "LT_thia_act", "UT_thia_act"))

ArcticARCOrosSummary[,1] <- "Arctic Cod \n n = 10, n.d. = 6"
ArcticARCOrosSummary[,2] <- ArcticSpeciesSep[[2]][1,6]
ArcticARCOrosSummary[,3] <- mean(ArcticARCOros)
ArcticARCOrosSummary[,4] <- median(ArcticARCOros)
ArcticARCOrosSummary[,5] <- sd(ArcticARCOros)
ArcticARCOrosSummary[,6] <- mean(ArcticARCOros) - sd(ArcticARCOros)
ArcticARCOrosSummary[,7] <- mean(ArcticARCOros) + sd(ArcticARCOros)

#---------------------------------Arctic BRWH ROS-------------------------------

ArcticBRWHros <- ros(ArcticSpeciesSep[[3]]$Thiaminase_Activity, ArcticSpeciesSep[[3]]$No_detect)

## ROS model for Broad Whitefish (n = 6)

ArcticBRWHrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                 c("species", "region", "mean_thia_act", 
                                   "median_thia_act", "sd_thia_act",
                                   "LT_thia_act", "UT_thia_act"))

ArcticBRWHrosSummary[,1] <- "Broad Whitefish, \n n = 9, n.d. = 3"
ArcticBRWHrosSummary[,2] <- ArcticSpeciesSep[[3]][1,6]
ArcticBRWHrosSummary[,3] <- mean(ArcticBRWHros)
ArcticBRWHrosSummary[,4] <- median(ArcticBRWHros)
ArcticBRWHrosSummary[,5] <- sd(ArcticBRWHros)
ArcticBRWHrosSummary[,6] <- mean(ArcticBRWHros) - sd(ArcticBRWHros)
ArcticBRWHrosSummary[,7] <- mean(ArcticBRWHros) + sd(ArcticBRWHros)

#---------------------------------Arctic RASM ROS-------------------------------

ArcticRASMros <- ros(ArcticSpeciesSep[[5]]$Thiaminase_Activity, ArcticSpeciesSep[[5]]$No_detect)

## ROS model for Rainbow Smelt (n = 10)

ArcticRASMrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                 c("species", "region", "mean_thia_act", 
                                   "median_thia_act", "sd_thia_act",
                                   "LT_thia_act", "UT_thia_act"))

ArcticRASMrosSummary[,1] <- "Rainbow Smelt \n n = 10, n.d. = 0"
ArcticRASMrosSummary[,2] <- ArcticSpeciesSep[[5]][1,6]
ArcticRASMrosSummary[,3] <- mean(ArcticRASMros)
ArcticRASMrosSummary[,4] <- median(ArcticRASMros)
ArcticRASMrosSummary[,5] <- sd(ArcticRASMros)
ArcticRASMrosSummary[,6] <- mean(ArcticRASMros) - sd(ArcticRASMros)
ArcticRASMrosSummary[,7] <- mean(ArcticRASMros) + sd(ArcticRASMros)

#---------------------------------Arctic FOSC ----------------------------------

ArcticFOSCsummaryN <- Arctic %>% 
  filter(Species == "Fourhorn Sculpin" & No_detect == FALSE) %>% 
  summarise(n = n(),
            mean_thia_act = mean(Thiaminase_Activity),
            median_thia_act = median(Thiaminase_Activity),
            sd_thia_act = sd(Thiaminase_Activity),
            LT_thia_act = mean(Thiaminase_Activity) - sd(Thiaminase_Activity),
            UT_thia_act = mean(Thiaminase_Activity) + sd(Thiaminase_Activity)) %>% 
  as.data.frame() %>% 
  print()

ArcticFOSCsummary <- ArcticFOSCsummaryN[,2:6]
ArcticFOSCsummary[,3:5] <- NA
ArcticFOSCsummary$region <- "Arctic"
ArcticFOSCsummary$species <- "Fourhorn Sculpin \n n = 10, n.d. = 8"
ArcticFOSCsummary <- ArcticFOSCsummary %>% 
  select(region, everything()) %>% 
  select(species, everything())

#---------------------------------Arctic SACO ----------------------------------

ArcticSACOsummaryN <- Arctic %>% 
  filter(Species == "Saffron Cod" & No_detect == FALSE) %>% 
  summarise(n = n(),
            mean_thia_act = mean(Thiaminase_Activity),
            median_thia_act = median(Thiaminase_Activity),
            sd_thia_act = sd(Thiaminase_Activity),
            LT_thia_act = mean(Thiaminase_Activity) - sd(Thiaminase_Activity),
            UT_thia_act = mean(Thiaminase_Activity) + sd(Thiaminase_Activity)) %>% 
  as.data.frame() %>% 
  print()

ArcticSACOsummary <- ArcticSACOsummaryN[,2:6]
ArcticSACOsummary$region <- "Arctic"
ArcticSACOsummary$species <- "Saffron Cod \n n = 10, n.d. = 9"
ArcticSACOsummary <- ArcticSACOsummary %>% 
  select(region, everything()) %>% 
  select(species, everything())


ArcticROSsum <- rbind(ArcticARCIrosSummary, ArcticARCOrosSummary, ArcticBRWHrosSummary,
                      ArcticRASMrosSummary, ArcticFOSCsummary, ArcticSACOsummary)
ArcticROSsum$RegionSpecies <- factor(paste(ArcticROSsum$region, ArcticROSsum$species, sep = " "))
ArcticROSsum$species <- factor(ArcticROSsum$species)
ArcticROSsum$region <- factor(ArcticROSsum$region)
ArcticROSsum$species <- reorder(ArcticROSsum$species, ArcticROSsum$mean_thia_act)

#---------------------------------Plotting Arctic ROS Summary-------------------
ggplot(data = ArcticROSsum, aes(x = species, y = mean_thia_act)) +
  geom_errorbar(aes(x = species, ymin = LT_thia_act, ymax = UT_thia_act,
                    width = 0.25)) +
  geom_point(size = 3.5) +
  theme_classic() +
  coord_flip() +
  ylab(expression(Mean~Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"}))) +
  xlab("Species") +
  guides(color = "none")

#---------------------------------Northern Bering Sea---------------------------

## I want to plot my Northern Bering Sea data

NBS <- meta[which(meta$Area == "Northern Bering Sea"),]

## Separate all the species from the Arctic must have run lines 22:40
## those lines create the meta object

names(NBS)

NBS %>% 
  group_by(Species) %>% 
  summarise(n = n(),
            NBS_detects = (sum(No_detect == FALSE)),
            NBS_non_detects = (sum(No_detect == TRUE)))

## Shows how many detects I have from each species, I will make an ros
## model for the species I have <=3 detects for: Capelin, Gonatus Squid, Pacific Herring,
## Pacific Herring (age 0), Rainbow Smelt, Saffron Cod, Sand Lance, Walleye Pollock (age 0)

NBSSpeciesSep <- NBS %>% 
  group_split(by = Species)

## Separate Species out by group, in order they are:
## 1) Capelin, n = 13, nondetect = 6 make ROS model
## 2) Gonatus Squid, n = 6, nondetect = 1 make ROS model
## 7) Pacific Herring,  n = 20, nondetect = 11, make ROS model
## 8) Pacific Herring (age 0), n = 8, nondetect = 3
## 9) Rainbow Smelt     n = 17, nondetect = 0, make ROS model
## 10) Saffron Cod       n = 4, nondetect = 0, make ROS model
## 11) Sand Lance, n = 18, nondetect = 11, make ROS model,
## 13) Walleye Pollock (age 0), n = 18, nondetect = 9, make ROS model
## left out: Greenland turbot, Humpy shrimp, Longhead Dap, Pacific cod (age 0),
## and Threespine stickleback

#---------------------------------NBS CPLN ROS----------------------------------

NBS_CPLNros <- ros(NBSSpeciesSep[[1]]$Thiaminase_Activity, NBSSpeciesSep[[1]]$No_detect)

## ROS model for Capelin, n = 7, nondetect = 6

NBS_CPLNrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                 c("species", "region", "mean_thia_act", 
                                   "median_thia_act", "sd_thia_act",
                                   "LT_thia_act", "UT_thia_act"))

NBS_CPLNrosSummary[,1] <- "Capelin \n n = 13, n.d. = 6"
NBS_CPLNrosSummary[,2] <- NBSSpeciesSep[[1]][1,6]
NBS_CPLNrosSummary[,3] <- mean(NBS_CPLNros)
NBS_CPLNrosSummary[,4] <- median(NBS_CPLNros)
NBS_CPLNrosSummary[,5] <- sd(NBS_CPLNros)
NBS_CPLNrosSummary[,6] <- mean(NBS_CPLNros) - sd(NBS_CPLNros)
NBS_CPLNrosSummary[,7] <- mean(NBS_CPLNros) + sd(NBS_CPLNros)

#---------------------------------NBS GNSQ ROS----------------------------------

NBS_GNSQros <- ros(NBSSpeciesSep[[2]]$Thiaminase_Activity, NBSSpeciesSep[[2]]$No_detect)

## ROS model for Gonatus Squid, n = 6, nondetect = 1

NBS_GNSQrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                               c("species", "region", "mean_thia_act", 
                                 "median_thia_act", "sd_thia_act",
                                 "LT_thia_act", "UT_thia_act"))

NBS_GNSQrosSummary[,1] <- "Gonatus Squid \n n = 6, n.d. = 1"
NBS_GNSQrosSummary[,2] <- NBSSpeciesSep[[2]][1,6]
NBS_GNSQrosSummary[,3] <- mean(NBS_GNSQros)
NBS_GNSQrosSummary[,4] <- median(NBS_GNSQros)
NBS_GNSQrosSummary[,5] <- sd(NBS_GNSQros)
NBS_GNSQrosSummary[,6] <- mean(NBS_GNSQros) - sd(NBS_GNSQros)
NBS_GNSQrosSummary[,7] <- mean(NBS_GNSQros) + sd(NBS_GNSQros)
print(NBS_GNSQrosSummary)

#---------------------------------NBS PCHG ROS----------------------------------

NBS_PCHGros <- ros(NBSSpeciesSep[[7]]$Thiaminase_Activity, NBSSpeciesSep[[7]]$No_detect)

## ROS model for Pacific Herring,  n = 20, nondetect = 11

NBS_PCHGrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                               c("species", "region", "mean_thia_act", 
                                 "median_thia_act", "sd_thia_act",
                                 "LT_thia_act", "UT_thia_act"))

NBS_PCHGrosSummary[,1] <- "Pacific Herring (age 1+) \n n = 20, n.d. = 11"
NBS_PCHGrosSummary[,2] <- NBSSpeciesSep[[7]][1,6]
NBS_PCHGrosSummary[,3] <- mean(NBS_PCHGros)
NBS_PCHGrosSummary[,4] <- median(NBS_PCHGros)
NBS_PCHGrosSummary[,5] <- sd(NBS_PCHGros)
NBS_PCHGrosSummary[,6] <- mean(NBS_PCHGros) - sd(NBS_PCHGros)
NBS_PCHGrosSummary[,7] <- mean(NBS_PCHGros) + sd(NBS_PCHGros)
print(NBS_PCHGrosSummary)

#---------------------------------NBS PCHG0 ROS----------------------------------

NBS_PCHG0ros <- ros(NBSSpeciesSep[[8]]$Thiaminase_Activity, NBSSpeciesSep[[8]]$No_detect)

## ROS model for Pacific Herring (age 0), n = 8, nondetect = 3

NBS_PCHG0rosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                               c("species", "region", "mean_thia_act", 
                                 "median_thia_act", "sd_thia_act",
                                 "LT_thia_act", "UT_thia_act"))

NBS_PCHG0rosSummary[,1] <- "Pacific Herring (age 0) \n n = 8, n.d. = 3"
NBS_PCHG0rosSummary[,2] <- NBSSpeciesSep[[8]][1,6]
NBS_PCHG0rosSummary[,3] <- mean(NBS_PCHG0ros)
NBS_PCHG0rosSummary[,4] <- median(NBS_PCHG0ros)
NBS_PCHG0rosSummary[,5] <- sd(NBS_PCHG0ros)
NBS_PCHG0rosSummary[,6] <- mean(NBS_PCHG0ros) - sd(NBS_PCHG0ros)
NBS_PCHG0rosSummary[,7] <- mean(NBS_PCHG0ros) + sd(NBS_PCHG0ros)
print(NBS_PCHG0rosSummary)

#---------------------------------NBS RASM ROS----------------------------------

NBS_RASMros <- ros(NBSSpeciesSep[[9]]$Thiaminase_Activity, NBSSpeciesSep[[9]]$No_detect)

## ROS model for Rainbow Smelt     n = 17, nondetect = 0

NBS_RASMrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                c("species", "region", "mean_thia_act", 
                                  "median_thia_act", "sd_thia_act",
                                  "LT_thia_act", "UT_thia_act"))

NBS_RASMrosSummary[,1] <- "Rainbow Smelt \n n = 17, n.d. = 0"
NBS_RASMrosSummary[,2] <- NBSSpeciesSep[[9]][1,6]
NBS_RASMrosSummary[,3] <- mean(NBS_RASMros)
NBS_RASMrosSummary[,4] <- median(NBS_RASMros)
NBS_RASMrosSummary[,5] <- sd(NBS_RASMros)
NBS_RASMrosSummary[,6] <- mean(NBS_RASMros) - sd(NBS_RASMros)
NBS_RASMrosSummary[,7] <- mean(NBS_RASMros) + sd(NBS_RASMros)
print(NBS_RASMrosSummary)

#---------------------------------NBS SACO ROS----------------------------------

NBS_SACOros <- ros(NBSSpeciesSep[[10]]$Thiaminase_Activity, NBSSpeciesSep[[10]]$No_detect)

## ROS model for Saffron Cod       n = 4, nondetect = 0, make ROS model

NBS_SACOrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                               c("species", "region", "mean_thia_act", 
                                 "median_thia_act", "sd_thia_act",
                                 "LT_thia_act", "UT_thia_act"))

NBS_SACOrosSummary[,1] <- "Saffron Cod \n n = 4, n.d. = 0"
NBS_SACOrosSummary[,2] <- NBSSpeciesSep[[10]][1,6]
NBS_SACOrosSummary[,3] <- mean(NBS_SACOros)
NBS_SACOrosSummary[,4] <- median(NBS_SACOros)
NBS_SACOrosSummary[,5] <- sd(NBS_SACOros)
NBS_SACOrosSummary[,6] <- mean(NBS_SACOros) - sd(NBS_SACOros)
NBS_SACOrosSummary[,7] <- mean(NBS_SACOros) + sd(NBS_SACOros)
print(NBS_SACOrosSummary)

#---------------------------------NBS SDLN ROS----------------------------------

NBS_SDLNros <- ros(NBSSpeciesSep[[11]]$Thiaminase_Activity, NBSSpeciesSep[[11]]$No_detect)

## ROS model for Saffron Cod       n = 4, nondetect = 0, make ROS model

NBS_SDLNrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                               c("species", "region", "mean_thia_act", 
                                 "median_thia_act", "sd_thia_act",
                                 "LT_thia_act", "UT_thia_act"))

NBS_SDLNrosSummary[,1] <- "Sand Lance \n n = 18, n.d. = 11"
NBS_SDLNrosSummary[,2] <- NBSSpeciesSep[[11]][1,6]
NBS_SDLNrosSummary[,3] <- mean(NBS_SDLNros)
NBS_SDLNrosSummary[,4] <- median(NBS_SDLNros)
NBS_SDLNrosSummary[,5] <- sd(NBS_SDLNros)
NBS_SDLNrosSummary[,6] <- mean(NBS_SDLNros) - sd(NBS_SDLNros)
NBS_SDLNrosSummary[,7] <- mean(NBS_SDLNros) + sd(NBS_SDLNros)
print(NBS_SDLNrosSummary)

#---------------------------------NBS WEPK ROS----------------------------------

NBS_WEPKros <- ros(NBSSpeciesSep[[13]]$Thiaminase_Activity, NBSSpeciesSep[[13]]$No_detect)

## ROS model for Walleye Pollock (age 0), n = 18, nondetect = 9

NBS_WEPKrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                               c("species", "region", "mean_thia_act", 
                                 "median_thia_act", "sd_thia_act",
                                 "LT_thia_act", "UT_thia_act"))

NBS_WEPKrosSummary[,1] <- "Walleye Pollock (age 0) \n n = 18, n.d. = 9"
NBS_WEPKrosSummary[,2] <- NBSSpeciesSep[[13]][1,6]
NBS_WEPKrosSummary[,3] <- mean(NBS_WEPKros)
NBS_WEPKrosSummary[,4] <- median(NBS_WEPKros)
NBS_WEPKrosSummary[,5] <- sd(NBS_WEPKros)
NBS_WEPKrosSummary[,6] <- mean(NBS_WEPKros) - sd(NBS_WEPKros)
NBS_WEPKrosSummary[,7] <- mean(NBS_WEPKros) + sd(NBS_WEPKros)
print(NBS_WEPKrosSummary)


# Bringing all NBS into one data frame------------------------------------------
(NBS_ROSsum <- rbind(NBS_CPLNrosSummary, NBS_GNSQrosSummary, NBS_PCHGrosSummary,
                     NBS_PCHG0rosSummary, NBS_RASMrosSummary, NBS_SACOrosSummary,
                     NBS_SDLNrosSummary, NBS_WEPKrosSummary))
NBS_ROSsum$RegionSpecies <- factor(paste(NBS_ROSsum$region, NBS_ROSsum$species, sep = " "))
NBS_ROSsum$species <- factor(NBS_ROSsum$species)
NBS_ROSsum$region <- factor(NBS_ROSsum$region)
NBS_ROSsum$species <- reorder(NBS_ROSsum$species, NBS_ROSsum$mean_thia_act)

#---------------------------------Plotting NBS ROS Summary-------------------
ggplot(data = NBS_ROSsum, aes(x = species, y = mean_thia_act)) +
  geom_errorbar(aes(x = species, ymin = LT_thia_act, ymax = UT_thia_act,
                    width = 0.25)) +
  geom_point(size = 3.5) +
  theme_classic() +
  coord_flip() +
  ylab(expression(Mean~Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"}))) +
  xlab("Species") +
  guides(color = "none")

#---------------------------------Southeast-------------------------------------

## I want to plot my Southeast Alaska Data

SEAK <- meta[which(meta$Area == "Southeast Alaska"),]

## Separate all the species from the Arctic must have run lines 22:40
## those lines create the meta object

names(SEAK)

SEAK %>% 
  group_by(Species) %>% 
  summarise(n = n(),
            SEAK_detects = (sum(No_detect == FALSE)),
            SEAK__non_detects = (sum(No_detect == TRUE)))

## Shows how many detects I have from each species, I will make an ros
## model for the species I have <=3 detects for: Capelin, Pacific herring, Sand Lance,
## Surf Smelt

SEAKSpeciesSep <- SEAK %>% 
  group_split(by = Species)

## Separate Species out by group, in order they are:
## 1) Capelin, n = 17, nondetect = 0 make ROS model
## 2) Pacific Herring, n = 10, nondetect = 5 make ROS model
## 3) Sand Lance,  n = 8, nondetect = 5, make ROS model
## 4) Surf Smelt, n = 11, nondetect = 5

#---------------------------------SEAK CPLN ROS----------------------------------

SEAK_CPLNros <- ros(SEAKSpeciesSep[[1]]$Thiaminase_Activity, SEAKSpeciesSep[[1]]$No_detect)

## ROS model for SEAK Capelin, n = 17, nondetect = 0

SEAK_CPLNrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                               c("species", "region", "mean_thia_act", 
                                 "median_thia_act", "sd_thia_act",
                                 "LT_thia_act", "UT_thia_act"))

SEAK_CPLNrosSummary[,1] <- "Capelin \n n = 17, n.d. = 0"
SEAK_CPLNrosSummary[,2] <- SEAKSpeciesSep[[1]][1,6]
SEAK_CPLNrosSummary[,3] <- mean(SEAK_CPLNros)
SEAK_CPLNrosSummary[,4] <- median(SEAK_CPLNros)
SEAK_CPLNrosSummary[,5] <- sd(SEAK_CPLNros)
SEAK_CPLNrosSummary[,6] <- mean(SEAK_CPLNros) - sd(SEAK_CPLNros)
SEAK_CPLNrosSummary[,7] <- mean(SEAK_CPLNros) + sd(SEAK_CPLNros)

#---------------------------------SEAK PCHG ROS----------------------------------

SEAK_PCHGros <- ros(SEAKSpeciesSep[[2]]$Thiaminase_Activity, SEAKSpeciesSep[[2]]$No_detect)

## ROS model for SEAK Pacific Herring, n = 10, nondetect = 5

SEAK_PCHGrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                c("species", "region", "mean_thia_act", 
                                  "median_thia_act", "sd_thia_act",
                                  "LT_thia_act", "UT_thia_act"))

SEAK_PCHGrosSummary[,1] <- "Pacific Herring (age 1+) \n n = 10, n.d. = 5"
SEAK_PCHGrosSummary[,2] <- SEAKSpeciesSep[[2]][1,6]
SEAK_PCHGrosSummary[,3] <- mean(SEAK_PCHGros)
SEAK_PCHGrosSummary[,4] <- median(SEAK_PCHGros)
SEAK_PCHGrosSummary[,5] <- sd(SEAK_PCHGros)
SEAK_PCHGrosSummary[,6] <- mean(SEAK_PCHGros) - sd(SEAK_PCHGros)
SEAK_PCHGrosSummary[,7] <- mean(SEAK_PCHGros) + sd(SEAK_PCHGros)

#---------------------------------SEAK SDLN ROS---------------------------------

SEAK_SDLNros <- ros(SEAKSpeciesSep[[3]]$Thiaminase_Activity, SEAKSpeciesSep[[3]]$No_detect)

## Sand Lance,  n = 8, nondetect = 5, make ROS model

SEAK_SDLNrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                c("species", "region", "mean_thia_act", 
                                  "median_thia_act", "sd_thia_act",
                                  "LT_thia_act", "UT_thia_act"))

SEAK_SDLNrosSummary[,1] <- "Sand Lance, \n n = 8, n.d. = 5"
SEAK_SDLNrosSummary[,2] <- SEAKSpeciesSep[[3]][1,6]
SEAK_SDLNrosSummary[,3] <- mean(SEAK_SDLNros)
SEAK_SDLNrosSummary[,4] <- median(SEAK_SDLNros)
SEAK_SDLNrosSummary[,5] <- sd(SEAK_SDLNros)
SEAK_SDLNrosSummary[,6] <- mean(SEAK_SDLNros) - sd(SEAK_SDLNros)
SEAK_SDLNrosSummary[,7] <- mean(SEAK_SDLNros) + sd(SEAK_SDLNros)

#---------------------------------SEAK SUSM ROS---------------------------------

SEAK_SUSMros <- ros(SEAKSpeciesSep[[4]]$Thiaminase_Activity, SEAKSpeciesSep[[4]]$No_detect)

## Surf Smelt, n = 11, nondetect = 5

SEAK_SUSMrosSummary <- setNames(data.frame(matrix(ncol = 7, nrow = 1)), 
                                c("species", "region", "mean_thia_act", 
                                  "median_thia_act", "sd_thia_act",
                                  "LT_thia_act", "UT_thia_act"))

SEAK_SUSMrosSummary[,1] <- "Surf Smelt \n n = 11, n.d. = 5"
SEAK_SUSMrosSummary[,2] <- SEAKSpeciesSep[[4]][1,6]
SEAK_SUSMrosSummary[,3] <- mean(SEAK_SUSMros)
SEAK_SUSMrosSummary[,4] <- median(SEAK_SUSMros)
SEAK_SUSMrosSummary[,5] <- sd(SEAK_SUSMros)
SEAK_SUSMrosSummary[,6] <- mean(SEAK_SUSMros) - sd(SEAK_SUSMros)
SEAK_SUSMrosSummary[,7] <- mean(SEAK_SUSMros) + sd(SEAK_SUSMros)

# Bringing all SEAK into one data frame------------------------------------------
(SEAK_ROSsum <- rbind(SEAK_CPLNrosSummary, SEAK_PCHGrosSummary, SEAK_SDLNrosSummary,
                      SEAK_SUSMrosSummary))
SEAK_ROSsum$RegionSpecies <- factor(paste(SEAK_ROSsum$region, SEAK_ROSsum$species, sep = " "))
SEAK_ROSsum$species <- factor(SEAK_ROSsum$species)
SEAK_ROSsum$region <- factor(SEAK_ROSsum$region)
SEAK_ROSsum$species <- reorder(SEAK_ROSsum$species, SEAK_ROSsum$mean_thia_act)

#---------------------------------Plotting SEAK ROS Summary-------------------
ggplot(data = SEAK_ROSsum, aes(x = species, y = mean_thia_act)) +
  geom_errorbar(aes(x = species, ymin = LT_thia_act, ymax = UT_thia_act,
                    width = 0.25)) +
  geom_point(size = 3.5) +
  theme_classic() +
  coord_flip() +
  ylab(expression(Mean~Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"}))) +
  xlab("Species") +
  guides(color = "none")

#Plotting All Regions Together arranged by Mean activity level------------------
xxxxxx <- rbind(NBS_ROSsum, ArcticROSsum, SEAK_ROSsum)
xxxxxx$species <- reorder(xxxxxx$species, xxxxxx$mean_thia_act)
xxxxxx$RegionSpecies <- droplevels(xxxxxx$RegionSpecies)
xxxxxx$RegionSpecies <- reorder(xxxxxx$RegionSpecies, xxxxxx$mean_thia_act)

mtPoint = 12
mtAllText = 36
mtWidth = 20
mtHeight = 20

megaThiaminase <- ggplot(data = xxxxxx, aes(x = species, y = mean_thia_act)) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "red", size = 2) +
  geom_errorbar(aes(x = species, ymin = LT_thia_act, ymax = UT_thia_act,
                    width = 0.25), size = 2) +
  geom_point(aes(color = region, shape = region), size = mtPoint) +
  theme_classic() +
  coord_flip() +
  ylab(expression(Mean~Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"}))) +
  xlab("Species") +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.1),
        text = element_text(size = mtAllText)) +
  scale_color_manual(values = c("#D55E00", "#56B4E9", "#009E73")) +
  scale_y_continuous(breaks = seq(0,30, by = 5), limits = c(-1,28))

megaThiaminase

ggsave("megaThiaminase.png", plot = megaThiaminase, path = dir.output,
       width = mtWidth, height = mtHeight, units = c("in"))

#Plotting All Regions Together arranged by region------------------------------
yyyyyy <- rbind(NBS_ROSsum, ArcticROSsum, SEAK_ROSsum)
yyyyyy$species <- reorder(yyyyyy$species, yyyyyy$region)
yyyyyy$RegionSpecies <- droplevels(yyyyyy$RegionSpecies)
yyyyyy$RegionSpecies <- reorder(yyyyyy$RegionSpecies, yyyyyy$mean_thia_act)

mtPoint = 12
mtAllText = 36
mtWidth = 20
mtHeight = 20

megaThiaminaseyyy <- ggplot(data = yyyyyy, aes(x = species, y = mean_thia_act)) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "red", size = 2) +
  geom_errorbar(aes(x = species, ymin = LT_thia_act, ymax = UT_thia_act,
                    width = 0.25), size = 2) +
  geom_point(aes(color = region, shape = region), size = mtPoint) +
  theme_classic() +
  coord_flip() +
  ylab(expression(Mean~Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"}))) +
  xlab("Species") +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.1),
        text = element_text(size = mtAllText)) +
  scale_color_manual(values = c("#D55E00", "#56B4E9", "#009E73")) +
  scale_y_continuous(breaks = seq(0,30, by = 5), limits = c(-1,28))

megaThiaminaseyyy

ggsave("megaThiaminaseyyy.png", plot = megaThiaminaseyyy, path = dir.output,
       width = mtWidth, height = mtHeight, units = c("in"))

#---------------------------RAINBOW SMELT COMPARISON BY REGION AND YEAR---------

RASM <- filter(meta, Species == "Rainbow Smelt")

RASM %>%
  group_by(Reg_Date) %>% 
  summarise(n = n())

RASM$Reg_Date <- factor(RASM$Reg_Date, 
                        levels = c("Northern Bering Sea 2022",
                                   "Northern Bering Sea 2021",
                                   "Arctic 2022", "Arctic 2021"),
                        labels = c("N. Bering Sea \n2022 \n n = 8",
                                   "N. Bering Sea \n2021 \n n = 9",
                                   "Arctic \n2022 \n n = 6", "Arctic \n2021 \n n = 4"))
RASM$Area <- droplevels(RASM$Area)
RASM$Area <- factor(RASM$Area, levels = c("Arctic", "Northern Bering Sea"))

names(RASM)

RASMAllText = 36
RASMWidth = 7.5
RASMHeight = 7.5

RASMthia <- ggplot(RASM, aes(x = Reg_Date, y = Thiaminase_Activity,
                 fill = Area)) +
  geom_boxplot() +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
  guides(fill = "none") +
  xlab("Region and Year") +
  ylab(expression(Rainbow~Smelt~Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"}))) +
  theme(text = element_text(size = RASMAllText))

ggsave("RASMthia.png", plot = RASMthia, path = dir.output,
       width = RASMWidth, height = RASMHeight, units = c("in"))




#OLD WORK-----------------------------------------------------------------------
warnings()

Arctic_filtered <- filter(Arctic, Species != "Fourhorn Sculpin")
Arctic_filtered <-filter(Arctic_filtered, Species != "Saffron Cod")

Arctic_filtered %>% 
  group_by(Species) %>% 
  summarise(Arctic_detects = (sum(Survey == "VVB" & No_detect == FALSE)))

Arctic_filtered$Species <- droplevels(Arctic_filtered$Species)

cboxplot(Arctic_filtered$Thiaminase_Activity, Arctic_filtered$No_detect, xgroup = Arctic_filtered$Species,
         show = FALSE, printstat = TRUE, Title = "Arctic",
         Xlab = "Species", Ylab = expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"})))


#---------------------------Southeast-------------------------------------------

Southeast <- meta[which(meta$Area == "Southeast Alaska"),]
names(Southeast)
Southeast %>% 
  group_by(Species) %>% 
  summarise(Southeast_detects = (sum(Area == "Southeast Alaska" & No_detect == FALSE)))

Southeast$Species <- droplevels(Southeast$Species)

cboxplot(Southeast$Thiaminase_Activity, Southeast$No_detect, xgroup = Southeast$Species,
         show = FALSE, printstat = TRUE, Title = "Southeast Alaska 2021 and 2022",
         Xlab = "Species", Ylab = expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"})))

#now make ros models for each species
#So, make split the dataframe by species

BeringSeaSpeciesSep <- BeringSea %>% 
  group_split(Species)
# 1 = Capelin, 2 = Gonatus Squid, 3 = Pacific Herring, 4 = Pacific Herring (age 0),
# 5 = Rainbow Smelt, 6 = Saffron Cod, 7 = Sand Lance, 8 = Walleye Pollock (age 0),

#BeringSeaSpeciesSep[[9]]$Species

# Capelin
BSCapelinROS <- ros(BeringSeaSpeciesSep[[1]]$Thiaminase_Activity,
                    BeringSeaSpeciesSep[[1]]$No_detect)

mean(BSCapelinROS)
median(BSCapelinROS)
sd(BSCapelinROS)

# Gonatus Squid
BSGonatusSquidROS <- ros(BeringSeaSpeciesSep[[2]]$Thiaminase_Activity,
                    BeringSeaSpeciesSep[[2]]$No_detect)

mean(BSGonatusSquidROS)
median(BSGonatusSquidROS)
sd(BSGonatusSquidROS)

# Pacific Herring
BSPacificHerringROS <- ros(BeringSeaSpeciesSep[[3]]$Thiaminase_Activity,
                         BeringSeaSpeciesSep[[3]]$No_detect)

mean(BSPacificHerringROS)
median(BSPacificHerringROS)
sd(BSPacificHerringROS)

# Pacific Herring (age 0)
BSPacificHerringAge0ROS <- ros(BeringSeaSpeciesSep[[4]]$Thiaminase_Activity,
                           BeringSeaSpeciesSep[[4]]$No_detect)

mean(BSPacificHerringAge0ROS)
median(BSPacificHerringAge0ROS)
sd(BSPacificHerringAge0ROS)

# Rainbow Smelt
BSRainbowSmeltROS <- ros(BeringSeaSpeciesSep[[5]]$Thiaminase_Activity,
                               BeringSeaSpeciesSep[[5]]$No_detect)

mean(BSRainbowSmeltROS)
median(BSRainbowSmeltROS)
sd(BSRainbowSmeltROS)

# Saffron Cod
BSSaffronCodROS <- ros(BeringSeaSpeciesSep[[6]]$Thiaminase_Activity,
                         BeringSeaSpeciesSep[[6]]$No_detect)

mean(BSSaffronCodROS)
median(BSSaffronCodROS)
sd(BSSaffronCodROS)

# Sand Lance
BSSandLanceROS <- ros(BeringSeaSpeciesSep[[7]]$Thiaminase_Activity,
                       BeringSeaSpeciesSep[[7]]$No_detect)

mean(BSSandLanceROS)
median(BSSandLanceROS)
sd(BSSandLanceROS)

# Walleye Pollock (age 0)
BSWalleyePollockAge0ROS <- ros(BeringSeaSpeciesSep[[8]]$Thiaminase_Activity,
                      BeringSeaSpeciesSep[[8]]$No_detect)

mean(BSWalleyePollockAge0ROS)
median(BSWalleyePollockAge0ROS)
sd(BSWalleyePollockAge0ROS)


# Rainbow Smelt ----------------------------------------------------------------
# For AMSS Poster, make boxplot of just Rainbow Smelt

RainbowSmelt <- meta[meta$Species %in% "Rainbow Smelt",]
RainbowSmelt$Reg_Date <- factor(RainbowSmelt$Reg_Date)


RainbowSmeltNBS22 <- RainbowSmelt[which(RainbowSmelt$Reg_Date == "Northern Bering Sea 2022"),] 
RainbowSmeltNBS22ros <- ros(RainbowSmeltNBS22$Thiaminase_Activity, RainbowSmeltNBS22$No_detect)
mean(RainbowSmeltNBS22ros) #8.984966
median(RainbowSmeltNBS22ros) #8.607865
sd(RainbowSmeltNBS22ros) #4.804945
length(RainbowSmeltNBS22$Sample.ID) #n = 8

RainbowSmeltNBS21 <- RainbowSmelt[which(RainbowSmelt$Reg_Date == "Northern Bering Sea 2021"),] 
RainbowSmeltNBS21ros <- ros(RainbowSmeltNBS21$Thiaminase_Activity, RainbowSmeltNBS21$No_detect)
mean(RainbowSmeltNBS21ros) #20.41208
median(RainbowSmeltNBS21ros) #16.51462
sd(RainbowSmeltNBS21ros) #13.68385
length(RainbowSmeltNBS21$Sample.ID) #n = 9

RainbowSmeltVVB21 <- RainbowSmelt[which(RainbowSmelt$Reg_Date == "Von_Biela 2021"),] 
RainbowSmeltVVB21ros <- ros(RainbowSmeltVVB21$Thiaminase_Activity, RainbowSmeltVVB21$No_detect)
mean(RainbowSmeltVVB21ros) #13.84052
median(RainbowSmeltVVB21ros) #14.76775
sd(RainbowSmeltVVB21ros) #4.989214
length(RainbowSmeltVVB21$Sample.ID) #n = 4


unique(RainbowSmelt$Reg_Date)


RainbowSmelt %>% 
  group_by(Survey) %>% 
  summarise(n = n())

ggplot(data = RainbowSmelt, aes(x = Reg_Date, y = Thiaminase_Activity,
                                fill = Reg_Date)) +
  geom_boxplot()


View(cboxplot(RainbowSmelt$Thiaminase_Activity, RainbowSmelt$No_detect, RainbowSmelt$Reg_Date))
cenboxplot(RainbowSmelt$Thiaminase_Activity, RainbowSmelt$No_detect, RainbowSmelt$Reg_Date, log = FALSE)
?cenboxplot
?cboxplot
# Capelin ----------------------------------------------------------------------

Capelin <- meta[meta$Species %in% "Capelin",]

Capelin %>% 
  group_by(Area) %>% 
  summarise(n = n(),
            n_Detects = sum(No_detect == FALSE))

Capelin$Area <- droplevels(Capelin$Area)

cboxplot(Capelin$Thiaminase_Activity, Capelin$No_detect, Capelin$Area,
         printstat = TRUE, dl.loc = "topleft")

?cboxplot
cboxplot
#Pacific Herring ---------------------------------------------------------------

PacificHerring <- meta[meta$Species %in% c("Pacific Herring"),]
PacificHerring$Survey <- droplevels(PacificHerring$Survey) 

PacificHerring %>% 
  group_by(Species, Survey) %>% 
  summarise(n = n(),
            n_Detects = sum(No_detect == FALSE))

cboxplot(PacificHerring$Thiaminase_Activity, PacificHerring$No_detect, xgroup = PacificHerring$Survey)

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
PacificHerring$Survey <- droplevels(PacificHerring$Survey)

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

cboxplot(Capelin$Thiaminase_Activity, Capelin$No_detect, xgroup = Capelin$Area)
?cboxplot
xx = cboxplot(RainbowSmelt$Thiaminase_Activity, RainbowSmelt$No_detect, xgroup = RainbowSmelt$Survey)

censtats(RainbowSmelt$Thiaminase_Activity, RainbowSmelt$No_detect)

censtats(SandLance$Thiaminase_Activity, SandLance$No_detect)


unique(RainbowSmelt$Survey)
SandLanceNBS <- SandLance[which(SandLance$Survey == c("NBS2021", "NBS2022")),]

RainbowSmeltNBS <- RainbowSmelt[which(RainbowSmelt$Survey == c("NBS2021", "NBS2022")),]
RainbowSmeltNBS <- RainbowSmelt[which(RainbowSmelt$Survey == c("NBS2021", "NBS2022")),]
RainbowSmeltVVB <- RainbowSmelt[which(RainbowSmelt$Survey == "VVB"),]

t.test(RainbowSmeltNBS$Thiaminase_Activity, RainbowSmeltVVB$Thiaminase_Activity)

censtats(SandLanceNBS2021$Thiaminase_Activity, San)

?which

RainbowSmeltNBSros <- ros(RainbowSmeltNBS$Thiaminase_Activity, RainbowSmeltNBS$No_detect)

mean(RainbowSmeltNBSros)

PacificHerring

PacificHerring$Survey
cboxplot(PacificHerring$Thiaminase_Activity, PacificHerring$No_detect, 
         xgroup = PacificHerring$Survey, show = TRUE)
?cboxplot

censummary(PacificHerring$Thiaminase_Activity, PacificHerring$No_detect,
           groups = PacificHerring$Survey)

PacificHerringros <- ros(PacificHerring$Thiaminase_Activity, PacificHerring$No_detect)

mean(PacificHerringros)



# 1-06-2023 Testing Figures for AMSS
# Capelin figure, SE Alaska vs. Bering Sea

Capelin <- filter(meta, Species == "Capelin")
Capelin$Area <- droplevels(Capelin$Area)

x11(width = 1100, height = 1000)
par(cex = 2.05, tcl = 0)
cboxplot(Capelin$Thiaminase_Activity, Capelin$No_detect, Capelin$Area,
         #Ylab = expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"})),
         Ylab = "",
         Xlab = "Region",
         Title = "Capelin",
         bxcol = c("#009E73", "#D55E00"))

#Rainbow Smelt Activity NBS21 v NBS22 vs 

RainbowSmelt <- filter(meta, Species == "Rainbow Smelt")
RainbowSmelt$Area <- droplevels(RainbowSmelt$Area)
RainbowSmelt$Reg_Date

RainbowSmelt$Reg_Date <- factor(RainbowSmelt$Reg_Date,
                                labels = c("Arctic\n2021",
                                           "Arctic\n2022",
                                           "Bering Sea\n2021",
                                           "Bering Sea\n2022"))


x11(width = 2200, height = 1000)
par(cex = 2.05, tcl = 0)
boxplot(RainbowSmelt$Thiaminase_Activity ~ RainbowSmelt$Reg_Date,
        ylab = expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"})),
        xlab = "Region/Year",
        main = "Rainbow Smelt",
        col = c("#009E73", "#D55E00", "#56B4E9", "#F0E442"),
        cex = 8)

?par
#Pacific Herring

PacificHerring <- filter(meta, Species == "Pacific Herring" | Species == "Pacific Herring (age 0)")
PacificHerring$Area <- droplevels(PacificHerring$Area)

x11(width = 1100, height = 1000)
par(cex = 2.05, tcl = 0)
cboxplot(PacificHerring$Thiaminase_Activity, PacificHerring$No_detect, PacificHerring$Area,
         Ylab = expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"})),
         Xlab = "Region",
         Title = "Pacific Herring",
         bxcol = c("#009E73", "#D55E00"))

#Sand Lance

SandLance <- filter(meta, Species == "Sand Lance" & Area != "Southern Bering Sea")
SandLance$Area <- droplevels(SandLance$Area)

x11(width = 1100, height = 1000)
par(cex = 2.05, tcl = 0)
cboxplot(SandLance$Thiaminase_Activity, SandLance$No_detect, SandLance$Area,
         #Ylab = expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"})),
         Ylab = "",
         Xlab = "Region",
         Title = "Sand Lance",
         bxcol = c("#009E73", "#D55E00"))

?cboxplot

par(mfrow = c(2,2))

cboxplot




names(meta)


unique(test$Species)
test <- filter(meta, Species == "Pacific Herring" | Species == "Pacific Herring (age 0)")
test <- filter(test, No_detect == "FALSE")

ggplot(data = test, aes(x = Fork.Length..cm., y = Thiaminase_Activity)) +
  geom_point() +
  geom_smooth(method = "lm")

herringLengthfit <- lm(Thiaminase_Activity ~ Fork.Length..cm., data = test)
summary(herringLengthfit)
