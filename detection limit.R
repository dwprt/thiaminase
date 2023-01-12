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
meta <- meta[-c(233:334),] #removing samples that were not analysed
meta <- meta[-c(233:241),] #removing samples that were not analysed

meta$Catch.Date <- mdy(meta$Catch.Date)
levels(meta$Area) <- c("Northern Bering Sea", "Southeast Alaska", "Southern Bering Sea", "Arctic")
meta$Reg_Date <- paste(meta$Area, " ", year(meta$Catch.Date), sep = "")




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

write_xlsx(meta_summary3, path = file.path(dir.output, "META_SUMMARY.xlsx"))
?writexl



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

BeringSea <- meta[which(meta$Area == "Bering Sea"),]

#Ok we have all the Bering Sea Samples, now I just want the species with =>3 detects

BeringSea <- BeringSea[BeringSea$Species %in% c("Capelin", "Gonatus Squid",
                                                    "Pacific Herring",
                                                    "Pacific Herring (age 0)",
                                                    "Rainbow Smelt", "Saffron Cod",
                                                    "Sand Lance", "Walleye Pollock (age 0)"),]

# Now make sure we only have factor levels for the species we are actually using

BeringSea$Species <- droplevels(BeringSea$Species)

# Now make a boxplot of Thiaminase Activity Levels in the Bering Sea by Species
# Using ROS methods to handle censored observations

cboxplot(BeringSea$Thiaminase_Activity, BeringSea$No_detect, xgroup = BeringSea$Species,
         show = FALSE, printstat = TRUE, Title = "NBS2021 and NBS2022",
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
