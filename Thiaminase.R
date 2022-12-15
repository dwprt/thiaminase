# Thiaminase Activity Assays
# Author: Drew Porter

library(dplyr)
library(ggplot2)
library(ggpmisc)
library(cowplot)

getwd()
#wd <- "Z:/Drew Porter/R/thiaminase"
setwd(wd)

dir.data <- file.path(wd, "data")
dir.output <- file.path(wd, "output")


meta <- read.csv(file = file.path(dir.data, "Thiaminase.Meta.2022.csv"),
                 stringsAsFactors = TRUE)

meta <- meta[-c(190:334),]

meta$Sample.ID <- as.character(meta$Sample.ID)

str(meta)

meta_noise_remove <- meta[-c(which(meta$Signal.Noise == FALSE)),]
meta_noise_remove <- meta_noise_remove[-c(190:334),]


meta_NA <- meta[-c(190:334),]



meta_noise_remove %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Thiaminase_Activity),
            SD = sd(Thiaminase_Activity),
            n = n(),
            Pct_ND = ((sum(No_detect == TRUE))/n()) * 100)


unique(meta_noise_remove$Species)

humpy <- which(meta_noise_remove$Species == "Humpy shrimp")
age0_p_cod <- which(meta_noise_remove$Species == "Pacific cod (age 0)")
dab <- which(meta_noise_remove$Species == "Longhead Dab")
turb <- which(meta_noise_remove$Species == "Greenland turbot")
sculp <- which(meta_noise_remove$Species == "Fourhorn Sculpin")

meta_noise_remove_imp <- meta_noise_remove[-c(humpy, age0_p_cod, dab, turb, sculp),]

meta_sep_species <- group_split(meta_species)

meta_sep_species[[14]]$Species

ARCI <- meta_sep_species[[2]]
ARCO <- meta_sep_species[[3]]
BRWH <- meta_sep_species[[4]]
Capelin <- meta_sep_species[[5]]
FOSC <- meta_sep_species[[6]]
Squid <- meta_sep_species[[7]]
Turbot <- meta_sep_species[[8]]
Shrimp <- meta_sep_species[[9]]
Dab <- meta_sep_species[[10]]
P_cod <- meta_sep_species[[11]]
P_herring <- meta_sep_species[[12]]
P_herring_age0 <- meta_sep_species[[13]]
RASM <- meta_sep_species[[14]]



ggplot(data = RASM, aes(x = Mass..g., y = Thiaminase_Activity,
                        color = Area)) +
  geom_point()

ggplot(data = RASM, aes(y = Thiaminase_Activity, x = Area)) +
  geom_boxplot()

names(RASM)


ggplot(data = meta_noise_remove_imp, aes(x = Species, y = Thiaminase_Activity, fill = Species)) +
  geom_boxplot() +
  ylab(expression(Thiaminase~Activity~(nmol~T~"∙"~g^{"-1"}~"∙"~m^{"-1"}))) +
  guides(fill = "none") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

?bquote
  "Thiaminase Activity (nmol T∙g"^"-1""∙m"^"-1)"
