library(lubridate)
library(ggplot2)
library(dplyr)

wd <- getwd()
dir.data <- file.path(wd, "data")
dir.output <- file.path(wd, "output")

ThiaminaseCCV <- read.csv(file = file.path(dir.data, "Thiaminase.CCV.2022.csv"),
                          stringsAsFactors = TRUE)

ThiaminaseCCV$Analysis_Date <- as.Date(ThiaminaseCCV$Analysis_Date)


?date
str(ThiaminaseCCV)

ggplot(data = ThiaminaseCCV, aes(x = Sample_ID, y = Thiaminase_Activity,
                                 fill = Sample_ID)) +
  geom_boxplot()

ThiaminaseCCV %>% 
  group_by(Sample_ID) %>% 
  summarise(Mean = mean(Thiaminase_Activity),
            SD = sd(Thiaminase_Activity))
