library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(showtext)
library(here)
library(ggridges)
library(viridis)
library(ggthemes)

#Specify font
font_add_google("Lato", "lato")
showtext_auto()

#Import data

tuesdata <- tidytuesdayR::tt_load(2023, week = 20)

tornados <- tuesdata$tornados

#convert the year column to dates
tornados$yr <- as.Date(paste0(tornados$yr, "-01-01"))

#Segment the data into 10 year buckets
tornados$year_bucket <- floor_date(tornados$yr, "10 years")

#Filter out outliers
lower_quantile <- quantile(tornados$wid, 0.05)
upper_quantile <- quantile(tornados$wid, 0.95)

filtered_tornados <- tornados[tornados$wid >= lower_quantile & tornados$wid <= upper_quantile, ]


Chart <- ggplot(filtered_tornados, aes(x=wid, y=year_bucket,   fill=factor(year_bucket))) +
  geom_density_ridges_gradient(scale = 1.4) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), trans = "sqrt") +
  scale_fill_viridis_d(name = "Tornado Width", option = "W",   alpha = 0.5) +
  guides(fill = FALSE) +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks = unique  (filtered_tornados$year_bucket),
  labels= c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")) +
  labs(title="Average Tornado Width by Decade") 

Chart + theme_wsj()+ scale_colour_wsj("colors6")+
  xlab("") +
  ylab("") +
  labs(title = "Tornado Size by Decade",
       subtitle = "Analysis of tornado width by decade for all tornados tracked by the NOAA.\n The size and number of tornados has increased in recent years.",
       caption = "Data from NOAA. #TidyTuesday Week 20. Chart by Sean Gardner (seanmgard.com)") +
  theme(axis.text.x = element_blank(),
        plot.caption = element_text(size = 8),
        plot.subtitle = element_text(size = 15),
        plot.title = element_text(size = 22))
