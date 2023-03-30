# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.

library(dplyr)
library(tidyr)
library(ggplot2)

plot3 <- function () {
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  df <- NEI %>% 
    filter(fips == "24510", year %in% c(1999, 2008)) %>% 
    mutate(type=as.factor(type)) %>% 
    group_by(type, year) %>% 
    summarise(emission=sum(Emissions))
  
  segment_df <- pivot_wider(df, names_from = year, values_from = emission)
  
  p <- ggplot(df, aes(x=year, y=emission)) +
    geom_point(aes(color=type)) +
    geom_segment(aes(x=1999, y=`1999`, xend=2008, yend=`2008`, color=type), data=segment_df) +
    xlab("Year") + ylab("Emission (tons of PM2.5)") + ggtitle("Changes of PM2.5 Emission by type for Baltimore")
  
  ggsave("plot3.png", p)
}


