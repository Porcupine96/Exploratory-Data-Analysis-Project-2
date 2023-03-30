# How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

library(dplyr)
library(tidyr)
library(ggplot2)

plot5 <- function () {
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  scc_values <- SCC %>% 
    filter(grepl("Moto", Short.Name)) %>%
    select(SCC) %>% 
    pull
  
  df <- NEI %>%
    filter(SCC %in% scc_values, fips == "24510", year %in% c(1999, 2008)) %>%
    group_by(year) %>% 
    summarise(emission=sum(Emissions))
  
  segment_df <- pivot_wider(df, names_from = year, values_from = emission)
  
  p <- ggplot(df, aes(x=year, y=emission)) +
    geom_point() +
    geom_segment(aes(x=1999, y=`1999`, xend=2008, yend=`2008`), data=segment_df) +
    xlab("Year") + ylab("Emission (tons of PM2.5)") + ggtitle("Change of PM2.5 Emission related to Motor Vehicles for Baltimore")
  
  ggsave("plot5.png", p)
}

