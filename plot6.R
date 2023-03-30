# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips ==
# "06037"). Which city has seen greater changes over time in motor vehicle
# emissions?

library(dplyr)
library(tidyr)
library(ggplot2)

plot6 <- function () {
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  scc_values <- SCC %>% 
    filter(grepl("Moto", Short.Name)) %>%
    select(SCC) %>% 
    pull
  
  df <- NEI %>%
    filter(SCC %in% scc_values, fips %in% c("24510", "06037"), year %in% c(1999, 2008)) %>%
    mutate(city=ifelse(fips == "24510", "Baltimore", "Los Angeles")) %>%
    group_by(city, year) %>% 
    summarise(emission=sum(Emissions))
  
  segment_df <- pivot_wider(df, names_from = year, values_from = emission)
  
  p <- ggplot(df, aes(x=year, y=emission)) +
    geom_point(aes(color=city)) +
    geom_segment(aes(x=1999, y=`1999`, xend=2008, yend=`2008`, color=city), data=segment_df) +
    xlab("Year") + ylab("Emission (tons of PM2.5)") + ggtitle("Change of PM2.5 Emission related to Motor Vehicles for Baltimore")
  
  ggsave("plot6.png", p)
}

