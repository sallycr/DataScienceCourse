# 
# R script to explore the total emissions changes in the Baltimore City, 
# Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008 
#
library(dplyr)

plotTotalPM25InBaltimore <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    
    # total emission of each year of Baltimore City, MaryLand
    emissionByYear <- NEI %>% 
        filter(fips == "24510") %>%
        select(year, Emissions) %>% 
        group_by(year) %>% 
        summarise_each(funs(sum))
    
    # plotting the total emission usnig barplot 
    png('plot2.png')
    barplot(emissionByYear$Emissions, names.arg=emissionByYear$year, 
            xlab='years', ylab='Total PM2.5 emission', 
            main='Total emissions from PM2.5 in Baltimore City, Maryland') 
    dev.off()
}

# plotTotalPM25InBaltimore()