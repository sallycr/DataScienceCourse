# 
# R script to explore the total emissions from PM2.5 decreased 
# in the United States from 1999 to 2008.  
#
library(dplyr)

plotTotalPM25Emission <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    
    # total emission of each year
    emissionByYear <- NEI %>% 
        select(year, Emissions) %>% 
        group_by(year) %>% 
        summarise_each(funs(sum))
    
    # plotting the total emission usnig barplot 
    png('plot1.png')
    barplot(emissionByYear$Emissions, names.arg=emissionByYear$year, 
            xlab='years', ylab='Total PM2.5 emission', 
            main='Total emissions from PM2.5 over the years') 
    dev.off()
}

# plotTotalPM25Emission()