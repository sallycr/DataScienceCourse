# 
# motor vehicle sources changes from 1999–20089 to 2008 
# in Baltimore City and Los Angeles County
#
library(dplyr)
library(ggplot2)

plotMotorVehicleSources <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    # get motor vehicle sources SCC codes 
    mvScc <- SCC %>% 
        filter(grepl('motor vehicle|highway veh', Short.Name, ignore.case=TRUE)) %>%
        select(SCC, Short.Name)
    
    # count sources of motor vehicle related in fip 24510
    sources <- NEI %>% 
        filter(SCC %in% mvScc[,1] & fips %in% c('24510','06037')) %>%
        select(year, fips, Emissions) %>% 
        group_by(year, fips) %>% 
        summarise_each(funs(sum))
    
    # plotting the coal related total emission changes 
    png('plot6.png')
    fip_names <- c(`24510`="Baltimore City", `06037`="Los Angeles County")
    qplot(year, Emissions, data=sources, geom="line") + 
        aes(color=fips) + geom_point(aes(color = fips)) + 
        labs(title="Total emissions from motor vehicles") + 
        labs(x="year", y="Total emission") + 
        facet_grid(. ~ fips, labeller = as_labeller(fip_names))
    dev.off()
}

# plotMotorVehicleSources()