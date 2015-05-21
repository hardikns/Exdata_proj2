require(ggplot2)
require(dplyr)

# This function fetches the dataset zip file, extracts it in the wd 

download_files <- function() {
    if (!file.exists("./dataset.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      dest="dataset.zip", method="curl")
    }
    unzip("./dataset.zip")   
}

read_data <- function() {
    if (!file.exists("./summarySCC_PM25.rds")) download_files()
    NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
    SCC <- tbl_df(readRDS("Source_Classification_Code.rds"))
    return(list(NEI=NEI, SCC=SCC))
}

plot6 <- function (pm25_data) {
    
    SCC_motor <- pm25_data$SCC %>% filter(grepl("Mobile", EI.Sector)) %>% filter(!grepl("Aircraft|Non-Road|Marine", EI.Sector))
    
    get_county_name <- function(fips) {
        if (fips == "24510") return("Baltimore City")
        if (fips == "06037") return("Los Angeles County")
        return("")
    }
    
    NEI_motor <- pm25_data$NEI %>% filter(SCC %in% as.character(SCC_motor$SCC)) %>%     # Only Motor Vehiles
        filter(fips %in% c("24510","06037")) %>%                                               # Only Baltimore City and  Los Angeles County
        select(Emissions, year, fips) %>%
        group_by(fips, year) %>% 
        summarise(TotalEmissions=sum(Emissions)) %>%
        mutate(county=get_county_name(fips))
    
    g <- ggplot(NEI_motor, aes(x=year, y=TotalEmissions)) +
        geom_point() +
        facet_grid(. ~ county) + geom_smooth(method="loess") +
        xlab("Years") + ylab("Yearly Emissions (tons)") +
        ggtitle("PM2.5 Emissions from Motor Vehicles")
    
    print(g)
}

if (!"pm25_data" %in% ls())
    pm25_data <- read_data()

png("plot6.png", width=800, height=480, units="px")
plot6(pm25_data)
dev.off()