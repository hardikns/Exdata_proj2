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


plot2 <- function (pm25_data) {
    NEI_yearly <- pm25_data$NEI %>% filter(fips=="24510") %>% select(Emissions, year) %>% group_by(year) %>% summarise(TotalEmissions=sum(Emissions))
    plot(NEI_yearly$year, NEI_yearly$TotalEmissions, 
         type="l", 
         xlab="",
         ylab="Yearly Emissions (tons)",
         main="PM2.5 Emissions Trend for Baltimore City, Maryland")
}

if (!"pm25_data" %in% ls())
    pm25_data <- read_data()

png("plot2.png", width=480, height=480, units="px")
plot2(pm25_data)
dev.off()