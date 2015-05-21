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

