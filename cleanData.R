##Download files from Social Security Administration via data.gov----
#https://catalog.data.gov/dataset/baby-names-from-social-security-card-applications-national-level-data

##Load packages----
library(dplyr)

##Combine individual annual files from SSA----
files <- list.files(ignore.case = T, 
                    path = "/Users/OldJess/Dropbox/r stuff (home)/names/namesThru2016", 
                    pattern = "*.txt") #list my files
DF <- NULL #create dummy df 
for (n in files) {
  dat <- read.csv(n, 
                  header = F, 
                  sep = ",", 
                  colClasses = "character")
  dat$file <- unlist(strsplit(n, 
                              split = ".", 
                              fixed = T))[1]
  DF <- rbind(DF, dat)
}
library(plyr)
DF <- rename(DF, c("V1" = "Name", 
                   "V2" = "Sex",
                   "V3" = "Count",
                   "file" = "Year"))
DF$Year <- as.numeric(substr(DF$Year, 
                             4, 
                             nchar(DF$Year)))

##To clean data--
DF$name <- tolower(DF$Name)
DF$Gender <- as.factor(DF$Gender)
colnames(DF)[2] <- "Sex"
write.table(DF, "data/NationalNames2016.txt", 
            sep = ",")

##Create another df/text file with percent female/male-----
base <- read.table("data/NationalNames2016.txt", header = T, stringsAsFactors = FALSE,  sep = ",", row.names = NULL, na.strings = c("NA","","#MULTIVALUE"))

##delete 'rownames' column:
base <- subset(base, select = -c(row.names))

##make df wide instead of long
base <- spread(base, Sex, Count)

## add variables for percent female & percent male
base$TotalCount <- ifelse(is.na(base$F), base$M, ifelse(is.na(base$M), base$F, ifelse(!is.na(base$F), base$F + base$M, NA)))
base$PctF <- round(ifelse(is.na(base$M), 1, (base$F / base$TotalCount)), 4) * 100
base$PctM <- round(ifelse(is.na(base$F), 1, (base$M / base$TotalCount)), 4) * 100
colnames(base)[colnames(base) == "PctF"] <- "PercentFeminine"
colnames(base)[colnames(base) == "PctM"] <- "PercentMasculine"
colnames(base)[colnames(base) == "TotalCount"] <- "TotalBabies"

write.table(base, "data/NationalNamesPct.txt", sep = ",")
