# R version of:
# https://github.com/jnvickery/EbookPackageAnalysis/blob/master/CharlestonEbooksPackageAnalysis.ipynb


# import stuff
library(tidyverse)
library(readxl)
library(magrittr)

# get the list elsevier ebook package codes
# change file path as needed
codes <- read_excel("H:/presentations and conferences/Charleston/Charleston 2018/presentation/packageNames.xlsx")
head(codes)


# limit to just the packages that have the FirstUsageDate betweeen 2012 and 2016 
# and where the PackageCode isn't missing
codes <- subset(codes, PackageCode != " " &  (FirstUsageDate >= 2012 & FirstUsageDate <= 2016),
                  select=c(PackageCode, FirstUsageDate))
head(codes)


# get the ebook title lists from "title lists" folder

# set directory for title lists
mydir <- file.path("H:", "presentations and conferences", "Charleston", "Charleston 2018", "presentation", "title lists")
mydir

# initialize dataframe for the title lists
titles <- data.frame()

# loop over the title list files and read them into the "titles" dataframe
for (yr in 2007:2017){
  fpath <- paste0(mydir,"/ebook",yr,".xlsx")
  df <- read_excel(fpath)
  df <- subset(df, select = c("Package name",	"Package ID",	"ISBN",	"Book Title",	"Year") )
  colnames(df) <- make.names(colnames(df), unique = FALSE, allow_ = TRUE)
  titles <- rbind(titles,df)
}  

# subset title lists where code only in list of purchased codes
titles <- subset(titles, titles$Package.ID %in% codes$PackageCode)

# reformat ISBN to remove dashes
titles$ISBN <- gsub("-", "", titles$ISBN)

# add FirstUsageDate from the pkgcodes dataframe
titles <- left_join(titles,codes, by=c("Package.ID" = "PackageCode"))


# get Elsevier usage data from standardized reports

# # re-assign mydir variable for base directory for usage reports
mydir <- file.path("H:", "presentations and conferences", "Charleston", "Charleston 2018", "presentation", "usage reports")
mydir

# minimum and maximum FirstUsageDate from the titles dataframe
minyr = min(titles$FirstUsageDate)
maxyr = max(titles$FirstUsageDate)

# loop through years to get Elsevier usage
# Usage must be from the year greater than or equal to the first usage year of the package
for (yr in minyr:maxyr){
  fpath <- paste0(mydir,"/",yr,"_Elsevier.xlsx")
  use <- read_excel(fpath, sheet="Sheet1", range=cell_cols("F:H"))
  use <- subset(use, select = c("TOTAL", "ISBN"))
  names(use)[names(use)=="TOTAL"] <- paste0("use",yr)
  use$ISBN <- gsub("-", "", use$ISBN)
  use$UsageYear = yr
  # replace_na(paste0("use",yr), 0)
  titles <- left_join(titles,use, by="ISBN")
  # set useYYYY to 0 if UsageYear is less than FirstUsageDate
  titles[[paste0("use",yr)]][titles$UsageYear < titles$FirstUsageDate ] <- 0
  # drop UsageYear
  titles <- subset(titles, select = -c(UsageYear) )
}  

# sum across "USE" columns to create TotalUse column
x = colnames( select(titles, starts_with("use")) )
titles$TotalUse <- rowSums(titles[,x],na.rm = TRUE) 

# group TotalUse into categories of 0, 1, 2-4, 5-50 and > 50
titles[,"UseCategory"] <- NA
attach(titles)
titles$UseCategory[TotalUse > 50] <- "> 50"
titles$UseCategory[TotalUse >= 5 & TotalUse <= 50] <- "5-50"
titles$UseCategory[TotalUse >= 2 & TotalUse <= 4] <- "2-4"
titles$UseCategory[TotalUse == 1] <- "1"
titles$UseCategory[TotalUse == 0] <- "0"
detach(titles)

# check frequencies of UseCategory
count(titles,UseCategory)

pfreq = function(...) {
  group_by(...) %>%
    summarise(N=n()) %>%
    mutate(Percent=paste0(round(100 * N/sum(N), 1), "%")) %>%
    arrange(desc(N))
}
pfreq(titles,UseCategory)

# reformat package names
count(titles,Package.name)

titles[,"Package"] <- NA
titles$Package <- gsub("(eBook - |)\\d*", "", titles$Package.name)

# summarize for counts and pct of package and usecategory
Summary <- titles %>%
  group_by(Package, Package.ID, UseCategory) %>%
  summarise(N = n()) %>%
  mutate(Pct=paste0(round(100 * N/sum(N), 1), "%"))


# graphs

# recode "> 50" so that the order is correct
Summary$UseCategory <- gsub("> 50", "50+", Summary$UseCategory)

ggplot(Summary, aes(x = Package.ID, y = N,fill = UseCategory)) + 
  geom_bar(position = position_fill(reverse=TRUE), stat = "identity")

for (var in unique(Summary$Package)) {
  print( ggplot(subset(Summary, Package==var), aes(x = Package.ID, y = N,fill = UseCategory)) +
           geom_bar(position = position_fill(reverse=TRUE), stat = "identity") +
           ggtitle(paste0("Usage of Elsevier ebook packages","\n",var), subtitle = NULL) +
           labs(fill = "Number of uses") +
           ylab("Proportion") +
           xlab("") +
           theme_classic() ) 
}