# -------------------------------------------
# Louisville Health Code Violations Study
#
# Date : September 10, 2015
# -------------------------------------------

## Load Packages
library(ggplot2)
library(ggthemes)
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(lubridate)
library(ggmap)
library(mapproj)

## Load data & Clean
scoresDF <- read.table(url('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=FoodServiceData.txt'), 
                       sep = '\t', quote = NULL, comment = '', header = TRUE) # Recorded scores for every restaurant/food processor
inspectionsDF <- read.csv('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=Health_Inspections.csv',
                          header = FALSE, stringsAsFactors = FALSE, colClasses = 'character') # Inspection attributes
establishmentsDF <- read.csv('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=Health_Establishments.csv',
                             header = TRUE)

inspectionsDF <- inspectionsDF[order(-xtfrm(inspectionsDF[,7])), ] # Re-order from newest to oldest

# Rename columns, because they are (for some reason) not included...thanks a lot
colnames(inspectionsDF) <- c('ID', 
                             'Inspection_ID', 
                             'Establishment_ID', 
                             'Request_ID', 
                             'EHS_Number',
                             'County_ID',
                             'Inspection_Date',
                             'Inspection_Type',
                             'Is_FollowUp',
                             'RF_Insp_ID',
                             'Blank',
                             'Grade',
                             'Score',
                             'Insp_Time_Hours',
                             'Insp_Time_Mins',
                             'Blank',
                             'Blank',
                             'Next_Insp_Date',
                             'Action_Code',
                             'Complaint_Resolved')

# Get rid of blanks
inspectionsDF <- inspectionsDF[, -which(grepl('Blank', names(inspectionsDF)))]

# Merge the two sets together
totes <- merge(scoresDF, inspectionsDF, 
               by.x = 'InspectionID', by.y = 'Inspection_ID')

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}

ggplot(subset(totes, Grade.x != ''), aes(x = Grade.x, y = Score.x)) +
  geom_point(alpha = 0.025) +
  stat_sum_single(median) +
  facet_wrap(~Zip, nrow = 6) +
  theme_fivethirtyeight()

ggplot(subset(totes, Grade.x != ''), aes(x = Grade.x, y = Score.x)) +
  ggtitle('Restaurant Inspection Scores in Louisville, by Zip') +
  geom_point(alpha = 0.025, size = 1.5) +
  stat_summary(fun.y = median, fun.ymin = min, fun.ymax = max,
             colour = "orange", size = 0.5, alpha = 0.75) +
  coord_flip() +
  facet_wrap(~Zip, nrow = 9) +
  theme_fivethirtyeight(base_size = 8) +
  theme(
    plot.background = element_rect(fill = 'white')
  )
# ggsave('Score-by-Zip.png', height = 6, width = 8, units = 'in', type = 'cairo-png')

# Find the names and merge against inspections
complaints <- subset(inspectionsDF, Inspection_Type == 'COMPLAINT')
named <- merge(establishmentsDF, complaints, by.x = 'EstablishmentID', by.y = 'Establishment_ID')

# Turn inspection dates into actual dates
named$Inspection_Date <-  as.Date(ymd(substr(named$Inspection_Date, 0, 10)))

# Create variable showing when each complaint was submitted
named$YearSubmitted <- as.factor(year(named$Inspection_Date))

# ----------------------- 
# Mapping Within Zipcodes
# ----------------------- 

## Grab plotting data
coords40219 <- data.frame('lat' = subset(named40219, grepl('FOOD', RCodeDesc))$latitude, 
                          'lon' = subset(named40219, grepl('FOOD', RCodeDesc))$longitude)

coords40219 <- subset(coords40219, lat > 0 & lon < 0)

# Import shape file
zip <- readOGR(dsn = "tl_2010_21_zcta510", layer = "tl_2010_21_zcta510")
zip@data$id <- rownames(zip@data)

# Convert polygons in zip to a data frame for plotting
zip.df <- fortify(zip)

# Join columns
zip.df <- join(zip.df, zip@data, by="id")
zip.df <- subset(zip.df, ZCTA5CE10 == '40219')

# Plot location of food-related complaints in 40219
jeffMall <- geocode('4801 Outer Loop, Louisville, KY 40219') # Grab Jefferson Mall geoinfo for comparison

ggplot() +
  geom_path(data = zip.df, aes(x=long, y=lat), size = 1, alpha = 0.25, color = 'Light Grey') +
  geom_point(data = coords40219, aes(x = lon, y = lat), alpha = 0.05, color = '#92151D') +
  geom_point(data = jeffMall, aes(x = lon, y = lat), color = 'Black', size = 3.25) +
  geom_text(data = jeffMall, aes(x = lon, y = lat, label = 'Jefferson Mall'), hjust = -0.0625, vjust = 1.025, size = 3) +
  coord_map()  +
  labs(title="Grade-A Share of Establishments\nLouisville Metro") +
  ggtitle(expression(atop(bold("Food Establishment Complaints"), atop("Zipcode 40219", "")))) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 18),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), panel.grid = element_blank()
        )

ggsave('40219-Food-Establishments.png', type = 'cairo-png')
