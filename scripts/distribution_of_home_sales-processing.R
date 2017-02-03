library(stringr)
library(reshape2)
library(data.table)
library(plyr)

##################################################################
#
# Processing Script for Distribution of Home Sales
# Created by Jenna Daly
# On 02/02/2017
#
##################################################################

# Read data
rawDatasetFileName <- file.path(getwd(), "raw", "Real_Estate_Sales_By_Town_for_2011__2012__2013__2014.csv")
raw <- read.csv(rawDatasetFileName, header=T, stringsAsFactors=F)

# setDT(raw)
# setkey(raw, Name)
#categorize data
setDT(raw) #converts DF to DT for categorization
raw <- raw[
  is.na(NonUseCode) & (ResidentialUnits == 1 | is.na(ResidentialUnits)) & ResidentialType %in% c("Single Family", 1),
  list(
    `Town/County` = Name,
    Year = ListYear,
    Price = as.numeric(gsub("$", "", SalePrice, fixed = T))
  )
  ]

# Categorize price ranges by $100k increments
raw[, `:=`(
  `Price Range` = as.character(floor(Price/100000)),
  Price = NULL
)]

# Relable categories
raw[
  ,
  `Price Range` := switch(
    `Price Range`,
    `0` = "Less than $100,000",
    `1` = "$100,000 to $199,999",
    `2` = "$200,000 to $299,999",
    `3` = "$300,000 to $399,999",
    "$400,000 and Over"
  ),
  by = `Price Range`
  ]

# For debugging purposes set year to 2011 and compare to existing town profiles
data <- raw[
 #Year == 2013,
 ,
  list(Value = .N),
  by = list(`Year`, `Town/County`, `Price Range`)
  ]

#data <- copy(raw)
#setkey(data, Year)
#cleanup
#remove(raw)

# We now have data, need backfilling
# backfill prices first - missing price categories are coded as zero
prices <- c(
  "Less than $100,000",
  "$100,000 to $199,999",
  "$200,000 to $299,999",
  "$300,000 to $399,999",
  "$400,000 and Over"
)
backfill <- expand.grid(
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = unique(data$`Town/County`),
  `Price Range` = prices
)
setkey(data, `Town/County`, `Price Range`)
data <- merge(data, backfill)
data[
  is.na(Value),
  Value := 0
  ]

# Now backfill missing towns (still with complete levels of price range) -> these are coded as NA
townFipsDatasetFileName <- file.path(getwd(), "raw", "town_fips.csv")
fips <- read.csv(townFipsDatasetFileName, header=T, stringsAsFactors=F)

#fips <- fread(
#  #file.path(getOption("common_path"), "Geography", "town_fips.csv")
#  file.path(getwd(), "raw", "town_fips.csv")
#)

setDT(fips)
backfill <- expand.grid(
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = fips[Town != "Connecticut"]$Town,
  `Price Range` = prices
)

setkey(data, `Town/County`, `Price Range`)

data <- merge(data, backfill)
# cleanup
#remove(prices, backfill)

# Add fips
setnames(fips, "Town", "Town/County")
setkey(fips, `Town/County`)
data <- merge(fips, data)
#remove(fips)

#take a copy of dataset
# Aggregate to one value - state level
state <- copy(data)

#sum value of each price range by price range
state <- state[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Year`, `Price Range`)
  ]


# take a copy of dataset
county <- copy(data)
setDT(county)
#assign 5 digit FIPS code to each county
county[, `:=`(
  FIPS = substr(FIPS, 1, 4),
  `Town/County` = NULL
)]


# Add county information from crosswalk
countyFipsDatasetFileName <- file.path(getwd(), "raw", "county_fips.csv")
fips <- read.csv(countyFipsDatasetFileName, header=T, stringsAsFactors=F)

#fips <- fread(
#  #file.path(getOption("common_path"), "Geography", "county_fips.csv")
#  file.path(getwd(), "raw", "county_fips.csv"), stringsAsFastors=F
#)

setDT(fips)
setDT(county)
setnames(fips, "County", "Town/County")
setkey(fips, FIPS)
setkey(county, FIPS)

#fips <- fips[, (2) := lapply(.SD, as.character), .SDcols = 2]
fips$FIPS <- as.character(fips$FIPS)
#fips <- as.character(fips[,2])
#fips <- fips[, lapply(.SD, as.character), by=fips]
#fips <- fips[,fips:=NULL]
county <- merge(fips, county, by="FIPS")
# head(fips)
# head(county)
# sapply(fips, class)
# sapply(county, class)

#merge(fips, county)
#remove(fips)

# Aggregate to county level
county <- county[
  ,
  list(
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Year`, `Town/County`, `FIPS`, `Price Range`)
  ]

data <- rbind(data, state, county)
#remove(state, county)

# Add extra information
data2 <- data[, `:=`(
  `Measure Type` = "Number",
  Variable = "Number of Home Sales"
)]

# Reorder columns
#setcolorder(data2, c(1, 2, 5, 3 ,6, 7, 4))

setcolorder(data2, c("Town/County", "FIPS", "Year", "Price Range", "Measure Type", "Variable", "Value"))

# turn price range into a factor for ordering purposes
data2[, `:=`(
  `Price Range` = factor(
    `Price Range`,
    levels = c(
      "Less than $100,000",
      "$100,000 to $199,999",
      "$200,000 to $299,999",
      "$300,000 to $399,999",
      "$400,000 and Over"
    )
  )
)]

data2 <- data2[order( `Town/County`, `FIPS`, `Year`, `Price Range`)]
#setkey(data2, Value)

# Write to File
write.table(
  data2,
  file.path(getwd(), "data", "distribution-of-home-sales.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)
