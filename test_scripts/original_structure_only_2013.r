library(data.table)
library(reshape2)

# Read data
raw <- fread(
  file.path(getwd(), "raw/Real_Estate_Sales_By_Town_for_2011__2012__2013__2014.csv")
)

#no NAs
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
#no NAs
data_all <- raw[
  #Year == 2013,
  ,
  list(Value = .N),
  by = list(`Year`, `Town/County`, `Price Range`)
  ]

data_2013 <- raw[
  Year == 2013,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

setDT(data_2013)
setDT(data_all)
setkey(data_all, `Year`, `Town/County`, `Price Range`)
setkey(data_2013,  `Town/County`, `Price Range`)

#cleanup
remove(raw)

# We now have data, need backfilling
# backfill prices first - missing price categories are coded as zero
prices <- c(
  "Less than $100,000",
  "$100,000 to $199,999",
  "$200,000 to $299,999",
  "$300,000 to $399,999",
  "$400,000 and Over"
)
year <- c(
  "2011", 
  "2012", 
  "2013", 
  "2014"
)
backfill_all <- expand.grid(
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = unique(data_all$`Town/County`),
  `Price Range` = prices,
  `Year` = year
  #`Year` = unique(data_all$`Year`), stringsAsFactors = T
)

backfill_2013 <- expand.grid(
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = unique(data_2013$`Town/County`),
  `Price Range` = prices
  )
setDT(backfill_all)
setDT(backfill_2013)
setkey(backfill_all, `Year`, `Town/County`, `Price Range`)
setkey(backfill_2013 , `Town/County`, `Price Range`)
data_all <- merge.data.frame(data_all, backfill_all, all.y=T) #first appearance of NAs check
data_2013 <- merge.data.frame(data_2013, backfill_2013, all.y=T) #first appearance of NAs check
#data_2013 <- data_2013[backfill_2013] #first appearance of NAs check
#===========================================================================================================ALL GOOD^^^^^
#turns NAs into 0s
setDT(data_all)
data_all[
  is.na(Value),
  Value := 0
  ]

setDT(data_2013)
data_2013[
  is.na(Value),
  Value := 0
  ]

# Now backfill missing towns (still with complete levels of price range) -> these are coded as NA
# fips <- fread(
#   file.path(getOption("common_path"), "Geography", "town_fips.csv")
# )

fips <- fread(
  file.path(getwd(), "raw/town_fips.csv")
)
year <- c(
  "2011", 
  "2012", 
  "2013", 
  "2014"
)
setDT(fips)
setkey(fips, `Town`)
#no NAs
backfill_all <- expand.grid( #just towns
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = fips[Town != "Connecticut"]$Town,
  #'Year' = year,
  `Price Range` = prices
  #`Year` = unique(data_all$`Year`), stringsAsFactors = T
)

backfill_2013 <- expand.grid(
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = fips[Town != "Connecticut"]$Town,
  `Price Range` = prices
  )

setkey(data_all, `Year`, `Town/County`, `Price Range`)
setkey(data_2013, `Town/County`, `Price Range`)
setDT(backfill_all)
setDT(backfill_2013)
setkey(backfill_all, `Town/County`, `Price Range`)
setkey(backfill_2013, `Town/County`, `Price Range`)

#NAs are back
data_all$Year = as.character(data_all$Year)
#data_all <- merge(data_all, backfill_all, all.y=T, by=.EACHI) #no


#merge(X, Y, by=c('id', 'ID'))

data_all <- merge.data.frame(data_all, backfill_all, 
#by.data_all='Year', by.backfill_all='Year',
by.data_all='Town/County', by.backfill_all='Town/County',
by.data_all='Price Range', by.backfill_all='Price Range', all.y = T)

#`Year`, `Town/County`, `Price Range`

#data_all <- data_all[backfill_all, on="Town/County"]
#data_2013<- data_2013[backfill_2013, on="Town/County"] #yes
data_2013 <- merge(data_2013, backfill_2013, all.y=T, by=.EACHI) #yes, using merge on the first time works 
# cleanup
remove(prices, backfill)





# Add fips
setnames(fips, "Town", "Town/County")
setkey(fips, `Town/County`)
data_2013 <- fips[data_2013] #NAs still present
data_all <- merge(fips, data_all) #NAs still present

remove(fips)

#take a copy of dataset
# Aggregate to one value - state level
state <- copy(data_2013)
state_all <- copy(data_all)
state_all <- state_all[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T),
    'Year' = year
  ),
  by = `Price Range`
  ]

# take a copy of dataset
county <- copy(data_2013)
county_all <- copy(data_all)
#NAs present
county_all[, `:=`(
  FIPS = substr(FIPS, 2, 5),
  `Town/County` = NULL
)]

# Add county information from crosswalk
# fips <- fread(
#   file.path(getOption("common_path"), "Geography", "county_fips.csv")
# )

fips <- fread(
  file.path(getwd(), "raw/county_fips.csv")
)
setnames(fips, "County", "Town/County")
setkey(fips, FIPS)
setkey(county_all, FIPS)
fips$FIPS <- as.character(fips$FIPS)
#NAs still present
county_all <- fips[county_all, on="FIPS"]
remove(fips)

# Aggregate to county level
county_all <- county_all[
  ,
  list(
    Value = sum(Value, na.rm = T),
    'Year' = year
  ),
  by = list(`Town/County`, FIPS, `Price Range`)
  ]
#NAs still present
data_2013 <- rbind(data_2013, state, county)
data_all <- rbind(data_all, state_all, county_all)
remove(state, county)

# Add extra information
data_2013[, `:=`(
  `Year` = year,
  `Measure Type` = "Number",
  Variable = "Number of Home Sales"
)]

data_all[, `:=`(
  `Measure Type` = "Number",
  Variable = "Number of Home Sales"
)]

# Reorder columns
# setcolorder(data, c(1, 2, 5, 3 ,6, 7, 4))
setcolorder(data_2013, c("Town/County", "FIPS", "Year", "Price Range", "Measure Type", "Variable", "Value"))
setcolorder(data_all, c("Town/County", "FIPS", "Year", "Price Range", "Measure Type", "Variable", "Value"))


# turn price range into a factor for ordering purposes

data_2013[, `:=`(
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

data_all[, `:=`(
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


data_2013 <- data_2013[order(`Town/County`, `Price Range`)]
data_all <- data_all[order(`Town/County`, `Price Range`)]


# Write to File
write.table(
  data,
  file.path(getwd(), "data", "distribution-of-home-sales_2013.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)
