##################################################################
#
# Processing Script for Distribution of Home Sales
# Created by Jenna Daly
# On 02/07/2017
#
##################################################################

# Read data
rawDatasetFileName <- file.path(getwd(), "raw", "Real_Estate_Sales_By_Town_for_2011__2012__2013__2014.csv")
raw <- read.csv(rawDatasetFileName, header=T, stringsAsFactors=F)
raw_full <- read.csv(rawDatasetFileName, header=T, stringsAsFactors=F)

setDT(raw)
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
setDT(raw)
setkey(raw, `Town/County`)

# For debugging purposes set year to 2011 and compare to existing town profiles
data_2011 <- raw[
  Year == 2011,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
]

data_2012 <- raw[
  Year == 2012,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

data_2013 <- raw[
  Year == 2013,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

data_2014 <- raw[
  Year == 2014,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

setDT(data_2011)
setkey(data_2011, `Town/County`, `Price Range`)

setDT(data_2012)
setkey(data_2012, `Town/County`, `Price Range`)

setDT(data_2013)
setkey(data_2013, `Town/County`, `Price Range`)

setDT(data_2014)
setkey(data_2014, `Town/County`, `Price Range`)

# We now have data, need backfilling
# backfill prices first - missing price categories are coded as zero
prices <- c(
  "Less than $100,000",
  "$100,000 to $199,999",
  "$200,000 to $299,999",
  "$300,000 to $399,999",
  "$400,000 and Over"
)

backfill_price_2011 <- expand.grid(
  `Town/County` = unique(data_2011$`Town/County`),
  `Price Range` = prices
)

backfill_price_2012 <- expand.grid(
  `Town/County` = unique(data_2012$`Town/County`),
  `Price Range` = prices
)

backfill_price_2013 <- expand.grid(
  `Town/County` = unique(data_2013$`Town/County`),
  `Price Range` = prices
)

backfill_price_2014 <- expand.grid(
  `Town/County` = unique(data_2014$`Town/County`),
  `Price Range` = prices
)


setDT(backfill_price_2011)
setkey(backfill_price_2011, `Town/County`, `Price Range`)

setDT(backfill_price_2012)
setkey(backfill_price_2012, `Town/County`, `Price Range`)

setDT(backfill_price_2013)
setkey(backfill_price_2013, `Town/County`, `Price Range`)

setDT(backfill_price_2014)
setkey(backfill_price_2014, `Town/County`, `Price Range`)

setDT(data_2011)
setkey(data_2011, `Town/County`, `Price Range`)

setDT(data_2012)
setkey(data_2012, `Town/County`, `Price Range`)

setDT(data_2013)
setkey(data_2013, `Town/County`, `Price Range`)

setDT(data_2014)
setkey(data_2014, `Town/County`, `Price Range`)

data_with_price_2011 <- merge(data_2011, backfill_price_2011, all.y = T) #NAs present
data_with_price_2012 <- merge(data_2012, backfill_price_2012, all.y = T) #NAs present

data_with_price_2013 <- merge(data_2013, backfill_price_2013, all.y = T) #NAs present
data_with_price_2014 <- merge(data_2014, backfill_price_2014, all.y = T) #NAs present

#any(is.na(data_with_price_2011))

#turns NAs into 0s (so they are truly 0)
setDT(data_with_price_2011)
data_with_price_2011[
  is.na(Value),
  Value := 0  
]

setDT(data_with_price_2012)
data_with_price_2012[
  is.na(Value),
  Value := 0  
  ]

setDT(data_with_price_2013)
data_with_price_2013[
  is.na(Value),
  Value := 0  
  ]

setDT(data_with_price_2014)
data_with_price_2014[
  is.na(Value),
  Value := 0  
  ]


# Now backfill missing towns (still with complete levels of price range) -> these are coded as NA
fipsDatasetFileName <- file.path(getwd(), "raw", "town_fips.csv")
fips<- read.csv(fipsDatasetFileName, header=T, stringsAsFactors=F)

setDT(fips)
setkey(fips, `Town`)

backfill_towns <- expand.grid(
  `Town/County` = fips[Town != "Connecticut"]$Town,
  `Price Range` = prices
)

setDT(backfill_towns)
setkey(backfill_towns, `Town/County`, `Price Range`)
setDT(data_with_price_2011)
setkey(data_with_price_2011, `Town/County`, `Price Range`)
setDT(data_with_price_2012)
setkey(data_with_price_2012, `Town/County`, `Price Range`)
setDT(data_with_price_2013)
setkey(data_with_price_2013, `Town/County`, `Price Range`)
setDT(data_with_price_2014)
setkey(data_with_price_2014, `Town/County`, `Price Range`)

data_with_price_and_towns_2011 <- merge(data_with_price_2011, backfill_towns, all.y = T) #NAs present
data_with_price_and_towns_2012 <- merge(data_with_price_2012, backfill_towns, all.y = T) #NAs present
data_with_price_and_towns_2013 <- merge(data_with_price_2013, backfill_towns, all.y = T) #NAs present
data_with_price_and_towns_2014 <- merge(data_with_price_2014, backfill_towns, all.y = T) #NAs present

#any(is.na(data_with_price_and_towns_2011))
#======================================================================================================================

# Add fips
setnames(fips, "Town", "Town/County")
setkey(fips, `Town/County`)

setDT(data_with_price_and_towns_2011)
setDT(data_with_price_and_towns_2012)
setDT(data_with_price_and_towns_2013)
setDT(data_with_price_and_towns_2014)

data_with_price_and_towns_and_fips_2011 <- merge(fips, data_with_price_and_towns_2011) #NAs still present
data_with_price_and_towns_and_fips_2012 <- merge(fips, data_with_price_and_towns_2012) #NAs still present
data_with_price_and_towns_and_fips_2013 <- merge(fips, data_with_price_and_towns_2013) #NAs still present
data_with_price_and_towns_and_fips_2014 <- merge(fips, data_with_price_and_towns_2014) #NAs still present

#remove(fips)

#take a copy of dataset
# Aggregate to one value - state level
state_2011 <- copy(data_with_price_and_towns_and_fips_2011)
state_2012 <- copy(data_with_price_and_towns_and_fips_2012)
state_2013 <- copy(data_with_price_and_towns_and_fips_2013)
state_2014 <- copy(data_with_price_and_towns_and_fips_2014)

state_2011 <- state_2011[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T)
  ),
  by = `Price Range`
  ]

state_2012 <- state_2012[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T)
  ),
  by = `Price Range`
  ]

state_2013 <- state_2013[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T)
  ),
  by = `Price Range`
  ]

state_2014 <- state_2014[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T)
  ),
  by = `Price Range`
  ]


# take a copy of dataset
county_2011 <- copy(data_with_price_and_towns_and_fips_2011)
county_2012 <- copy(data_with_price_and_towns_and_fips_2012)
county_2013 <- copy(data_with_price_and_towns_and_fips_2013)
county_2014 <- copy(data_with_price_and_towns_and_fips_2014)

county_2011[, `:=`(
  FIPS = substr(FIPS, 1, 4),
  `Town/County` = NULL
)]
county_2012[, `:=`(
  FIPS = substr(FIPS, 1, 4),
  `Town/County` = NULL
)]

county_2013[, `:=`(
  FIPS = substr(FIPS, 1, 4),
  `Town/County` = NULL
)]
county_2014[, `:=`(
  FIPS = substr(FIPS, 1, 4),
  `Town/County` = NULL
)]

# Add county information from crosswalk
fipsDatasetFileName <- file.path(getwd(), "raw", "county_fips.csv")
fips<- read.csv(fipsDatasetFileName, header=T, stringsAsFactors=F)

setDT(fips)
setnames(fips, "County", "Town/County")
setkey(fips, FIPS)
setkey(county_2011, FIPS)
setkey(county_2012, FIPS)
setkey(county_2013, FIPS)
setkey(county_2014, FIPS)

fips$FIPS <- as.character(fips$FIPS)
#NAs still present
county_2011 <- merge.data.frame(fips, county_2011, on="FIPS")
county_2012 <- merge.data.frame(fips, county_2012, on="FIPS")
county_2013 <- merge.data.frame(fips, county_2013, on="FIPS")
county_2014 <- merge.data.frame(fips, county_2014, on="FIPS")

#remove(fips)

setDT(county_2011)
setDT(county_2012)
setDT(county_2013)
setDT(county_2014)

# Aggregate to county level
county_2011 <- county_2011[
  ,
  list(
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Town/County`, FIPS, `Price Range`)
  ]

county_2012 <- county_2012[
  ,
  list(
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Town/County`, FIPS, `Price Range`)
  ]

county_2013 <- county_2013[
  ,
  list(
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Town/County`, FIPS, `Price Range`)
  ]

county_2014 <- county_2014[
  ,
  list(
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Town/County`, FIPS, `Price Range`)
  ]

#NAs still present
data_all_2011 <- rbind(data_with_price_and_towns_and_fips_2011, state_2011, county_2011)
data_all_2012 <- rbind(data_with_price_and_towns_and_fips_2012, state_2012, county_2012)
data_all_2013 <- rbind(data_with_price_and_towns_and_fips_2013, state_2013, county_2013)
data_all_2014 <- rbind(data_with_price_and_towns_and_fips_2014, state_2014, county_2014)

#remove(state, county)

data_all_2011$Year = 2011
data_all_2012$Year = 2012
data_all_2013$Year = 2013
data_all_2014$Year = 2014

#merge all years together
data_all <- rbind(data_all_2011,
                  data_all_2012,
                  data_all_2013,
                  data_all_2014)

# Add extra information
data_all[, `:=`(
  `Measure Type` = "Number",
  Variable = "Number of Home Sales"
)]

#reorder columns
setcolorder(data_all, c("Town/County", "FIPS", "Year", "Price Range", "Measure Type", "Variable", "Value"))

#convert price range to factor for ordering purposes
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

setDT(data_all)
#order combined dataset
data_all <- data_all[order(`Town/County`, `FIPS` , `Price Range`)]

#any(is.na(data_all))

# Write to File
write.table(
  data_all,
  file.path(getwd(), "data", "distribution-of-home-sales_2011-2014.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)
