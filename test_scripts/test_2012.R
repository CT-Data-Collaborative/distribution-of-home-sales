##################################################################
#
# Processing Script for Distribution of Home Sales
# Created by Jenna Daly
# On 02/07/2017
#
##################################################################

# Read data
raw_full <- fread(
  file.path(getwd(), "raw/Real_Estate_Sales_By_Town_for_2011__2012__2013__2014.csv")
)

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
setDT(raw)
setkey(raw, `Town/County`)

# For debugging purposes set year to 2012 and compare to existing town profiles
data_2012 <- raw[
  Year == 2012,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

setDT(data_2012)
setkey(data_2012, `Town/County`, `Price Range`)

# We now have data, need backfilling
# backfill prices first - missing price categories are coded as zero
prices <- c(
  "Less than $100,000",
  "$100,000 to $199,999",
  "$200,000 to $299,999",
  "$300,000 to $399,999",
  "$400,000 and Over"
)

backfill_price <- expand.grid(
  `Town/County` = unique(data_2012$`Town/County`),
  `Price Range` = prices
)

setDT(backfill_price)
setkey(backfill_price, `Town/County`, `Price Range`)
setDT(data_2012)
setkey(data_2012, `Town/County`, `Price Range`)

data_with_price_2012 <- merge(data_2012, backfill_price, all.y = T) #NAs present
#any(is.na(data_with_price_2012))

#turns NAs into 0s (so they are truly 0)
setDT(data_with_price_2012)
data_with_price_2012[
  is.na(Value),
  Value := 0  
  ]

# Now backfill missing towns (still with complete levels of price range) -> these are coded as NA
fips <- fread(
  file.path(getwd(), "raw/town_fips.csv")
)
setDT(fips)
setkey(fips, `Town`)

backfill_towns <- expand.grid(
  `Town/County` = fips[Town != "Connecticut"]$Town,
  `Price Range` = prices
)

setDT(backfill_towns)
setkey(backfill_towns, `Town/County`, `Price Range`)
setDT(data_with_price_2012)
setkey(data_with_price_2012, `Town/County`, `Price Range`)

data_with_price_and_towns_2012 <- merge.data.frame(data_with_price_2012, backfill_towns, all.y = T) #NAs present
#any(is.na(data_with_price_and_towns_2012))
#======================================================================================================================

# Add fips
setnames(fips, "Town", "Town/County")
setkey(fips, `Town/County`)
data_with_price_and_towns_and_fips_2012 <- merge(fips, data_with_price_and_towns_2012) #NAs still present

#remove(fips)

#take a copy of dataset
# Aggregate to one value - state level
state_2012 <- copy(data_with_price_and_towns_and_fips_2012)
state_2012 <- state_2012[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T)
  ),
  by = `Price Range`
  ]

# take a copy of dataset
county_2012 <- copy(data_with_price_and_towns_and_fips_2012)
county_2012[, `:=`(
  FIPS = substr(FIPS, 2, 5),
  `Town/County` = NULL
)]

# Add county information from crosswalk
fips <- fread(
  file.path(getwd(), "raw/county_fips.csv")
)
setnames(fips, "County", "Town/County")
setkey(fips, FIPS)
setkey(county_2012, FIPS)
fips$FIPS <- as.character(fips$FIPS)
#NAs still present
county_2012 <- merge.data.frame(fips, county_2012, on="FIPS")
#remove(fips)

setDT(county_2012)
# Aggregate to county level
county_2012 <- county_2012[
  ,
  list(
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Town/County`, FIPS, `Price Range`)
  ]
#NAs still present
data_all_2012 <- rbind(data_with_price_and_towns_and_fips_2012, state_2012, county_2012)
#remove(state, county)

#Add extra information
data_all_2012[, `:=`(
  `Measure Type` = "Number",
  Variable = "Number of Home Sales"
)]

data_all_2012$Year = 2012

# Reorder columns
setcolorder(data_all_2012, c("Town/County", "FIPS", "Year", "Price Range", "Measure Type", "Variable", "Value"))


# turn price range into a factor for ordering purposes
data_all_2012[, `:=`(
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


data_all_2012 <- data_all_2012[order(`Town/County`, `Price Range`)]
#any(is.na(data_all_2012))


#merge all files together


# Write to File
write.table(
  data_all,
  file.path(getwd(), "data", "distribution-of-home-sales_2012-2014.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)
