library(data.table)
library(reshape2)

# Read data
raw <- fread(
  file.path(getwd(), "raw/Real_Estate_Sales_By_Town_for_2011__2012__2013__2014.csv")
)

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
backfill <- expand.grid(
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = unique(data$`Town/County`),
  `Price Range` = prices, 
  `Year` = unique(data$`Year`), stringsAsFactors =F
)
setkey(data, `Town/County`, `Price Range`, `Year`)
#data <- data[backfill]
data <- merge(data, backfill, all.y = T)
############################################################################################################
data[
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

backfill <- expand.grid(
  # CT values are aggregated later, including it here would just mess that up
  `Town/County` = fips[Town != "Connecticut"]$Town,
  `Price Range` = prices, 
  `Year` = unique(data$`Year`), stringsAsFactors =F
)

setkey(data, `Town/County`, `Price Range`, `Year`)

data <- data[backfill]
# cleanup
remove(prices, backfill)

# Add fips
setnames(fips, "Town", "Town/County")
setkey(fips, `Town/County`)
data <- fips[data]
remove(fips)

#take a copy of dataset
# Aggregate to one value - state level
state <- copy(data)
state <- state[
  ,
  list(
    `Town/County` = "Connecticut",
    FIPS = "09",
    Value = sum(Value, na.rm = T)
  ),
  by = `Price Range`
  ]

# take a copy of dataset
county <- copy(data)

county[, `:=`(
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
setkey(county, FIPS)
fips$FIPS <- as.character(fips$FIPS)
county <- fips[county, on="FIPS"]
remove(fips)

# Aggregate to county level
county <- county[
  ,
  list(
    Value = sum(Value, na.rm = T)
  ),
  by = list(`Town/County`, FIPS, `Price Range`)
  ]

data <- rbind(data, state, county, fill=T)
remove(state, county)

# Add extra information
data[, `:=`(
  #Year = 2013,
  `Measure Type` = "Number",
  Variable = "Number of Home Sales"
)]

# Reorder columns
setcolorder(data, c("Town/County", "FIPS", "Year", "Price Range", "Measure Type", "Variable", "Value"))

# turn price range into a factor for ordering purposes

data[, `:=`(
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

data <- data[order(`Town/County`, `Price Range`)]

# Write to File
write.table(
  data,
  file.path(getwd(), "data", "distribution-of-home-sales.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)