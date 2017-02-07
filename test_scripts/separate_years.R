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


prices <- c(
  "Less than $100,000",
  "$100,000 to $199,999",
  "$200,000 to $299,999",
  "$300,000 to $399,999",
  "$400,000 and Over"
)

backfill_price <- expand.grid(
  `Town/County` = unique(raw$`Town/County`),
  `Price Range` = prices
)

setDT(backfill_price)
setkey(backfill_price, `Town/County`, `Price Range`)

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
#=======================================================2011==================================================================
# For debugging purposes set year to 2011 and compare to existing town profiles
data_2011 <- raw[
  Year == 2011,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
]
data_2011$Year = 2011

setDT(data_2011)
setkey(data_2011, `Town/County`, `Price Range`)

data_with_price_2011 <- merge(data_2011, backfill_price, all.y = T)
#any(is.na(data_with_price_2011))
data_with_price_2011$Year = 2011

#turns NAs into 0s (so they are truly 0)
setDT(data_with_price_2011)
data_with_price_2011[
  is.na(Value),
  Value := 0  
  ]

#data_with_price_and_towns_2011 <- merge.data.frame(data_with_price_2011, backfill_towns, all.y = T)
#any(is.na(data_with_price_2011))
#=======================================================2012=================================================================
data_2012 <- raw[
  Year == 2012,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

setDT(data_2012)
setkey(data_2012, `Town/County`, `Price Range`)

data_with_price_2012 <- merge(data_2012, backfill_price, all.y = T)
#any(is.na(data_with_price_2012))

#turns NAs into 0s (so they are truly 0)
setDT(data_with_price_2012)
data_with_price_2012[
  is.na(Value),
  Value := 0  
  ]

data_with_price_and_towns_2012 <- merge.data.frame(data_with_price_2012, backfill_towns, all.y = T)
#any(is.na(data_with_price_2012))
#=======================================================2013==================================================================
data_2013 <- raw[
  Year == 2013,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

setDT(data_2013)
setkey(data_2013, `Town/County`, `Price Range`)

data_with_price_2013 <- merge(data_2013, backfill_price, all.y = T)
#any(is.na(data_with_price_2013))

#turns NAs into 0s (so they are truly 0)
setDT(data_with_price_2013)
data_with_price_2013[
  is.na(Value),
  Value := 0  
  ]

data_with_price_and_towns_2013 <- merge.data.frame(data_with_price_2013, backfill_towns, all.y = T)
#any(is.na(data_with_price_and_towns_2013))
#=======================================================2014==================================================================
data_2014 <- raw[
  Year == 2014,
  list(Value = .N),
  by = list(`Town/County`, `Price Range`)
  ]

setDT(data_2014)
setkey(data_2014, `Town/County`, `Price Range`)

data_with_price_2014 <- merge(data_2014, backfill_price, all.y = T)
#any(is.na(data_with_price_2014))

#turns NAs into 0s (so they are truly 0)
setDT(data_with_price_2014)
data_with_price_2014[
  is.na(Value),
  Value := 0  
  ]

data_with_price_and_towns_2014 <- merge.data.frame(data_with_price_2014, backfill_towns, all.y = T)
#any(is.na(data_with_price_and_towns_2014))

data_all_years <- rbind(data_with_price_and_towns_2011, data_with_price_and_towns_2012, data_with_price_and_towns_2013, data_with_price_and_towns_2014)

