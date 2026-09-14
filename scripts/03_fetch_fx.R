library(jsonlite)

# Config

out_dir <- "data/clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_year <- 1976
end_year   <- 2024

# Fetch exchange rates
# PA.NUS.FCRF: official exchange rate, LCU per US$, period average
# (World Bank / IMF International Financial Statistics). Norway and Chile
# in one call since the API accepts semicolon-separated country codes.

url <- sprintf(
  "https://api.worldbank.org/v2/country/NOR;CHL/indicator/PA.NUS.FCRF?format=json&date=%d:%d&per_page=1000",
  start_year, end_year
)

resp <- fromJSON(url)
fx_raw <- resp[[2]]

fx <- data.frame(
  country = fx_raw$country$value,
  year    = as.integer(fx_raw$date),
  lcu_per_usd = as.numeric(fx_raw$value)
)

cat("Rows fetched:", nrow(fx), "\n")
cat("Countries:", paste(unique(fx$country), collapse = ", "), "\n")
cat("Missing values:", sum(is.na(fx$lcu_per_usd)), "\n")

# Reshape to one row per year, one column per country

fx_wide <- reshape(
  fx, timevar = "country", idvar = "year", direction = "wide"
)
names(fx_wide) <- sub("lcu_per_usd\\.", "", names(fx_wide))
names(fx_wide)[names(fx_wide) == "Norway"] <- "nok_per_usd"
names(fx_wide)[names(fx_wide) == "Chile"]  <- "clp_per_usd"
fx_wide <- fx_wide[order(fx_wide$year), ]

# Cross rate: NOK per CLP, since no direct NOK/CLP market exists
# NOK/USD divided by CLP/USD cancels the USD leg

fx_wide$nok_per_clp <- fx_wide$nok_per_usd / fx_wide$clp_per_usd

write.csv(fx_wide, file.path(out_dir, "fx_nok_usd_clp.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nSaved:", file.path(out_dir, "fx_nok_usd_clp.csv"), "\n")
print(head(fx_wide, 5))
print(tail(fx_wide, 5))

# Merge into the salmon price series: express each country's own
# export price in its own local currency, alongside the existing USD figure

salmon <- read.csv(file.path(out_dir, "salmon_trade_country_year.csv"),
                    stringsAsFactors = FALSE)

salmon <- merge(salmon, fx_wide[, c("year", "nok_per_usd", "clp_per_usd")],
                 by = "year", all.x = TRUE)

salmon$price_local <- ifelse(
  salmon$country == "Norway",
  salmon$price_usd_per_kg * salmon$nok_per_usd,
  ifelse(salmon$country == "Chile",
         salmon$price_usd_per_kg * salmon$clp_per_usd,
         NA)
)

salmon <- salmon[order(salmon$country, salmon$year), ]

write.csv(salmon, file.path(out_dir, "salmon_trade_with_fx.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nSaved:", file.path(out_dir, "salmon_trade_with_fx.csv"), "\n")
print(tail(salmon[salmon$country == "Norway", ], 5))
print(tail(salmon[salmon$country == "Chile", ], 5))
