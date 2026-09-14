# Norway vs Chile salmon export trade: FishStatJ quantity + value reconciliation
# Input:  data/raw/fishdata.csv, data/raw/fishdata_value.csv (FishStatJ exports)
# Output: data/clean/salmon_trade_long_by_commodity.csv
#         data/clean/salmon_trade_country_year.csv

# Config

raw_dir  <- "data/raw"
qty_file <- file.path(raw_dir, "fishdata.csv")
val_file <- file.path(raw_dir, "fishdata_value.csv")
out_dir  <- "data/clean"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Encoding
# FishStatJ exports are Windows-1252, not UTF-8. Convert before parsing.

convert_to_utf8 <- function(path) {
  utf8_path <- sub("\\.csv$", "_utf8.csv", path)
  con_in  <- file(path, open = "rb")
  raw     <- readBin(con_in, "raw", n = file.info(path)$size)
  close(con_in)
  txt <- iconv(rawToChar(raw), from = "CP1252", to = "UTF-8")
  writeLines(txt, utf8_path, useBytes = TRUE)
  utf8_path
}

qty_utf8 <- convert_to_utf8(qty_file)
val_utf8 <- convert_to_utf8(val_file)

# Read wide files
# Each year column is paired with an unnamed FAO flag column; header row
# is reconstructed manually. Footer rows (totals, citation) are dropped.

read_fishstat_raw <- function(path) {
  df <- read.csv(path, header = FALSE, skip = 1, stringsAsFactors = FALSE,
                  strip.white = TRUE)
  years <- 1976:2024
  stopifnot(ncol(df) == 5 + length(years) * 2)

  value_cols  <- paste0("y", years)
  flag_cols   <- paste0("y", years, "_flag")
  interleaved <- as.vector(rbind(value_cols, flag_cols))

  names(df) <- c("country", "commodity", "trade_flow", "unit_name", "unit",
                 interleaved)

  df[df$trade_flow == "Exports" & !is.na(df$trade_flow), ]
}

qty_wide <- read_fishstat_raw(qty_utf8)
val_wide <- read_fishstat_raw(val_utf8)

cat("Quantity file:", nrow(qty_wide), "commodity rows\n")
cat("Value file:   ", nrow(val_wide), "commodity rows\n")

# Wide to long

wide_to_long <- function(df, value_name) {
  years <- 1976:2024
  long <- reshape(
    df,
    varying   = list(paste0("y", years), paste0("y", years, "_flag")),
    v.names   = c(value_name, "flag"),
    timevar   = "year",
    times     = years,
    idvar     = c("country", "commodity", "trade_flow"),
    direction = "long"
  )
  rownames(long) <- NULL
  long[, c("country", "commodity", "trade_flow", "year", value_name, "flag")]
}

qty_long <- wide_to_long(qty_wide, "quantity_tonnes")
val_long <- wide_to_long(val_wide, "value_usd1000")

# Numeric cleanup
# "..." denotes not-available in FAO exports; coerce to NA before numeric.

clean_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("...", "")] <- NA
  suppressWarnings(as.numeric(x))
}

qty_long$quantity_tonnes <- clean_numeric(qty_long$quantity_tonnes)
val_long$value_usd1000   <- clean_numeric(val_long$value_usd1000)

# Merge quantity + value

merged <- merge(
  qty_long[, c("country", "commodity", "year", "quantity_tonnes", "flag")],
  val_long[, c("country", "commodity", "year", "value_usd1000", "flag")],
  by = c("country", "commodity", "year"),
  suffixes = c("_qty", "_val"),
  all = TRUE
)

cat("Merged rows:", nrow(merged), "\n")
cat("Quantity without matching value:",
    sum(!is.na(merged$quantity_tonnes) & is.na(merged$value_usd1000)), "\n")
cat("Value without matching quantity:",
    sum(is.na(merged$quantity_tonnes) & !is.na(merged$value_usd1000)), "\n")

# Country-year aggregate

agg <- aggregate(
  cbind(quantity_tonnes, value_usd1000) ~ country + year,
  data = merged,
  FUN = sum,
  na.rm = TRUE
)

# value_usd1000 and quantity_tonnes both carry a factor of 1000; it cancels.
agg$price_usd_per_kg <- agg$value_usd1000 / agg$quantity_tonnes

agg <- agg[order(agg$country, agg$year), ]

# Save

write.csv(merged, file.path(out_dir, "salmon_trade_long_by_commodity.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(agg, file.path(out_dir, "salmon_trade_country_year.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nSaved:\n")
cat(" -", file.path(out_dir, "salmon_trade_long_by_commodity.csv"), "\n")
cat(" -", file.path(out_dir, "salmon_trade_country_year.csv"), "\n")

cat("\nPreview:\n")
print(head(agg, 10))
print(tail(agg, 10))

