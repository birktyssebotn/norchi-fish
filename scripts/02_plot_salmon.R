library(ggplot2)

# Config

in_file  <- "data/clean/salmon_trade_country_year.csv"
fig_dir  <- "output/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

df <- read.csv(in_file, stringsAsFactors = FALSE)

theme_thesis <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# Volume: absolute levels

p_volume <- ggplot(df, aes(x = year, y = quantity_tonnes / 1000, color = country)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Chile" = "#C8102E", "Norway" = "#00205B")) +
  labs(title = "Salmon export volume (absolute), Norway vs Chile",
       x = NULL, y = "Thousand tonnes", color = NULL) +
  theme_thesis

ggsave(file.path(fig_dir, "volume_convergence.png"), p_volume,
       width = 8, height = 5, dpi = 300)

# Volume: relative convergence (share of combined Norway+Chile output)
# Absolute tonnage gap widens over time even as Chile's share rises;
# these are different claims and both are shown rather than picking one.

wide_qty <- reshape(
  df[, c("country", "year", "quantity_tonnes")],
  timevar = "country", idvar = "year", direction = "wide"
)
names(wide_qty) <- sub("quantity_tonnes\\.", "", names(wide_qty))
wide_qty <- wide_qty[wide_qty$Norway > 0 & wide_qty$Chile > 0, ]
wide_qty$chile_share <- wide_qty$Chile / (wide_qty$Chile + wide_qty$Norway)

p_share <- ggplot(wide_qty, aes(x = year, y = chile_share)) +
  geom_line(linewidth = 1, color = "#C8102E") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Chile's share of combined Norway + Chile export volume",
       x = NULL, y = "Chile share") +
  theme_thesis

ggsave(file.path(fig_dir, "volume_share.png"), p_share,
       width = 8, height = 5, dpi = 300)

# Price: convergence to near-parity
# Drop non-finite rows (zero-export years produce NaN/Inf, not real prices)

df_price <- df[is.finite(df$price_usd_per_kg), ]

p_price <- ggplot(df_price, aes(x = year, y = price_usd_per_kg, color = country)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Chile" = "#C8102E", "Norway" = "#00205B")) +
  labs(title = "Average salmon export unit value, Norway vs Chile",
       x = NULL, y = "USD / kg", color = NULL) +
  theme_thesis

ggsave(file.path(fig_dir, "price_convergence.png"), p_price,
       width = 8, height = 5, dpi = 300)

# Price gap: Norway minus Chile, by year
# Requires both countries present for that year

wide_price <- reshape(
  df_price[, c("country", "year", "price_usd_per_kg")],
  timevar = "country", idvar = "year", direction = "wide"
)
names(wide_price) <- sub("price_usd_per_kg\\.", "", names(wide_price))
wide_price <- wide_price[complete.cases(wide_price), ]
wide_price$gap <- wide_price$Norway - wide_price$Chile

p_gap <- ggplot(wide_price, aes(x = year, y = gap)) +
  geom_col(fill = "#00205B") +
  labs(title = "Norway-Chile average export unit-value gap",
       x = NULL, y = "USD / kg (Norway minus Chile)") +
  theme_thesis

ggsave(file.path(fig_dir, "price_gap.png"), p_gap,
       width = 8, height = 5, dpi = 300)

cat("Saved figures to", fig_dir, "\n")
