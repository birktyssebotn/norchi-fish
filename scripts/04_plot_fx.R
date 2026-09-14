library(ggplot2)

# Config

fig_dir <- "output/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

fx     <- read.csv("data/clean/fx_nok_usd_clp.csv", stringsAsFactors = FALSE)
salmon <- read.csv("data/clean/salmon_trade_with_fx.csv", stringsAsFactors = FALSE)

theme_thesis <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# CLP has depreciated far more than NOK against the dollar over this period,
# so the two are shown on separate panels rather than one shared axis.

p_nok <- ggplot(fx, aes(x = year, y = nok_per_usd)) +
  geom_line(linewidth = 1, color = "#00205B") +
  labs(title = "NOK per USD", x = NULL, y = "NOK / USD") +
  theme_thesis

p_clp <- ggplot(fx, aes(x = year, y = clp_per_usd)) +
  geom_line(linewidth = 1, color = "#C8102E") +
  labs(title = "CLP per USD", x = NULL, y = "CLP / USD") +
  theme_thesis

ggsave(file.path(fig_dir, "fx_nok_usd.png"), p_nok, width = 8, height = 5, dpi = 300)
ggsave(file.path(fig_dir, "fx_clp_usd.png"), p_clp, width = 8, height = 5, dpi = 300)

# NOK/CLP cross rate: how many kroner one peso buys.
# Log scale, since the peso's depreciation against the krone spans
# roughly two orders of magnitude over the sample period.

p_cross <- ggplot(fx, aes(x = year, y = nok_per_clp)) +
  geom_line(linewidth = 1, color = "#5B3A29") +
  scale_y_log10() +
  labs(title = "NOK per CLP (log scale)", x = NULL, y = "NOK / CLP") +
  theme_thesis

ggsave(file.path(fig_dir, "fx_nok_per_clp.png"), p_cross, width = 8, height = 5, dpi = 300)

# Does the USD price convergence survive in local currency terms?
# Each country's price is plotted in its own local currency; the two
# lines are not on the same scale (kroner vs pesos) and are not meant
# to be compared level-to-level, only for shape/trend.

price_local <- salmon[is.finite(salmon$price_local), ]

p_local <- ggplot(price_local, aes(x = year, y = price_local, color = country)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Chile" = "#C8102E", "Norway" = "#00205B")) +
  facet_wrap(~ country, scales = "free_y") +
  labs(title = "Salmon export price in local currency",
       x = NULL, y = "Local currency / kg", color = NULL) +
  theme_thesis +
  theme(legend.position = "none")

ggsave(file.path(fig_dir, "price_local_currency.png"), p_local,
       width = 9, height = 5, dpi = 300)

cat("Saved FX figures to", fig_dir, "\n")
