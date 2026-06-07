## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  out.width = "70%"
)

library(ggplot2)

## ----world_data---------------------------------------------------------------
# import real-world dataset
world_2000 <- fiodata::world_2000

# Examine the structure
world_2000$n_countries
world_2000$n_sectors
nrow(world_2000$intermediate_transactions)
ncol(world_2000$intermediate_transactions)

# Show the countries and sectors
world_2000$countries
world_2000$sectors

## ----examine_transactions-----------------------------------------------------
# Show a small subset of transactions (first 6 countries x sectors)
knitr::kable(
  world_2000$intermediate_transactions[1:6, 1:6],
  digits = 0,
  caption = "Sample of Intermediate Transactions (millions of dollars)"
)

# Show total production for first few country-sectors
knitr::kable(
  t(world_2000$total_production[1, 1:6]),
  digits = 0,
  caption = "Sample of Total Production (millions of dollars)"
)

## ----compute_multipliers------------------------------------------------------
world_2000$compute_multiregional_multipliers()

# Show first 10 rows of multipliers for readability
knitr::kable(
  world_2000$multiregional_multipliers[1:10, 1:10],
  digits = 4,
  caption = "Multi-Regional Multipliers (first 10 country-sectors)"
)

## ----interpret_multipliers----------------------------------------------------
# Find Brazil agriculture row
bra_agr_index <- which(grepl("BRA.*Agriculture", world_2000$multiregional_multipliers$shock_label))[1]
bra_agr <- world_2000$multiregional_multipliers[bra_agr_index, ]

# Show key multiplier components
multiplier_cols <- c("shock_label", "intra_regional_multiplier", "spillover_multiplier", "total_multiplier", "multiplier_to_USA")
available_cols <- intersect(multiplier_cols, names(bra_agr))
knitr::kable(bra_agr[, available_cols],
  digits = 4,
  caption = "Example: Brazil Agriculture Multipliers"
)

## ----spillover_matrix---------------------------------------------------------
spillover_matrix <- world_2000$get_spillover_matrix()

## ----us_effects---------------------------------------------------------------
# Effects of a shock to US Electrical sector
electrical_effects <- spillover_matrix[grepl("Manufacturing", rownames(spillover_matrix)), "USA_Electrical and optical equipment"]
knitr::kable(
  electrical_effects,
  digits = 10,
  caption = "Effects of a unit shock to US Electrical sector"
)

## ----net_spillover------------------------------------------------------------
net_spillover <- world_2000$get_net_spillover_matrix()
knitr::kable(net_spillover, digits = 4, caption = "Net Spillover Effects Matrix")

## ----key_sectors--------------------------------------------------------------
world_2000$compute_key_sectors()

key_sectors_table <- world_2000$key_sectors[, c(
  "country", "sector_name",
  "power_dispersion", "sensitivity_dispersion", "key_sectors"
)]
knitr::kable(head(key_sectors_table), digits = 4, caption = "Key Sectors Analysis")

## ----identify_key_sectors-----------------------------------------------------
knitr::kable(
  subset(key_sectors_table, key_sectors == "Key Sector"),
  digits = 4,
  caption = "World Key Sectors"
)

## ----interdependence----------------------------------------------------------
interdependence <- world_2000$get_regional_interdependence()
knitr::kable(interdependence, digits = 4) |> head()

## -----------------------------------------------------------------------------
ggplot(interdependence, aes(x = reorder(country, self_reliance), y = self_reliance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Self-reliance index by country",
    x = "Country",
    y = "Average intra-regional multiplier"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
ggplot(interdependence, aes(x = reorder(country, total_spillover_out), y = total_spillover_out)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total spillover to other countries by country",
    x = "Country",
    y = "Sum of induced multipliers abroad"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
interdependence |>
  subset(!country %in% "ROW") |>
  ggplot(aes(x = reorder(country, total_spillover_in), y = total_spillover_in)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total spillover absorbed by country",
    x = "Country",
    y = "Sum of induced multipliers at home"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
interdependence |>
  subset(!country %in% "ROW") |>
  ggplot(aes(x = reorder(country, spillover_balance), y = spillover_balance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Spillover balance by country",
    x = "Country",
    y = "Spillover out - Spillover in"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
ggplot(interdependence, aes(x = reorder(country, spillover_export_share), y = spillover_export_share)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Spillover export share by country",
    x = "Country",
    y = "total_spillover_out / (out + in)"
  ) +
  theme_minimal()

## ----bilateral_trade----------------------------------------------------------
# Get bilateral trade flows
trade_flow1 <- world_2000$get_bilateral_trade("BRA", "USA")
knitr::kable(trade_flow1[1:10, 1:5],
  digits = 2,
  caption = paste("Trade flows from", "BRA", "to", "USA", "(first 10 buying sectors, first 5 supplying sectors)")
)

trade_flow2 <- world_2000$get_bilateral_trade("USA", "BRA")
knitr::kable(trade_flow2[1:10, 1:5],
  digits = 2,
  caption = paste("Trade flows from", "USA", "to", "BRA", "(first 10 buying sectors, first 5 supplying sectors)")
)

## ----extract_country----------------------------------------------------------
# Extract domestic economy for deutschland in the dataset
deutsch_iom <- world_2000$extract_country("DEU")

## ----country_tables-----------------------------------------------------------
# Show a subset of the domestic intermediate transactions
knitr::kable(deutsch_iom$intermediate_transactions[1:8, 1:8],
  digits = 0,
  caption = paste("Deutschland Domestic Intermediate Transactions (first 8x8 sectors, millions USD)")
)

# Show total production
knitr::kable(t(deutsch_iom$total_production[1, 1:12]),
  digits = 0,
  caption = paste("Deutschland Total Production (first 12 sectors, millions USD)")
)

## ----country_analysis---------------------------------------------------------
deutsch_iom$compute_tech_coeff()$compute_leontief_inverse()$compute_multiplier_output()

knitr::kable(head(deutsch_iom$multiplier_output, 12),
  digits = 4,
  caption = paste("Deutschland Domestic Output Multipliers (first 12 sectors)")
)

