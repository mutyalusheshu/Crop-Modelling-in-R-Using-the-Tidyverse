# Crop Modelling in R Using the Tidyverse: A Practical Guide

*February 2026*

---

Feeding a growing global population under the shadow of climate change is one of the defining agricultural challenges of our time. Crop models — mathematical representations of how crops grow, develop, and yield — give researchers and agronomists a powerful lens for exploring "what if" scenarios: What happens to maize yields if temperatures rise by 2°C? How does shifting a planting date affect wheat productivity?

R, with its rich ecosystem of packages, has become a go-to environment for agricultural data analysis. And when paired with the **tidyverse** — a coherent collection of R packages built around a common data philosophy — crop modelling workflows become cleaner, more reproducible, and easier to communicate. This post walks you through the key concepts and code patterns to get you started.

---

## What Is Crop Modelling?

Crop models simulate the physiological and phenological processes of crops — from germination to harvest — using inputs like weather data, soil properties, and management practices. Common models include DSSAT, APSIM, and STICS (Brisson et al., 2003). While these tools have their own interfaces, R is increasingly used as the analytical layer around them: preparing inputs, running sensitivity analyses, calibrating parameters, and visualising outputs (Wallach et al., 2019).

---

## Setting Up Your Environment

```r
# Install and load the tidyverse
install.packages("tidyverse")
library(tidyverse)

# Additional packages useful for crop data
install.packages(c("lubridate", "broom", "patchwork"))
library(lubridate)
library(broom)
library(patchwork)
```

The tidyverse bundles packages like `dplyr`, `tidyr`, `ggplot2`, `purrr`, and `readr` — all of which you will use extensively in a crop modelling pipeline (Wickham et al., 2019).

---

## Step 1: Loading and Tidying Weather Data

Crop models are hungry for weather data — typically daily minimum and maximum temperature, precipitation, and solar radiation. Let's simulate a season's worth of data and tidy it up.

```r
set.seed(42)

weather <- tibble(
  date        = seq(as.Date("2023-04-01"), as.Date("2023-09-30"), by = "day"),
  tmax_C      = 20 + 10 * sin(seq(0, pi, length.out = 183)) + rnorm(183, 0, 2),
  tmin_C      = 10 + 8  * sin(seq(0, pi, length.out = 183)) + rnorm(183, 0, 1.5),
  precip_mm   = rgamma(183, shape = 0.5, rate = 0.1),
  srad_MJm2   = 15 + 8  * sin(seq(0, pi, length.out = 183)) + rnorm(183, 0, 1)
)

# Add derived variables using mutate
weather <- weather %>%
  mutate(
    tmean_C  = (tmax_C + tmin_C) / 2,
    month    = month(date, label = TRUE),
    doy      = yday(date)
  )

glimpse(weather)
```

The `%>%` pipe operator — now also available as `|>` natively in R 4.1+ — lets you chain transformations in a readable, left-to-right style, a core principle of tidyverse programming (Wickham, 2014).

---

## Step 2: Computing Growing Degree Days (GDD)

Growing Degree Days are the workhorse metric of phenological modelling. They accumulate heat above a base temperature and drive crop development (McMaster & Wilhelm, 1997).

```r
BASE_TEMP <- 10  # Base temperature for maize (°C)

weather <- weather %>%
  mutate(
    gdd_daily = pmax(tmean_C - BASE_TEMP, 0)
  ) %>%
  mutate(
    gdd_cumulative = cumsum(gdd_daily)
  )

# Identify key phenological stages
# Approximate GDD thresholds for maize
stages <- tibble(
  stage = c("Emergence", "Silking", "Physiological Maturity"),
  gdd_threshold = c(60, 800, 1400)
)

stage_dates <- stages %>%
  mutate(
    date = map_dbl(gdd_threshold, ~ {
      idx <- which(weather$gdd_cumulative >= .x)[1]
      as.numeric(weather$date[idx])
    }) %>% as.Date(origin = "1970-01-01")
  )

print(stage_dates)
```

Here `pmax()` prevents negative GDD contributions, and `map_dbl()` from the `purrr` package applies a function across multiple threshold values — a tidyverse-friendly alternative to loops (Henry & Wickham, 2023).

---

## Step 3: Modelling Crop Yield Response

A simplified radiation-use efficiency (RUE) model estimates biomass accumulation by multiplying intercepted radiation by a conversion efficiency factor (Monteith, 1977). Here we implement a basic version across multiple planting date scenarios.

```r
# Define a simple RUE-based biomass model
simulate_biomass <- function(weather_df, rue = 1.5, lai_max = 4) {
  weather_df %>%
    mutate(
      # Fraction of radiation intercepted (Beer's law approximation)
      # Assume LAI rises linearly then plateaus
      lai    = pmin(lai_max * gdd_cumulative / 800, lai_max),
      fi     = 1 - exp(-0.5 * lai),
      # Daily biomass increment (g/m²)
      bio_inc = rue * srad_MJm2 * fi
    ) %>%
    summarise(
      total_biomass_gm2 = sum(bio_inc, na.rm = TRUE),
      gdd_total         = max(gdd_cumulative)
    )
}

# Run scenarios across different planting dates using purrr::map
planting_dates <- seq(as.Date("2023-04-01"), as.Date("2023-05-15"), by = "7 days")

results <- tibble(planting_date = planting_dates) %>%
  mutate(
    sim = map(planting_date, function(pd) {
      weather %>%
        filter(date >= pd) %>%
        mutate(
          gdd_daily      = pmax(tmean_C - BASE_TEMP, 0),
          gdd_cumulative = cumsum(gdd_daily)
        ) %>%
        simulate_biomass()
    })
  ) %>%
  unnest(sim)

print(results)
```

Using `map()` with `unnest()` is one of the most powerful tidyverse patterns for iterative simulation — keeping all scenarios in a single, tidy data frame (Wickham & Grolemund, 2017).

---

## Step 4: Visualising Results with ggplot2

Good science communication starts with good plots. The `ggplot2` package, built on the Grammar of Graphics (Wilkinson, 2005), makes publication-quality figures achievable with relatively little code.

```r
# Plot 1: Cumulative GDD over the season
p1 <- ggplot(weather, aes(x = date, y = gdd_cumulative)) +
  geom_line(colour = "#E67E22", linewidth = 1.2) +
  geom_vline(
    data = stage_dates,
    aes(xintercept = date, linetype = stage),
    colour = "steelblue", linewidth = 0.8
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title    = "Cumulative Growing Degree Days (Maize Season 2023)",
    x        = "Date",
    y        = "Cumulative GDD (°C·day)",
    linetype = "Phenological Stage"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Plot 2: Biomass vs planting date
p2 <- ggplot(results, aes(x = planting_date, y = total_biomass_gm2)) +
  geom_col(fill = "#27AE60", alpha = 0.85) +
  geom_smooth(method = "loess", colour = "#2C3E50", se = FALSE, linewidth = 1) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
  labs(
    title = "Simulated Biomass by Planting Date",
    x     = "Planting Date",
    y     = "Total Biomass (g/m²)"
  ) +
  theme_minimal(base_size = 13)

# Combine plots using patchwork
p1 / p2 + plot_annotation(
  title   = "Crop Model Outputs — Maize, 2023",
  caption = "Simulated data. RUE model after Monteith (1977)."
)
```

The `patchwork` package provides an intuitive syntax for combining multiple ggplots — `/` stacks vertically, `|` places side by side (Pedersen, 2022).

---

## Step 5: Sensitivity Analysis with tidyr and broom

How sensitive is simulated biomass to the assumed Radiation Use Efficiency? A simple sensitivity sweep, made tidy:

```r
rue_values <- seq(0.8, 2.5, by = 0.1)

sensitivity <- tibble(rue = rue_values) %>%
  mutate(
    biomass = map_dbl(rue, ~ {
      weather %>%
        simulate_biomass(rue = .x) %>%
        pull(total_biomass_gm2)
    })
  )

ggplot(sensitivity, aes(x = rue, y = biomass)) +
  geom_line(colour = "#8E44AD", linewidth = 1.3) +
  geom_point(colour = "#8E44AD", size = 2.5) +
  labs(
    title = "Sensitivity of Biomass to Radiation Use Efficiency",
    x     = "RUE (g/MJ intercepted PAR)",
    y     = "Total Biomass (g/m²)"
  ) +
  theme_minimal(base_size = 13)

# Fit a linear model and extract tidy coefficients with broom
lm_fit <- lm(biomass ~ rue, data = sensitivity)
tidy(lm_fit)
glance(lm_fit)
```

The `broom` package converts model objects into tidy data frames, making it easy to pass results downstream — into tables, plots, or further calculations (Robinson et al., 2023).

---

## Step 6: Handling Multiple Sites with Grouped Operations

Real crop modelling often involves multiple locations. `dplyr`'s `group_by()` and `group_modify()` let you scale simulations elegantly.

```r
# Simulate multiple sites with different temperature offsets
sites <- tibble(
  site      = c("Site_A", "Site_B", "Site_C"),
  temp_adj  = c(0, +1.5, -1.0)   # Temperature adjustment (°C)
)

multi_site_results <- sites %>%
  mutate(
    sim = map2(site, temp_adj, function(s, adj) {
      weather %>%
        mutate(
          tmean_C        = tmean_C + adj,
          gdd_daily      = pmax(tmean_C - BASE_TEMP, 0),
          gdd_cumulative = cumsum(gdd_daily)
        ) %>%
        simulate_biomass() %>%
        mutate(site = s)
    })
  ) %>%
  select(sim) %>%
  unnest(sim)

print(multi_site_results)
```

`map2()` iterates over two vectors simultaneously — here the site name and temperature adjustment — keeping code concise and vectorised (Henry & Wickham, 2023).

---

## Best Practices for Reproducible Crop Modelling in R

Good crop modelling is as much about process as it is about equations. A few principles to keep your work reproducible and shareable:

**Use projects and here::here().** The `here` package resolves file paths relative to your project root, avoiding hard-coded paths that break on other machines.

**Version your data.** Raw weather and soil data should be treated as immutable. Write processed outputs to a separate directory and never overwrite raw inputs.

**Document with Quarto or R Markdown.** Narrative and code together — this is the gold standard for scientific communication (Allaire et al., 2022).

**Pin your package versions.** Use `renv` to snapshot your package library, ensuring collaborators run identical code environments (Ushey & Wickham, 2023).

---

## Conclusion

The tidyverse transforms R into an expressive, readable environment for crop modelling workflows. From ingesting raw weather data and computing GDD, to running multi-scenario simulations with `purrr::map()` and visualising outputs with `ggplot2`, these tools let you focus on the agronomic questions rather than the mechanics of data wrangling.

As climate uncertainty intensifies, the ability to rapidly prototype, iterate, and communicate crop model results will only grow in importance. The tidyverse gives you the scaffolding — now it's time to build.

---

## References

Allaire, J. J., Xie, Y., Dervieux, C., McPherson, J., Luraschi, J., Ushey, K., ... & Iannone, R. (2022). *rmarkdown: Dynamic Documents for R*. R package version 2.20. https://rmarkdown.rstudio.com

Brisson, N., Gary, C., Justes, E., Roche, R., Mary, B., Ripoche, D., ... & Sinoquet, H. (2003). An overview of the crop model STICS. *European Journal of Agronomy*, 18(3–4), 309–332. https://doi.org/10.1016/S1161-0301(02)00110-7

Henry, L., & Wickham, H. (2023). *purrr: Functional Programming Tools*. R package version 1.0.1. https://purrr.tidyverse.org

McMaster, G. S., & Wilhelm, W. W. (1997). Growing degree-days: One equation, two interpretations. *Agricultural and Forest Meteorology*, 87(4), 291–300. https://doi.org/10.1016/S0168-1923(97)00027-0

Monteith, J. L. (1977). Climate and the efficiency of crop production in Britain. *Philosophical Transactions of the Royal Society B*, 281(980), 277–294. https://doi.org/10.1098/rstb.1977.0140

Pedersen, T. L. (2022). *patchwork: The Composer of Plots*. R package version 1.1.2. https://patchwork.data-imaginist.com

Robinson, D., Hayes, A., & Couch, S. (2023). *broom: Convert Statistical Objects into Tidy Tibbles*. R package version 1.0.4. https://broom.tidymodels.org

Ushey, K., & Wickham, H. (2023). *renv: Project Environments*. R package version 1.0.0. https://rstudio.github.io/renv

Wallach, D., Makowski, D., Jones, J. W., & Brun, F. (2019). *Working with Dynamic Crop Models: Methods, Tools, and Examples for Agriculture and Environment* (3rd ed.). Academic Press.

Wickham, H. (2014). Tidy Data. *Journal of Statistical Software*, 59(10), 1–23. https://doi.org/10.18637/jss.v059.i10

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., ... & Yutani, H. (2019). Welcome to the tidyverse. *Journal of Open Source Software*, 4(43), 1686. https://doi.org/10.21105/joss.01686

Wickham, H., & Grolemund, G. (2017). *R for Data Science: Import, Tidy, Transform, Visualize, and Model Data*. O'Reilly Media. https://r4ds.had.co.nz

Wilkinson, L. (2005). *The Grammar of Graphics* (2nd ed.). Springer.

---

*Thanks for reading. If you found this useful, consider sharing it with a colleague or hitting the like button below. Questions or corrections? Drop them in the comments — I'd love to hear from you.*
