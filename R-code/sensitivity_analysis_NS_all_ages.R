# =====================================================================
# Counterfactual sensitivity analysis: population error required to
# explain away the North-South mortality difference, England 1981-2024
#
# Construct (Nafilyan): counterfactual northern population needed for
# the North to match the South's mortality rate, expressed as a
# difference from the observed mid-year estimate, as % of the estimate.
# Refinement (per VN's own suggestion): computed within five-year
# age-sex strata and summed, rather than on crude aggregate rates.
#
# Data: tot_death.xlsx, github.com/civicdatacoop/NorthSouthMortality
# (mid-year estimates as published before the 29 July 2026 revision;
# that revision altered national totals by <0.1%).
# =====================================================================

# ---- packages (installed automatically if missing) -------------------

pkgs <- c("readxl", "dplyr", "tidyr", "readr", "ggplot2", "scales")

new <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(new)) install.packages(new)

invisible(lapply(pkgs, library, character.only = TRUE))


# ---- configuration ---------------------------------------------------

data_file <- "tot_death.xlsx"

age_band <- "all"

age_specific <- TRUE

if (!file.exists(data_file))
  stop(
    "Cannot find '", data_file, "' in ", getwd(),
    " - setwd() to its folder, or set data_file to its full path."
  )


# ---- load ------------------------------------------------------------

raw <- read_excel(data_file)

dat <- raw


# ---- counterfactual --------------------------------------------------

if (age_specific) {
  
  needed_by_year <- dat %>%
    group_by(Year, AgeGroup, Sex, Area) %>%
    summarise(
      Death = sum(Death),
      Population = sum(Population),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = Area,
      values_from = c(Death, Population)
    ) %>%
    mutate(
      needed = Death_North / (Death_South / Population_South)
    ) %>%
    group_by(Year) %>%
    summarise(
      population_n_same_rate_south = sum(needed),
      .groups = "drop"
    )
}


out <- dat %>%
  group_by(Year, Area) %>%
  summarise(
    Death = sum(Death),
    Population = sum(Population),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Area,
    values_from = c(Death, Population)
  ) %>%
  transmute(
    year = Year,
    deaths_n = Death_North,
    population_n = Population_North,
    deaths_s = Death_South,
    population_s = Population_South,
    drate_n = deaths_n / population_n,
    drate_s = deaths_s / population_s
  )


out <- if (age_specific) {
  left_join(
    out,
    needed_by_year,
    by = c("year" = "Year")
  )
} else {
  mutate(
    out,
    population_n_same_rate_south = deaths_n / drate_s
  )
}


out <- out %>%
  mutate(
    absolute_difference =
      population_n - population_n_same_rate_south,
    
    relative_difference =
      absolute_difference / population_n
  ) %>%
  arrange(year)


# ---- self-check against independently verified values ---------------

chk <- function(y)
  round(out$relative_difference[out$year == y], 4)


if (age_band == "all" && age_specific) {
  
  stopifnot(
    chk(1981) == -0.0986,
    chk(2019) == -0.2777,
    chk(2021) == -0.3201,
    chk(2024) == -0.2962
  )
  
  message(
    "Self-check passed: all-age age-sex-specific series ",
    "matches verified values."
  )
}


# ---- outputs ---------------------------------------------------------

output_suffix <- "all_ages_age_sex_specific"

csv_name <- paste0(
  "sa_",
  output_suffix,
  ".csv"
)

write_csv(out, csv_name)


p <- ggplot(
  out,
  aes(year, relative_difference)
) +
  geom_line(
    colour = "black",
    linewidth = 0.4
  ) +
  scale_y_continuous(
    "Relative difference (%)",
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    "Year",
    breaks = seq(1980, 2020, 10)
  ) +
  labs(
    title = paste0(
      "Difference in population size needed to explain away",
      " the N/S difference\n in mortality rate"
    ),
    subtitle = "All ages, age-sex specific"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      size = 12,
      face = "bold"
    )
  )


print(p)


png_name <- paste0(
  "sa_",
  output_suffix,
  ".png"
)

ggsave(
  png_name,
  p,
  width = 7.5,
  height = 4.6,
  dpi = 150
)


message(
  "Written to ",
  getwd(),
  ": ",
  csv_name,
  " and ",
  png_name
)


print(
  filter(
    out,
    year %in% c(1981, 2019, 2020, 2021, 2024)
  ) %>%
    select(year, relative_difference),
  n = 5
)
