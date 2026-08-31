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
#
# Provenance: the originally circulated sa_25-49_all.csv was computed
# on ALL-AGES totals despite its label. Set age_band <- "all" and
# age_specific <- FALSE below to reproduce it exactly.
# =====================================================================

# ---- packages (installed automatically if missing) -------------------
pkgs <- c("readxl", "dplyr", "tidyr", "readr", "ggplot2", "scales")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- configuration ---------------------------------------------------
data_file    <- "tot_death.xlsx"   # path to the data file (edit if elsewhere,
                                   # e.g. the full path returned by file.choose())
age_band     <- "25-49"            # "25-49" (corrected) or "all" (original)
age_specific <- TRUE               # TRUE: stratum counterfactuals summed
                                   # FALSE: crude aggregate rates (original)

if (!file.exists(data_file))
  stop("Cannot find '", data_file, "' in ", getwd(),
       " - setwd() to its folder, or set data_file to its full path.")

bands_2549 <- c("25-29", "30-34", "35-39", "40-44", "45-49")

# ---- load ------------------------------------------------------------
raw <- read_excel(data_file)   # Area, Sex, AgeGroup, Year, Death, Cause, Population
dat <- raw %>%
  { if (age_band == "25-49") filter(., AgeGroup %in% bands_2549) else . }

# ---- counterfactual --------------------------------------------------
if (age_specific) {
  needed_by_year <- dat %>%
    group_by(Year, AgeGroup, Sex, Area) %>%
    summarise(Death = sum(Death), Population = sum(Population), .groups = "drop") %>%
    pivot_wider(names_from = Area, values_from = c(Death, Population)) %>%
    mutate(needed = Death_North / (Death_South / Population_South)) %>%
    group_by(Year) %>%
    summarise(population_n_same_rate_south = sum(needed), .groups = "drop")
}

out <- dat %>%
  group_by(Year, Area) %>%
  summarise(Death = sum(Death), Population = sum(Population), .groups = "drop") %>%
  pivot_wider(names_from = Area, values_from = c(Death, Population)) %>%
  transmute(year         = Year,
            deaths_n     = Death_North,
            population_n = Population_North,
            deaths_s     = Death_South,
            population_s = Population_South,
            drate_n      = deaths_n / population_n,
            drate_s      = deaths_s / population_s)

out <- if (age_specific) left_join(out, needed_by_year, by = c("year" = "Year")) else mutate(out, population_n_same_rate_south = deaths_n / drate_s)

out <- out %>%
  mutate(absolute_difference = population_n - population_n_same_rate_south,
         relative_difference = absolute_difference / population_n) %>%
  arrange(year)

# ---- self-check against independently verified values ---------------
chk <- function(y) round(out$relative_difference[out$year == y], 4)
if (age_band == "25-49" && age_specific) {
  stopifnot(chk(1981) == -0.0765, chk(2019) == -0.3922,
            chk(2021) == -0.4979, chk(2024) == -0.4343)
  message("Self-check passed: 25-49 age-specific series matches verified values.")
}
if (age_band == "all" && !age_specific) {
  stopifnot(chk(1981) == -0.0376, chk(2024) == -0.1891)
  message("Self-check passed: reproduces the originally circulated all-ages series.")
}

# ---- outputs ---------------------------------------------------------
csv_name <- sprintf("sa_%s_%s.csv", age_band,
                    if (age_specific) "age_specific" else "crude")
write_csv(out, csv_name)

p <- ggplot(out, aes(year, relative_difference)) +
  geom_line(colour = "black", linewidth = 0.4) +
  scale_y_continuous("Relative difference (%)",
                     labels = percent_format(accuracy = 1)) +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10)) +
  labs(title = paste0("Difference in population size needed to explain away",
                      " the N/S difference\n in mortality rate"),
       subtitle = if (age_band == "25-49" && age_specific)
                    "25 to 49, age-specific" else
                  if (age_band == "all") "All ages" else
                    paste(age_band, "crude")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"))

print(p)   # display in the R session (needed when the script is source()d)
ggsave(sub("csv$", "png", csv_name), p, width = 7.5, height = 4.6, dpi = 150)
message("Written to ", getwd(), ": ", csv_name, " and matching .png")

print(filter(out, year %in% c(1981, 2019, 2020, 2021, 2024)) %>%
        select(year, relative_difference), n = 5)
