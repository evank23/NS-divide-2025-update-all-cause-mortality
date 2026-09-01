# =====================================================================
# North-South divide in English mortality, 1981-2024
# Analyses and figures, run from tot_death.xlsx
# (github.com/civicdatacoop/NorthSouthMortality: all-cause deaths and
#  mid-year population estimates by area, sex, five-year age group and year)
#
# Iain Buchan (buchan@liverpool.ac.uk) 1 Sep 2026
#
# Sections
#   1. Directly standardised mortality rates, under 75 and 25-49        -> Figure 1
#   2. Within-year age-sex-adjusted North-South IRRs, by age band       -> Results
#   3. Age-group-specific IRRs by year and sex                          -> Figure 2
#   4. Age-group-specific IRRs by five time periods and sex             -> Figure 3
#   5. Segmented (hinge) Poisson models 2010-24, by sex and age band    -> Table 1
#   5b. The same models re-estimated with negative binomial and
#       quasi-Poisson regression (overdispersion sensitivity)             -> appendix table S1
#   6. Crude rates by age band, sex and region                          -> Figure 4, appendix
#   7. Counterfactual population sensitivity analysis                   -> appendix
#   8. Demographics                                                     -> appendix
#
# Model notes
#   * Poisson models are fitted with glm(family = poisson) and an offset of
#     log(population), i.e. Stata's  poisson ..., exposure(population).
#   * A Poisson model with a single binary covariate and an exposure offset has the
#     closed form  b = log[(D_N/P_N)/(D_S/P_S)],  se(b) = sqrt(1/D_N + 1/D_S); the
#     age-group-specific IRRs (sections 3 and 4) use this form, which section 3
#     verifies against glm() for one cell. The same holds for cells pooled over years.
#   * DSMRs: for each sex, the standard is the England age distribution summed over
#     1981-2024 within the age band; the reported DSMR is the mean of the two
#     sex-specific standardised rates, with CI = DSMR +/- 1.96 * DSMR / sqrt(deaths).
#   * Segmented models:  deaths ~ north + age group + year + hinge + north:year +
#     north:hinge  with hinge = max(0, year - 2019), fitted by sex on 2010-24.
#   * Overdispersion sensitivity (5b): the segmented models are refitted with
#     MASS::glm.nb (negative binomial, theta by maximum likelihood; alpha = 1/theta)
#     and glm(family = quasipoisson) (Poisson estimates, SEs scaled by the
#     Pearson dispersion). Stata's nbreg stalls on these near-Poisson data.
#
# Outputs (written to ./ns_output/)
#   dsmr.csv, excess_by_year.csv, excess_by_age_year.csv, excess_by_age_period.csv,
#   table1.csv, table_s1.csv, crude_rates.csv, counterfactual.csv, demographics.csv,
#   fig1_dsmr, fig2_contour, fig3_age_period, fig4_crude - each as .png, .svg, .pdf
#   and, when the devEMF package is installed, .emf
#
# Needs: readxl, dplyr, tidyr, readr, ggplot2 (installed automatically if absent) and
#        MASS (shipped with R) for glm.nb;
#        optional: svglite (cleaner SVG), devEMF (EMF output) and ragg (PNG with system
#        fonts) - installed if possible
# =====================================================================

pkgs <- c("readxl", "dplyr", "tidyr", "readr", "ggplot2")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
for (opt in c("svglite", "devEMF", "ragg"))      # optional graphics devices (SVG, EMF, PNG with system fonts)
  if (!requireNamespace(opt, quietly = TRUE)) try(install.packages(opt), silent = TRUE)

# ---- configuration ---------------------------------------------------
data_file <- "tot_death.xlsx"     # set a full path if the file is elsewhere
outdir    <- "ns_output"
fig_font  <- "Arial"

if (!file.exists(data_file))
  stop("Cannot find '", data_file, "' in ", getwd(), " - setwd() to its folder or set data_file.")
dir.create(outdir, showWarnings = FALSE)

ages   <- c("<1","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39",
            "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
midage <- c(0.5, 3, seq(7.5, 87.5, by = 5))
bands  <- list(all = 0:18, under75 = 0:15, a2549 = 6:10, a0024 = 0:5)     # agecat codes
band_label <- c(all = "All ages", under75 = "Under 75", a2549 = "25-49", a0024 = "0-24")

# ---- data --------------------------------------------------------------
d <- read_excel(data_file) %>%
  select(Area, Sex, AgeGroup, Year, Death, Population) %>%
  mutate(agecat = match(AgeGroup, ages) - 1L,
         north  = as.integer(Area == "North"),
         male   = as.integer(Sex == "Male"))
stopifnot(!anyNA(d),
          nrow(d) == 19 * 2 * 2 * 44,
          !any(duplicated(d[, c("Area", "Sex", "agecat", "Year")])))

# closed-form single-covariate Poisson (see header)
stratum_irr <- function(dN, pN, dS, pS) {
  b  <- log((dN / pN) / (dS / pS)); se <- sqrt(1 / dN + 1 / dS)
  tibble(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
         excess = 100 * (exp(b) - 1),
         excess_lo = 100 * (exp(b) - 1.96 * exp(b) * se - 1),   # IRR +/- 1.96 * se(IRR), as plotted
         excess_hi = 100 * (exp(b) + 1.96 * exp(b) * se - 1))
}
fmt_irr <- function(fit, term, digits = 3) {
  b <- coef(fit)[term]; se <- sqrt(vcov(fit)[term, term])
  sprintf(paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"),
          exp(b), exp(b - 1.96 * se), exp(b + 1.96 * se))
}
fmt_p <- function(p) ifelse(p < 0.0001, "<0.0001", formatC(signif(p, 2), format = "fg", digits = 2))

# ---- 1. Directly standardised mortality rates -----------------------------
dsmr <- bind_rows(lapply(c("under75", "a2549"), function(b) {
  db  <- d %>% filter(agecat %in% bands[[b]])
  std <- db %>% group_by(male, agecat) %>% summarise(P = sum(Population), .groups = "drop") %>%
    group_by(male) %>% mutate(w = P / sum(P)) %>% ungroup() %>% select(male, agecat, w)
  db %>% left_join(std, by = c("male", "agecat")) %>%
    group_by(Year, Area, male) %>%
    summarise(smr_sex = sum(Death / Population * w) * 1e4, deaths = sum(Death), .groups = "drop") %>%
    group_by(Year, Area) %>%
    summarise(dsmr = mean(smr_sex), deaths = sum(deaths), .groups = "drop") %>%
    mutate(lo = dsmr - 1.96 * dsmr / sqrt(deaths), hi = dsmr + 1.96 * dsmr / sqrt(deaths),
           band = band_label[[b]])
}))
write_csv(dsmr, file.path(outdir, "dsmr.csv"))

# ---- 2. Within-year age-sex-adjusted IRRs ---------------------------------
ex_year <- bind_rows(lapply(names(bands), function(b) {
  bind_rows(lapply(1981:2024, function(yr) {
    bind_rows(lapply(c("Persons", "Female", "Male"), function(sx) {
      dd <- d %>% filter(agecat %in% bands[[b]], Year == yr)
      if (sx != "Persons") dd <- dd %>% filter(Sex == sx)
      f <- if (sx == "Persons")
        glm(Death ~ north + male + factor(agecat) + offset(log(Population)), family = poisson, data = dd)
      else
        glm(Death ~ north + factor(agecat) + offset(log(Population)), family = poisson, data = dd)
      b1 <- coef(f)["north"]; se <- sqrt(vcov(f)["north", "north"])
      tibble(band = band_label[[b]], Year = yr, sex = sx,
             excess = 100 * (exp(b1) - 1), lo = 100 * (exp(b1 - 1.96 * se) - 1), hi = 100 * (exp(b1 + 1.96 * se) - 1))
    }))
  }))
}))
write_csv(ex_year, file.path(outdir, "excess_by_year.csv"))

# ---- 3. Age-group-specific IRRs by year and sex -----------------------------
cells <- d %>% group_by(Year, agecat, Sex, Area) %>%
  summarise(D = sum(Death), P = sum(Population), .groups = "drop") %>%
  pivot_wider(names_from = Area, values_from = c(D, P))
persons <- d %>% group_by(Year, agecat, Area) %>%
  summarise(D = sum(Death), P = sum(Population), .groups = "drop") %>%
  pivot_wider(names_from = Area, values_from = c(D, P)) %>% mutate(Sex = "Persons")
ex_age_year <- bind_rows(cells, persons) %>%
  bind_cols(stratum_irr(.$D_North, .$P_North, .$D_South, .$P_South)) %>%
  mutate(AgeGroup = ages[agecat + 1]) %>%
  select(Year, Sex, agecat, AgeGroup, D_North, P_North, D_South, P_South, irr, lo, hi, excess, excess_lo, excess_hi) %>%
  arrange(Sex, Year, agecat)
write_csv(ex_age_year, file.path(outdir, "excess_by_age_year.csv"))
# closed form checked against glm() for one cell (males 35-39 in 2021)
chk <- d %>% filter(Sex == "Male", agecat == 8, Year == 2021)
g   <- glm(Death ~ north + offset(log(Population)), family = poisson, data = chk)
cf  <- ex_age_year %>% filter(Sex == "Male", agecat == 8, Year == 2021)
stopifnot(abs(exp(coef(g)["north"]) - cf$irr) < 1e-8)

# ---- 4. Age-group-specific IRRs by five periods and sex ---------------------
periods <- tibble(period = c("1981-89", "1990-99", "2000-09", "2010-19", "2020-24"),
                  from = c(1981, 1990, 2000, 2010, 2020), to = c(1989, 1999, 2009, 2019, 2024))
ex_age_period <- bind_rows(lapply(seq_len(nrow(periods)), function(i) {
  dp <- d %>% filter(Year >= periods$from[i], Year <= periods$to[i])
  s  <- dp %>% group_by(agecat, Sex, Area) %>% summarise(D = sum(Death), P = sum(Population), .groups = "drop")
  p  <- dp %>% group_by(agecat, Area) %>% summarise(D = sum(Death), P = sum(Population), .groups = "drop") %>% mutate(Sex = "Persons")
  bind_rows(s, p) %>% pivot_wider(names_from = Area, values_from = c(D, P)) %>%
    bind_cols(stratum_irr(.$D_North, .$P_North, .$D_South, .$P_South)) %>%
    mutate(period = periods$period[i], AgeGroup = ages[agecat + 1])
})) %>% select(period, Sex, agecat, AgeGroup, excess, excess_lo, excess_hi, irr, lo, hi) %>%
  arrange(Sex, period, agecat)
write_csv(ex_age_period, file.path(outdir, "excess_by_age_period.csv"))

# ---- 5. Segmented Poisson models 2010-24 -------------------------------------
table1 <- bind_rows(lapply(c("all", "under75", "a2549", "a0024"), function(b) {
  bind_rows(lapply(c("Female", "Male"), function(sx) {
    dd <- d %>% filter(agecat %in% bands[[b]], Sex == sx, Year >= 2010, Year <= 2024) %>%
      mutate(hinge = pmax(0, Year - 2019))
    f  <- glm(Death ~ north + factor(agecat) + Year + hinge + north:Year + north:hinge + offset(log(Population)),
              family = poisson, data = dd)
    V  <- vcov(f); cf <- coef(f)
    b5 <- cf["north:Year"]; b6 <- cf["north:hinge"]
    se_tot <- sqrt(V["north:Year", "north:Year"] + V["north:hinge", "north:hinge"] + 2 * V["north:Year", "north:hinge"])
    p5 <- summary(f)$coefficients["north:Year", 4]; p6 <- summary(f)$coefficients["north:hinge", 4]
    tibble(band = band_label[[b]], sex = sx,
           irr_2010_19 = fmt_irr(f, "north:Year"), p_2010_19 = fmt_p(p5),
           irr_add_2020_24 = fmt_irr(f, "north:hinge"), p_add_2020_24 = fmt_p(p6),
           irr_total_2020_24 = sprintf("%.3f (%.3f-%.3f)", exp(b5 + b6), exp(b5 + b6 - 1.96 * se_tot), exp(b5 + b6 + 1.96 * se_tot)),
           dispersion = round(sum(residuals(f, type = "pearson")^2) / f$df.residual, 2))
  }))
}))
write_csv(table1, file.path(outdir, "table1.csv"))

# ---- 5b. Overdispersion sensitivity: negative binomial and quasi-Poisson -------
# Same data, formula and terms as Table 1; theta/alpha and the Pearson dispersion are
# reported alongside the IRRs. glm.nb converges in one theta iteration for all eight
# models (alpha 0.002-0.005).
seg_formula <- Death ~ north + factor(agecat) + Year + hinge + north:Year + north:hinge + offset(log(Population))
irr_ci <- function(est, se, digits = 3)
  sprintf(paste0("%.", digits, "f (%.", digits, "f-%.", digits, "f)"), exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se))
table_s1 <- bind_rows(lapply(c("all", "under75", "a2549", "a0024"), function(b) {
  bind_rows(lapply(c("Female", "Male"), function(sx) {
    dd <- d %>% filter(agecat %in% bands[[b]], Sex == sx, Year >= 2010, Year <= 2024) %>%
      mutate(hinge = pmax(0, Year - 2019))
    fp <- glm(seg_formula, family = poisson, data = dd)
    fq <- glm(seg_formula, family = quasipoisson, data = dd)
    nb_warn <- character(0)
    fn <- withCallingHandlers(MASS::glm.nb(seg_formula, data = dd, control = glm.control(maxit = 200)),
                              warning = function(w) { nb_warn <<- c(nb_warn, conditionMessage(w)); invokeRestart("muffleWarning") })
    term_row <- function(fit, term) {
      cf <- summary(fit)$coefficients
      c(irr = irr_ci(cf[term, 1], cf[term, 2]), p = fmt_p(2 * pnorm(-abs(cf[term, 1] / cf[term, 2]))))
    }
    out <- tibble(band = band_label[[b]], sex = sx)
    for (m in list(c("poisson", "fp"), c("negbin", "fn"), c("quasipoisson", "fq"))) {
      fit <- get(m[2])
      r1 <- term_row(fit, "north:Year"); r2 <- term_row(fit, "north:hinge")
      out[[paste0(m[1], "_irr_2010_19")]]     <- r1[["irr"]]; out[[paste0(m[1], "_p_2010_19")]]     <- r1[["p"]]
      out[[paste0(m[1], "_irr_add_2020_24")]] <- r2[["irr"]]; out[[paste0(m[1], "_p_add_2020_24")]] <- r2[["p"]]
    }
    out %>% mutate(negbin_alpha = signif(1 / fn$theta, 2), negbin_converged = isTRUE(fn$converged),
                   negbin_warnings = paste(unique(nb_warn), collapse = " | "),
                   quasipoisson_dispersion = round(summary(fq)$dispersion, 2))
  }))
}))
write_csv(table_s1, file.path(outdir, "table_s1.csv"))

# ---- 6. Crude rates ------------------------------------------------------------
crude <- bind_rows(lapply(names(bands), function(b) {
  db <- d %>% filter(agecat %in% bands[[b]])
  bind_rows(db %>% group_by(Year, Area, Sex) %>% summarise(D = sum(Death), P = sum(Population), .groups = "drop"),
            db %>% group_by(Year, Area) %>% summarise(D = sum(Death), P = sum(Population), .groups = "drop") %>% mutate(Sex = "Persons")) %>%
    mutate(rate = D / P * 1e4, lo = rate - 1.96 * rate / sqrt(D), hi = rate + 1.96 * rate / sqrt(D), band = band_label[[b]])
}))
write_csv(crude, file.path(outdir, "crude_rates.csv"))
broad2024 <- bind_rows(lapply(list(c("0-24", 0, 5), c("25-49", 6, 10), c("50-74", 11, 15), c("75+", 16, 18)), function(x) {
  d %>% filter(agecat >= as.numeric(x[2]), agecat <= as.numeric(x[3]), Year == 2024) %>%
    group_by(Area) %>% summarise(rate2024 = sum(Death) / sum(Population) * 1e4, .groups = "drop") %>% mutate(band = x[1])
})) %>% pivot_wider(names_from = Area, values_from = rate2024) %>% mutate(north_higher_pct = 100 * (North / South - 1))
write_csv(broad2024, file.path(outdir, "crude_rates_broad_bands_2024.csv"))

# ---- 7. Counterfactual population sensitivity analysis --------------------------
# Within each five-year age-sex stratum, the northern population that would give the
# South's rate (northern deaths / southern rate), summed across strata.
cf_bands <- list(all = 0:18, under75 = 0:15, a2549 = 6:10)
counterfactual <- bind_rows(lapply(names(cf_bands), function(b) {
  d %>% filter(agecat %in% cf_bands[[b]]) %>%
    group_by(Year, agecat, Sex, Area) %>% summarise(D = sum(Death), P = sum(Population), .groups = "drop") %>%
    pivot_wider(names_from = Area, values_from = c(D, P)) %>%
    mutate(needed = D_North / (D_South / P_South)) %>%
    group_by(Year) %>% summarise(population_north = sum(P_North), counterfactual = sum(needed), .groups = "drop") %>%
    mutate(pct_larger = 100 * (counterfactual / population_north - 1), band = band_label[[b]])
}))
write_csv(counterfactual, file.path(outdir, "counterfactual.csv"))

# ---- 8. Demographics ------------------------------------------------------------
lo_age <- c(0, 1, seq(5, 85, 5)); hi_age <- c(1, seq(5, 85, 5), 100)
med_age <- function(P) { cum <- cumsum(P); h <- sum(P) / 2; k <- which(cum >= h)[1]
  lo_age[k] + (h - ifelse(k == 1, 0, cum[k - 1])) / P[k] * (hi_age[k] - lo_age[k]) }
demog <- d %>% filter(Year %in% c(1981, 2024)) %>% arrange(Year, Sex, Area, agecat) %>%
  group_by(Year, Sex, Area) %>%
  summarise(population = sum(Population),
            median_age = med_age(Population),
            mean_age   = sum(midage[agecat + 1] * Population) / sum(Population),
            pct_65plus = 100 * sum(Population[agecat >= 14]) / sum(Population),
            pct_25_49  = 100 * sum(Population[agecat >= 6 & agecat <= 10]) / sum(Population), .groups = "drop") %>%
  group_by(Year, Sex) %>% mutate(ratio_N_to_S = population[Area == "North"] / population[Area == "South"]) %>% ungroup()
write_csv(demog, file.path(outdir, "demographics.csv"))

# ---- figures --------------------------------------------------------------------
# All text in Arial. On Windows the family is registered with the GDI devices; if Arial is
# not installed (Linux), the metric-compatible Liberation Sans is registered under that
# name for rendering and the SVG font-family is still written as Arial.
if (.Platform$OS.type == "windows") grDevices::windowsFonts(Arial = grDevices::windowsFont("Arial"))
if (requireNamespace("systemfonts", quietly = TRUE) &&
    !any(systemfonts::system_fonts()$family == fig_font)) {
  lib <- systemfonts::system_fonts() %>% filter(family == "Liberation Sans")
  pick <- function(sty) { f <- lib$path[lib$style == sty]; if (length(f)) f[1] else lib$path[1] }
  if (nrow(lib)) systemfonts::register_font(fig_font, plain = pick("Regular"), bold = pick("Bold"),
                                            italic = pick("Italic"), bolditalic = pick("Bold Italic"))
}
save_fig <- function(p, name, w, h) {
  # PNG via ragg when available (system fonts, no Windows font-database warnings)
  ggsave(file.path(outdir, paste0(name, ".png")), p, width = w, height = h, dpi = 150)
  svg_file <- file.path(outdir, paste0(name, ".svg"))
  ggsave(svg_file, p, width = w, height = h)
  txt <- readLines(svg_file, warn = FALSE)
  txt <- gsub("font-family: \"[^\"]*\"", paste0("font-family: \"", fig_font, "\""), txt)
  writeLines(txt, svg_file)
  ggsave(file.path(outdir, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)
  if (requireNamespace("devEMF", quietly = TRUE)) {
    devEMF::emf(file.path(outdir, paste0(name, ".emf")), width = w, height = h, emfPlus = TRUE)
    print(p); invisible(dev.off())
  }
}
region_cols <- c(North = "maroon", South = "blue")
stress <- data.frame(xmin = c(1990, 2008, 2020), xmax = c(1991, 2009, 2022))
ylab_years <- c(1981, seq(1985, 2020, 5), 2024)

# Figure 1: DSMR, a) under 75, b) 25-49
f1 <- dsmr %>% mutate(panel = factor(ifelse(band == "Under 75", "a) Aged under 75 years", "b) Aged 25\u201349 years")))
p1 <- ggplot(f1, aes(Year, dsmr, colour = Area)) +
  geom_rect(data = stress, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "gold", alpha = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi, group = Area), fill = "grey75", alpha = 0.6, colour = NA) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ panel, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = region_cols, breaks = c("North", "South")) +
  scale_x_continuous(breaks = ylab_years) +
  labs(x = "Year", y = "Standardised mortality rate (per 10 000)", colour = NULL) +
  theme_bw(base_family = fig_font) + theme(legend.position = "right", strip.text = element_text(hjust = 0, face = "bold"),
                                           axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())
save_fig(p1, "fig1_dsmr", 6.5, 8)

# Figure 2: age-by-year northern excess, filled contours with birth-cohort lines
lo_bound <- c(0, 1, seq(5, 85, 5))
cohorts <- bind_rows(lapply(seq(1900, 2020, 5), function(cb) {
  tibble(cohort = cb, Year = 1981:2024, age = 1981:2024 - cb) %>% filter(age >= 0) %>%
    mutate(idx = findInterval(age, lo_bound) - 1)
}))
f2 <- ex_age_year %>% filter(Sex != "Persons") %>%
  mutate(z = pmin(pmax(excess, -20), 80),
         Sex = factor(Sex, levels = c("Male", "Female"), labels = c("a) Males", "b) Females")))
contour_cols <- c("#0000FF", "#00FFFF", "#008000", "#00FF00", "#FFFF00", "#FFD200", "#FF7F00", "#FF0000", "#C10534", "#90353B")  # Stata contour palette, matching the original Figure 2
p2 <- ggplot(f2, aes(Year, agecat)) +
  geom_contour_filled(aes(z = z), breaks = seq(-20, 80, 10)) +
  geom_step(data = cohorts, aes(Year, idx, group = cohort), direction = "hv", colour = "black", linewidth = 0.3) +
  facet_wrap(~ Sex, ncol = 1) +
  scale_fill_manual(values = contour_cols, name = "% northern\nexcess mortality", drop = FALSE,
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = 0:18, labels = ages, expand = c(0, 0)) +
  scale_x_continuous(breaks = ylab_years, expand = c(0, 0)) +
  labs(x = "Year", y = "Age category") +
  theme_bw(base_family = fig_font) + theme(strip.text = element_text(hjust = 0, face = "bold"), panel.grid = element_blank(),
                                           axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p2, "fig2_contour", 7.5, 10)

# Figure 3: age-specific excess in five periods, a) males, b) females
period_cols <- c("1981-89" = "blue", "1990-99" = "red", "2000-09" = "darkgreen", "2010-19" = "purple", "2020-24" = "darkorange")
f3 <- ex_age_period %>% filter(Sex != "Persons") %>%
  mutate(AgeGroup = factor(AgeGroup, levels = ages),
         Sex = factor(Sex, levels = c("Male", "Female"), labels = c("a) Males", "b) Females")))
p3 <- ggplot(f3, aes(AgeGroup, excess, colour = period, group = period)) +
  geom_ribbon(aes(ymin = excess_lo, ymax = excess_hi, group = period), fill = "grey80", colour = NA, alpha = 0.5) +
  geom_line(linewidth = 0.5) + facet_wrap(~ Sex, ncol = 1) +
  scale_colour_manual(values = period_cols, name = NULL) +
  labs(x = "Age group", y = "% northern excess mortality") +
  theme_bw(base_family = fig_font) + theme(legend.position = "bottom", strip.text = element_text(hjust = 0, face = "bold"),
                                           axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())
save_fig(p3, "fig3_age_period", 7, 9.5)

# Figure 4: crude rates by age band and sex
f4 <- crude %>% filter(Sex != "Persons") %>%
  mutate(band = factor(band, levels = c("All ages", "Under 75", "25-49", "0-24"),
                       labels = c("All ages", "Aged under 75", "Aged 25\u201349", "Aged 0\u201324")),
         Sex = factor(Sex, levels = c("Male", "Female"), labels = c("Males", "Females")))
p4 <- ggplot(f4, aes(Year, rate, colour = Area)) +
  geom_rect(data = stress, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "gold", alpha = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi, group = Area), fill = "grey75", alpha = 0.6, colour = NA) +
  geom_line(linewidth = 0.4) +
  facet_grid(band ~ Sex, scales = "free_y") +
  scale_colour_manual(values = region_cols, breaks = c("North", "South")) +
  scale_x_continuous(breaks = ylab_years) +
  labs(x = "Year", y = "Crude mortality rate (per 10 000)", colour = NULL) +
  theme_bw(base_family = fig_font) + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
                                           panel.grid.minor = element_blank())
save_fig(p4, "fig4_crude", 7.5, 10)

# ---- summary --------------------------------------------------------------------
cat("\nDSMR per 10 000, North / South\n")
for (b in c("Under 75", "25-49")) for (y in c(1981, 2019, 2024)) {
  r <- dsmr %>% filter(band == b, Year == y)
  cat(sprintf("  %-8s %d: %.1f (%.1f-%.1f) / %.1f (%.1f-%.1f)\n", b, y,
              r$dsmr[r$Area == "North"], r$lo[r$Area == "North"], r$hi[r$Area == "North"],
              r$dsmr[r$Area == "South"], r$lo[r$Area == "South"], r$hi[r$Area == "South"]))
}
cat("\nAge-sex-adjusted northern excess mortality, % (95% CI)\n")
for (b in c("Under 75", "25-49")) for (y in c(1981, 2019, 2024)) {
  r <- ex_year %>% filter(band == b, Year == y, sex == "Persons")
  cat(sprintf("  %-8s %d: %.1f (%.1f-%.1f)\n", b, y, r$excess, r$lo, r$hi))
}
cat("\nTable 1: IRR 2010-19 trend, p | additional 2020-24, p | total 2020-24 trend\n")
for (i in seq_len(nrow(table1))) with(table1[i, ],
  cat(sprintf("  %-8s %-6s: %s p=%s | %s p=%s | %s\n", band, sex, irr_2010_19, p_2010_19, irr_add_2020_24, p_add_2020_24, irr_total_2020_24)))
cat("\nTable S1: negative binomial IRR 2010-19 trend, p | additional 2020-24, p | alpha | quasi-Poisson dispersion\n")
for (i in seq_len(nrow(table_s1))) with(table_s1[i, ],
  cat(sprintf("  %-8s %-6s: %s p=%s | %s p=%s | alpha %s | phi %s\n", band, sex, negbin_irr_2010_19, negbin_p_2010_19,
              negbin_irr_add_2020_24, negbin_p_add_2020_24, negbin_alpha, quasipoisson_dispersion)))
cat("\nCrude rates 2024 per 10 000, North / South\n")
for (i in seq_len(nrow(broad2024))) with(broad2024[i, ], cat(sprintf("  %-5s: %.1f / %.1f (North higher by %.0f%%)\n", band, North, South, north_higher_pct)))
cat("\nCounterfactual 2024: northern population needed to match southern rates\n")
cf24 <- counterfactual %>% filter(Year == 2024)
for (i in seq_len(nrow(cf24))) cat(sprintf("  %-8s: %.1f%% larger\n", cf24$band[i], cf24$pct_larger[i]))
message("\nOutputs written to ./", outdir, "/")
