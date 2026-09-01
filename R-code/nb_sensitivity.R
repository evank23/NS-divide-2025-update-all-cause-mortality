# Negative binomial sensitivity analysis for the segmented Poisson models (Table 1)
# North-South divide in English mortality 1981-2024 - reads All_cause_dataset_from_xlsx.csv
# (cell-level deaths and populations by five-year age group, sex, region and year, built from tot_death.xlsx)
suppressMessages({library(MASS)})
d <- read.csv("All_cause_dataset_from_xlsx.csv")
d <- subset(d, year >= 2010 & year <= 2024)
d$t     <- d$year - 2010
d$chng4 <- pmax(0, d$year - 2019)
d$agecat <- factor(d$agecat)
bands <- list("All ages" = 0:18, "Under 75" = 0:15, "25-49" = 6:10, "0-24" = 0:5)
f <- deaths ~ north * t + north * chng4 + agecat + offset(log(population))
res <- list()
for (b in names(bands)) for (sx in c("Female", "Male")) {
  s <- subset(d, as.integer(as.character(agecat)) %in% bands[[b]] & male == ifelse(sx == "Male", 1, 0))
  s$agecat <- droplevels(s$agecat)
  # Poisson (the paper's model)
  mp <- glm(f, family = poisson, data = s)
  # quasi-Poisson: same point estimates, SEs scaled by sqrt(dispersion)
  mq <- glm(f, family = quasipoisson, data = s)
  # negative binomial (NB2), theta estimated by maximum likelihood
  w <- NULL
  mnb <- withCallingHandlers(glm.nb(f, data = s, control = glm.control(maxit = 200)),
                             warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  get <- function(m, term, vc = NULL) {
    est <- coef(m)[term]; se <- if (is.null(vc)) sqrt(diag(vcov(m)))[term] else sqrt(diag(vc))[term]
    p <- 2 * pnorm(-abs(est / se))
    sprintf("%.3f (%.3f-%.3f) p=%s", exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se), format(signif(p, 2)))
  }
  res[[length(res) + 1]] <- data.frame(
    band = b, sex = sx, n_cells = nrow(s),
    poisson_pre = get(mp, "north:t"), poisson_post = get(mp, "north:chng4"),
    qp_dispersion = round(summary(mq)$dispersion, 2),
    quasi_pre = get(mq, "north:t"), quasi_post = get(mq, "north:chng4"),
    nb_theta = signif(mnb$theta, 3), nb_alpha = signif(1 / mnb$theta, 2),
    nb_pre = get(mnb, "north:t"), nb_post = get(mnb, "north:chng4"),
    nb_converged = mnb$converged, nb_iter_theta = mnb$iter,
    nb_warnings = if (is.null(w)) "" else paste(unique(w), collapse = " | "),
    stringsAsFactors = FALSE)
}
out <- do.call(rbind, res)
write.csv(out, "nb_sensitivity_segmented.csv", row.names = FALSE)
options(width = 220)
print(out[, c("band", "sex", "poisson_pre", "nb_pre", "poisson_post", "nb_post", "nb_alpha", "nb_converged", "qp_dispersion")], row.names = FALSE)
cat("\nWarnings from glm.nb:\n"); print(out[, c("band", "sex", "nb_warnings")], row.names = FALSE)
