# Given scores at t1 and t2, return relevant statistics, including:
# - Summary statistics (mean at t1, mean at t2)
# - Change (d[av], with 95% CI)
# - Test (t-test, with p-value)
d_av <- function(.data, var) {

  t1 <- var %+% "_t1"
  t2 <- var %+% "_t2"

  t1_mean <- mean(.data[[t1]], na.rm = T)
  t2_mean <- mean(.data[[t2]], na.rm = T)

  t1_sd <- sd(.data[[t1]], na.rm = T)
  t2_sd <- sd(.data[[t2]], na.rm = T)

  n <- nrow(.data)

  d <- d.dep.t.avg(
    m1 = t2_mean,
    m2 = t1_mean,
    sd1 = t2_sd,
    sd2 = t1_sd,
    n = n,
    a = .05
  )

  t_test <- t.test(.data[[t1]], .data[[t2]], paired = T)

  increased_1_sd <- mean(.data[[t2]] >= .data[[t1]] + t1_sd)
  decreased_1_sd <- mean(.data[[t2]] <= .data[[t1]] - t1_sd)

  out <- tibble(
    t1_mean,
    t1_sd,
    t2_mean,
    t2_sd,
    increased_1_sd,
    decreased_1_sd,
    d_av_est = d$d,
    d_av_low = d$dlow,
    d_av_high = d$dhigh,
    t_df = t_test$parameter,
    t_stat = t_test$statistic,
    t_p = t_test$p.value
  )

  return(out)

}

# Given data and a regression formula, return relevant statistics, including:
# - Regression coefficients in raw units
# - Regression coefficients in standardized units (i.e., against scale(y))
# - p-values
regression <- function(.data, .formula) {

  lm_out <- lm(
    data = .data,
    formula = .formula
  )

  tidy_out <- tidy(lm_out, conf.int = T)

  tidy_out$dv <- as.character(attr(lm_out$terms, "variables")[[2]])

  tidy_out %>%
    select(
      dv,
      iv = term,
      b = estimate,
      lower = conf.low,
      upper = conf.high,
      p = p.value
    ) %>%
    return()

}


# Given treatment and control scores at t1 and t2, calculate and return d(ppc2),
# from Morris 2008 ("Estimating Effect Sizes...")
d_ppc2 <- function(var) {

  t1 <- var %+% "_t1"
  t2 <- var %+% "_t2"

  t_t1 <- intervention_only[[t1]]
  t_t2 <- intervention_only[[t2]]

  c_t1 <- feedback_only[[t1]]
  c_t2 <- feedback_only[[t2]]

  # Defining inputs to calculation
  t_t1_mean <- mean(t_t1)
  t_t1_sd <- sd(t_t1)

  t_t2_mean <- mean(t_t2)
  t_t2_sd <- sd(t_t2)

  t_n <- length(t_t1)
  t_r <- cor(t_t1, t_t2)

  c_t1_mean <- mean(c_t1)
  c_t1_sd <- sd(c_t1)

  c_t2_mean <- mean(c_t2)
  c_t2_sd <- sd(c_t2)

  c_n <- length(c_t1)
  c_r <- cor(c_t1, c_t2)

  t_change <- t_t2_mean - t_t1_mean
  c_change <- c_t2_mean - c_t1_mean

  diff_in_diff <- t_change - c_change # Difference-in-differences, i.e., the numerator

  df <- t_n + c_n - 2 # Degrees of freedom

  pooled_sd_t1 <- sqrt((((t_n - 1) * (t_t1_sd^2)) + ((c_n - 1) * (c_t1_sd^2))) / (df)) # Pooled SD (t1test), i.e., the denominator

  weighted_average_r <- ((t_n * t_r) + (c_n * c_r)) / (t_n + c_n) # Morris 2008 uses the population parameter; Jane et al. use this

  variance_scaling_factor <- (t_n + c_n) / (t_n * c_n) # Scales the variance in relation to the sample size

  bias_adjustment <- 1 - (3 / ((4 * df) - 1)) # c(p) in Morris 2008, eq. 10

  dppc2 <- bias_adjustment * (diff_in_diff / pooled_sd_t1) # From Morris eq. 8

  dppc2_alt <- diff_in_diff / pooled_sd_t1 # From Jane et al. eq. 7.26; not used but kept for t2erity

  se <- sqrt(2 * (bias_adjustment^2) * (1 - weighted_average_r) * variance_scaling_factor * (df / (df - 2)) * (1 + (dppc2^2 / (2 * (1 - weighted_average_r) * variance_scaling_factor))) - dppc2^2) # From Morris eq. 25

  se_alt <- sqrt(2 * (1 - weighted_average_r) * variance_scaling_factor * (1 + (dppc2^2 / (2 * (1 - weighted_average_r) * variance_scaling_factor))) - dppc2^2) # From Jane et al. eq. 7.28 with a typo fixed

  upper_ci <- dppc2 + (1.959964 * se)
  lower_ci <- dppc2 - (1.959964 * se)

  out <- tibble(
    d_ppc2_est = dppc2,
    d_ppc2_low = lower_ci,
    d_ppc2_high = upper_ci
  )

  return(out)

}

# Tidy printing
print_nicely <- function(., .digits) {

  {.} %>%
    kable(digits = .digits) %>%
    kable_styling()

}
