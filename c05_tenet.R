# Comment on Wang, Jiang, Lin, Xie and Stanley (2018): "Interconnectedness and
# systemic risk of China's financial institutions", Emerging Markets Review
# 35, 1-18.
# Leonard Nikita Baur, Chemnitz University of Technology

# c05_tenet estimates the TENET networks of Wang et al. (2018) twice, once
# with unadjusted and once with adjusted weekly returns, and compares both
# with their published results. Everything except the returns is identical
# in the two runs, so every difference between them comes from the
# corporate-action weeks alone.
#
# Their setup, followed here (Table A3): 24 A-share institutions (14 banks,
# 7 securities firms, 3 insurers), weekly log returns 2008-2016, rolling
# windows of 51 weeks, tau = 0.01 (main specification) and 0.05, seven macro
# state variables (c03) and four firm characteristics (c04), both lagged by
# one week, market capitalisation at the window end in SRR and SRE.
#
# Estimator: single-index quantile regression with an L1 penalty (Haerdle,
# Wang and Yu 2016). lambda is chosen by SIC on a grid of n_lambda = 15
# values in both steps.
#
# With 51 observations, tau = 0.01 puts half an observation below the
# quantile: each fit rests on the worst week of its window. A single
# mechanical drop in an unadjusted series therefore dominates every window
# that contains it.
#
# Comparison with Wang et al.:
#   - their Fig. 1 (total connectedness) and Fig. 2 (in-strength by sector),
#     extracted from the vector graphics of the published PDF (Online
#     Appendix B), in external/ (versioned, unlike data/):
#       wang2018_fig1_tc.csv, wang2018_fig2_iss_banks.csv,
#       wang2018_fig2_iss_securities.csv, wang2018_fig2_iss_insurers.csv
#     columns x (decimal year) and y. A missing file is skipped.
#   - their case study, the window ending 31 Dec 2014 (their Tables 2-4,
#     Table 1 of the comment).
#
# Runtime: 4 runs (2 versions x 2 tau) of 410 windows, parallel over
# n_cores. Every window is cached in data/processed/tenet/<run>/; a rerun
# only estimates what is missing. Delete a run's folder after changing the
# estimator or the inputs.
#
# Input:  data/processed/bs_weekly.rds, bs_events.rds (c02)
#         data/processed/state_vars.rds (c03)
#         data/processed/firm_chars.rds, mcap_weekly.rds (c04)
#         external/wang2018_fig*.csv (extracted figures)
# Output: data/processed/tenet/<run>/w_<week>.rds  one network per window
#         data/processed/tenet_summary.rds           all results, for c99
#         data/processed/c05_plots.rds               both figures, for c99
#         output/c05_comparison.csv                  correlations (Table A4)
#         output/c05_bootstrap.csv                   equation (A2) (Table A4)
#         output/c05_ca_test.csv                     equation (A3) (Table A5)
#         output/c05_case_study.csv                  case study (Table 1)
#         output/c05_broker_ranks.csv                case study, ISI ranks
#         output/c05_tc_phase.csv                    TC by phase
#         output/c05_paper_peaks.csv                 peaks of their series
#         output/c05_levels.png                      levels (Figure A1)
#         output/c05_normalised.png                  paths (Figure 1)

# ---- 0. Packages, parameters -------------------------------------------------
library(tidyverse)
library(here)
library(quantreg)
library(KernSmooth)
library(furrr)

tau_grid  <- c(0.01, 0.05)
ws        <- 50                       # window = ws + 1 = 51 weeks, as theirs
n_lambda  <- 15
max_iter  <- 2
crit_init <- "sic"                    # criterion of the first lasso step
sample_start <- as.Date("2008-01-01")  # Wang et al.: weekly returns 2008-2016
snap_week <- as.Date("2014-12-29")    # week containing 31 Dec 2014
end_week  <- as.Date("2016-12-26")    # last week of 2016
shade_min <- -0.35                    # threshold, Figure 1 and equation (A3)
n_cores   <- max(1, parallelly::availableCores() - 1)
set.seed(20261007)

cache_root <- here("data", "processed", "tenet")
dir_out    <- here("output")
dir_ext    <- here("external")
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out,    recursive = TRUE, showWarnings = FALSE)

# Monday of the week a date falls in (same as c02)
week_of <- function(d) floor_date(d, "week", week_start = 1)

# Their sample with their abbreviations (Table 1); joined by stock code
wang <- tribble(
  ~abbr,    ~code,    ~sector,
  "PAB",    "000001", "bank",
  "NBCB",   "002142", "bank",
  "SPDB",   "600000", "bank",
  "HXB",    "600015", "bank",
  "CMBC",   "600016", "bank",
  "CMB",    "600036", "bank",
  "NJBK",   "601009", "bank",
  "CIB",    "601166", "bank",
  "BOB",    "601169", "bank",
  "BOCOM",  "601328", "bank",
  "ICBC",   "601398", "bank",
  "CCB",    "601939", "bank",
  "BOC",    "601988", "bank",
  "CNCB",   "601998", "bank",
  "NESC",   "000686", "securities",
  "GYSC",   "000728", "securities",
  "CJSC",   "000783", "securities",
  "CITICS", "600030", "securities",
  "SLSC",   "600109", "securities",
  "HTSEC",  "600837", "securities",
  "PSC",    "601099", "securities",
  "PAI",    "601318", "insurance",
  "CPIC",   "601601", "insurance",
  "CLI",    "601628", "insurance"
)
codes   <- wang$code
abbr_of <- set_names(wang$abbr, wang$code)

# Their case study, window ending 31 Dec 2014, tau = 0.01: the ten strongest
# edges (Table 2) and the top 10 by ISI, OSI, SRR and SRE (Tables 3 and 4)
wang_edges <- tribble(
  ~rank, ~from,   ~to,     ~paper,
  1,     "CCB",   "CJSC",  2.18,
  2,     "HTSEC", "PSC",   1.95,
  3,     "HXB",   "NESC",  1.68,
  4,     "NBCB",  "PSC",   1.51,
  5,     "CLI",   "PSC",   1.46,
  6,     "BOB",   "CJSC",  1.41,
  7,     "BOC",   "CMBC",  1.26,
  8,     "BOCOM", "CJSC",  1.03,
  9,     "CMB",   "CJSC",  0.88,
  10,    "CMBC",  "NESC",  0.81
)
wang_top <- list(
  isi = c("CJSC", "PSC", "NESC", "CMBC", "GYSC", "BOB", "CITICS", "SLSC", "CLI", "HXB"),
  osi = c("CMBC", "HTSEC", "BOCOM", "CLI", "BOC", "CCB", "NBCB", "BOB", "HXB", "CITICS"),
  srr = c("CCB", "CMBC", "CJSC", "BOC", "CLI", "ICBC", "PAI", "PSC", "CPIC", "CNCB"),
  sre = c("BOC", "CLI", "BOCOM", "CMBC", "CCB", "HTSEC", "SPDB", "PAI", "ICBC", "CNCB")
)
paper_tab <- tribble(
  ~measure, ~abbr,    ~paper,
  "isi",    "CJSC",   7.25,    "isi", "PSC",   6.55,    "isi", "NESC",  3.63,
  "isi",    "CMBC",   2.14,    "isi", "GYSC",  1.75,    "isi", "BOB",   1.27,
  "isi",    "CITICS", 0.78,    "isi", "SLSC",  0.74,    "isi", "CLI",   0.60,
  "isi",    "HXB",    0.60,
  "osi",    "CMBC",   3.57,    "osi", "HTSEC", 2.69,    "osi", "BOCOM", 2.40,
  "osi",    "CLI",    2.35,    "osi", "BOC",   2.34,    "osi", "CCB",   2.21,
  "osi",    "NBCB",   2.04,    "osi", "BOB",   2.02,    "osi", "HXB",   1.92,
  "osi",    "CITICS", 1.25,
  "srr",    "CCB",    8.17e23, "srr", "CMBC",  6.12e23, "srr", "CJSC",  4.82e23,
  "srr",    "BOC",    3.86e23, "srr", "CLI",   3.65e23, "srr", "ICBC",  1.56e23,
  "srr",    "PAI",    1.32e23, "srr", "PSC",   1.21e23, "srr", "CPIC",  8.87e22,
  "srr",    "CNCB",   8.54e22,
  "sre",    "BOC",    1.50e24, "sre", "CLI",   4.99e23, "sre", "BOCOM", 4.20e23,
  "sre",    "CMBC",   3.35e23, "sre", "CCB",   3.15e23, "sre", "HTSEC", 8.65e22,
  "sre",    "SPDB",   8.00e22, "sre", "PAI",   7.80e22, "sre", "ICBC",  7.43e22,
  "sre",    "CNCB",   7.42e22
)

# ---- 1. Data -----------------------------------------------------------------
bs_weekly  <- readRDS(here("data", "processed", "bs_weekly.rds"))
events     <- readRDS(here("data", "processed", "bs_events.rds"))
state      <- readRDS(here("data", "processed", "state_vars.rds"))
firm_chars <- readRDS(here("data", "processed", "firm_chars.rds"))
mcap       <- readRDS(here("data", "processed", "mcap_weekly.rds"))

state_names <- c("liq_spread", "d_tb3m", "d_slope", "d_credit", "ret_mkt",
                 "vol_mkt", "ret_re")
chars_names <- c("lev", "mm", "mtb", "size")

stopifnot(all(codes %in% bs_weekly$code), all(codes %in% firm_chars$code))

# ---- 2. Weekly matrices ------------------------------------------------------
# State variables and firm characteristics enter with a one-week lag: the
# return of week t is regressed on the values of week t-1.
states_lagged <- state |>
  arrange(week) |>
  mutate(across(all_of(state_names), dplyr::lag)) |>
  select(week, all_of(state_names))

chars_lagged <- firm_chars |>
  arrange(code, week) |>
  group_by(code) |>
  mutate(across(all_of(chars_names), dplyr::lag)) |>
  ungroup() |>
  select(code, week, all_of(chars_names))

# Weeks with returns of all 24 institutions in both versions and all lagged
# inputs, up to the end of 2016
ret_wide <- function(v) {
  bs_weekly |>
    filter(version == v, code %in% codes) |>
    select(week, code, ret) |>
    pivot_wider(names_from = code, values_from = ret)
}
ret_u <- ret_wide("unadjusted")
ret_a <- ret_wide("adjusted")

complete_weeks <- function(d) d$week[complete.cases(d)]

# A week counts only if all 24 institutions have all four characteristics
chars_weeks <- chars_lagged |>
  group_by(week) |>
  summarise(ok = n() == length(codes) &&
              all(!is.na(lev), !is.na(mm), !is.na(mtb), !is.na(size))) |>
  filter(ok) |>
  pull(week)

weeks <- Reduce(intersect, list(
  complete_weeks(ret_u[, c("week", codes)]),
  complete_weeks(ret_a[, c("week", codes)]),
  complete_weeks(states_lagged),
  chars_weeks
)) |>
  as.Date(origin = "1970-01-01") |>
  sort()

# Their sample: weekly returns 2008-2016, i.e. from the first week after the
# one containing 1 Jan 2008, as in c02's Table 1 check
weeks <- weeks[weeks > week_of(sample_start) & weeks <= end_week]

cat("Weeks:", length(weeks), "from", format(min(weeks)), "to",
    format(max(weeks)), "\n")

to_mat <- function(wide) {
  as.matrix(wide[match(weeks, wide$week), codes])
}
R_list <- list(unadjusted = to_mat(ret_u), adjusted = to_mat(ret_a))

M_mat <- as.matrix(states_lagged[match(weeks, states_lagged$week), state_names])

B_arr <- map(set_names(chars_names), \(v) {
  chars_lagged |>
    select(week, code, value = all_of(v)) |>
    pivot_wider(names_from = code, values_from = value) |>
    to_mat()
})

# Market capitalisation (c04 uses the abbreviations of c01, e.g. BCM for
# BOCOM; joined via the code); a suspended week keeps the last value
MC_mat <- mcap |>
  left_join(bs_weekly |> distinct(abbr, code), by = "abbr") |>
  filter(code %in% codes) |>
  select(week, code, mcap) |>
  complete(week = weeks, code = codes) |>
  arrange(code, week) |>
  group_by(code) |>
  fill(mcap, .direction = "down") |>
  ungroup() |>
  filter(week %in% weeks) |>
  pivot_wider(names_from = code, values_from = mcap) |>
  to_mat()

stopifnot(!anyNA(R_list$unadjusted), !anyNA(R_list$adjusted), !anyNA(M_mat),
          !anyNA(MC_mat), !any(map_lgl(B_arr, anyNA)))

# 2a. Where the two versions differ: weeks with a corporate action and the
#     lowest weekly return per institution in each version
tibble(abbr         = abbr_of[codes],
       weeks_differ = colSums(abs(R_list$unadjusted - R_list$adjusted) > 0.005),
       min_unadj    = round(apply(R_list$unadjusted, 2, min), 4),
       min_adj      = round(apply(R_list$adjusted,   2, min), 4)) |>
  arrange(min_unadj) |>
  print(n = Inf)

# ---- 3. Estimation functions -------------------------------------------------
quiet_rq <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("nonunique", conditionMessage(w))) invokeRestart("muffleWarning")
  })
}

minmax <- function(m) {
  m <- apply(as.matrix(m), 2, \(x) (x - min(x)) / (max(x) - min(x)))
  m[!is.finite(m)] <- 0
  m
}

check_loss <- function(r, tau) sum(r * (tau - (r < 0)))

# L1-penalised linear quantile regression, intercept unpenalised,
# lambda by SIC or GACV on a log grid below lambda_max
qr_lasso <- function(X, y, tau, n_grid = n_lambda, tol = 1e-5, crit = "sic") {
  n  <- length(y)
  p  <- ncol(X)
  X1 <- cbind(1, X)
  l_max <- max(abs(crossprod(X, tau - (y < quantile(y, tau)))))
  grid  <- l_max * exp(seq(0, log(0.01), length.out = n_grid))
  fits <- purrr::map(grid, \(l) {
    b <- tryCatch(rq.fit.lasso(X1, y, tau = tau, lambda = c(0, rep(l, p)))$coefficients,
                  error = \(e) NULL)
    if (is.null(b)) return(NULL)
    b[-1][abs(b[-1]) < tol] <- 0
    df   <- sum(b[-1] != 0)
    loss <- check_loss(y - X1 %*% b, tau)
    list(beta = b[-1], lambda = l,
         sic  = log(loss / n) + log(n) / (2 * n) * df,
         gacv = if (df < n) loss / (n - df) else Inf)   # Yuan (2006)
  }) |> purrr::compact()
  if (!length(fits)) return(list(beta = rep(0, p), lambda = NA_real_))
  fits[[which.min(purrr::map_dbl(fits, crit))]]
}

lprq0 <- function(x, y, h, tau, x0) {
  z <- x - x0
  f <- quiet_rq(rq(y ~ z, weights = dnorm(z / h), tau = tau))
  unname(coef(f))
}

bandwidth <- function(index, y, tau) {
  o  <- order(index)
  hm <- tryCatch(dpill(index[o], y[o]), error = \(e) 0.08)
  if (!is.finite(hm)) hm <- 0.08
  10 * hm * (tau * (1 - tau) / dnorm(qnorm(tau))^2)^0.2
}

siqr <- function(y, X, tau, x_eval, max_iter = 2, crit = 0.01, crit_init = "sic") {
  n <- length(y)
  d <- ncol(X)
  zero <- list(partial = rep(0, d), lambda = NA_real_)
  
  # step 1: initial direction
  init <- qr_lasso(X, y, tau, crit = crit_init)
  beta <- init$beta
  if (all(beta == 0)) return(modifyList(zero, list(lambda = init$lambda)))
  beta     <- beta / sqrt(sum(beta^2))
  beta_new <- rep(0, d)
  lambda   <- init$lambda
  iter     <- 1
  
  # step 2: pseudo data, SIC
  while (iter < max_iter && sum((beta_new - beta)^2) > crit) {
    if (iter > 1) beta <- beta_new
    index <- as.vector(X %*% beta)
    h     <- bandwidth(index, y, tau)
    ab    <- t(vapply(index, \(x0) lprq0(index, y, h, tau, x0), numeric(2)))
    ii   <- rep(seq_len(n), each = n)
    kk   <- rep(seq_len(n), times = n)
    xnew <- ab[kk, 2] * (X[ii, , drop = FALSE] - X[kk, , drop = FALSE])
    ynew <- y[ii] - ab[kk, 1]
    wts  <- dnorm(as.vector(xnew %*% beta) / h)
    keep <- wts > 1e-10 * max(wts)
    fit  <- qr_lasso(xnew[keep, , drop = FALSE] * sqrt(wts[keep]),
                     ynew[keep] * sqrt(wts[keep]), tau)
    if (all(fit$beta == 0)) return(modifyList(zero, list(lambda = fit$lambda)))
    beta_new <- fit$beta / sqrt(sum(fit$beta^2))
    lambda   <- fit$lambda
    iter     <- iter + 1
  }
  if (iter > 1) beta <- beta_new
  
  index <- as.vector(X %*% beta)
  h     <- bandwidth(index, y, tau)
  g     <- lprq0(index, y, h, tau, sum(x_eval * beta))
  list(partial = g[2] * beta, lambda = lambda)
}

# One window ending at row e; all institutions are in every window
tenet_window <- function(e, tau, cache_dir, crit_init = "sic") {
  file <- file.path(cache_dir, sprintf("w_%s.rds", weeks[e]))
  if (file.exists(file)) return(invisible(file))
  
  rows <- (e - ws):e
  R    <- R_mat[rows, , drop = FALSE]
  N    <- ncol(R)
  m    <- minmax(M_mat[rows, ])
  
  var_q <- vapply(seq_len(N), \(k) {
    f <- quiet_rq(rq(R[, k] ~ m, tau = tau))
    tail(as.vector(fitted(f)), 1)
  }, numeric(1))
  
  D      <- matrix(0, N, N, dimnames = list(codes, codes))
  lambda <- rep(NA_real_, N)
  for (j in seq_len(N)) {
    b  <- minmax(sapply(B_arr, \(x) x[rows, codes[j]]))
    X  <- cbind(R[, -j, drop = FALSE], m, b)
    xe <- c(var_q[-j], m[nrow(m), ], b[nrow(b), ])
    fit <- tryCatch(siqr(R[, j], X, tau, xe, max_iter, crit_init = crit_init),
                    error = \(e) NULL)
    if (is.null(fit)) next
    D[j, -j]  <- fit$partial[seq_len(N - 1)]
    lambda[j] <- fit$lambda
  }
  
  saveRDS(list(week = weeks[e], D = D, mc = MC_mat[e, ], lambda = lambda), file)
  invisible(file)
}

# ---- 4. Run ------------------------------------------------------------------
win_end <- seq(ws + 1, length(weeks))
cat("Windows:", length(win_end), "ending", format(weeks[min(win_end)]), "to",
    format(weeks[max(win_end)]), "\n")

runs <- expand_grid(version = names(R_list), tau = tau_grid) |>
  mutate(run = sprintf("%s_%.2f", version, tau),
         dir = file.path(cache_root, run))

plan(multisession, workers = n_cores)
for (i in seq_len(nrow(runs))) {
  tau_i <- runs$tau[i]
  dir_i <- runs$dir[i]
  dir.create(dir_i, recursive = TRUE, showWarnings = FALSE)
  todo <- win_end[!file.exists(file.path(dir_i, sprintf("w_%s.rds", weeks[win_end])))]
  message("Run ", runs$run[i], ": ", length(todo), " windows to estimate")
  if (!length(todo)) next
  
  glob <- mget(c("MC_mat", "B_arr", "M_mat", "weeks", "codes", "ws",
                 "max_iter", "n_lambda", "crit_init", "tenet_window", "siqr",
                 "qr_lasso", "lprq0", "bandwidth", "minmax", "check_loss",
                 "quiet_rq", "tau_i", "dir_i"))
  glob$R_mat <- R_list[[runs$version[i]]]
  
  future_walk(todo, \(e) tenet_window(e, tau_i, dir_i, crit_init),
              .progress = TRUE,
              .options = furrr_options(seed = TRUE, scheduling = 5,
                                       globals = glob,
                                       packages = c("quantreg", "KernSmooth",
                                                    "purrr")))
}
plan(sequential)

# ---- 5. Measures -------------------------------------------------------------

read_run <- function(dir) {
  map(file.path(dir, sprintf("w_%s.rds", weeks[win_end])), readRDS)
}
all_runs <- map(set_names(runs$dir, runs$run), read_run)

split_run <- function(d) {
  d |>
    separate_wider_delim(run, "_", names = c("version", "tau"),
                         cols_remove = FALSE) |>
    mutate(tau = as.numeric(tau))
}

# 5-0. Fits without a result, per run: an error in siqr() or no lasso
#      solution at all leaves lambda at NA and the institution's row at 0,
#      which lowers TC. The unadjusted and the adjusted run should not
#      differ much here.
fit_log <- imap(all_runs, \(wins, run) {
  tibble(run       = run,
         fits      = length(wins) * length(codes),
         no_result = sum(map_int(wins, \(w) sum(is.na(w$lambda)))))
}) |>
  list_rbind() |>
  split_run()

print(fit_log)

# ISI: in-strength (row sums, rows = receivers); OSI: out-strength; SRR and
# SRE weight both by market capitalisation shares at the window end
measures <- function(w) {
  A  <- abs(w$D)
  mc <- w$mc / sum(w$mc)
  tibble(abbr = abbr_of[colnames(A)],
         isi  = rowSums(A),
         osi  = colSums(A),
         srr  = mc * as.vector(A %*% mc),
         sre  = mc * as.vector(t(A) %*% mc))
}

sector_codes <- split(wang$code, wang$sector)

tc_all <- imap(all_runs, \(wins, run) {
  map(wins, \(w) {
    A <- abs(w$D)
    tibble(week = w$week, tc = sum(A),
           density = mean(A[row(A) != col(A)] > 0))
  }) |>
    list_rbind() |>
    mutate(run = run)
}) |>
  list_rbind() |>
  split_run()

iss_all <- imap(all_runs, \(wins, run) {
  map(wins, \(w) tibble(week   = w$week,
                        sector = names(sector_codes),
                        iss    = map_dbl(sector_codes,
                                         \(k) sum(abs(w$D[k, , drop = FALSE]))))) |>
    list_rbind() |>
    mutate(run = run)
}) |>
  list_rbind() |>
  split_run()

# 5a. Density and mean TC per run (Section 4 of the comment); Wang et al.
#     describe the tau = 0.05 network as too sparse for China
tc_all |>
  group_by(version, tau) |>
  summarise(mean_density = round(mean(density), 3), mean_tc = round(mean(tc), 2),
            .groups = "drop") |>
  print()

# 5b. Total connectedness relative to its own mean, by phase, tau = 0.01
#     (Online Appendix C).
#     Wang et al. report peaks from mid-2014 to the end of 2016. The first
#     window ends before their figures start and is dropped, as in 6a.
tc_phase <- tc_all |>
  filter(tau == 0.01, week > weeks[min(win_end)]) |>
  mutate(rel = tc / mean(tc), .by = version) |>
  mutate(phase = case_when(week < as.Date("2014-07-01") ~ "1 until mid-2014",
                           week < as.Date("2016-07-01") ~ "2 mid-2014 to mid-2016",
                           TRUE                         ~ "3 second half of 2016")) |>
  summarise(peak_week = week[which.max(rel)],
            peak      = round(max(rel), 2),
            rel       = round(mean(rel), 2),
            .by = c(version, phase)) |>
  arrange(version, phase)

print(tc_phase)

# ---- 6. Comparison with Wang et al. ------------------------------------------
# 6a. Their figures (extracted series, Online Appendix B)
read_ext <- function(f, panel) {
  p <- file.path(dir_ext, f)
  if (!file.exists(p)) {
    message("not found, skipped: ", p)
    return(NULL)
  }
  read_csv(p, show_col_types = FALSE) |>
    distinct(x, .keep_all = TRUE) |>
    arrange(x) |>
    transmute(panel = panel, x, y)
}

paper_series <- bind_rows(
  tibble(panel = character(), x = numeric(), y = numeric()),
  read_ext("wang2018_fig1_tc.csv",             "total"),
  read_ext("wang2018_fig2_iss_banks.csv",      "bank"),
  read_ext("wang2018_fig2_iss_securities.csv", "securities"),
  read_ext("wang2018_fig2_iss_insurers.csv",   "insurance")
)

# Peak window of each of their series (window end), to compare with the
# start of the stock market crash in mid-June 2015
paper_peaks <- paper_series |>
  slice_max(y, n = 1, by = panel) |>
  mutate(week = as.Date(lubridate::date_decimal(x)))

print(paper_peaks)

ours_long <- bind_rows(
  tc_all  |> transmute(panel = "total", week, value = tc, version, tau),
  iss_all |> transmute(panel = sector, week, value = iss, version, tau)
)

# Correlation of our series with theirs, interpolated at our window dates,
# per panel, version and tau (Table A4). Levels differ between estimators;
# the timing is what the data drive.
comparison <- ours_long |>
  mutate(x = lubridate::decimal_date(week)) |>
  group_by(panel) |>
  group_modify(\(d, key) {
    p <- filter(paper_series, panel == key$panel)
    if (nrow(p) < 2) return(tibble())
    d |>
      filter(x >= min(p$x), x <= max(p$x)) |>
      mutate(paper = approx(p$x, p$y, xout = x)$y) |>
      group_by(version, tau) |>
      summarise(weeks    = n(),
                pearson  = round(cor(value, paper), 3),
                spearman = round(cor(value, paper, method = "spearman"), 3),
                .groups = "drop")
  }) |>
  ungroup() |>
  arrange(tau, panel, version)

print(comparison, n = Inf)

# 6b. Case study, window ending 31 Dec 2014, all four runs against their
#     Tables 2-4 at tau = 0.01 (Table 1 of the comment).
#     edges_theirs: how many of their 10 strongest edges are among ours;
#     to_brokers: how many of our 10 strongest edges end at CJSC, PSC or
#     NESC (theirs: 9); isi ... sre: overlap of the top-10 lists.
case_study <- imap(all_runs, \(wins, run) {
  w <- keep(wins, \(w) w$week == snap_week)
  if (!length(w)) return(NULL)
  w <- w[[1]]
  A <- abs(w$D)
  dimnames(A) <- list(abbr_of[rownames(A)], abbr_of[colnames(A)])
  edges <- as.data.frame(as.table(A), stringsAsFactors = FALSE) |>
    set_names(c("to", "from", "d")) |>
    filter(d > 0)
  top_edges <- slice_max(edges, d, n = 10, with_ties = FALSE)
  m <- measures(w)
  m_long <- pivot_longer(m, -abbr, names_to = "measure", values_to = "ours")
  tibble(run          = run,
         edges_theirs = nrow(semi_join(top_edges, wang_edges, by = c("from", "to"))),
         to_brokers   = sum(top_edges$to %in% c("CJSC", "PSC", "NESC")),
         isi = length(intersect(slice_max(m, isi, n = 10)$abbr, wang_top$isi)),
         osi = length(intersect(slice_max(m, osi, n = 10)$abbr, wang_top$osi)),
         srr = length(intersect(slice_max(m, srr, n = 10)$abbr, wang_top$srr)),
         sre = length(intersect(slice_max(m, sre, n = 10)$abbr, wang_top$sre)),
         # rank correlation of their published values with ours for the
         # same institutions and edges
         rho_edges = cor(wang_edges$paper,
                         left_join(wang_edges, edges, by = c("from", "to"))$d |>
                           coalesce(0), method = "spearman"),
         rho_isi = with(inner_join(filter(paper_tab, measure == "isi"),
                                   filter(m_long, measure == "isi"),
                                   by = c("measure", "abbr")),
                        cor(paper, ours, method = "spearman")))
}) |>
  compact() |>
  list_rbind()

if (nrow(case_study) > 0) {
  case_study <- case_study |>
    split_run() |>
    mutate(across(starts_with("rho"), \(x) round(x, 3))) |>
    arrange(tau, version)
}
print(case_study, width = Inf)

#     ISI ranks of the four securities firms with an ex-date whose mechanical
#     component lies below -0.35 in the case-study window (NESC, CJSC, PSC,
#     SLSC) next to their rank in Wang et al. (Table 3)
broker_ranks <- imap(all_runs, \(wins, run) {
  w <- keep(wins, \(w) w$week == snap_week)
  if (!length(w)) return(NULL)
  measures(w[[1]]) |>
    mutate(run = run, rank_isi = min_rank(desc(isi))) |>
    filter(abbr %in% c("CJSC", "PSC", "NESC", "SLSC")) |>
    mutate(rank_paper = match(abbr, wang_top$isi)) |>
    select(run, abbr, isi, rank_isi, rank_paper)
}) |>
  compact() |>
  list_rbind() |>
  split_run() |>
  arrange(tau, version, rank_isi)

print(broker_ranks, n = Inf)

# 6c. Paired circular block bootstrap over window-end weeks (Politis and
#     Romano 1992). Neighbouring windows share ws of their ws + 1 weeks, so
#     one block is one window long. Each draw uses the same weeks for their
#     series, the unadjusted and the adjusted one.
#     d_pearson, d_spearman: unadjusted minus adjusted correlation with theirs
#     b_adj, b_ca: equation (A2), their series on adjusted and on
#     (unadjusted - adjusted), all standardised. Both in Table A4.
n_boot <- 10000
block  <- ws + 1

boot_data <- ours_long |>
  mutate(x = lubridate::decimal_date(week)) |>
  pivot_wider(id_cols = c(panel, tau, week, x),
              names_from = version, values_from = value) |>
  group_by(panel) |>
  group_modify(\(d, key) {
    p <- filter(paper_series, panel == key$panel)
    if (nrow(p) < 2) return(tibble())
    d |>
      filter(x >= min(p$x), x <= max(p$x)) |>
      mutate(paper = approx(p$x, p$y, xout = x)$y)
  }) |>
  ungroup() |>
  arrange(panel, tau, week)

# indices of one circular block draw of length n
cbb_index <- function(n, block) {
  starts <- sample.int(n, ceiling(n / block), replace = TRUE)
  idx    <- as.vector(outer(0:(block - 1), starts, `+`))
  ((idx - 1) %% n + 1)[seq_len(n)]
}

boot_stats <- function(d) {
  z   <- \(v) (v - mean(v)) / sd(v)
  y   <- z(d$paper)
  adj <- z(d$adjusted)
  ca  <- z(d$unadjusted - d$adjusted)
  b   <- coef(lm(y ~ adj + ca))
  c(d_pearson  = cor(d$unadjusted, d$paper) - cor(d$adjusted, d$paper),
    d_spearman = cor(d$unadjusted, d$paper, method = "spearman") -
      cor(d$adjusted,   d$paper, method = "spearman"),
    b_adj      = unname(b["adj"]),
    b_ca       = unname(b["ca"]))
}

set.seed(20261009)
boot_res <- boot_data |>
  group_by(panel, tau) |>
  group_modify(\(d, key) {
    est   <- boot_stats(d)
    draws <- t(replicate(n_boot, boot_stats(d[cbb_index(nrow(d), block), ])))
    tibble(stat     = names(est),
           estimate = round(est, 3),
           lo       = round(apply(draws, 2, quantile, 0.025), 3),
           hi       = round(apply(draws, 2, quantile, 0.975), 3),
           p_le0    = round(colMeans(draws <= 0), 3))
  }) |>
  ungroup()

print(boot_res, n = Inf)

# 6d. Implementation-free test, equation (A3) (Table A5). Their published
#     series are regressed on two variables built from raw data only, so the
#     result does not depend on our TENET estimates:
#       n_ca:   institutions of the panel with an ex-date whose mechanical
#               component is below the threshold within the 51 weeks of the
#               window
ca_thr    <- c(shade_min, -0.05)       # as in Figure 1 and in Section 2
sector_of <- set_names(wang$sector, wang$code)

ca_weeks <- events |>
  filter(code %in% codes) |>
  transmute(code, effect, sector = sector_of[code], week = week_of(date))

win_start <- function(end) weeks[match(end, weeks) - ws]

count_ca <- function(end, panel, thr) {
  d <- filter(ca_weeks, effect < thr, week >= win_start(end), week <= end)
  if (panel != "total") d <- filter(d, sector == panel)
  n_distinct(d$code)
}

stress_of <- function(end) {
  r <- state$ret_mkt[state$week >= win_start(end) & state$week <= end]
  -min(r, na.rm = TRUE)
}

ca_data <- expand_grid(thr   = ca_thr,
                       panel = c("total", "bank", "securities", "insurance"),
                       week  = weeks[win_end]) |>
  mutate(n_ca   = pmap_int(list(week, panel, thr), count_ca),
         stress = map_dbl(week, stress_of),
         x      = lubridate::decimal_date(week)) |>
  group_by(panel) |>
  group_modify(\(d, key) {
    p <- filter(paper_series, panel == key$panel)
    if (nrow(p) < 2) return(tibble())
    d |>
      filter(x >= min(p$x), x <= max(p$x)) |>
      mutate(paper = approx(p$x, p$y, xout = x)$y)
  }) |>
  ungroup() |>
  arrange(thr, panel, week)

ca_stats <- function(d) {
  out <- set_names(rep(NA_real_, 5),
                   c("r_ca", "r_stress", "g_ca", "g_stress", "dr2_ca"))
  # a draw without any window with a large corporate action has a constant
  # n_ca and cannot be standardised; it is counted in na_share instead
  if (sd(d$n_ca) == 0 || sd(d$stress) == 0) return(out)
  z  <- \(v) (v - mean(v)) / sd(v)
  y  <- z(d$paper)
  n  <- z(d$n_ca)
  s  <- z(d$stress)
  m1 <- lm(y ~ s)
  m2 <- lm(y ~ n + s)
  out[] <- c(cor(d$paper, d$n_ca),
             cor(d$paper, d$stress),
             unname(coef(m2)["n"]),
             unname(coef(m2)["s"]),
             summary(m2)$r.squared - summary(m1)$r.squared)
  out
}


set.seed(20261104)
ca_res <- ca_data |>
  group_by(thr, panel) |>
  group_modify(\(d, key) {
    est   <- ca_stats(d)
    draws <- t(replicate(n_boot, suppressWarnings(
      ca_stats(d[cbb_index(nrow(d), block), ]))))
    tibble(stat     = names(est),
           estimate = round(est, 3),
           lo       = round(apply(draws, 2, quantile, 0.025, na.rm = TRUE), 3),
           hi       = round(apply(draws, 2, quantile, 0.975, na.rm = TRUE), 3),
           na_share = round(colMeans(is.na(draws)), 3))
  }) |>
  ungroup()

print(ca_res, n = Inf)

# ---- 7. Figures --------------------------------------------------------------
# Labels and colours shared by both figures
panel_levels <- c(total      = "Total connectedness (their Fig. 1)",
                  bank       = "Banks, in-strength (their Fig. 2)",
                  securities = "Securities, in-strength (their Fig. 2)",
                  insurance  = "Insurers, in-strength (their Fig. 2)")
series_levels <- c("Wang et al. (2018)", "BaoStock unadjusted", "BaoStock adjusted")
series_cols   <- set_names(c("black", "#1b4f72", "#e67e22"), series_levels)

# 7a. Figure A1: levels, tau = 0.01, TC and ISS by sector, both versions and
#     theirs
levels_plot <- bind_rows(
  ours_long |> filter(tau == 0.01) |>
    transmute(panel, week, value, series = paste("BaoStock", version)),
  paper_series |>
    transmute(panel, week = as.Date(lubridate::date_decimal(x)), value = y,
              series = "Wang et al. (2018)")
) |>
  mutate(panel = factor(panel, names(panel_levels)))

p_levels <- ggplot(levels_plot, aes(week, value, colour = series)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y",
             labeller = as_labeller(panel_levels)) +
  scale_colour_manual(values = series_cols, breaks = series_levels) +
  labs(x = NULL, y = NULL, colour = NULL,
       title = "TENET, tau = 0.01: total connectedness and in-strength by sector") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

print(p_levels)

# 7b. Figure 1 of the comment: shapes instead of levels, tau = 0.01. Every
#     series is divided by its own mean over the period all series in its
#     panel cover. Shaded: windows containing a large corporate action
#     (mechanical component below shade_min; a pure bonus or capitalisation
#     issue reaches it from about 0.42 new shares per share) of an
#     institution in that panel, from the ex-date week until it leaves the
#     window ws weeks later. Label: the institution.
rng <- levels_plot |>
  group_by(panel, series) |>
  summarise(lo = min(week), hi = max(week), .groups = "drop") |>
  group_by(panel) |>
  summarise(lo = max(lo), hi = min(hi))

norm_data <- levels_plot |>
  inner_join(rng, by = "panel") |>
  filter(week >= lo, week <= hi) |>
  group_by(panel, series) |>
  mutate(value = value / mean(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(panel  = factor(panel_levels[as.character(panel)], levels = panel_levels),
         series = factor(series, levels = series_levels))

code_sector <- set_names(wang$sector, wang$code)
shade_raw <- events |>
  filter(effect < shade_min, code %in% codes) |>
  transmute(abbr   = abbr_of[code],
            sector = code_sector[code],
            start  = floor_date(date, "week", week_start = 1),
            end    = start + weeks(ws))

shade <- bind_rows(mutate(shade_raw, panel = sector),
                   mutate(shade_raw, panel = "total")) |>
  mutate(panel = factor(panel, levels(levels_plot$panel))) |>
  inner_join(rng, by = "panel") |>
  filter(end >= lo, start <= hi) |>
  mutate(start = pmax(start, lo),
         end   = pmin(end, hi),
         panel = factor(panel_levels[as.character(panel)], levels = panel_levels))

shade |> distinct(abbr, start) |> arrange(start) |> print(n = Inf)

p_norm <- ggplot(norm_data, aes(week, value, colour = series)) +
  geom_rect(data = shade, inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "grey50", alpha = 0.10) +
  geom_text(data = shade, inherit.aes = FALSE,
            aes(x = start, y = Inf, label = abbr),
            hjust = -0.1, vjust = 1.4, size = 2.3, colour = "grey30",
            check_overlap = TRUE) +
  geom_hline(yintercept = 1, linewidth = 0.2, colour = "grey60") +
  geom_line(linewidth = 0.45) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = series_cols, breaks = series_levels) +
  labs(x = NULL, y = "Relative to own mean (mean = 1)", colour = NULL,
       title = "Connectedness follows the windows with bonus-share weeks",
       subtitle = paste0("TENET, tau = 0.01, 51-week windows, each series relative to its mean.\n",
                         "Shaded: windows containing a bonus-share ex-date (label: institution)")) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0))

print(p_norm)

# ---- 8. Save -----------------------------------------------------------------
saveRDS(list(tc = tc_all, iss = iss_all, case_study = case_study,
             broker_ranks = broker_ranks, tc_phase = tc_phase,
             paper_peaks = paper_peaks, ca_res = ca_res,
             comparison = comparison, boot = boot_res,
             wang = list(edges = wang_edges, top = wang_top, tab = paper_tab)),
        file.path(here("data", "processed"), "tenet_summary.rds"))

# plot objects, so that c99 builds the figures of the comment from them
saveRDS(list(norm = p_norm, levels = p_levels),
        file.path(here("data", "processed"), "c05_plots.rds"))

write_csv(case_study,   file.path(dir_out, "c05_case_study.csv"))
write_csv(broker_ranks, file.path(dir_out, "c05_broker_ranks.csv"))
write_csv(tc_phase,     file.path(dir_out, "c05_tc_phase.csv"))
write_csv(boot_res, file.path(dir_out, "c05_bootstrap.csv"))
write_csv(comparison, file.path(dir_out, "c05_comparison.csv"))
write_csv(paper_peaks, file.path(dir_out, "c05_paper_peaks.csv"))
write_csv(ca_res, file.path(dir_out, "c05_ca_test.csv"))
ggsave(file.path(dir_out, "c05_levels.png"), p_levels,
       width = 17, height = 20, units = "cm", dpi = 300, bg = "white")
ggsave(file.path(dir_out, "c05_normalised.png"), p_norm,
       width = 17, height = 20, units = "cm", dpi = 300, bg = "white")


