# Comment on Wang, Jiang, Lin, Xie and Stanley (2018): "Interconnectedness and
# systemic risk of China's financial institutions", Emerging Markets Review
# 35, 1-18.
# Leonard Nikita Baur, Chemnitz University of Technology

# c02_returns builds weekly log returns from the BaoStock prices of c01, in
# both versions, and lists every corporate action with its effect on the
# unadjusted return. Checks, reported in Section 2 and Online Appendix A of
# the comment:
#   1. internal: unadjusted minus adjusted weekly return must equal the summed
#      effect of the corporate actions in that week, equation (A1) (section 4)
#   2. Table 1 of Wang et al.: mean, maximum, minimum, standard deviation and
#      Jarque-Bera per institution vs. ours, unadjusted and adjusted, all 24
#      institutions, 2008-2016, Table A2 (section 5)
# plus the price-limit bound of equation (2) and the weeks of the reported
# minima (sections 4b and 4c).
#
# Weeks: closing price of the last trading day of each week (Monday to
# Friday). A week without trading carries the last price forward, so its
# return is 0 and the price change shows up in the week trading resumes.
# This reproduces the weekly series of Wang et al. (check 2).
#
# Input:  data/raw/bs_prices.rds, bs_index.rds, bs_dividends.rds (c01)
# Output: data/processed/bs_weekly.rds   weekly close and return, long format
#         data/processed/bs_events.rds   corporate actions with their effect
#         output/table1_check.csv        check 2, for Table A1 (c99)
#         output/table1_matches.csv      check 2, Table A2 (c99)

# ---- 0. Packages, paths, data ------------------------------------------------
library(tidyverse)
library(here)

dir_proc <- here("data", "processed")
dir_out  <- here("output")
dir.create(dir_proc, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out,  recursive = TRUE, showWarnings = FALSE)

sample_start <- as.Date("2008-01-01")    # Wang et al.: weekly returns 2008-2016
sample_end   <- as.Date("2016-12-31")
limit_week   <- 5 * log(0.9)             # five-session bound, equation (2): -0.5268

prices    <- readRDS(here("data", "raw", "bs_prices.rds"))
index     <- readRDS(here("data", "raw", "bs_index.rds"))
dividends <- readRDS(here("data", "raw", "bs_dividends.rds"))

# Monday of the week a date falls in
week_of <- function(d) floor_date(d, "week", week_start = 1)

# BaoStock text columns to numbers; empty strings become NA
num <- function(x) suppressWarnings(as.numeric(x))

# ---- 1. Week grid ------------------------------------------------------------
# Every week in which the CSI 300 traded on at least one day. Weeks without a
# single trading day (some Spring Festival and National Day weeks) drop out,
# so the return is measured across the holiday, as in a WIND weekly series.
weeks <- index |>
  filter(!is.na(close)) |>
  distinct(week = week_of(date)) |>
  arrange(week)

# ---- 2. Weekly closing prices and returns ------------------------------------
# 2a. Last close per institution, version and week, from trading days only
#     (tradestatus 1). n_days counts the trading days in the week.
weekly_raw <- prices |>
  filter(tradestatus == 1, close > 0) |>
  mutate(week = week_of(date)) |>
  group_by(abbr, code, version, week) |>
  summarise(close  = last(close, order_by = date),
            n_days = n(),
            .groups = "drop")

# 2b. Every grid week from an institution's first traded week on. Weeks
#     without trading get NA first, then the last close (fill), i.e. a
#     return of 0. The first week has no return (lag is NA).
weekly <- weekly_raw |>
  group_by(abbr, code, version) |>
  summarise(first = min(week), .groups = "drop") |>
  cross_join(weeks) |>
  filter(week >= first) |>
  select(-first) |>
  left_join(weekly_raw, by = c("abbr", "code", "version", "week")) |>
  arrange(abbr, version, week) |>
  group_by(abbr, code, version) |>
  fill(close, .direction = "down") |>
  mutate(n_days = coalesce(n_days, 0L),
         ret    = log(close / lag(close))) |>
  ungroup()

# ---- 3. Corporate actions and their effect -----------------------------------
# The backward-adjusted price is the unadjusted price times an adjustment
# factor F. F rises on every ex-date, so the ratio k = adjusted / unadjusted
# jumps there, and the unadjusted return is lower by exactly that jump, the
# mechanical component of equation (A1), ln(P_ref / P_{e-1}):
#   effect = -(log k_t - log k_{t-1})
# Every trading day with |effect| > 0.0005 counts as a corporate action.
daily <- prices |>
  filter(tradestatus == 1, close > 0) |>
  select(abbr, code, version, date, close) |>
  pivot_wider(names_from = version, values_from = close) |>
  filter(!is.na(adjusted), !is.na(unadjusted)) |>
  arrange(abbr, date) |>
  group_by(abbr) |>
  mutate(prev_date = lag(date),
         p_prev    = lag(unadjusted),            # last raw close before the day
         log_k     = log(adjusted / unadjusted),
         effect    = -(log_k - lag(log_k))) |>
  ungroup()

# What BaoStock's dividend table says happened: cash per share before tax,
# bonus shares per share (from profits, dividStocksPs) and capitalisation
# shares per share (from the capital reserve, dividReserveToStockPs). Rights
# issues are not in this table.
div_info <- dividends |>
  transmute(abbr,
            ex_date = as.Date(na_if(dividOperateDate, "")),
            cash    = num(dividCashPsBeforeTax),
            bonus   = coalesce(num(dividStocksPs), 0) +
              coalesce(num(dividReserveToStockPs), 0)) |>
  filter(!is.na(ex_date)) |>
  distinct(abbr, ex_date, .keep_all = TRUE)

# Match each price jump to the latest ex-date on or before that day. If the
# ex-date fell into a suspension, the jump shows on the first trading day
# after it, so the ex-date must lie after the previous trading day.
# effect_div recomputes the effect from the dividend table, equation (A1)
# without rights issues (R = 0):
#   log(1 / (1 + bonus)) + log(1 - cash / p_prev)
# The type "bonus shares" covers bonus and capitalisation issues.
events <- daily |>
  filter(abs(effect) > 5e-4) |>
  left_join(div_info, by = join_by(abbr, closest(date >= ex_date))) |>
  mutate(matched = !is.na(ex_date) & ex_date > prev_date,
         across(c(ex_date, cash, bonus), \(x) if_else(matched, x, NA)),
         effect_div = log(1 / (1 + bonus)) + log(1 - coalesce(cash, 0) / p_prev),
         type = case_when(!matched  ~ "not in dividend table (e.g. rights issue)",
                          bonus > 0 ~ "bonus shares",
                          TRUE      ~ "cash dividend")) |>
  select(abbr, code, date, ex_date, type, bonus, cash, p_prev,
         effect, effect_div)

# Count by type, and how well the dividend table explains the price jumps
# (max_gap is NA for jumps without an entry in the dividend table)
events |>
  group_by(type) |>
  summarise(n = n(),
            max_gap = if (all(is.na(effect_div))) NA_real_ else
              max(abs(effect - effect_div), na.rm = TRUE)) |>
  print()

# The list of large actions 2008-2016 (effect below -0.05: bonus and
# capitalisation issues, rights issues): the weeks in which an unadjusted
# return falls by the mechanical component, without any loss to investors
events |>
  filter(effect < -0.05, date >= sample_start) |>
  arrange(date) |>
  mutate(across(c(effect, effect_div), \(x) round(x, 4))) |>
  print(n = Inf)

# ---- 4. Check 1: internal consistency ----------------------------------------
# Unadjusted minus adjusted weekly return should equal the summed effect of
# the actions in that week. Both sides come from the same adjustment ratio k,
# so this holds by construction unless k also moves on days below the 0.0005
# threshold. max_abs_resid at machine precision (around 1e-15) shows it does
# not: the whole gap between the two versions sits on the detected event
# days. Whether BaoStock's dividend table explains those jumps is shown by
# max_gap in section 3.
ev_week <- events |>
  mutate(week = week_of(date)) |>
  group_by(abbr, week) |>
  summarise(effect = sum(effect), .groups = "drop")

check1 <- weekly |>
  select(abbr, code, week, version, ret) |>
  pivot_wider(names_from = version, values_from = ret, names_prefix = "ret_") |>
  left_join(ev_week, by = c("abbr", "week")) |>
  mutate(effect = coalesce(effect, 0),
         resid  = ret_unadjusted - ret_adjusted - effect)

check1 |>
  summarise(weeks = sum(!is.na(resid)),
            max_abs_resid = max(abs(resid), na.rm = TRUE)) |>
  print()

check1 |>
  slice_max(abs(resid), n = 5) |>
  print()

# ---- 4b. Price-limit bound ---------------------------------------------------
# Equation (2) of the comment.
# Under the daily limit of 10 %, the return over k trading days cannot fall
# below k * log(0.9). k is n_days, the institution's own trading days in the
# week: after a suspension, only the days it traded count. Limit prices are
# rounded to CNY 0.01, which can put a single day marginally below log(0.9);
# slack allows 0.003 per trading day for that.
slack <- 0.003

bound_check <- weekly |>
  filter(week > week_of(sample_start), week <= sample_end, !is.na(ret)) |>
  left_join(ev_week, by = c("abbr", "week")) |>
  mutate(effect   = coalesce(effect, 0),
         bound    = n_days * log(0.9),
         violates = ret < bound - slack * n_days,
         # return without the mechanical component, equation (A1); only
         # meaningful for the unadjusted version
         ret_net  = if_else(version == "unadjusted", ret - effect, NA_real_))

# N for the text (Section 2): violating institution-weeks per version. The
# comment reports nine unadjusted and none of the 11,040 adjusted.
bound_check |>
  count(version, violates) |>
  print()

# Every violating week: the eight minima below the bound in Table A1 and
# SLSC in the week of 1 June 2009. For the unadjusted version, ret_net must
# lie above the bound: the violation is then fully explained by a corporate
# action.
bound_check |>
  filter(violates) |>
  arrange(version, ret) |>
  select(abbr, version, week, n_days, ret, bound, effect, ret_net) |>
  print(n = Inf)

# ---- 4c. Reported minima and corporate actions -------------------------------
# For each institution: the week of its unadjusted minimum (= Wang et al.'s
# minimum), the effect of corporate actions in that week and the return net
# of it, i.e. what the week looked like without the mechanical component.
min_weeks <- bound_check |>
  filter(version == "unadjusted") |>
  group_by(abbr) |>
  slice_min(ret, n = 1, with_ties = FALSE) |>
  ungroup() |>
  left_join(events |>
              mutate(week = week_of(date)) |>
              group_by(abbr, week) |>
              summarise(types = paste(sort(unique(type)), collapse = " + "),
                        .groups = "drop"),
            by = c("abbr", "week")) |>
  arrange(ret) |>
  select(abbr, week, n_days, ret, bound, effect, ret_net, types)

print(min_weeks, n = Inf)

# Minima whose week contains corporate actions of more than 0.05 log points,
# how many of them include a bonus or capitalisation issue and the range of
# their net returns (Section 2 of the comment: 15 of 24, between -0.19 and
# +0.01)
min_weeks |>
  filter(effect < -0.05) |>
  summarise(n       = n(),
            n_bonus = sum(str_detect(types, "bonus"), na.rm = TRUE),
            net_min = min(ret_net), net_max = max(ret_net)) |>
  print()

# ---- 5. Check 2: Table 1 of Wang et al. --------------------------------------
# Their descriptive statistics of weekly returns 2008-2016 (Table 1): mean,
# maximum, minimum, standard deviation and Jarque-Bera, with their
# abbreviations. We compute the same statistics over the same weeks, once
# unadjusted and once adjusted, and count per statistic how many of the 24
# institutions each version reproduces (Table A2 of the comment).
table1 <- tribble(
  ~abbr_paper, ~code,    ~mean,   ~max,   ~min,    ~sd,    ~jb,
  "PAB",       "000001", -0.0031, 0.2225, -0.5246, 0.0650,  2636.271,
  "NBCB",      "002142", -0.0006, 0.2170, -0.2905, 0.0538,   220.7804,
  "SPDB",      "600000", -0.0026, 0.2187, -0.3935, 0.0605,   962.1555,
  "HXB",       "600015", -0.0013, 0.2095, -0.3365, 0.0554,   336.6739,
  "CMBC",      "600016", -0.0011, 0.2152, -0.2834, 0.0523,   466.8645,
  "CMB",       "600036", -0.0017, 0.1579, -0.2415, 0.0498,   106.5210,
  "NJBK",      "601009", -0.0013, 0.1750, -0.6294, 0.0575, 19282.96,
  "CIB",       "601166", -0.0025, 0.1978, -0.6575, 0.0679, 10527.92,
  "BOB",       "601169", -0.0017, 0.1990, -0.2705, 0.0504,   280.9081,
  "BOCOM",     "601328", -0.0021, 0.1868, -0.2074, 0.0483,   135.9020,
  "ICBC",      "601398", -0.0013, 0.1572, -0.1462, 0.0357,   212.8555,
  "CCB",       "601939", -0.0013, 0.1929, -0.1405, 0.0403,   188.9955,
  "BOC",       "601988", -0.0014, 0.2150, -0.1628, 0.0377,   326.2409,
  "CNCB",      "601998", -0.0010, 0.2891, -0.1921, 0.0517,   329.7216,
  "NESC",      "000686", -0.0031, 0.3503, -0.7389, 0.0821,  4371.845,
  "GYSC",      "000728", -0.0017, 0.4121, -0.2367, 0.0715,   268.2221,
  "CJSC",      "000783", -0.0029, 0.3273, -0.7580, 0.0807,  5934.358,
  "CITICS",    "600030", -0.0037, 0.3255, -0.4173, 0.0709,   876.8093,
  "SLSC",      "600109", -0.0036, 0.3814, -0.7276, 0.0882,  4804.728,
  "HTSEC",     "600837", -0.0027, 0.3333, -0.8821, 0.0816, 18945.82,
  "PSC",       "601099", -0.0046, 0.4368, -0.4014, 0.0739,  1036.749,
  "PAI",       "601318", -0.0023, 0.1931, -0.8821, 0.0666, 83396.99,
  "CPIC",      "601601", -0.0012, 0.1818, -0.2120, 0.0547,    15.53332,
  "CLI",       "601628", -0.0019, 0.2719, -0.2425, 0.0561,   268.3861
)

# Jarque-Bera: n/6 * (S^2 + (K - 3)^2 / 4), with skewness S and kurtosis K
# from the population moments
jarque_bera <- function(x) {
  m  <- x - mean(x)
  s2 <- mean(m^2)
  S  <- mean(m^3) / s2^1.5
  K  <- mean(m^4) / s2^2
  length(x) / 6 * (S^2 + (K - 3)^2 / 4)
}

ours <- weekly |>
  filter(week > week_of(sample_start), week <= sample_end, !is.na(ret)) |>
  group_by(code, version) |>
  summarise(n_weeks  = n(),
            week_min = week[which.min(ret)],
            mean = mean(ret), max = max(ret), min = min(ret),
            sd   = sd(ret),   jb  = jarque_bera(ret),
            .groups = "drop")

# Matching rule: Table 1 rounds mean, maximum, minimum and SD to four
# decimals and prints Jarque-Bera with seven significant digits. A value
# matches if ours, rounded the same way, equals theirs. 1e-9 only absorbs
# floating-point noise.
is_match <- function(stat, ours, paper) {
  if_else(stat == "jb",
          abs(signif(ours, 7) - paper) < 1e-9 * abs(paper),
          abs(round(ours, 4) - paper) < 1e-9)
}

check2 <- table1 |>
  pivot_longer(c(mean, max, min, sd, jb), names_to = "stat", values_to = "paper") |>
  left_join(ours |>
              pivot_longer(c(mean, max, min, sd, jb),
                           names_to = "stat", values_to = "ours"),
            by = c("code", "stat"), relationship = "one-to-many") |>
  mutate(match = is_match(stat, ours, paper))

# 5a. The key result, Table A2: matches per statistic out of 24, by version
#     (117 of 120 unadjusted, 30 adjusted). If the unadjusted column is close
#     to 24 everywhere and the adjusted one is not, Wang et al. used
#     unadjusted prices.
matches <- check2 |>
  filter(!is.na(version)) |>
  group_by(stat = factor(stat, c("mean", "max", "min", "sd", "jb")), version) |>
  summarise(matches = sum(match, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = version, values_from = matches) |>
  select(stat, unadjusted, adjusted)

print(matches)
write_csv(matches, file.path(dir_out, "table1_matches.csv"))

# 5b. Institutions where the unadjusted version misses a statistic, with
#     both of our values next to theirs
check2 |>
  filter(!is.na(version)) |>
  select(abbr_paper, code, stat, paper, version, ours) |>
  pivot_wider(names_from = version, values_from = ours) |>
  select(abbr_paper, code, stat, paper, unadjusted, adjusted) |>
  filter(!is_match(stat, unadjusted, paper)) |>
  mutate(across(c(paper, unadjusted, adjusted),
                \(x) if_else(stat == "jb", round(x, 1), round(x, 4)))) |>
  print(n = Inf)

# 5c. Institutions missing from BaoStock (no values in either version)
anti_join(table1, ours, by = "code") |>
  select(abbr_paper, code) |>
  print(n = Inf)

# 5d. Full comparison in one row per institution, for Table A1 (c99):
#     paper / unadjusted / adjusted for every statistic, plus the week of
#     the unadjusted minimum and the number of weeks
check2 |>
  filter(!is.na(version)) |>
  select(abbr_paper, code, stat, paper, version, ours) |>
  pivot_wider(names_from = version, values_from = ours) |>
  pivot_wider(names_from = stat, values_from = c(paper, unadjusted, adjusted),
              names_glue = "{stat}_{.value}") |>
  left_join(ours |>
              filter(version == "unadjusted") |>
              select(code, n_weeks, week_min_unadj = week_min),
            by = "code") |>
  write_csv(file.path(dir_out, "table1_check.csv"))


# ---- 6. Save -----------------------------------------------------------------
saveRDS(weekly, file.path(dir_proc, "bs_weekly.rds"))
saveRDS(events, file.path(dir_proc, "bs_events.rds"))
