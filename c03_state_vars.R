# Comment on Wang, Jiang, Lin, Xie and Stanley (2018): "Interconnectedness and
# systemic risk of China's financial institutions", Emerging Markets Review
# 35, 1-18.
# Leonard Nikita Baur, Chemnitz University of Technology

# c03_state_vars builds the weekly macro state variables for TENET from public
# sources, 2007-2016. TENET regresses each institution's return on these
# variables lagged by one week; the lag is applied in c05, not here.
#
# The seven variables of Wang et al. (2018, Section 3), following Adrian and
# Brunnermeier (2016) and Haerdle, Wang and Yu (2016); Table A3 of the
# comment compares them with theirs:
#   liq_spread  (i)   3-month SHIBOR minus 3-month treasury yield
#   d_tb3m      (ii)  change in the 3-month treasury yield
#   d_slope     (iii) change in the slope, 10-year minus 3-month treasury
#   d_credit    (iv)  change in the credit spread, AAA yield minus treasury
#                     yield (see the deviation below)
#   ret_mkt     (v)   weekly CSI 300 log return
#   vol_mkt     (vi)  conditional variance of the CSI 300 returns from a
#                     GARCH(1,1), here estimated on the weekly returns
#   ret_re      (vii) weekly log return of the CSI 300 Real Estate Index
# Rates in percent, returns as weekly log returns. Weekly values are taken on
# the last trading day of the week, on the same week grid as c02.
#
# Deviation from Wang et al.: their credit spread is the ten-year ChinaBond
# AAA corporate bond yield minus the ten-year treasury yield. The public
# ChinaBond query has no corporate bond curve, and its AAA curves carry no
# ten-year yield before 2010 (check 3b). The default here is the one-year
# AAA medium-term note yield minus the one-year treasury yield, the only AAA
# spread available for the whole sample. credit_curve and credit_tenor in
# section 0 switch it. Unadjusted and adjusted TENET runs in c05 use the same
# state variables, so the choice does not touch their comparison.
#
# Sources, all public and free:
#   CSI 300                      BaoStock, via c01
#   CSI 300 Real Estate Index    China Securities Index Co., the index
#                                provider (csindex.com.cn), until early
#                                2010, BaoStock thereafter (BaoStock serves
#                                it from October 2009); spliced in section 4
#   SHIBOR 3M                    Eastmoney data centre (the data behind
#                                AkShare's rate_interbank), daily from 2006
#   treasury and AAA MTN curves  ChinaBond, yield.chinabond.com.cn (the data
#                                behind AkShare's bond_china_yield)
# The downloads run from R (httr), without Python. They are saved to
# data/raw and reused on later runs; set refresh <- TRUE to download again.
#
# Input:  data/raw/bs_index.rds, data/raw/bs_index_re.rds (c01)
# Output: data/raw/pub_shibor.rds         SHIBOR 3M, daily
#         data/raw/pub_chinabond.rds      treasury and AAA curves, daily
#         data/raw/pub_csi300_re.rds      CSI 300 Real Estate Index from the
#                                         provider, daily, 2007 to early 2010
#         data/processed/state_vars.rds   weekly state variables
#         output/state_vars_coverage.csv  coverage and summary per variable
#         output/state_vars.png           the seven series, 2008-2016

# ---- 0. Packages, paths, settings --------------------------------------------
library(tidyverse)
library(here)
library(httr)
library(jsonlite)
library(rvest)
# fGarch is called with fGarch:: only; attaching it would mask dplyr verbs

dir_raw  <- here("data", "raw")
dir_proc <- here("data", "processed")
dir_out  <- here("output")
dir.create(dir_proc, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out,  recursive = TRUE, showWarnings = FALSE)

start_date   <- as.Date("2007-01-01")   # download range, as in c01
end_date     <- as.Date("2016-12-31")
sample_start <- as.Date("2008-01-01")   # Wang et al.: weekly data 2008-2016

refresh      <- FALSE      # TRUE: download SHIBOR and ChinaBond again
credit_curve <- "mtn_aaa"  # AAA curve for the credit spread: "mtn_aaa"
                        # (medium-term notes) or "bank_aaa" (commercial
                        # bank bonds); see check 3b
credit_tenor <- "1Y"       # tenor of the credit spread, same for both legs
re_index     <- "csi300_re_spliced"  # CSI 300 Real Estate, as in Wang et
                        # al.; see section 4

ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")

index    <- readRDS(here("data", "raw", "bs_index.rds"))
index_re <- readRDS(here("data", "raw", "bs_index_re.rds"))

# Monday of the week a date falls in (same as c02)
week_of <- function(d) floor_date(d, "week", week_start = 1)

# GET with up to three tries; NULL if all of them fail, with the last HTTP
# status (or the connection error) as a message
get_retry <- function(url, query, tries = 3) {
  why <- NA
  for (i in seq_len(tries)) {
    r <- tryCatch(GET(url, query = query, ua, timeout(30)),
                  error = function(e) conditionMessage(e))
    if (inherits(r, "response") && status_code(r) == 200) return(r)
    why <- if (inherits(r, "response")) paste("HTTP", status_code(r)) else r
    Sys.sleep(5 * i)
  }
  message("  failed after ", tries, " tries: ", why)
  NULL
}

# ---- 1. Week grid ------------------------------------------------------------
# Every week in which the CSI 300 traded on at least one day, as in c02
weeks <- index |>
  filter(!is.na(close)) |>
  distinct(week = week_of(date)) |>
  arrange(week)

# Last daily value of a column in each grid week. A week without a value
# carries the previous one forward, so its change is 0 and the move shows up
# in the next week with data. At most max_fill weeks in a row are carried
# forward; a longer gap stays NA, so a missing stretch of data shows up in
# check 6a instead of turning into a run of zero changes.
max_fill <- 2

weekly_last <- function(df, col) {
  df |>
    filter(!is.na(.data[[col]])) |>
    mutate(week = week_of(date)) |>
    group_by(week) |>
    summarise(value = last(.data[[col]], order_by = date), .groups = "drop") |>
    right_join(weeks, by = "week") |>
    arrange(week) |>
    mutate(since = sequence(rle(cumsum(!is.na(value)))$lengths) - 1) |>
    fill(value, .direction = "down") |>
    mutate(value = if_else(since > max_fill, NA_real_, value)) |>
    select(week, !!col := value)
}

# ---- 2. SHIBOR 3M ------------------------------------------------------------
# Eastmoney serves the full history in pages of 500 days. MARKET_CODE 001 is
# the Shanghai interbank market, INDICATOR_ID 203 the 3-month tenor.
shibor_file <- file.path(dir_raw, "pub_shibor.rds")

parse_shibor <- function(json_text) {
  res <- fromJSON(json_text)$result
  if (is.null(res)) stop("Eastmoney returned no SHIBOR data")
  res
}

pull_shibor_page <- function(page) {
  r <- get_retry(
    "https://datacenter-web.eastmoney.com/api/data/v1/get",
    query = list(
      reportName  = "RPT_IMP_INTRESTRATEN",
      columns     = paste0("REPORT_DATE,REPORT_PERIOD,IR_RATE,CHANGE_RATE,",
                           "INDICATOR_ID,LATEST_RECORD,MARKET,MARKET_CODE,",
                           "CURRENCY,CURRENCY_CODE"),
      filter      = '(MARKET_CODE="001")(CURRENCY_CODE="CNY")(INDICATOR_ID="203")',
      pageNumber  = page, pageSize = 500,
      sortTypes   = 1, sortColumns = "REPORT_DATE",
      source      = "WEB", client = "WEB"
    )
  )
  if (is.null(r)) stop("SHIBOR page ", page, " could not be downloaded")
  parse_shibor(content(r, as = "text", encoding = "UTF-8"))
}

if (refresh || !file.exists(shibor_file)) {
  first  <- pull_shibor_page(1)
  rest   <- map(seq_len(first$pages)[-1], \(p) {
    Sys.sleep(0.5)
    pull_shibor_page(p)$data
  })
  shibor <- bind_rows(c(list(first$data), rest)) |>
    as_tibble() |>
    transmute(date     = as.Date(substr(REPORT_DATE, 1, 10)),
              shibor3m = as.numeric(IR_RATE)) |>
    filter(between(date, start_date, end_date)) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)
  saveRDS(shibor, shibor_file)
}
shibor <- readRDS(shibor_file)

# Range, number of days, and the share of days quoted with more than two decimals
shibor |>
  summarise(days = n(), first = min(date), last = max(date),
            min = min(shibor3m), max = max(shibor3m),
            share_4_decimals = mean(abs(shibor3m * 100 - round(shibor3m * 100)) > 1e-6)) |>
  print()

# ---- 3. ChinaBond yield curves -----------------------------------------------
# historyQuery returns the treasury curve and the AAA medium-term note curve
# (from 2010 also the AAA commercial bank bond curve) for every day of the
# requested range; one request must cover less than a year, so the download
# runs quarter by quarter. Curve names are matched on their Chinese
# characters (the script is saved as UTF-8):
#   国债  = guozhai (treasury)
#   票据  = piaoju (notes, in the medium-term note curve)
#   银行  = yinhang (bank, in the commercial bank bond curve)
cb_file <- file.path(dir_raw, "pub_chinabond.rds")
tenors  <- c("3M", "6M", "1Y", "3Y", "5Y", "7Y", "10Y", "30Y")

parse_chinabond <- function(html_text) {
  tabs <- html_text |>
    str_replace_all("&nbsp;?", "") |>
    read_html() |>
    html_table(header = TRUE, convert = FALSE)
  tabs <- keep(tabs, \(t) ncol(t) == 10)
  if (length(tabs) == 0) return(tibble())
  tab <- tabs[[which.max(map_int(tabs, nrow))]]
  names(tab) <- c("curve_name", "date", tenors)
  tab |>
    as_tibble() |>
    mutate(date  = as.Date(date),
           across(all_of(tenors), \(x) suppressWarnings(as.numeric(x))),
           curve = case_when(
             str_detect(curve_name, "AAA") &
               str_detect(curve_name, "票据") ~ "mtn_aaa",
             str_detect(curve_name, "AAA") &
               str_detect(curve_name, "银行") ~ "bank_aaa",
             str_detect(curve_name, "国债")   ~ "treasury",
             TRUE                                     ~ "other")) |>
    filter(!is.na(date))
}

pull_chinabond <- function(from, to) {
  r <- get_retry(
    "https://yield.chinabond.com.cn/cbweb-pbc-web/pbc/historyQuery",
    query = list(startDate = format(from), endDate = format(to),
                 gjqx = 0, qxId = "ycqx", locale = "cn_ZH")
  )
  if (is.null(r)) {
    warning("ChinaBond ", from, " to ", to, " failed", call. = FALSE)
    return(tibble())
  }
  parse_chinabond(content(r, as = "text", encoding = "UTF-8"))
}

quarters <- tibble(from = seq(floor_date(start_date, "quarter"), end_date,
                              by = "quarter")) |>
  mutate(to = pmin(from + months(3) - days(1), end_date))

if (refresh || !file.exists(cb_file)) {
  chinabond <- quarters |>
    mutate(data = map2(from, to, \(f, t) {
      Sys.sleep(1)
      pull_chinabond(f, t)
    }, .progress = "ChinaBond")) |>
    select(data) |>
    unnest(data) |>
    distinct(curve_name, date, .keep_all = TRUE) |>
    arrange(curve, date)
  saveRDS(chinabond, cb_file)
}
chinabond <- readRDS(cb_file)
# Each curve type must occur at most once per day; two treasury or two AAA
# curves on the same day would make the spreads in section 5 ambiguous
stopifnot(!anyDuplicated(chinabond[chinabond$curve != "other", c("curve", "date")]))

# 3a. Quarters without a single treasury day: a failed request. Rerun with
#     refresh <- TRUE if this list is not empty.
quarters |>
  left_join(chinabond |>
              filter(curve == "treasury") |>
              count(from = floor_date(date, "quarter"), name = "days"),
            by = "from") |>
  filter(is.na(days)) |>
  print(n = Inf)

# 3b. First and last day with a value, per curve and tenor. The AAA curves
#     start late or with short tenors only. credit_curve and credit_tenor in
#     section 0 must be covered from 2007 on, otherwise the first windows
#     lose d_credit and check 6a reports missing weeks.
chinabond |>
  pivot_longer(all_of(tenors), names_to = "tenor", values_to = "yield") |>
  filter(!is.na(yield)) |>
  group_by(curve, tenor = factor(tenor, tenors)) |>
  summarise(first = min(date), last = max(date), days = n(), .groups = "drop") |>
  print(n = Inf)

# ---- 4. Real estate index ----------------------------------------------------
# Wang et al. use the CSI 300 Real Estate Index (000952). BaoStock serves it
# (sh.000952, pulled in c01) only from October 2009. The years before come
# from the index provider, China Securities Index Co. (csindex.com.cn); the
# two series are spliced. The provider's server blocks an address after
# about 20 requests in a row, so it is asked only for what BaoStock lacks:
# the quarters up to two quarters after BaoStock's first day, which leaves
# an overlap to check that both series are the same index. Each request runs
# to the first day of the next quarter, so no day is lost if the end date is
# exclusive. The download is incremental and stops at the first refused
# request; run c03 again later if 4a still lists missing quarters.
csi_re_file <- file.path(dir_raw, "pub_csi300_re.rds")

bs_re    <- index_re |> filter(index == "csi300_re") |> select(date, close)
bs_first <- min(bs_re$date)

pull_csindex <- function(from, to) {
  r <- get_retry(
    "https://www.csindex.com.cn/csindex-home/perf/index-perf",
    query = list(indexCode = "000952",
                 startDate = format(from, "%Y%m%d"),
                 endDate   = format(to, "%Y%m%d"))
  )
  if (is.null(r)) return(NULL)
  d <- fromJSON(content(r, as = "text", encoding = "UTF-8"))$data
  if (is.null(d) || length(d) == 0) return(tibble())
  as_tibble(d) |>
    transmute(date  = as.Date(as.character(tradeDate), "%Y%m%d"),
              close = as.numeric(close))
}

csi_re <- if (!refresh && file.exists(csi_re_file)) readRDS(csi_re_file) else
  tibble(date = as.Date(character()), close = numeric())

csi_needed <- quarters |>
  filter(from < floor_date(bs_first, "quarter") + months(6))

csi_missing <- csi_needed |>
  filter(!from %in% floor_date(csi_re$date, "quarter"))

if (nrow(csi_missing) > 0) {
  csi_new <- list()
  for (i in seq_len(nrow(csi_missing))) {
    f <- csi_missing$from[i]
    d <- pull_csindex(f, f + months(3))
    if (is.null(d)) {
      message("csindex refused the request for ", f,
              "; stopping here, run c03 again later")
      break
    }
    csi_new[[i]] <- d
    Sys.sleep(3)
  }
  csi_re <- bind_rows(csi_re, bind_rows(csi_new)) |>
    filter(between(date, start_date, end_date), !is.na(close)) |>
    distinct(date, .keep_all = TRUE) |>
    arrange(date)
  saveRDS(csi_re, csi_re_file)
}

# Only days on which the CSI 300 traded: the provider lists some holidays
csi_re <- csi_re |> semi_join(index |> select(date), by = "date")

# 4a. Provider series: days, range, and the needed quarters still missing.
#     The list must be empty: rerun c03 later if it is not.
csi_re |>
  summarise(days = n(), first = min(date), last = max(date)) |>
  print()

csi_still_missing <- csi_needed |>
  filter(!from %in% floor_date(csi_re$date, "quarter"))
print(csi_still_missing, n = Inf)

if (nrow(csi_still_missing) > 0) {
  stop("CSI 300 Real Estate: ", nrow(csi_still_missing), " quarters still ",
       "missing from the provider. The server refuses requests for a while ",
       "after a burst; run c03 again later, the saved part is kept.",
       call. = FALSE)
}

# 4b. Overlap of provider and BaoStock: if both are the same index, their
#     daily closes agree (ratio 1) and their daily returns correlate at 1.
#     The script stops if not.
overlap <- csi_re |>
  inner_join(bs_re, by = "date", suffix = c("_csi", "_bs")) |>
  arrange(date) |>
  mutate(ratio = close_bs / close_csi)

overlap_check <- overlap |>
  summarise(days      = n(),
            first     = min(date), last = max(date),
            ratio_min = min(ratio), ratio_max = max(ratio),
            cor_daily = cor(diff(log(close_csi)), diff(log(close_bs))))
print(overlap_check)

stopifnot(overlap_check$days >= 100, overlap_check$cor_daily > 0.999)

# 4c. Spliced series: the provider until its last day, BaoStock after that,
#     rescaled to the provider's level on the last common day so the seam
#     carries no jump (the ratio in 4b should be 1 anyway).
seam_ratio <- last(overlap$ratio)
csi300_re_spliced <- bind_rows(
  csi_re,
  bs_re |>
    filter(date > max(csi_re$date)) |>
    mutate(close = close / seam_ratio)
)

# 4d. Weekly returns of all candidates. The spliced series is the index of
#     Wang et al.; the SSE and SZSE real estate indices from c01 should move
#     closely with it. re_index in section 0 picks one.
re_all <- bind_rows(
  index_re |> select(index, date, close),
  csi300_re_spliced |> mutate(index = "csi300_re_spliced")
)

re_weekly <- re_all |>
  group_split(index) |>
  map(\(d) weekly_last(d, "close") |>
        transmute(week, index = d$index[1], ret = log(close / lag(close)))) |>
  bind_rows()

re_weekly |>
  filter(week >= week_of(sample_start)) |>
  pivot_wider(names_from = index, values_from = ret) |>
  select(-week) |>
  cor(use = "pairwise.complete.obs") |>
  round(3) |>
  print()

stopifnot(re_index %in% re_weekly$index)

# ---- 5. State variables ------------------------------------------------------
# 5a. Weekly CSI 300 return and its GARCH(1,1) conditional variance. Wang et
#     al. do not state the frequency of their GARCH (Table A3); as they work
#     with weekly data throughout, it runs here on the weekly returns,
#     2007-2016, with a constant mean. Returns go in as percent for
#     the optimiser; the conditional variance is scaled back to the units of
#     ret_mkt. fGarch, not tseries::garch: on simulated weekly data with
#     known parameters, tseries::garch put beta at 0 while fGarch recovered
#     the true values. alpha + beta close to 1 is the usual result for
#     weekly stock index returns.
mkt <- weekly_last(index, "close") |>
  transmute(week, ret_mkt = log(close / lag(close)))

r_pct <- 100 * na.omit(mkt$ret_mkt)
garch_fit <- fGarch::garchFit(~ garch(1, 1), data = r_pct,
                              include.mean = TRUE, trace = FALSE)
print(round(garch_fit@fit$matcoef, 4))

vol <- mkt |>
  filter(!is.na(ret_mkt)) |>
  mutate(vol_mkt = (fGarch::volatility(garch_fit) / 100)^2) |>
  select(week, vol_mkt)

# 5b. Daily spreads first, then the weekly value. Treasury and AAA notes come
#     from the same ChinaBond table, so their days coincide; SHIBOR is joined
#     on the date.
tsy <- chinabond |>
  filter(curve == "treasury") |>
  transmute(date, tb3m = `3M`, slope = `10Y` - `3M`)

credit <- chinabond |>
  filter(curve %in% c("treasury", credit_curve)) |>
  select(curve, date, y = all_of(credit_tenor)) |>
  pivot_wider(names_from = curve, values_from = y) |>
  transmute(date, credit = .data[[credit_curve]] - treasury)

liq <- shibor |>
  inner_join(tsy, by = "date") |>
  transmute(date, liq_spread = shibor3m - tb3m)

re <- re_weekly |>
  filter(index == re_index) |>
  select(week, ret_re = ret)

# 5c. All seven on the week grid, in the order of Wang et al. The levels
#     tb3m, slope and credit stay in the file for checking; c05 uses only
#     the seven variables.
state_names <- c("liq_spread", "d_tb3m", "d_slope", "d_credit", "ret_mkt",
                 "vol_mkt", "ret_re")

state <- weeks |>
  left_join(mkt, by = "week") |>
  left_join(vol, by = "week") |>
  left_join(weekly_last(liq, "liq_spread"), by = "week") |>
  left_join(weekly_last(tsy, "tb3m"), by = "week") |>
  left_join(weekly_last(tsy, "slope"), by = "week") |>
  left_join(weekly_last(credit, "credit"), by = "week") |>
  left_join(re, by = "week") |>
  arrange(week) |>
  mutate(d_tb3m   = tb3m - lag(tb3m),
         d_slope  = slope - lag(slope),
         d_credit = credit - lag(credit)) |>
  select(week, all_of(state_names), tb3m, slope, credit)

# ---- 6. Checks ---------------------------------------------------------------
# 6a. Coverage and summary per variable. na_in_sample counts missing weeks
#     from the first week of 2008 on; it must be 0 for all seven, because c05
#     needs every variable in every window. share_zero is the share of weeks
#     with exactly no change: high values point to a coarsely rounded series.
coverage <- state |>
  select(week, all_of(state_names)) |>
  pivot_longer(-week, names_to = "variable") |>
  group_by(variable = factor(variable, state_names)) |>
  summarise(first        = min(week[!is.na(value)]),
            last         = max(week[!is.na(value)]),
            na_in_sample = sum(is.na(value) & week >= week_of(sample_start)),
            share_zero   = round(mean(value == 0, na.rm = TRUE), 3),
            mean = mean(value, na.rm = TRUE), sd  = sd(value, na.rm = TRUE),
            min  = min(value, na.rm = TRUE),  max = max(value, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(c(mean, sd, min, max), \(x) round(x, 4)))

print(coverage)
# 6a'. Download dates of the raw files (last modification). These sources
#      are not versioned and may revise or remove data, so the dates go with
#      the data when the code is shared. pub_csi300_re.rds is filled in
#      several runs; its date is that of the last addition.
file.info(file.path(dir_raw, c("pub_shibor.rds", "pub_chinabond.rds",
                               "pub_csi300_re.rds")))["mtime"] |>
  print()

# 6b. The seven series over the sample period, for a visual check
p <- state |>
  filter(week >= week_of(sample_start)) |>
  select(week, all_of(state_names)) |>
  pivot_longer(-week, names_to = "variable") |>
  mutate(variable = factor(variable, state_names)) |>
  ggplot(aes(week, value)) +
  geom_line(linewidth = 0.3, colour = "#1b4f72") +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = NULL,
       title = "Weekly state variables, 2008-2016",
       subtitle = "Public sources: BaoStock, Eastmoney (SHIBOR), ChinaBond, CSI") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

print(p)

# ---- 7. Save -----------------------------------------------------------------
saveRDS(state, file.path(dir_proc, "state_vars.rds"))
write_csv(coverage, file.path(dir_out, "state_vars_coverage.csv"))
ggsave(file.path(dir_out, "state_vars.png"), p, width = 9, height = 9,
       dpi = 300, bg = "white")
