# Comment on Wang, Jiang, Lin, Xie and Stanley (2018): "Interconnectedness and
# systemic risk of China's financial institutions", Emerging Markets Review
# 35, 1-18.
# Leonard Nikita Baur, Chemnitz University of Technology

# c01_pull_baostock pulls daily A-share prices for the 24 institutions in
# Wang et al. and the CSI 300 from BaoStock (free, no registration), in two
# versions:
#   - unadjusted (adjustflag "3"): closing prices not adjusted for corporate
#     actions, which reproduce 117 of the 120 descriptive statistics in
#     Table 1 of Wang et al. (see c02)
#   - adjusted   (adjustflag "1", backward adjusted): adjusted for cash
#     dividends, bonus and capitalisation issues and rights issues
# plus BaoStock's record of corporate actions (ex-dates, bonus and
# capitalisation issues, cash dividends), its adjustment factors and three
# real estate sector indices (for c03).
#
# Period 2007-2016: Wang et al.'s sample is 2008-2016, 2007 is lead time.
# Start and end date are set once in section 0. The data in the comment
# were pulled with BaoStock 0.9.4; the pull date is in bs_pull_log.rds.
#
# BaoStock is a Python library. reticulate runs it from R inside its own
# virtual environment, set up in section 1 on the first run. Start this
# script in a fresh R session (Session > Restart R): reticulate can only
# choose its Python environment before Python has been started.
#
# Output (data/raw/):
#   bs_prices.rds     daily prices, both versions, long format
#   bs_index.rds      CSI 300, daily
#   bs_dividends.rds  corporate actions per institution
#   bs_adjfactor.rds  adjustment factors with ex-dates
#   bs_pull_log.rds   rows and date range per institution and version, date
#                     of the pull
#   bs_index_re.rds   real estate sector indices, daily (for c03)

# ---- 0. Packages and paths ---------------------------------------------------
library(tidyverse)
library(here)
library(reticulate)

dir_raw <- here("data", "raw")
dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)

start_date <- "2007-01-01"
end_date   <- "2016-12-31"

# ---- 1. Python environment ---------------------------------------------------
# First run only: creates a virtual environment "baostock" with the versions
# used for the comment. If no Python 3.12 is found, install_python()
# downloads 3.12.10 (a few minutes). Later runs skip straight to
# use_virtualenv().
if (!virtualenv_exists("baostock")) {
  python <- virtualenv_starter("3.12")
  if (is.null(python)) python <- install_python("3.12.10")
  virtualenv_create("baostock", python = python,
                    packages = c("baostock==0.9.4", "pandas==3.1.0",
                                 "numpy==2.5.4"))
}
use_virtualenv("baostock", required = TRUE)
bs <- import("baostock")

# BaoStock's own get_data() breaks with pandas 2 and later as soon as a
# result has more than one page (2,000 rows, i.e. every price series here).
# bs_collect() reads the pages one by one, as in BaoStock's own demos, and
# returns one DataFrame, which reticulate converts to an R data frame.
py_run_string("
import pandas as pd
def bs_collect(rs):
    rows = []
    while (rs.error_code == '0') and rs.next():
        rows.append(rs.get_row_data())
    return pd.DataFrame(rows, columns=rs.fields)
")

# ---- 2. Institutions ---------------------------------------------------------
# The 24 A-share institutions of Wang et al.: 14 banks, 7 securities firms,
# 3 insurers. c02 matches them to their Table 1 by stock code; the
# abbreviations only label (BCM is BOCOM in their Table 1).
wang <- tribble(
  ~abbr,    ~code,    ~sector,
  "PAB",    "000001", "bank",
  "SPDB",   "600000", "bank",
  "HXB",    "600015", "bank",
  "CMBC",   "600016", "bank",
  "CMB",    "600036", "bank",
  "NJBK",   "601009", "bank",
  "CIB",    "601166", "bank",
  "BOB",    "601169", "bank",
  "BCM",    "601328", "bank",
  "ICBC",   "601398", "bank",
  "CCB",    "601939", "bank",
  "BOC",    "601988", "bank",
  "CNCB",   "601998", "bank",
  "NBCB",   "002142", "bank",
  "CITICS", "600030", "securities",
  "SLSC",   "600109", "securities",
  "HTSEC",  "600837", "securities",
  "PSC",    "601099", "securities",
  "NESC",   "000686", "securities",
  "GYSC",   "000728", "securities",
  "CJSC",   "000783", "securities",
  "PAI",    "601318", "insurance",
  "CPIC",   "601601", "insurance",
  "CLI",    "601628", "insurance"
) |>
  # BaoStock codes: Shanghai (6xxxxx) as "sh.", Shenzhen (0xxxxx) as "sz."
  mutate(bs_code = paste0(if_else(str_starts(code, "6"), "sh.", "sz."), code))

stopifnot(nrow(wang) == 24, !anyDuplicated(wang$code))

# ---- 3. Pull functions -------------------------------------------------------
# Every query returns a result object. error_code "0" means success; anything
# else is reported as a warning and gives an empty table, so one failed
# query does not stop the whole pull. Sys.sleep() keeps the request rate low.
fetch <- function(rs, what) {
  if (rs$error_code != "0") {
    warning(what, ": ", rs$error_msg, call. = FALSE)
    return(tibble())
  }
  as_tibble(py$bs_collect(rs))
}

price_fields <- "date,code,open,high,low,close,preclose,volume,amount,tradestatus,pctChg,isST"

pull_prices <- function(bs_code, flag) {
  rs <- bs$query_history_k_data_plus(
    bs_code, price_fields,
    start_date = start_date, end_date = end_date,
    frequency = "d", adjustflag = flag
  )
  Sys.sleep(0.2)
  fetch(rs, paste(bs_code, "prices, adjustflag", flag))
}

# yearType = "operate": corporate actions by the year of the ex-date (the
# default "report" sorts them by the year the plan was announced)
pull_dividends <- function(bs_code, year) {
  rs <- bs$query_dividend_data(code = bs_code, year = as.character(year),
                               yearType = "operate")
  Sys.sleep(0.2)
  fetch(rs, paste(bs_code, year, "dividends"))
}

pull_adjfactor <- function(bs_code) {
  rs <- bs$query_adjust_factor(code = bs_code,
                               start_date = start_date, end_date = end_date)
  Sys.sleep(0.2)
  fetch(rs, paste(bs_code, "adjustment factors"))
}

# ---- 4. Pull -----------------------------------------------------------------
lg <- bs$login()
stopifnot(lg$error_code == "0")
pull_date <- Sys.Date()   # BaoStock data can be revised later; kept in the log

# 4a. Prices: 24 institutions x 2 versions = 48 queries
prices <- wang |>
  select(abbr, bs_code) |>
  crossing(version = c("unadjusted", "adjusted")) |>
  mutate(flag = if_else(version == "unadjusted", "3", "1"),
         data = map2(bs_code, flag, pull_prices, .progress = "prices")) |>
  select(abbr, version, data) |>
  unnest(data) |>
  mutate(
    date = as.Date(date),
    code = str_remove(code, "^s[hz]\\."),
    # BaoStock delivers text; empty strings (e.g. no trade) become NA
    across(c(open, high, low, close, preclose, volume, amount, pctChg),
           \(x) suppressWarnings(as.numeric(x))),
    tradestatus = as.integer(tradestatus),   # 1 traded, 0 suspended
    isST        = as.integer(isST)           # 1 under special treatment
  )

# 4b. CSI 300 (sh.000300), unadjusted: an index has no corporate actions
index <- bs$query_history_k_data_plus(
  "sh.000300", "date,code,open,high,low,close,preclose,volume,amount,pctChg",
  start_date = start_date, end_date = end_date,
  frequency = "d", adjustflag = "3"
) |>
  fetch("CSI 300") |>
  mutate(date = as.Date(date),
         across(c(open, high, low, close, preclose, volume, amount, pctChg),
                \(x) suppressWarnings(as.numeric(x))))

# 4c. Corporate actions: one query per institution and year (240 for
# 2007-2016). Kept as text here; c02 converts the columns it needs (ex-date
# dividOperateDate, cash dividend dividCashPsBeforeTax, bonus issue
# dividStocksPs, capitalisation issue dividReserveToStockPs).
div_years <- year(as.Date(start_date)):year(as.Date(end_date))

dividends <- wang |>
  select(abbr, bs_code) |>
  crossing(year = div_years) |>
  mutate(data = map2(bs_code, year, pull_dividends, .progress = "dividends")) |>
  select(abbr, data) |>
  unnest(data)

# 4d. Adjustment factors: one row per ex-date and institution
adjfactor <- wang |>
  select(abbr, bs_code) |>
  mutate(data = map(bs_code, pull_adjfactor, .progress = "adjustment factors")) |>
  select(abbr, data) |>
  unnest(data)

# 4e. Real estate sector indices for the state variables in c03. Wang et al.
# use the CSI 300 Real Estate Index (sh.000952), which BaoStock serves only
# from October 2009; c03 splices it with the provider's series. SSE Real
# Estate (sh.000006) and SZSE Real Estate (sz.399241) are pulled as well, so
# that c03 can show how closely the three move together.
re_codes <- c(sse_re = "sh.000006", szse_re = "sz.399241",
              csi300_re = "sh.000952")

pull_index <- function(bs_code) {
  rs <- bs$query_history_k_data_plus(
    bs_code, "date,code,close",
    start_date = start_date, end_date = end_date,
    frequency = "d", adjustflag = "3"
  )
  Sys.sleep(0.2)
  fetch(rs, paste(bs_code, "index"))
}

index_re <- tibble(index = names(re_codes), bs_code = re_codes) |>
  mutate(data = map(bs_code, pull_index)) |>
  select(index, data) |>
  unnest(data) |>
  mutate(date  = as.Date(date),
         close = suppressWarnings(as.numeric(close)))

bs$logout()

# ---- 5. Checks ---------------------------------------------------------------
# 5a. Rows and date range per institution and version. Both versions must have
# the same rows; the script stops otherwise. An institution with NA is
# missing from BaoStock. Eleven institutions were listed on the A-share
# market during 2007 and start later: CLI (Jan), CIB (Feb), PAI (Mar),
# CNCB (Apr), BCM (May), NJBK and NBCB (Jul), BOB and CCB (Sep), CPIC and
# PSC (Dec).
pull_log <- prices |>
  group_by(abbr, version) |>
  summarise(n = n(), first = min(date), last = max(date), .groups = "drop") |>
  pivot_wider(names_from = version, values_from = c(n, first, last)) |>
  mutate(pulled_on = pull_date)

stopifnot(identical(pull_log$n_adjusted, pull_log$n_unadjusted))

wang |>
  select(abbr, sector) |>
  left_join(pull_log, by = "abbr") |>
  print(n = 24)

# 5b. Direction of adjustflag. Ping An's ten-for-ten capitalisation issue
# with a cash dividend of CNY 0.50 (ex-date 27 Jul 2015): the unadjusted
# close roughly halves from the 24th to the 27th, the adjusted one does not.
# If it is the other way round, the flags "1" and "3" in 4a are swapped.
prices |>
  filter(abbr == "PAI",
         between(date, as.Date("2015-07-22"), as.Date("2015-07-29"))) |>
  select(version, date, close) |>
  pivot_wider(names_from = version, values_from = close) |>
  print()

# 5c. Ping An's corporate action as BaoStock records it: should show the
# ex-date 2015-07-27, one new share per share and CNY 0.50 cash
dividends |>
  filter(abbr == "PAI", str_starts(dividOperateDate, "2015")) |>
  select(abbr, dividOperateDate, dividCashPsBeforeTax,
         dividStocksPs, dividReserveToStockPs) |>
  print()

# 5d. CSI 300 range: first trading day 2007, last trading day 2016
print(range(index$date))

# 5e. What the trading screen showed on Ping An's ex-date. There, BaoStock's
# unadjusted preclose should be the ex-rights reference price,
# (previous close - 0.50) / 2, not the previous close itself, and pctChg
# should be the change relative to that reference price.
prices |>
  filter(abbr == "PAI", version == "unadjusted",
         between(date, as.Date("2015-07-24"), as.Date("2015-07-28"))) |>
  select(date, preclose, close, pctChg) |>
  print()

# ---- 6. Save -----------------------------------------------------------------
saveRDS(prices,    file.path(dir_raw, "bs_prices.rds"))
saveRDS(index,     file.path(dir_raw, "bs_index.rds"))
saveRDS(dividends, file.path(dir_raw, "bs_dividends.rds"))
saveRDS(adjfactor, file.path(dir_raw, "bs_adjfactor.rds"))
saveRDS(pull_log,  file.path(dir_raw, "bs_pull_log.rds"))
saveRDS(index_re,  file.path(dir_raw, "bs_index_re.rds"))
