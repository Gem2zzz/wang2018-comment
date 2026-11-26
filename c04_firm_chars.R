# Comment on Wang, Jiang, Lin, Xie and Stanley (2018): "Interconnectedness and
# systemic risk of China's financial institutions", Emerging Markets Review
# 35, 1-18.
# Leonard Nikita Baur, Chemnitz University of Technology

# c04_firm_chars builds the four firm characteristics of Wang et al. (2018,
# Section 3) from quarterly consolidated balance sheets, interpolated to
# weekly frequency with a cubic spline, and the weekly market capitalisation
# that c05 needs for SRR and SRE.
#   lev   (i)   leverage: total assets / total equity (book values)
#   mm    (ii)  maturity mismatch: (short-term debt - short-term investments
#               - cash) / total liabilities
#   mtb   (iii) market-to-book: market value of equity / total equity
#   size  (iv)  log10 of total equity (book value of equity, BVE)
# Table A3 of the comment compares them with Wang et al.
#
# Source: Wang et al. take the balance sheets from CSMAR. Here they come from
# Eastmoney's F10 pages (the data behind AkShare's
# stock_balance_sheet_by_report_em), which carry the full consolidated
# balance sheet per quarter, with separate templates for banks, securities
# firms and insurers. Prices for the market value come from c01 (BaoStock).
#
# Choices Wang et al. do not spell out (Table A3), for all 24 institutions:
#   - BVE is total equity including minority interests (CSMAR's "total
#     owners' equity"), so that total assets = total liabilities + BVE.
#   - Market value of equity: unadjusted A-share close times total share
#     capital (all share classes; par value CNY 1, so share capital in CNY is
#     the number of shares).
#   - Maturity mismatch. Wang et al. follow Adrian and Brunnermeier (2016),
#     whose short-term debt is debt in current liabilities, without
#     deposits. Mapped onto the Chinese templates (missing items count as 0):
#       short-term debt = borrowing from the central bank + deposits from
#                         other banks and financial institutions + funds
#                         borrowed from banks + repurchase agreements +
#                         short-term loans + short-term financing bills
#       short-term inv. = trading financial assets + funds lent to banks +
#                         reverse repurchase agreements
#       cash            = cash and balances with the central bank + deposits
#                         with banks (banks); cash and settlement reserves
#                         (securities firms, insurers), including client
#                         funds held by brokers, as in the raw balance sheet
#   - Market-to-book is taken at each quarter end (close of the last trading
#     day on or before it) and interpolated like the other three.
#     If the quarter-end report already shows the shares of a bonus or
#     capitalisation issue that goes ex only after the quarter end, the share
#     count is divided by (1 + bonus), so that it matches the pre-ex closing
#     price.
#   - The spline is a natural cubic spline through the quarter-end values,
#     evaluated at the Friday of each grid week, only between a firm's first
#     and last quarter end (no extrapolation). As in Wang et al., a week
#     between two quarter ends uses the later quarter end as well.
#
# Weekly market capitalisation for SRR/SRE: unadjusted weekly close times
# shares, from the share capital of the last quarter end, corrected for bonus
# and capitalisation issues (c02 events) between that quarter end and the
# week (section 4). Rights issues and placements enter with the next quarter
# end.
#
# The download is incremental: one file per institution in
# data/raw/em_balance/, every run fetches only what is missing and stops at
# the first refused request. Run c04 again later if 1a lists missing
# institutions.
#
# Input:  data/raw/bs_prices.rds, data/raw/bs_index.rds (c01)
#         data/processed/bs_events.rds, bs_weekly.rds (c02)
# Output: data/raw/em_balance/*.rds         balance sheets, quarterly
#         data/processed/firm_chars_q.rds    the four characteristics,
#                                            quarterly
#         data/processed/firm_chars.rds      the four characteristics, weekly
#         data/processed/mcap_weekly.rds     market capitalisation, weekly
#         output/firm_chars_coverage.csv     coverage and summary
#         output/firm_chars.png              the four series per institution

# ---- 0. Packages, paths, settings --------------------------------------------
library(tidyverse)
library(here)
library(httr)
library(jsonlite)

dir_raw  <- here("data", "raw")
dir_em   <- here("data", "raw", "em_balance")
dir_proc <- here("data", "processed")
dir_out  <- here("output")
for (d in c(dir_em, dir_proc, dir_out)) dir.create(d, recursive = TRUE,
                                                   showWarnings = FALSE)

sample_start <- as.Date("2008-01-01")   # Wang et al.: weekly data 2008-2016

# Quarter ends 2006 Q4 to 2016 Q4: the spline needs a value before and after
# every week of 2007-2016
quarter_ends <- seq(as.Date("2007-01-01"), as.Date("2017-01-01"),
                    by = "quarter") - 1

refresh <- FALSE   # TRUE: download all balance sheets again

ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")

prices <- readRDS(here("data", "raw", "bs_prices.rds"))
index  <- readRDS(here("data", "raw", "bs_index.rds"))
events <- readRDS(here("data", "processed", "bs_events.rds"))

# The 24 institutions, as in c01. Eastmoney's company type selects the
# balance sheet template: 3 bank, 1 securities firm, 2 insurer.
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
  mutate(em_code  = paste0(if_else(str_starts(code, "6"), "SH", "SZ"), code),
         em_ctype = c(bank = 3, securities = 1, insurance = 2)[sector])

stopifnot(nrow(wang) == 24, !anyDuplicated(wang$code))

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

# ---- 1. Balance sheets from Eastmoney ----------------------------------------
# The balance sheet items used below. Each template has only some of them;
# a missing one becomes NA here and counts as 0 in the sums of section 2.
items <- c(
  "TOTAL_ASSETS", "TOTAL_LIABILITIES", "TOTAL_EQUITY", "TOTAL_PARENT_EQUITY",
  "SHARE_CAPITAL",
  # short-term debt
  "LOAN_PBC", "IOFI_DEPOSIT", "BORROW_FUND", "SELL_REPO_FINASSET",
  "SHORT_LOAN", "SHORT_FIN_PAYABLE",
  # short-term investments
  "FVTPL_FINASSET", "TRADE_FINASSET", "LEND_FUND", "BUY_RESALE_FINASSET",
  # cash
  "CASH_DEPOSIT_PBC", "DEPOSIT_INTERBANK", "MONETARYFUNDS",
  "SETTLE_EXCESS_RESERVE"
)

# One request returns up to five report dates
pull_balance <- function(em_code, em_ctype, dates) {
  r <- get_retry(
    "https://emweb.securities.eastmoney.com/PC_HSF10/NewFinanceAnalysis/zcfzbAjaxNew",
    query = list(companyType = em_ctype, reportDateType = 0, reportType = 1,
                 dates = paste(format(dates), collapse = ","),
                 code = em_code)
  )
  if (is.null(r)) return(NULL)
  d <- fromJSON(content(r, as = "text", encoding = "UTF-8"))$data
  if (is.null(d) || length(d) == 0) return(tibble())
  d <- as_tibble(d)
  for (it in setdiff(items, names(d))) d[[it]] <- NA_real_
  d |>
    transmute(date = as.Date(substr(REPORT_DATE, 1, 10)),
              across(all_of(items), \(x) suppressWarnings(as.numeric(x))))
}

# One file per institution; an existing file is complete and skipped
em_file <- function(code) file.path(dir_em, paste0(code, ".rds"))

todo <- wang |> filter(refresh | !file.exists(em_file(code)))
chunks <- split(quarter_ends, ceiling(seq_along(quarter_ends) / 5))

for (i in seq_len(nrow(todo))) {
  f <- todo[i, ]
  message(f$abbr, " (", i, "/", nrow(todo), ")")
  parts   <- list()
  refused <- FALSE
  for (ch in chunks) {
    d <- pull_balance(f$em_code, f$em_ctype, ch)
    if (is.null(d)) { refused <- TRUE; break }
    parts[[length(parts) + 1]] <- d
    Sys.sleep(1)
  }
  if (refused) {
    message("Eastmoney refused a request for ", f$abbr,
            "; stopping here, run c04 again later")
    break
  }
  saveRDS(bind_rows(parts) |> distinct(date, .keep_all = TRUE) |> arrange(date),
          em_file(f$code))
}

# 1a. Institutions without a balance sheet file. Must be empty; otherwise
#     run c04 again later.
missing_inst <- wang |> filter(!file.exists(em_file(code)))
print(missing_inst |> select(abbr, code))
# Download dates of the balance sheet files (last modification), reported
# with the data when the code is shared
file.info(list.files(dir_em, full.names = TRUE))$mtime |>
  range() |>
  print()
if (nrow(missing_inst) > 0) {
  stop(nrow(missing_inst), " institutions without balance sheets; run c04 ",
       "again later, the downloaded ones are kept.", call. = FALSE)
}

balance <- wang |>
  select(abbr, code, sector) |>
  mutate(data = map(code, \(c) readRDS(em_file(c)))) |>
  unnest(data)

# 1b. Quarters per institution 2007-2016 and the first one. Institutions
#     listed during 2007 start later; any other gap is a missing report.
balance |>
  filter(between(date, as.Date("2007-01-01"), as.Date("2016-12-31"))) |>
  group_by(abbr, sector) |>
  summarise(quarters = n(), first = min(date), last = max(date),
            .groups = "drop") |>
  arrange(quarters) |>
  print(n = 24)

# 1c. Accounting identity: total assets = total liabilities + total equity.
#     max_gap is the largest deviation relative to total assets; it should
#     be close to 0 everywhere.
balance |>
  group_by(abbr) |>
  summarise(max_gap = max(abs(TOTAL_ASSETS - TOTAL_LIABILITIES - TOTAL_EQUITY) /
                            TOTAL_ASSETS, na.rm = TRUE)) |>
  arrange(desc(max_gap)) |>
  print(n = 5)

# ---- 2. Quarterly characteristics --------------------------------------------
# Share capital per quarter end and the bonus and capitalisation issues after
# each quarter end, needed for the market value at quarter ends (here) and
# for the weekly market capitalisation (section 4).
# included: the report of q_date already shows the new shares although the
# ex-date comes later (e.g. CMBC, 31 Mar 2008, ex-date 7 Apr 2008)
shares_q <- balance |>
  filter(!is.na(SHARE_CAPITAL)) |>
  select(abbr, q_date = date, shares = SHARE_CAPITAL) |>
  arrange(abbr, q_date) |>
  group_by(abbr) |>
  mutate(ratio = shares / lag(shares)) |>
  ungroup()

bonus <- events |>
  filter(type == "bonus shares") |>
  select(abbr, ex_date = date, bonus) |>
  left_join(shares_q, by = join_by(abbr, closest(ex_date > q_date))) |>
  mutate(included = !is.na(ratio) & abs(ratio / (1 + bonus) - 1) < 0.02) |>
  select(abbr, q_date, ex_date, bonus, included)

# At those quarter ends the close is still pre-ex, so the share count is
# divided by (1 + bonus) for the market value
shares_fix <- bonus |>
  filter(included) |>
  group_by(abbr, date = q_date) |>
  summarise(f_q = 1 / prod(1 + bonus), .groups = "drop")

print(shares_fix, n = Inf)

# Market value at each quarter end: unadjusted close of the last trading day
# on or before it

close_q <- balance |>
  distinct(abbr, date) |>
  left_join(prices |>
              filter(version == "unadjusted", tradestatus == 1, close > 0) |>
              select(abbr, pdate = date, close),
            by = join_by(abbr, closest(date >= pdate))) |>
  select(abbr, date, close)

s0 <- function(...) rowSums(cbind(...), na.rm = TRUE)   # sum, NA counts as 0

chars_q <- balance |>
  left_join(close_q, by = c("abbr", "date")) |>
  left_join(shares_fix, by = c("abbr", "date")) |>
  mutate(
    bve  = TOTAL_EQUITY,
    mve  = close * SHARE_CAPITAL * coalesce(f_q, 1),
    std  = s0(LOAN_PBC, IOFI_DEPOSIT, BORROW_FUND, SELL_REPO_FINASSET,
              SHORT_LOAN, SHORT_FIN_PAYABLE),
    sti  = s0(coalesce(FVTPL_FINASSET, TRADE_FINASSET), LEND_FUND,
              BUY_RESALE_FINASSET),
    cash = s0(CASH_DEPOSIT_PBC, DEPOSIT_INTERBANK, MONETARYFUNDS,
              SETTLE_EXCESS_RESERVE),
    lev  = TOTAL_ASSETS / bve,
    mm   = (std - sti - cash) / TOTAL_LIABILITIES,
    mtb  = mve / bve,
    size = log10(bve)
  ) |>
  select(abbr, code, sector, date, lev, mm, mtb, size, bve, mve,
         shares = SHARE_CAPITAL, std, sti, cash)

# ---- 3. Weekly spline --------------------------------------------------------
# Natural cubic spline through the quarter ends of one institution and one
# characteristic, evaluated at the Friday of each grid week between its first
# and last quarter end
weeks <- index |>
  filter(!is.na(close)) |>
  distinct(week = week_of(date)) |>
  arrange(week)

spline_weekly <- function(q_dates, values) {
  ok <- !is.na(values)
  out <- rep(NA_real_, nrow(weeks))
  if (sum(ok) < 4) return(out)
  x  <- as.numeric(q_dates[ok])
  fr <- as.numeric(weeks$week + 4)
  inside <- fr >= min(x) & fr <= max(x)
  out[inside] <- splinefun(x, values[ok], method = "natural")(fr[inside])
  out
}

chars_vars <- c("lev", "mm", "mtb", "size")

firm_chars <- chars_q |>
  group_by(abbr, code, sector) |>
  group_modify(\(d, key) {
    d <- arrange(d, date)
    bind_cols(weeks, map(set_names(chars_vars),
                         \(v) spline_weekly(d$date, d[[v]])) |> as_tibble())
  }) |>
  ungroup()

# ---- 4. Weekly market capitalisation -----------------------------------------
# Unadjusted weekly close times shares. Shares come from the share capital of
# the last quarter end on or before the Friday of the week, corrected for
# bonus and capitalisation issues whose ex-date falls after that quarter end.
# Some reports already show the share count after an issue that goes ex only
# after the quarter end (CMBC, 31 Mar 2008, ex-date 7 Apr 2008): their share
# capital jumps by the bonus ratio against the quarter before. For those,
# weeks before the ex-date get the share count divided by (1 + bonus); for
# all others, weeks from the ex-date on get it multiplied by (1 + bonus).
week_close <- prices |>
  filter(version == "unadjusted", tradestatus == 1, close > 0) |>
  mutate(week = week_of(date)) |>
  group_by(abbr, week) |>
  summarise(close = last(close, order_by = date), .groups = "drop")

mcap_weekly <- week_close |>
  mutate(fri = week + 4) |>
  left_join(shares_q |> select(abbr, q_date, shares),
            by = join_by(abbr, closest(fri >= q_date))) |>
  left_join(bonus, by = c("abbr", "q_date"), relationship = "many-to-many") |>
  mutate(f = case_when(is.na(ex_date)              ~ 1,
                       included  & fri <  ex_date  ~ 1 / (1 + bonus),
                       !included & fri >= ex_date  ~ 1 + bonus,
                       TRUE                        ~ 1)) |>
  group_by(abbr, week, close, shares) |>
  summarise(factor = prod(f), .groups = "drop") |>
  mutate(mcap = close * shares * factor) |>
  select(abbr, week, close, shares_adj = factor, mcap) |>
  arrange(abbr, week)

# 4a. Bonus and capitalisation issues whose share count was already in the
#     earlier report
bonus |> filter(included) |> print(n = Inf)

# ---- 5. Checks ---------------------------------------------------------------
# 5a. Coverage and summary per characteristic and sector over the sample
#     weeks. na_in_sample counts weeks from 2008 on without a value; c05
#     needs a value in every week in which an institution's return exists.
coverage <- firm_chars |>
  filter(week >= week_of(sample_start)) |>
  pivot_longer(all_of(chars_vars), names_to = "variable") |>
  group_by(variable = factor(variable, chars_vars), sector) |>
  summarise(na_in_sample = sum(is.na(value)),
            mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
            min  = min(value, na.rm = TRUE),  max = max(value, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(c(mean, sd, min, max), \(x) round(x, 3)))

print(coverage, n = Inf)

# 5b. Institutions with missing weeks from 2008 on, and which characteristic
firm_chars |>
  filter(week >= week_of(sample_start)) |>
  pivot_longer(all_of(chars_vars), names_to = "variable") |>
  filter(is.na(value)) |>
  count(abbr, variable, name = "weeks_missing") |>
  print(n = Inf)

# 5c. Market capitalisation against the adjusted weekly return of c02. Both
#     move together except for new shares (placements, rights issues: share
#     count up) and cash dividends (a few percent down). A bonus or
#     capitalisation issue counted twice or missed shows up as about
#     +-log(1 + bonus), e.g. +-0.26 for three-for-ten or +-0.69 for
#     ten-for-ten. Listed: the largest gap per institution from 2008 on.
weekly_adj <- readRDS(here("data", "processed", "bs_weekly.rds")) |>
  filter(version == "adjusted") |>
  select(abbr, week, ret_adj = ret)

mcap_weekly |>
  filter(week >= week_of(sample_start)) |>
  group_by(abbr) |>
  mutate(d_log = log(mcap / lag(mcap))) |>
  ungroup() |>
  left_join(weekly_adj, by = c("abbr", "week")) |>
  mutate(gap = d_log - ret_adj) |>
  group_by(abbr) |>
  slice_max(abs(gap), n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(abbr, week, d_log, ret_adj, gap) |>
  arrange(desc(abs(gap))) |>
  mutate(across(c(d_log, ret_adj, gap), \(x) round(x, 3))) |>
  print(n = 8)

# 5d. The four characteristics per institution, for a visual check
p <- firm_chars |>
  filter(week >= week_of(sample_start)) |>
  pivot_longer(all_of(chars_vars), names_to = "variable") |>
  mutate(variable = factor(variable, chars_vars)) |>
  ggplot(aes(week, value, group = abbr, colour = sector)) +
  geom_line(linewidth = 0.25, alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c(bank = "#1b4f72", securities = "#e67e22",
                                 insurance = "#27ae60")) +
  labs(x = NULL, y = NULL, colour = NULL,
       title = "Firm characteristics, weekly (spline), 2008-2016",
       subtitle = "Quarterly consolidated balance sheets from Eastmoney; one line per institution") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

print(p)

# ---- 6. Save -----------------------------------------------------------------
saveRDS(chars_q,     file.path(dir_proc, "firm_chars_q.rds"))
saveRDS(firm_chars,  file.path(dir_proc, "firm_chars.rds"))
saveRDS(mcap_weekly, file.path(dir_proc, "mcap_weekly.rds"))
write_csv(coverage,  file.path(dir_out, "firm_chars_coverage.csv"))
ggsave(file.path(dir_out, "firm_chars.png"), p, width = 9, height = 7,
       dpi = 300, bg = "white")
