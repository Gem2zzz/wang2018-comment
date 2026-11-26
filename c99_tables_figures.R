# Comment on Wang, Jiang, Lin, Xie and Stanley (2018): "Interconnectedness and
# systemic risk of China's financial institutions", Emerging Markets Review
# 35, 1-18.
# Leonard Nikita Baur, Chemnitz University of Technology

# c99_tables_figures builds the tables and figures of the comment from the
# saved outputs of c02 and c05. No estimation here: rerun after any upstream
# change and replace the files in the manuscript. Table A3 (specification
# and inputs) is written by hand.
#
# Section 2 and Online Appendix A (return data):
#   tab_1_extreme_weeks.docx       weekly minima, affected institutions (not
#                                  in the manuscript)
#   tab_a1_extreme_weeks_all.docx  Table A1: weekly minima, all 24
#   tab_2_matches.docx             Table A2: matches per statistic
#   fig_minima.png                 scatter of the minima (not in the manuscript)
#
# Section 4 and Online Appendices B and C (results):
#   tab_3_time_paths.docx    Table A4: correlations and equation (A2)
#   tab_4_ca_test.docx       Table A5: equation (A3) on their published series
#   tab_5_case_study.docx    Table 1: window ending 31 Dec 2014
#   fig_1_tenet_paths.png    Figure 1: time paths relative to their means
#   fig_a1_tenet_levels.png  Figure A1: levels (Online Appendix B)
# ---- 0. Setup ----------------------------------------------------------------
library(tidyverse)
library(here)
library(flextable)
library(ggrepel)

dir_out <- here("output", "paper")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

tab_font <- "Times New Roman"   # match the manuscript font
tab_size <- 9

# Price-limit bound of equation (2) under the 10 % daily limit: the weekly
# bound is k * log(0.9), with k the institution's trading days in the week.
# slack allows for limit prices rounded to CNY 0.01, as in c02, section 4b.
bound_day <- log(0.9)
slack     <- 0.003
bound     <- 5 * bound_day   # five-day bound, reference line in fig_minima

# ---- 1. Data -----------------------------------------------------------------
# code as text, otherwise read_csv drops the leading zeros ("000001" -> 1)
t1 <- read_csv(here("output", "table1_check.csv"),
               col_types = cols(code = col_character()))

# matches per statistic, computed once in c02 (exact at reported precision)
matches <- read_csv(here("output", "table1_matches.csv"), show_col_types = FALSE)

# trading days of each institution in the week of its unadjusted minimum
days_min <- readRDS(here("data", "processed", "bs_weekly.rds")) |>
  filter(version == "unadjusted") |>
  select(code, week, n_days)

# ---- 2. Table A1: weekly minima ----------------------------------------------
# tab1_all covers all 24 institutions (Table A1 in Online Appendix A). The
# shorter table keeps only the institutions whose adjusted minimum differs
# from the reported one, i.e. those whose minimum is affected by corporate
# actions; it is not in the manuscript.
tab1_all <- t1 |>
  left_join(days_min, by = c("code", "week_min_unadj" = "week")) |>
  transmute(abbr       = abbr_paper,
            week       = format(week_min_unadj, "%Y-%m-%d"),
            days       = n_days,
            paper      = min_paper,
            unadjusted = min_unadjusted,
            adjusted   = min_adjusted,
            # below the bound for the trading days of that week
            below      = if_else(min_paper < days * (bound_day - slack), "yes", ""),
            # adjusted minimum differs from theirs at the reported precision
            affected   = abs(round(min_adjusted, 4) - min_paper) > 1e-8) |>
  arrange(desc(affected), paper)

# Same layout for both tables; hline_after = NULL draws no inner line
make_tab1 <- function(df, hline_after = NULL) {
  ft <- df |>
    select(-affected) |>
    flextable() |>
    set_header_labels(abbr = "Institution", week = "Week", days = "Days",
                      paper = "Wang et al. (2018)",
                      unadjusted = "BaoStock, unadjusted",
                      adjusted = "BaoStock, adjusted",
                      below = "Below bound") |>
    add_header_row(values = c("", "Minimum weekly log return", ""),
                   colwidths = c(3, 3, 1)) |>
    colformat_double(j = c("paper", "unadjusted", "adjusted"), digits = 4) |>
    theme_booktabs()
  if (!is.null(hline_after)) {
    ft <- hline(ft, i = hline_after,
                border = officer::fp_border(width = 0.5), part = "body")
  }
  ft |>
    align(j = 3:7, align = "center", part = "all") |>
    font(fontname = tab_font, part = "all") |>
    fontsize(size = tab_size, part = "all") |>
    padding(padding.top = 1, padding.bottom = 1, part = "all") |>
    line_spacing(space = 1, part = "all") |>
    autofit()
}

# Shorter table (not in the manuscript): the 16 affected institutions
ft1 <- make_tab1(filter(tab1_all, affected))
print(ft1)
save_as_docx(ft1, path = file.path(dir_out, "tab_1_extreme_weeks.docx"))

# Table A1 (Online Appendix A): all 24, line below the affected ones
ft_a1 <- make_tab1(tab1_all, hline_after = sum(tab1_all$affected))
print(ft_a1)
save_as_docx(ft_a1, path = file.path(dir_out, "tab_a1_extreme_weeks_all.docx"))

# ---- 3. Table A2: matches per statistic --------------------------------------
stat_labels <- c(mean = "Mean", sd = "Std. dev.", min = "Minimum",
                 max = "Maximum", jb = "Jarque-Bera")

tab2 <- matches |>
  mutate(stat = factor(stat_labels[stat], levels = stat_labels)) |>
  arrange(stat) |>
  mutate(stat = as.character(stat))
tab2 <- bind_rows(tab2, tibble(stat       = "Total",
                               unadjusted = sum(tab2$unadjusted),
                               adjusted   = sum(tab2$adjusted))) |>
  mutate(n = if_else(stat == "Total", 120L, 24L),
         across(c(unadjusted, adjusted), \(x) paste0(x, "/", n))) |>
  select(-n)

ft2 <- tab2 |>
  flextable() |>
  set_header_labels(stat = "Statistic", unadjusted = "Unadjusted",
                    adjusted = "Adjusted") |>
  theme_booktabs() |>
  align(j = 2:3, align = "center", part = "all") |>
  font(fontname = tab_font, part = "all") |>
  fontsize(size = tab_size, part = "all") |>
  autofit()
print(ft2)
save_as_docx(ft2, path = file.path(dir_out, "tab_2_matches.docx"))

# ---- 4. Scatter of the minima (not in the manuscript) ------------------------
fig_data <- t1 |>
  select(abbr = abbr_paper, paper = min_paper,
         Unadjusted = min_unadjusted, Adjusted = min_adjusted) |>
  pivot_longer(c(Unadjusted, Adjusted), names_to = "series", values_to = "ours") |>
  mutate(series = factor(series, levels = c("Unadjusted", "Adjusted")))

# labels for unadjusted points below the bound; HTSEC and PAI share a value
fig_labels <- fig_data |>
  filter(series == "Unadjusted", paper < bound) |>
  group_by(key = round(ours, 3)) |>
  summarise(paper = first(paper), ours = first(ours),
            label = paste(sort(abbr), collapse = ", "), .groups = "drop")

p_fig1 <- ggplot(fig_data, aes(paper, ours, colour = series, shape = series)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_hline(yintercept = bound, linetype = "dotted", linewidth = 0.3) +
  geom_vline(xintercept = bound, linetype = "dotted", linewidth = 0.3) +
  annotate("text", x = -0.9, y = bound, label = "price-limit bound",
           hjust = 0, vjust = -0.5, size = 2.5) +
  geom_point(size = 1.6) +
  geom_text_repel(data = fig_labels, aes(paper, ours, label = label),
                  inherit.aes = FALSE, size = 2.3, colour = "grey20",
                  nudge_x = 0.08, direction = "y", hjust = 0,
                  segment.size = 0.2, min.segment.length = 0) +
  scale_colour_manual(values = c(Unadjusted = "#1b4f72", Adjusted = "#e67e22")) +
  scale_shape_manual(values = c(Unadjusted = 16, Adjusted = 17)) +
  coord_equal(xlim = c(-0.9, -0.1), ylim = c(-0.9, -0.1)) +
  labs(x = "Minimum weekly log return, Wang et al. (2018), Table 1",
       y = "Minimum weekly log return, BaoStock",
       colour = NULL, shape = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
print(p_fig1)
ggsave(file.path(dir_out, "fig_minima.png"), p_fig1,
       width = 12, height = 12, units = "cm", dpi = 600, bg = "white")

# ---- 5. Results: data and helpers --------------------------------------------
tenet <- readRDS(here("data", "processed", "tenet_summary.rds"))
plots <- readRDS(here("data", "processed", "c05_plots.rds"))
stopifnot(!is.null(tenet$ca_res), !is.null(tenet$wang))

panel_labels <- c(total = "Total", bank = "Banks", securities = "Securities",
                  insurance = "Insurers")

# fixed decimals and a typographic minus, as in the text
fmt <- function(x, digits = 3) sub("^-", "\u2212", formatC(x, format = "f", digits = digits))
fmt_ci <- function(est, lo, hi, digits = 3) {
  paste0(fmt(est, digits), "\n[", fmt(lo, digits), ", ", fmt(hi, digits), "]")
}

# one label row per quantile level above its block of rows
add_panel_rows <- function(df) {
  df |>
    group_by(tau) |>
    group_modify(\(d, key) bind_rows(tibble(series = paste0("\u03c4 = ", fmt(key$tau, 2))), d)) |>
    ungroup() |>
    select(-tau)
}

style_tab <- function(ft) {
  ft |>
    theme_booktabs() |>
    font(fontname = tab_font, part = "all") |>
    fontsize(size = tab_size, part = "all") |>
    padding(padding.top = 1, padding.bottom = 1, part = "all") |>
    line_spacing(space = 1, part = "all")
}

# ---- 6. Table A4: correlations and equation (A2) -----------------------------
corr_wide <- tenet$comparison |>
  select(panel, version, tau, pearson, spearman) |>
  pivot_wider(names_from = version, values_from = c(pearson, spearman))

boot_wide <- tenet$boot |>
  filter(stat %in% c("d_pearson", "b_adj", "b_ca")) |>
  mutate(value = fmt_ci(estimate, lo, hi)) |>
  select(panel, tau, stat, value) |>
  pivot_wider(names_from = stat, values_from = value)

tab3 <- corr_wide |>
  left_join(boot_wide, by = c("panel", "tau")) |>
  mutate(series = panel_labels[panel],
         across(c(starts_with("pearson"), starts_with("spearman")), fmt)) |>
  arrange(tau, match(panel, names(panel_labels))) |>
  select(tau, series, pearson_unadjusted, pearson_adjusted, d_pearson,
         spearman_unadjusted, spearman_adjusted, b_adj, b_ca) |>
  add_panel_rows()
p3   <- which(str_starts(tab3$series, "\u03c4"))
tab3 <- mutate(tab3, across(everything(), \(x) replace_na(x, "")))

ft3 <- tab3 |>
  flextable() |>
  set_header_labels(series = "Series",
                    pearson_unadjusted = "Unadj.", pearson_adjusted = "Adj.",
                    d_pearson = "Difference",
                    spearman_unadjusted = "Unadj.", spearman_adjusted = "Adj.") |>
  compose(part = "header", j = "b_adj", value = as_paragraph("\u03b2", as_sub("adj"))) |>
  compose(part = "header", j = "b_ca", value = as_paragraph("\u03b2", as_sub("ca"))) |>
  add_header_row(values = c("", "Pearson", "Spearman", "Equation (A2)"),
                 colwidths = c(1, 3, 2, 2)) |>
  style_tab() |>
  italic(i = p3, part = "body") |>
  merge_h_range(i = p3, j1 = 1, j2 = ncol(tab3)) |>
  hline(i = p3[-1] - 1, border = officer::fp_border(width = 0.5), part = "body") |>
  align(j = 2:ncol(tab3), align = "center", part = "all") |>
  autofit()
print(ft3)
save_as_docx(ft3, path = file.path(dir_out, "tab_3_time_paths.docx"))

# ---- 7. Table A5: equation (A3) on their published series --------------------
tab4 <- tenet$ca_res |>
  filter(stat %in% c("g_ca", "g_stress", "dr2_ca")) |>
  mutate(value = if_else(stat == "dr2_ca", fmt(estimate), fmt_ci(estimate, lo, hi)),
         thr   = if_else(thr < -0.2, "t35", "t05")) |>
  select(panel, thr, stat, value) |>
  pivot_wider(names_from = c(thr, stat), values_from = value) |>
  mutate(series = panel_labels[panel]) |>
  arrange(match(panel, names(panel_labels))) |>
  select(series, t35_g_ca, t35_g_stress, t35_dr2_ca, t05_g_ca, t05_g_stress, t05_dr2_ca)

ft4 <- tab4 |>
  flextable() |>
  set_header_labels(series = "Series", t35_dr2_ca = "\u0394R\u00b2", t05_dr2_ca = "\u0394R\u00b2")
for (j in c("t35_g_ca", "t05_g_ca")) {
  ft4 <- compose(ft4, part = "header", j = j, value = as_paragraph("\u03b3", as_sub("ca")))
}
for (j in c("t35_g_stress", "t05_g_stress")) {
  ft4 <- compose(ft4, part = "header", j = j, value = as_paragraph("\u03b3", as_sub("stress")))
}
ft4 <- ft4 |>
  add_header_row(values = c("", paste("Threshold", fmt(-0.35, 2)),
                            paste("Threshold", fmt(-0.05, 2))),
                 colwidths = c(1, 3, 3)) |>
  style_tab() |>
  align(j = 2:7, align = "center", part = "all") |>
  autofit()
print(ft4)
save_as_docx(ft4, path = file.path(dir_out, "tab_4_ca_test.docx"))

# numbers that appear only in the text (Online Appendix C)
tenet$boot |>
  filter(stat == "d_spearman", tau == 0.01) |>
  select(panel, estimate, lo, hi) |>
  print()

tenet$ca_res |>
  filter(panel == "insurance", stat == "g_ca") |>
  select(thr, na_share) |>
  print()

# ---- 8. Table 1: case study, window ending 31 Dec 2014 -----------------------
cs <- tenet$case_study
br <- tenet$broker_ranks
w  <- tenet$wang
brokers <- c("CJSC", "PSC", "NESC")
sig3 <- \(x) formatC(signif(x, 3), digits = 3, format = "fg", flag = "#")

runs_long <- bind_rows(
  cs |> transmute(version, tau, key = "links_brokers", value = as.character(to_brokers)),
  cs |> transmute(version, tau, key = "links_theirs",  value = as.character(edges_theirs)),
  br |> transmute(version, tau, key = paste0("rank_", abbr), value = as.character(rank_isi)),
  br |> filter(abbr %in% c("CJSC", "PSC")) |>
    transmute(version, tau, key = paste0("isi_", abbr), value = sig3(isi)),
  cs |> transmute(version, tau, key = "rho_isi",   value = fmt(rho_isi, 2)),
  cs |> transmute(version, tau, key = "rho_edges", value = fmt(rho_edges, 2)),
  cs |> pivot_longer(c(isi, osi, srr, sre), names_to = "m", values_to = "v") |>
    transmute(version, tau, key = paste0("overlap_", m), value = as.character(v))
) |>
  mutate(col = paste0(version, "_", tau)) |>
  pivot_wider(id_cols = key, names_from = col, values_from = value)

wang_isi <- \(a) fmt(w$tab$paper[w$tab$measure == "isi" & w$tab$abbr == a], 2)
wang_col <- tibble(
  key  = c("links_brokers", "links_theirs",
           paste0("rank_", c("CJSC", "PSC", "NESC", "SLSC")),
           "isi_CJSC", "isi_PSC", "rho_isi", "rho_edges",
           paste0("overlap_", c("isi", "osi", "srr", "sre"))),
  wang = c(as.character(sum(w$edges$to %in% brokers)), "\u2013",
           as.character(match(c("CJSC", "PSC", "NESC", "SLSC"), w$top$isi)),
           wang_isi("CJSC"), wang_isi("PSC"), rep("\u2013", 6))
)

rows5 <- tribble(
  ~group,                              ~key,              ~label,
  "Ten strongest links",               "links_brokers",   "ending at CJSC, PSC or NESC",
  "Ten strongest links",               "links_theirs",    "also in the top ten of Wang et al.",
  "ISI rank",                          "rank_CJSC",       "CJSC",
  "ISI rank",                          "rank_PSC",        "PSC",
  "ISI rank",                          "rank_NESC",       "NESC",
  "ISI rank",                          "rank_SLSC",       "SLSC",
  "ISI",                               "isi_CJSC",        "CJSC",
  "ISI",                               "isi_PSC",         "PSC",
  "Rank correlation with Wang et al.", "rho_isi",         "ISI, their top ten institutions",
  "Rank correlation with Wang et al.", "rho_edges",       "Link weights, their top ten links",
  "Overlap of top-ten lists",          "overlap_isi",     "ISI",
  "Overlap of top-ten lists",          "overlap_osi",     "OSI",
  "Overlap of top-ten lists",          "overlap_srr",     "SRR",
  "Overlap of top-ten lists",          "overlap_sre",     "SRE"
)

tab5 <- rows5 |>
  left_join(wang_col, by = "key") |>
  left_join(runs_long, by = "key") |>
  mutate(group = factor(group, levels = unique(rows5$group))) |>
  group_by(group) |>
  group_modify(\(d, key) bind_rows(tibble(label = as.character(key$group)), d)) |>
  ungroup() |>
  select(label, wang, unadjusted_0.01, adjusted_0.01, unadjusted_0.05, adjusted_0.05)
g5   <- which(is.na(tab5$wang))
tab5 <- mutate(tab5, across(everything(), \(x) replace_na(x, "")))

ft5 <- tab5 |>
  flextable() |>
  set_header_labels(label = "", wang = "Wang et al.",
                    unadjusted_0.01 = "Unadj.", adjusted_0.01 = "Adj.",
                    unadjusted_0.05 = "Unadj.", adjusted_0.05 = "Adj.") |>
  add_header_row(values = c("", "", "\u03c4 = 0.01", "\u03c4 = 0.05"),
                 colwidths = c(1, 1, 2, 2)) |>
  style_tab() |>
  italic(i = g5, part = "body") |>
  merge_h_range(i = g5, j1 = 1, j2 = 6) |>
  padding(i = setdiff(seq_len(nrow(tab5)), g5), j = 1, padding.left = 12, part = "body") |>
  align(j = 2:6, align = "center", part = "all") |>
  autofit()
print(ft5)
save_as_docx(ft5, path = file.path(dir_out, "tab_5_case_study.docx"))

# ---- 9. Figures 1 and A1 -----------------------------------------------------
# c05 plots without title and subtitle; captions go into the manuscript
p_fig2 <- plots$norm + labs(title = NULL, subtitle = NULL)
ggsave(file.path(dir_out, "fig_1_tenet_paths.png"), p_fig2,
       width = 17, height = 19, units = "cm", dpi = 600, bg = "white")

p_fig_a2 <- plots$levels + labs(title = NULL)
ggsave(file.path(dir_out, "fig_a1_tenet_levels.png"), p_fig_a2,
       width = 17, height = 20, units = "cm", dpi = 600, bg = "white")
