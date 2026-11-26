# Replication code: Comment on Wang et al. (2018)

Code for the comment on Wang, Jiang, Lin, Xie and Stanley (2018),
"Interconnectedness and systemic risk of China's financial institutions",
Emerging Markets Review 35, 1-18.

## Requirements
- R 4.5.1; R packages in renv.lock
- reticulate 1.47.0; c01 creates the Python environment "baostock"
  (Python 3.12.10, baostock 0.9.4, pandas 3.1.0, numpy 2.5.4)

## Run
Run c01, c02, c03, c04, c05 and c99 in this order, each in a fresh R session.
c03 and c04 stop if a server refuses requests; run them again later.

## Data
All data are public and downloaded by the scripts (BaoStock, Eastmoney,
ChinaBond, China Securities Index Co.); data/ is not included.
external/ holds the series extracted from Figures 1 and 2 of Wang et al.
(2018).

## License
MIT, see LICENSE.