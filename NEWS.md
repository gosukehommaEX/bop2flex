# bop2flex 0.1.0

* First version.
* `flex_design()` calibrates the BOP2 design of Zhou et al. (2017) and the BOP2-FE design of Xu et al. (2025), with the power or the O'Brien-Fleming type efficacy cutoff and binding or non-binding futility, using exact operating characteristics.
* `flex_design()` also returns, in `calibration$equivalent`, every grid point that gives the selected boundaries. The reported tuning parameters are the first of them in grid order.
* `flex_plan()` specifies a planned design by its tuning parameters or by its boundaries.
* `flex_boundary()`, `flex_oc()` and `flex_posterior()` compute boundaries, exact operating characteristics and posterior probabilities.
* `flex_spending()` and `flex_update()` compute boundaries at actual analyses from the error spending function implied by the planned design, with a floor on the efficacy boundary at the planned final posterior cutoff.
* `flex_update()` accepts another spending function (`spending`) and can omit the floor (`efficacy_floor = FALSE`), for sensitivity analyses.
* `flex_naive()`, `flex_compare()` and `flex_schedules()` compare the implied spending approach with the naive use of the planned cutoffs.
* The tests check the boundaries and operating characteristics against Zhou et al. (2017, Tables I and II), Xu et al. (2025, Tables 1 to 3 and Supplementary Table S1) and an independent Python implementation.
