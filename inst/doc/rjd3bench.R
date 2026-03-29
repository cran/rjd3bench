## ----setup, include = FALSE---------------------------------------------------
is_cran <- identical(Sys.getenv("_R_CHECK_CRAN_INCOMING_"), "TRUE")

knitr::opts_chunk$set(
    collapse = TRUE,
    echo = TRUE,
    eval = !is_cran,
    comment = "#>"
)

## -----------------------------------------------------------------------------
library("rjd3bench")
Retail <- rjd3toolkit::Retail
qna_data <- rjd3bench::qna_data

## -----------------------------------------------------------------------------
# Example 1: TD using Chow-Lin to disaggregate annual value added in construction sector using a quarterly indicator
Y <- ts(qna_data$B1G_Y_data[, "B1G_FF"], frequency = 1, start = c(2009, 1))
x <- ts(qna_data$TURN_Q_data[, "TURN_INDEX_FF"], frequency = 4, start = c(2009, 1))
td <- rjd3bench::temporal_disaggregation(Y, indicators = x)

y <- td$estimation$disagg # the disaggregated series
print(td)

# Example 2: interpolation using Fernandez without indicator when the last value (default) of the interpolated series is the one consistent with the low frequency series.
Y <- rjd3toolkit::aggregate(rjd3toolkit::Retail$RetailSalesTotal, 1)
ti <- temporal_interpolation(Y, indicators = NULL, model = "Rw", freq = 4, nfcsts = 2)
y <- ti$estimation$interp # the interpolated series

# Example 3: TD of atypical frequency data using Fernandez with an offset of 1 period
Y <- c(500,510,525,520)
x <- c(97,
       98, 98.5, 99.5, 104, 99,
       100, 100.5, 101, 105.5, 103,
       104.5, 103.5, 104.5, 109, 104,
       107, 103, 108, 113, 110)
td_raw <- temporal_disaggregation_raw(Y, indicators = x, startoffset = 1,  model = "Rw", freqratio = 5)
y <- td_raw$estimation$disagg # the disaggregated series

# Example 4: interpolation of atypical frequency data using Fernandez without offset, when the first value of the interpolated  series is the one consistent with the low frequency series.
Y <- c(500,510,525,520)
x <- c(490, 492.5, 497.5, 520, 495,
       500, 502.5, 505, 527.5, 515,
       522.5, 517.5, 522.5, 545, 520,
       535, 515, 540, 565, 550,
       560)
ti_raw <- temporal_interpolation_raw(Y, indicators = x,  model = "Rw", freqratio = 5, obsposition = 1)
y <- ti_raw$estimation$interp

## -----------------------------------------------------------------------------
# Example: Use of model-based Denton for temporal disaggregation
Y <- ts(qna_data$B1G_Y_data[, "B1G_FF"], frequency = 1, start = c(2009, 1))
x <- ts(qna_data$TURN_Q_data[, "TURN_INDEX_FF"], frequency = 4, start = c(2009, 1))
td_mbd <- denton_modelbased(Y, x, outliers = list("2020-01-01" = 100, "2020-04-01" = 100))

y_mbd <- td_mbd$estimation$disagg
print(td_mbd)

## -----------------------------------------------------------------------------
# Example: Use of ADL models for temporal disaggregation

Y <- ts(qna_data$B1G_Y_data[, "B1G_FF"], frequency = 1, start = c(2009, 1))
x <- ts(qna_data$TURN_Q_data[, "TURN_INDEX_FF"], frequency = 4, start = c(2009, 1))

## 1. without constraints

td_adl <- adl_disaggregation(Y, indicators = x, xar = "FREE")
y <- td_adl$estimation$disagg # the disaggregated series
print(td_adl)

## 2. with constraints

### b1 = -phi * b0 (~ Chow-Lin)
td_adl_constr_1 <- adl_disaggregation(Y, indicators = x, xar = "SAME")

### phi = 1 and b1 = -b0 (~ Fernandez)
td_adl_constr_2 <- adl_disaggregation(Y, constant = FALSE, indicators = x, xar = "SAME", phi = 1, phi.fixed = TRUE)

### b1 = 0 (~ Santos Silva-Cardoso)
td_adl_constr_3 <- adl_disaggregation(Y, indicators = x, xar = "NONE")

## LR test for assessing constraint(s)
LR_stat <- -2 * (td_adl_constr_1$likelihood$ll - td_adl$likelihood$ll) # -> H0 not rejected. Chow-Lin specification is supported here.

## -----------------------------------------------------------------------------
Y <- ts(qna_data$B1G_Y_data[, "B1G_FF"], frequency = 1, start = c(2009, 1))
x <- ts(qna_data$TURN_Q_data[, "TURN_INDEX_FF"], frequency = 4, start = c(2009, 1))
td_rv <- temporaldisaggregationI(Y, indicator = x)

y_rv <- td_rv$estimation$disagg # the disaggregated series
print(td_rv)

## ----eval = FALSE-------------------------------------------------------------
# # Comparison with Chow-Lin
# td_cl <- temporal_disaggregation(Y, indicators = x)
# y_cl <- td_cl$estimation$disagg
# stats::ts.plot(y_rv, y_cl, gpars=list(col=c("red", "blue"), xaxt="n", main="Reverse regression vs Chow-Lin"))
# legend("topleft",c("Reverse regression", "Chow-Lin"), lty = c(1,1), col=c("red", "blue"))

## -----------------------------------------------------------------------------
# Example 1: use of Denton method for benchmarking
Y <- ts(qna_data$B1G_Y_data[, "B1G_HH"], frequency = 1, start = c(2009, 1))

y_den0 <- denton(t = Y, nfreq = 4) # denton PFD without high frequency series

x <- y_den0 + rnorm(n = length(y_den0), mean = 0, sd = 10)
y_den1 <- denton(s = x, t = Y) # denton PFD (= the default)
y_den2 <- denton(s = x, t = Y, d = 2, mul = FALSE) # denton ASD

# Example 2: use of of Denton method for benchmarking atypical frequency data
Y <- c(500,510,525,520)
x <- c(97, 98, 98.5, 99.5, 104,
       99, 100, 100.5, 101, 105.5,
       103, 104.5, 103.5, 104.5, 109,
       104, 107, 103, 108, 113,
       110)

y_denraw <- denton_raw(x, Y, freqratio = 5) # for example, x and Y could be annual and quiquennal series respectively

## -----------------------------------------------------------------------------
# Example: use GRP method for benchmarking
Y <- ts(qna_data$B1G_Y_data[, "B1G_HH"], frequency = 1, start = c(2009, 1))
y_den0 <- denton(t = Y, nfreq = 4)
x <- y_den0 + rnorm(n = length(y_den0), mean = 0, sd = 10)

y_grpf <- grp(s = x, t = Y)
y_grpl <- grp(s = x, t = Y, objective = "Log")

## -----------------------------------------------------------------------------
# Example: use cubic splines for benchmarking
y_cs1 <- cubicspline(t = Y, nfreq = 4) # without high frequency series (smoothing)

x <- y_cs1 + rnorm(n = length(y_cs1), mean = 0, sd = 10)
y_cs2 <- cubicspline(s = x, t = Y) # with a high frequency preliminary series to benchmark

## -----------------------------------------------------------------------------
# Example: use Cholette method for benchmarking
Y <- ts(qna_data$B1G_Y_data[, "B1G_HH"], frequency = 1, start = c(2009, 1))
xn <- c(denton(t = Y, nfreq = 4) + rnorm(n = length(Y)*4, mean = 0, sd = 10), 5750, 5800)
x <- ts(xn, start = start(Y), frequency = 4)

y_cho1 <- cholette(s = x, t = Y, rho = 0.729, lambda = 1, bias = "Multiplicative")  # proportional benchmarking
y_cho2 <- cholette(s = x, t = Y, rho = 0.729, lambda = 1) # proportional benchmarking with no bias (assuming bm=1)
y_cho3 <- cholette(s = x, t = Y, rho = 0.729, lambda = 0, bias = "Additive")  # additive benchmarking
y_cho4 <- cholette(s = x, t = Y, rho = 1, lambda = 1) # Denton PFD
y_cho5 <- cholette(s = x, t = Y, rho = 0, lambda = 0.5) # pro-rating

## -----------------------------------------------------------------------------
# Example: use the multivariate Cholette method for reconciliation
x1 <- ts(c(7, 7.2, 8.1, 7.5, 8.5, 7.8, 8.1, 8.4), frequency = 4, start = c(2010, 1))
x2 <- ts(c(18, 19.5, 19.0, 19.7, 18.5, 19.0, 20.3, 20.0), frequency = 4, start = c(2010, 1))
x3 <- ts(c(1.5, 1.8, 2, 2.5, 2.0, 1.5, 1.7, 2.0), frequency = 4, start = c(2010, 1))

z <- ts(c(27.1, 29.8, 29.9, 31.2, 29.3, 27.9, 30.9, 31.8), frequency = 4, start = c(2010, 1))

Y1 <- ts(c(30.0, 30.6), frequency = 1, start = c(2010, 1))
Y2 <- ts(c(80.0, 81.2), frequency = 1, start = c(2010, 1))
Y3 <- ts(c(8.0, 8.1), frequency = 1, start = c(2010, 1))

### check consistency between temporal and contemporaneous constraints
lfs <- cbind(Y1,Y2,Y3)
as.numeric(rowSums(lfs) - stats::aggregate.ts(z)) # should all be 0

data_list <- list(x1 = x1, x2 = x2, x3 = x3, z = z, Y1 = Y1, Y2 = Y2, Y3 = Y3)
tc <- c("Y1 = sum(x1)", "Y2 = sum(x2)", "Y3 = sum(x3)") # temporal constraints
cc <- c("z = x1+x2+x3") # (binding) contemporaneous constraint
cc_nb <- c("0 = x1+x2+x3-z") # non-binding contemporaneous constraint

rec1 <- multivariatecholette(xlist = data_list, tcvector = tc, ccvector = cc, rho = .5, lambda = .5) # trade-off values for rho and lambda
print(rec1)
rec2 <- multivariatecholette(xlist = data_list, tcvector = tc, ccvector = cc, rho = 1) # Denton
rec3 <- multivariatecholette(xlist = data_list, tcvector = tc, ccvector = cc, rho = 0.729) # Cholette
rec4 <- multivariatecholette(xlist = data_list, tcvector = NULL, ccvector = cc) # no temporal constraints
rec5 <- multivariatecholette(xlist = data_list, tcvector = tc, ccvector = cc_nb) # non-binding contemporaneous constraint

## -----------------------------------------------------------------------------
# Example of calendarization (from Quenneville et al (2012))

## Observed data
obs <- list(
    list(start = "2009-02-18", end = "2009-03-17", value = 9000),
    list(start = "2009-03-18", end = "2009-04-14", value = 5000),
    list(start = "2009-04-15", end = "2009-05-12", value = 9500),
    list(start = "2009-05-13", end = "2009-06-09", value = 7000))

## calendarization in absence of daily indicator values (or weights)
cal_1 <- calendarization(obs, 12, end = "2009-06-30", dailyweights = NULL, stde = TRUE)

ym_1 <- cal_1$rslt
eym_1 <- cal_1$erslt
yd_1 <- cal_1$days
eyd_1 <- cal_1$edays
print(cal_1)

## calendarization in presence of daily indicator values (or weights)
x <- rep(c(1.0, 1.2, 1.8 , 1.6, 0.0, 0.6, 0.8), 19)
cal_2 <- calendarization(obs, 12, end = "2009-06-30", dailyweights = x, stde = TRUE)

ym_2 <- cal_2$rslt
eym_2 <- cal_2$erslt
yd_2 <- cal_2$days
eyd_2 <- cal_2$edays

