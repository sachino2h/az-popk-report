
# function to calculate median and IQR
.median_iqr = function(value, fmt = sig, ...) {
  median <- fmt(median(value, na.rm = TRUE), ...)
  p25 <- fmt(quantile(value, 0.25, na.rm = TRUE), ...)
  p75 <- fmt(quantile(value, 0.75, na.rm = TRUE), ...)
  paste0(median, " (", p25, ", ", p75, ")")
}

# custom function for pt_demographics that makes ANZ specific summaries 
cont_long_custom <- function(value, ..., fmt = sig,
                             digits = 3, maxex = 5) {
  value <- na.omit(value)
  ans <- tibble(
    `Mean (SD)` = pmtables:::.mean_sd(value, fmt = fmt, digits = digits, maxex = maxex),
    `Median (IQR)` = .median_iqr(value, fmt = fmt, digits = digits, maxex = maxex),
    `Min / Max` = pmtables:::.min_max(value, fmt = fmt, digits = digits, maxex = maxex),
    `Missing` = as.character(sum(is.na(value)), digits = digits, maxex = maxex)
  )
  ans
}