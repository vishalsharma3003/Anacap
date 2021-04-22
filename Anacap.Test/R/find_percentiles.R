find_percentiles <- function(input_data) {
  as.data.frame(t(format(
    round(quantile(
      input_data, c(.01, .05, .25, .50, .75, .95, .99), na.rm = T
    )),
    big.interval = 3,
    big.mark = ","
  )))
}
