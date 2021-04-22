find_numeric_summary <- function(input_data) {
  data_types <- find_data_types(input_data)
  data_types <-
    data_types %>%
    filter(`Data Type` == "num") %>%
    filter(Variable != "loanid") %>%
    mutate(`Data Type` = to_title_case(`Data Type`)) %>%
    rename("Type" = "Data Type")
  missing_information <- find_missing_information(input_data)
  missing_information <-
    missing_information %>%
    filter(Variable %in% data_types$Variable) %>%
    select("Variable", "Missing", "% Missing")
  numeric_summary <- merge(data_types, missing_information)
  percentiles_1 <-
    find_percentiles(input_data$`Origination Balance`)
  percentiles_2 <-
    find_percentiles(input_data$`Outstanding Balance`)
  percentiles_3 <- find_percentiles(input_data$interest.rate)
  percentiles_table <-
    rbind(percentiles_1, percentiles_2, percentiles_3) %>% mutate(Variable = c(
      "Origination Balance",
      "Outstanding Balance",
      "interest.rate"
    )) %>%
    select(c(8, 1:7))
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  colMin <- function(data) sapply(data, min, na.rm = TRUE)
  column_max <-
    rownames_to_column(as.data.frame(format(round(colMax(
      input_data %>% select(
        "Origination Balance",
        "Outstanding Balance",
        "interest.rate"
      )
    )), big.interval = 3, big.mark = ",")), "Variable")
  column_min <-
    rownames_to_column(as.data.frame(format(round(colMin(
      input_data %>% select(
        "Origination Balance",
        "Outstanding Balance",
        "interest.rate"
      )
    )), big.interval = 3, big.mark = ",")), "Variable")
  column_mean <-
    rownames_to_column(as.data.frame(format(round(
      colMeans(
        input_data %>% select(
          "Origination Balance",
          "Outstanding Balance",
          "interest.rate"
        ),
        na.rm = T
      )
    ), big.interval = 3, big.mark = ",")), "Variable")
  min_max_mean <-
    merge(merge(column_max, column_min, sort = F), column_mean, sort = F)
  numeric_summary <-
    merge(numeric_summary, percentiles_table, sort = F)
  numeric_summary <- merge(numeric_summary, min_max_mean, sort = F)
  numeric_summary <-
    numeric_summary %>% select(c(1, 2, 3, 4, 13, 5:8, 14, 9:12))
  colnames(numeric_summary) <-
    c(
      "Variable",
      "Type",
      "Missing",
      "% Missing",
      "Min",
      "p01",
      "p05",
      "p25",
      "p50",
      "mean",
      "p75",
      "p95",
      "p99",
      "Max"
    )
  numeric_summary
}
