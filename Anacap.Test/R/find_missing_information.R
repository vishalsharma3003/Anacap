find_missing_information <- function(input_data) {
  n <- dim(input_data)[1]
  na_count <-
    sapply(input_data, function(y)
      sum(length(which(is.na(
        y
      )))))
  na_count <- data.frame(na_count)
  na_count <- tibble::rownames_to_column(na_count, var = "Variable")
  na_count <- na_count %>%
    mutate(percent = paste0(round((na_count / n) * 100, 2), "%"))
  data_types <- find_data_types(input_data)
  na_count <-
    merge(
      na_count,
      data_types,
      by.x = "Variable",
      by.y = "Variable",
      sort = FALSE
    )
  na_count <- na_count %>% mutate(unique_values = unique(Variable))
  unique_values <- find_unique_values(input_data)
  na_count
  na_count <-
    merge(
      na_count,
      unique_values,
      by.x = "Variable",
      by.y = "Variable",
      all.x = TRUE,
      sort = FALSE
    )
  na_count <- na_count[, c(1, 4, 2, 3, 6)]
  na_count <-
    na_count %>% rename("Missing" = "na_count", "% Missing" = "percent")
  na_count
}
