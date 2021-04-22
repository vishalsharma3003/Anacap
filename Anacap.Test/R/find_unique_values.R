find_unique_values <- function(input_data) {
  unique_values <-
    rownames_to_column(as.data.frame(sapply(input_data, function(x) {
      length(unique(x))
    })), var = "variable")
  colnames(unique_values) <- c("Variable", "Unique Values")
  unique_values
}
