find_data_types <- function(input_data) {
  data_types <-
    setDT(as.data.frame(sapply(input, class)), keep.rownames = "Variable")
  colnames(data_types) <- c("variable", "data_type")
  data_types <-
    data_types %>% mutate(data_type = ifelse(
      data_type == "numeric",
      "num",
      ifelse(
        data_type == "character",
        "char",
        ifelse(data_type == "Date", "date", data_type)
      )
    ))
  colnames(data_types) <- c("Variable", "Data Type")
  data_types
}
