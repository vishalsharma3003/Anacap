#' Descriptive Statistics
#'
#' Reads the input file provided by Anacap and creates a new file with descriptive statistics
#' @param input_data A tibble of input data
#' @param output_path The path of where output file has to be created
#' @keywords stats
#' @export
#' @examples
#' create_descriptive_statistics(input_data)

create_descriptive_statistics <- function(input_data, output_path) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    print("Package \"tibble\" needed for this function to work. Please install it.",
          call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    print("Package \"ggplot2\" needed for this function to work. Please install it.",
          call. = FALSE)
  }
  if (is_tibble(class(input_data))) {
    print(
      "The input data should be a tibble only. Use tibble::to_tibble() function to convert into tibble"
    )
  }

  overall_tab <- find_missing_information(input_data)
  numeric_tab <- find_numeric_summary(input_data)

  graph_path = output_path
  png(filename = paste0(graph_path, "graph1_numeric.png"))
  hist(
    input_data$`Origination Balance`,
    col = "#4472c4",
    border = "white",
    main = "Origination Balance",
    xlab = "",
    ylab = ""
  )
  dev.off()

  graph_path = output_path
  png(filename = paste0(graph_path, "graph2_numeric.png"))
  hist(
    input_data$`Outstanding Balance`,
    col = "#4472c4",
    border = "white",
    main = "Outstanding Balance",
    xlab = "",
    ylab = ""
  )
  dev.off()

  graph_path = output_path
  png(filename = paste0(graph_path, "graph3_numeric.png"))
  hist(
    input_data$interest.rate,
    col = "#4472c4",
    border = "white",
    main = "interest.rate",
    xlab = "",
    ylab = ""
  )
  dev.off()

  char_tab_table_1 <-
    input_data %>% group_by(country) %>% summarize(Count = n())
  char_tab_table_1 <- char_tab_table_1[c(4, 1:3), ]
  char_tab_table_1 <-
    rbind(char_tab_table_1, c("Grand Total", colSums(char_tab_table_1[, 2])))

  char_tab_table_2 <-
    input_data %>% group_by(Asset) %>% summarize(Count = n())
  char_tab_table_2 <- char_tab_table_2[c(3, 1:2), ]
  char_tab_table_2 <-
    rbind(char_tab_table_2, c("Grand Total", colSums(char_tab_table_2[, 2])))

  char_tab_table_3 <-
    input_data %>% group_by(currency) %>% summarize(Count = n())
  char_tab_table_3 <-
    rbind(char_tab_table_3, c("Grand Total", colSums(char_tab_table_3[, 2])))

  output_workbook <- createWorkbook()
  addWorksheet(output_workbook, "Overall")
  addWorksheet(output_workbook, "Numeric")
  addWorksheet(output_workbook, "Char")

  writeData(
    output_workbook,
    sheet = "Overall",
    x = overall_tab,
    startRow = 4,
    borders = "all"
  )
  writeData(output_workbook,
            sheet = "Numeric",
            x = numeric_tab,
            startRow = 3)
  insertImage(
    output_workbook,
    sheet = "Numeric",
    paste0(graph_path, "graph1_numeric.png"),
    startRow = 12,
    width = 7,
    height = 6
  )
  insertImage(
    output_workbook,
    sheet = "Numeric",
    paste0(graph_path, "graph2_numeric.png"),
    startRow = 45,
    width = 7,
    height = 6
  )
  insertImage(
    output_workbook,
    sheet = "Numeric",
    paste0(graph_path, "graph3_numeric.png"),
    startRow = 78,
    width = 7,
    height = 6
  )
  writeData(output_workbook,
            sheet = "Char",
            x = char_tab_table_1,
            startRow = 4)
  writeData(output_workbook,
            sheet = "Char",
            x = char_tab_table_2,
            startRow = 13)
  writeData(output_workbook,
            sheet = "Char",
            x = char_tab_table_3,
            startRow = 22)

  saveWorkbook(output_workbook, paste0(output_path, "Output Statistics.xlsx"))
  file.remove(c(
    paste0(graph_path, "graph1_numeric.png"),
    paste0(graph_path, "graph2_numeric.png"),
    paste0(graph_path, "graph3_numeric.png")
  ))
}
