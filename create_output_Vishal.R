library(readxl)
input <-
  read_xlsx(
    "./Q1a - Input.xlsx",
    col_types = c(
      "numeric",
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "guess",
      "text",
      "guess"
    )
  )
input$`Origination Date` <-
  as.Date(input$`Origination Date`, "%d/%m/%Y")
input$`date created` <- as.Date(input$`date created`, "%d-%b-%y")
create_descriptive_statistics(input_data = input, output_path = "./Anacap/")
