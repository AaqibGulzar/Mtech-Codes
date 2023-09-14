stock_data_tbl <- c("AAPL", "GOOG", "NFLX", "NVDA") %>%
  tq_get(from = "2010-01-01", to = "2019-12-31")
stock_pivot_table <- stock_data_tbl %>%
  pivot_table(
    .rows = ~ YEAR(date),
    .columns = ~ symbol,
    .values = ~ PCT_CHANGE_FIRSTLAST(adjusted)
  ) %>%
  rename(year = 1)
stock_plot <- stock_data_tbl %>%
  group_by(symbol) %>%
  plot_time_series(
    date,
    adjusted,
    .color_var = symbol,
    .facet_ncol = 2,
    .interactive = FALSE
  )
wb <- createWorkbook()
addWorksheet(wb, sheetName = "stock_analysis")
print(stock_plot)
wb %>% insertPlot(sheet = "stock_analysis",
                  startCol = "G",
                  startRow = 3)
writeDataTable(wb, sheet = "stock_analysis", x = stock_pivot_table)
saveWorkbook(wb, "c:/Users/Hp/Desktop/R Programming/CODES FOR R/stock_analysis.xlsx")
openXL("c:/Users/Hp/Desktop/R Programming/CODES FOR R/stock_analysis.xlsx")