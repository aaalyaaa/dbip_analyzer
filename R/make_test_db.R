
#'Create HTML Report with DB-IP Data Visualizations with test data
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`test_docs/test_index.html`)
#' @export

make_demo_dashboard <- function() {

  data("demo", package = "dbipAnalyzer")
  test_dir <- file.path(getwd(), "test_docs")
  if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
  dir.create(test_dir, recursive = TRUE)

  arrow::write_parquet(demo, file.path(test_dir, "demo.parquet"))

  quarto_dir <- system.file("quarto", package = "dbipAnalyzer")
  qmd_file <- file.path(quarto_dir, "index1.qmd")

  if (!file.exists(qmd_file)) {
    stop("Файл index1.qmd не найден в inst/quarto/")
  }

  all_files <- list.files(quarto_dir, full.names = TRUE)
  file.copy(all_files, test_dir, recursive = TRUE)

  processed_dir <- file.path(test_dir, "processed")
  dir.create(processed_dir, recursive = TRUE)
  file.copy(file.path(test_dir, "demo.parquet"),
            file.path(processed_dir, "demo.parquet"))

  old_wd <- getwd()
  setwd(test_dir)
  cat("Ищем demo.rda...\n")

  # 1. Проверяем в исходниках (для разработки)
  demo_rda_path <- NULL

  # Сначала ищем в предполагаемой директории исходников
  potential_paths <- c(
    "../../dbipAnalyzer/data/demo.rda",          # Относительный путь
    "../dbipAnalyzer/data/demo.rda",             # Другой относительный путь
    file.path(getwd(), "dbipAnalyzer/data/demo.rda"),  # Текущая директория
    file.path(dirname(.libPaths()[1]), "dbipAnalyzer/data/demo.rda")  # Рядом с библиотекой
  )

  for (path in potential_paths) {
    if (file.exists(path)) {
      demo_rda_path <- path
      break
    }
  }

  # 2. Если не нашли, пробуем через system.file
  if (is.null(demo_rda_path)) {
    demo_rda_path <- system.file("data", "demo.rda", package = "dbipAnalyzer", mustWork = FALSE)
  }

  # 3. Если нашли файл, копируем его
  if (!is.null(demo_rda_path) && file.exists(demo_rda_path)) {
    temp_data_dir <- file.path(temp_quarto, "data")
    dir.create(temp_data_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(demo_rda_path, file.path(temp_data_dir, "demo.rda"))
    cat("Скопирован demo.rda из:", demo_rda_path, "\n")
  } else {
    warning("Файл demo.rda не найден. Будет использоваться запасной вариант.")
  }
  quarto::quarto_render("index1.qmd", quiet = FALSE)
  setwd(old_wd)

  html_file <- file.path(test_dir, "index1.html")
  final_file <- file.path(test_dir, "test_index.html")

  if (file.exists(html_file)) {
    file.rename(html_file, final_file)
  } else if (file.exists(file.path(test_dir, "docs", "index.html"))) {
    file.copy(file.path(test_dir, "docs", "index.html"), final_file)
  }

  files_to_keep <- c("demo.parquet", "test_index.html")
  all_current <- list.files(test_dir, full.names = TRUE)

  for (f in all_current) {
    if (!basename(f) %in% files_to_keep) {
      unlink(f, recursive = TRUE)
    }
  }

  message("Дашборд успешно создан и находится в файле test_docs/test_index.html", final_file)
  if (interactive()) browseURL(final_file)

  return(invisible(final_file))
}
