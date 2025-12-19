
#'Create HTML Report with DB-IP Data Visualizations
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`docs/index.html`)
#' @export

make_dashboard <- function() {

  user_dir <- getwd()
  user_data_path <- file.path(user_dir, "processed", "dbip_data.parquet")

  if (!file.exists(user_data_path)) {
    stop("Run run_etl_pipeline() first")
  }

  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  temp_dir <- tempfile("dashboard_")
  dir.create(temp_dir)
  file.copy(quarto_path, temp_dir, recursive = TRUE)

  temp_quarto <- file.path(temp_dir, "quarto")

  file.copy(user_data_path, file.path(temp_quarto, "dbip_data.parquet"))
  temp_processed <- file.path(temp_quarto, "processed")
  if (!dir.exists(temp_processed)) dir.create(temp_processed)
  file.copy(user_data_path, file.path(temp_processed, "dbip_data.parquet"))

  # Создаем директорию docs в рабочей директории пользователя
  final_docs_dir <- file.path(user_dir, "docs")
  if (!dir.exists(final_docs_dir)) {
    dir.create(final_docs_dir, recursive = TRUE)
  }

  # Определяем полный путь к целевому файлу
  final_html_path <- file.path(user_dir, "docs", "index.html")

  # Временная копия для отладки
  cat("🔍 Debug info:\n")
  cat("User dir:", user_dir, "\n")
  cat("Final HTML path:", final_html_path, "\n")
  cat("Temp quarto dir:", temp_quarto, "\n")

  # Выполняем рендеринг с указанием выходного файла
  tryCatch({
    quarto::quarto_render(
      input = file.path(temp_quarto, "index.qmd"),
      output_file = final_html_path,
      quiet = FALSE
    )
    cat("✅ Quarto render completed\n")
  }, error = function(e) {
    cat("❌ Quarto render error:", e$message, "\n")
    stop(e)
  })

  # Проверяем, создался ли файл
  if (file.exists(final_html_path)) {
    cat("✅ Dashboard created\n")
    cat("📄 Full path to report:", normalizePath(final_html_path), "\n")

    # Возвращаем путь невидимо
    return(invisible(final_html_path))
  } else {
    # Проверяем, создался ли файл где-то еще
    temp_html <- file.path(temp_quarto, "docs", "index.html")
    if (file.exists(temp_html)) {
      # Копируем из временной директории
      file.copy(temp_html, final_html_path, overwrite = TRUE)
      cat("✅ Dashboard created (copied from temp)\n")
      cat("📄 Full path to report:", normalizePath(final_html_path), "\n")
      return(invisible(final_html_path))
    }
    stop("Failed to create dashboard: HTML file not created")
  }
}
