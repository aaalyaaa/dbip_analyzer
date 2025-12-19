
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
    stop("Файл данных не найден. Сначала запустите run_etl_pipeline()")
  }

  cat("📁 Текущая рабочая директория:", user_dir, "\n")
  cat("📁 Файл данных найден:", file.exists(user_data_path), "\n")

  # Создаем временную директорию
  temp_dir <- tempfile("dashboard_")
  dir.create(temp_dir, recursive = TRUE)
  cat("📁 Создана временная директория:", temp_dir, "\n")

  # Копируем шаблон Quarto
  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_path == "") {
    stop("Не найден шаблон Quarto в пакете dbipAnalyzer")
  }

  file.copy(quarto_path, temp_dir, recursive = TRUE)
  temp_quarto <- file.path(temp_dir, "quarto")

  # Копируем данные
  temp_data_dir <- file.path(temp_quarto, "processed")
  dir.create(temp_data_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(user_data_path, file.path(temp_data_dir, "dbip_data.parquet"))


  old_wd <- getwd()
  setwd(temp_quarto)

  cat("🔧 Запускаем рендеринг Quarto...\n")
  quarto::quarto_render(".", quiet = FALSE)


  setwd(old_wd)


  created_html <- file.path(temp_quarto, "docs", "index.html")
  cat("🔍 Ищем файл по пути:", created_html, "\n")
  cat("🔍 Файл существует:", file.exists(created_html), "\n")

  if (!file.exists(created_html)) {

    all_html_files <- list.files(temp_dir, pattern = "\\.html$",
                                 recursive = TRUE, full.names = TRUE)
    cat("🔍 Все найденные HTML файлы:", all_html_files, "\n")

    if (length(all_html_files) > 0) {
      created_html <- all_html_files[1]
      cat("✅ Используем файл:", created_html, "\n")
    } else {
      stop("HTML файл не был создан")
    }
  }


  user_docs_dir <- file.path(user_dir, "docs")
  if (!dir.exists(user_docs_dir)) {
    dir.create(user_docs_dir, recursive = TRUE)
    cat("📁 Создана папка docs в:", user_docs_dir, "\n")
  }


  target_path <- file.path(user_docs_dir, "index.html")
  cat("📋 Копируем файл...\n")
  cat("   Из:", created_html, "\n")
  cat("   В:", target_path, "\n")

  file.copy(created_html, target_path, overwrite = TRUE)

  if (file.exists(target_path)) {
    file_size <- file.info(target_path)$size
    cat("\n✅ УСПЕХ! Dashboard создан!\n")
    cat("📄 Файл:", normalizePath(target_path), "\n")


  } else {
    cat("\n❌ ОШИБКА: Файл не скопирован\n")
    cat("   Проверьте права доступа к директории\n")
  }

  invisible(target_path)
}
