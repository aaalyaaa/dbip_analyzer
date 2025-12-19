
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

  # Создаем временную директорию
  temp_dir <- tempfile("dashboard_")
  dir.create(temp_dir, recursive = TRUE)

  # Копируем шаблон Quarto
  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_path == "") {
    stop("Не найден шаблон Quarto")
  }

  file.copy(quarto_path, temp_dir, recursive = TRUE)
  temp_quarto <- file.path(temp_dir, "quarto")

  # Копируем данные
  temp_data_dir <- file.path(temp_quarto, "processed")
  dir.create(temp_data_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(user_data_path, file.path(temp_data_dir, "dbip_data.parquet"))

  # Рендерим отчет БЕЗ фонового режима
  old_wd <- getwd()
  setwd(temp_quarto)

  cat("🔧 Запускаем рендеринг Quarto...\n")

  quarto::quarto_render(".", as_job = FALSE, quiet = FALSE)

  setwd(old_wd)

  # Проверяем созданный файл
  created_html <- file.path(temp_quarto, "docs", "index.html")

  if (!file.exists(created_html)) {
    # Даем время на завершение
    Sys.sleep(2)

    if (!file.exists(created_html)) {
      # Ищем в других местах
      all_html_files <- list.files(temp_dir, pattern = "\\.html$",
                                   recursive = TRUE, full.names = TRUE)

      if (length(all_html_files) == 0) {
        stop("HTML файл не был создан. Проверьте логи Quarto.")
      } else {
        created_html <- all_html_files[1]
      }
    }
  }

  # Создаем папку docs в рабочей директории
  user_docs_dir <- file.path(user_dir, "docs")
  if (!dir.exists(user_docs_dir)) {
    dir.create(user_docs_dir, recursive = TRUE)
    cat("📁 Создана папка docs\n")
  }

  # Копируем ВСЮ папку docs с ресурсами
  temp_docs_dir <- file.path(temp_quarto, "docs")

  if (dir.exists(temp_docs_dir)) {
    # Получаем все файлы из временной docs
    all_files <- list.files(temp_docs_dir,
                            full.names = TRUE,
                            recursive = TRUE,
                            all.files = TRUE,
                            no.. = TRUE)

    cat("📋 Копируем файлы оформления...\n")

    for (file in all_files) {
      # Получаем относительный путь
      rel_path <- substr(file, nchar(temp_docs_dir) + 2, nchar(file))
      target_file <- file.path(user_docs_dir, rel_path)

      # Создаем директорию для файла
      target_dir <- dirname(target_file)
      if (!dir.exists(target_dir)) {
        dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Копируем файл
      file.copy(file, target_file, overwrite = TRUE)
    }
  }

  # Проверяем результат
  target_path <- file.path(user_docs_dir, "index.html")

  if (file.exists(target_path)) {
    file_size <- file.info(target_path)$size
    cat("\n✅ УСПЕХ! Dashboard создан со всеми стилями!\n")
    cat("📄 Файл:", normalizePath(target_path), "\n")

    # Открываем в браузере
    if (interactive()) {
      utils::browseURL(target_path)
    }
  } else {
    stop("Не удалось создать dashboard")
  }

  invisible(target_path)
}

