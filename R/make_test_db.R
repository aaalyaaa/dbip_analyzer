
#'Create HTML Report with DB-IP Data Visualizations with test data
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`test_docs/test_index.html`)
#' @export

make_test_dashboard <- function() {
  # 1. Подготовка директории
  test_docs_dir <- file.path(getwd(), "test_docs")
  if (dir.exists(test_docs_dir)) unlink(test_docs_dir, recursive = TRUE)
  dir.create(test_docs_dir, recursive = TRUE)

  message("Создание демо-дашборда...")

  # 2. Копируем ВСЕ файлы из inst/quarto, сохраняя структуру
  quarto_dir <- system.file("quarto", package = "dbipAnalyzer")

  # Копируем всю папку quarto целиком
  file.copy(quarto_dir, test_docs_dir, recursive = TRUE)

  # Переименовываем скопированную папку в корневую структуру
  copied_quarto_dir <- file.path(test_docs_dir, "quarto")
  if (dir.exists(copied_quarto_dir)) {
    # Перемещаем все файлы из quarto/ в корень test_docs
    quarto_files <- list.files(copied_quarto_dir, full.names = TRUE)
    for (file in quarto_files) {
      file.copy(file, test_docs_dir, recursive = TRUE)
    }
    # Удаляем временную папку
    unlink(copied_quarto_dir, recursive = TRUE)
  }

  cat("Файлы в test_docs:", paste(list.files(test_docs_dir), collapse = ", "), "\n")

  # 3. Загружаем демо данные
  data("demo", package = "dbipAnalyzer")

  # 4. Создаем структуру папок
  processed_dir <- file.path(test_docs_dir, "processed")
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  # Сохраняем данные как dbip_data.parquet (как ожидает index.qmd)
  arrow::write_parquet(demo, file.path(processed_dir, "dbip_data.parquet"))

  # 5. Создаем index.qmd специально для демо-дашборда
  # Просто используем исходный index.qmd, так как он уже настроен на
  # загрузку данных из processed/dbip_data.parquet
  index_path <- file.path(test_docs_dir, "index.qmd")

  # Удаляем index1.qmd, он не нужен
  index1_path <- file.path(test_docs_dir, "index1.qmd")
  if (file.exists(index1_path)) {
    file.remove(index1_path)
  }

  # 6. Переходим в test_docs и рендерим
  old_wd <- getwd()
  setwd(test_docs_dir)

  cat("Рабочая директория:", getwd(), "\n")
  cat("Файлы в директории:", paste(list.files(), collapse = ", "), "\n")
  cat("Файлы в processed:", paste(list.files("processed"), collapse = ", "), "\n")

  # Рендерим index.qmd с использованием _quarto.yml
  cat("Рендеринг Quarto...\n")
  quarto::quarto_render(as_job = FALSE, quiet = FALSE)

  setwd(old_wd)

  # 7. Ищем созданный HTML файл
  # Ищем в стандартной папке docs (как указано в _quarto.yml)
  html_path <- file.path(test_docs_dir, "docs", "index.html")

  if (!file.exists(html_path)) {
    # Если нет, ищем в корне
    html_path <- file.path(test_docs_dir, "index.html")
  }

  if (!file.exists(html_path)) {
    # Ищем любой HTML файл
    html_files <- list.files(test_docs_dir, pattern = "\\.html$",
                             recursive = TRUE, full.names = TRUE)
    if (length(html_files) > 0) {
      html_path <- html_files[1]
    } else {
      stop("HTML файл не создан")
    }
  }

  # 8. Переименовываем в test_index.html
  final_html <- file.path(test_docs_dir, "test_index.html")

  # Копируем HTML файл
  file.copy(html_path, final_html, overwrite = TRUE)

  # 9. Обрабатываем папку с ресурсами
  # Ищем папку ресурсов
  resource_dirs <- list.dirs(test_docs_dir, recursive = FALSE, full.names = TRUE)
  resource_dirs <- resource_dirs[grepl("(index|index1)_files$", resource_dirs)]

  if (length(resource_dirs) > 0) {
    # Переименовываем первую найденную папку
    old_res_dir <- resource_dirs[1]
    new_res_dir <- file.path(test_docs_dir, "test_index_files")

    if (!dir.exists(new_res_dir)) {
      file.rename(old_res_dir, new_res_dir)
      cat("Переименована папка ресурсов: ", basename(old_res_dir), " -> test_index_files\n")
    }

    # Обновляем ссылки в HTML
    if (file.exists(final_html)) {
      html_content <- readLines(final_html, warn = FALSE)
      # Заменяем старые ссылки на новые
      html_content <- gsub('(index|index1)_files/', 'test_index_files/', html_content)
      writeLines(html_content, final_html)
    }
  }

  # 10. Очищаем временные файлы, но оставляем важные
  files_to_keep <- c(
    "test_index.html",
    "test_index_files",
    "quarto.css"
  )

  all_files <- list.files(test_docs_dir, full.names = FALSE)
  files_to_remove <- setdiff(all_files, files_to_keep)

  for (file in files_to_remove) {
    path <- file.path(test_docs_dir, file)
    if (file.exists(path)) {
      if (file.info(path)$isdir) {
        unlink(path, recursive = TRUE)
      } else {
        file.remove(path)
      }
    }
  }

  # 11. Результат
  message("\n✅ Демо-дашборд успешно создан!")
  message("📁 Файл: ", final_html)
  message("📊 Использованы данные: demo (", nrow(demo), " строк)")

  if (interactive() && file.exists(final_html)) {
    utils::browseURL(final_html)
  }

  invisible(final_html)
}
