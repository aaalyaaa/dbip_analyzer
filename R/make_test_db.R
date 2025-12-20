
#'Create HTML Report with DB-IP Data Visualizations with test data
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`test_docs/test_index.html`)
#' @export

make_demo_dashboard <- function() {
  # 1. Создаем и очищаем test_docs директорию
  test_docs_dir <- file.path(getwd(), "test_docs")
  if (dir.exists(test_docs_dir)) {
    unlink(test_docs_dir, recursive = TRUE)
  }
  dir.create(test_docs_dir, recursive = TRUE)

  cat("Создаем демо-дашборд в test_docs...\n")

  # 2. Копируем все файлы из quarto шаблона
  quarto_dir <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_dir == "") {
    stop("Не найден шаблон Quarto в пакете")
  }

  # Копируем ВСЕ файлы из inst/quarto
  file.copy(quarto_dir, test_docs_dir, recursive = TRUE)

  # Теперь у нас в test_docs_dir лежат:
  # - index.qmd
  # - index1.qmd
  # - _quarto.yml
  # - quarto.css

  # 3. Загружаем встроенные данные demo
  data("demo", package = "dbipAnalyzer")

  # 4. Создаем структуру папок с данными
  data_dir <- file.path(test_docs_dir, "data")
  processed_dir <- file.path(test_docs_dir, "processed")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  # Сохраняем данные в формате parquet
  arrow::write_parquet(demo, file.path(data_dir, "demo.parquet"))
  arrow::write_parquet(demo, file.path(processed_dir, "dbip_data.parquet"))

  # 5. Модифицируем _quarto.yml чтобы output был в текущей директории
  quarto_yml_path <- file.path(test_docs_dir, "_quarto.yml")
  if (file.exists(quarto_yml_path)) {
    yml_content <- readLines(quarto_yml_path, warn = FALSE)

    # Заменяем output-dir: docs на текущую директорию
    yml_content <- gsub(
      "output-dir:\\s*docs",
      "output-dir: .",
      yml_content
    )

    # Или добавляем если нет
    if (!any(grepl("output-dir:", yml_content))) {
      # Находим project section
      project_idx <- grep("^project:", yml_content)
      if (length(project_idx) > 0) {
        yml_content <- c(
          yml_content[1:project_idx],
          "  output-dir: .",
          yml_content[(project_idx + 1):length(yml_content)]
        )
      }
    }

    writeLines(yml_content, quarto_yml_path)
    cat("Обновлен _quarto.yml\n")
  }

  # 6. Переходим в test_docs_dir и рендерим
  old_wd <- getwd()
  setwd(test_docs_dir)

  cat("Рендеринг index1.qmd как test_index.html...\n")

  # Рендерим с указанием output_file
  quarto::quarto_render(
    input = "index1.qmd",
    output_file = "test_index.html",  # ⬅️ Ключевое изменение!
    as_job = FALSE,
    quiet = FALSE
  )

  setwd(old_wd)

  # 7. Проверяем что создалось
  result_html <- file.path(test_docs_dir, "test_index.html")

  # Если не создался test_index.html, ищем другие варианты
  if (!file.exists(result_html)) {
    html_files <- list.files(test_docs_dir, pattern = "\\.html$", full.names = TRUE)
    if (length(html_files) > 0) {
      result_html <- html_files[1]
      cat("Найден HTML файл:", basename(result_html), "\n")
    } else {
      stop("HTML файл не был создан")
    }
  }

  # 8. Удаляем исходные файлы которые не нужны пользователю
  cat("Очищаем временные файлы...\n")

  files_to_remove <- c(
    "index.qmd",        # исходный файл
    "index1.qmd",       # исходный файл
    "_quarto.yml",      # конфигурация
    "quarto.css",       # стили
    "data",             # папка с демо данными
    "processed"         # папка с данными
  )

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

  # 9. Переименовываем папку index_files если она есть (для index1.qmd)
  index_files_dir <- file.path(test_docs_dir, "index_files")
  if (dir.exists(index_files_dir)) {
    # Проверяем не создал ли quarto уже test_index_files
    test_index_files_dir <- file.path(test_docs_dir, "test_index_files")

    if (!dir.exists(test_index_files_dir)) {
      file.rename(index_files_dir, test_index_files_dir)
      cat("Переименована папка ресурсов\n")
    }
  }

  # 10. Обновляем ссылки в HTML если нужно
  if (file.exists(result_html)) {
    html_content <- readLines(result_html, warn = FALSE)

    # Проверяем есть ли ссылки на index_files
    has_index_files <- any(grepl('"index_files/', html_content))

    if (has_index_files) {
      # Заменяем ссылки на index_files
      html_content <- gsub('"index_files/', '"test_index_files/', html_content)
      html_content <- gsub("'index_files/", "'test_index_files/", html_content)

      writeLines(html_content, result_html)
      cat("Обновлены ссылки в HTML файле\n")
    }
  }

  # 11. Показываем результат пользователю
  cat("\n" + stringr::str_dup("=", 50) + "\n")
  cat("✅ ДЕМО-ДАШБОРД УСПЕШНО СОЗДАН!\n")
  cat("\n📁 Папка: test_docs/\n")

  # Показываем структуру файлов
  created_files <- list.files(test_docs_dir, recursive = TRUE)
  if (length(created_files) > 0) {
    cat("📋 Созданные файлы:\n")
    for (f in created_files) {
      cat("  - ", f, "\n")
    }
  }

  cat("\n📍 Основной файл: ", basename(result_html), "\n")
  cat("📊 Размер: ", round(file.info(result_html)$size / 1024, 1), "KB\n")
  cat("\n" + stringr::str_dup("=", 50) + "\n")

  # 12. Автоматически открываем в браузере
  if (interactive() && file.exists(result_html)) {
    cat("\nОткрываю дашборд в браузере...\n")
    utils::browseURL(result_html)
  }

  invisible(result_html)
}
