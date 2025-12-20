
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

  # 2. Копируем ВСЕ файлы из inst/quarto
  quarto_dir <- system.file("quarto", package = "dbipAnalyzer")

  # Проверяем, что папка существует
  if (quarto_dir == "") {
    stop("Не найдена папка quarto в пакете")
  }

  # Выводим список файлов в quarto папке
  cat("Файлы в quarto папке пакета:\n")
  print(list.files(quarto_dir, full.names = TRUE))

  # Копируем все файлы из quarto_dir в test_docs_dir
  file.copy(
    list.files(quarto_dir, full.names = TRUE, include.dirs = TRUE),
    test_docs_dir,
    recursive = TRUE
  )

  # Выводим список скопированных файлов
  cat("\nФайлы в test_docs после копирования:\n")
  print(list.files(test_docs_dir, full.names = FALSE, recursive = FALSE))

  # 3. Загружаем демо данные
  data("demo", package = "dbipAnalyzer")

  # 4. Создаем структуру папок
  processed_dir <- file.path(test_docs_dir, "processed")
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  # Сохраняем данные
  arrow::write_parquet(demo, file.path(processed_dir, "dbip_data.parquet"))

  # 5. Проверяем, существует ли index1.qmd
  index1_path <- file.path(test_docs_dir, "index1.qmd")
  cat("\nПроверяем index1.qmd:\n")
  cat("Существует?", file.exists(index1_path), "\n")
  cat("Путь:", normalizePath(index1_path, mustWork = FALSE), "\n")

  if (!file.exists(index1_path)) {
    cat("\nФайл index1.qmd не найден! Доступные файлы:\n")
    print(list.files(test_docs_dir, pattern = "\\.qmd$", full.names = TRUE))

    # Если нет index1.qmd, используем index.qmd
    if (file.exists(file.path(test_docs_dir, "index.qmd"))) {
      cat("Использую index.qmd вместо index1.qmd\n")
      index1_path <- file.path(test_docs_dir, "index.qmd")
    } else {
      stop("Нет ни index.qmd, ни index1.qmd файлов!")
    }
  }


  # 6. Переходим в test_docs и рендерим
  old_wd <- getwd()
  setwd(test_docs_dir)

  cat("\n=== НАЧАЛО РЕНДЕРИНГА ===\n")
  cat("Рабочая директория:", getwd(), "\n")
  cat("Файлы в директории:\n")
  print(list.files())

  # Проверяем наличие файла перед рендерингом
  input_file <- ifelse(grepl("index1\\.qmd$", index1_path), "index1.qmd", "index.qmd")
  cat("Рендерим файл:", input_file, "\n")
  cat("Файл существует?", file.exists(input_file), "\n")

  # 7. Пробуем рендерить
  tryCatch({
    quarto::quarto_render(
      input = input_file,
      as_job = FALSE,
      quiet = FALSE
    )
    cat("\nРендеринг завершен успешно!\n")
  }, error = function(e) {
    cat("\nОШИБКА при рендеринге:\n")
    cat(e$message, "\n")

    # Попробуем альтернативный способ
    cat("\nПробую альтернативный способ рендеринга...\n")
    system(paste("quarto render", input_file))
  })

  setwd(old_wd)

  # 8. Ищем созданный HTML файл
  # Сначала ищем в docs/
  html_path <- file.path(test_docs_dir, "docs", paste0(tools::file_path_sans_ext(input_file), ".html"))

  if (!file.exists(html_path)) {
    # Ищем в корне
    html_path <- file.path(test_docs_dir, paste0(tools::file_path_sans_ext(input_file), ".html"))
  }

  if (!file.exists(html_path)) {
    # Ищем любой HTML файл
    html_files <- list.files(test_docs_dir, pattern = "\\.html$",
                             recursive = TRUE, full.names = TRUE)
    if (length(html_files) > 0) {
      html_path <- html_files[1]
      cat("Найден HTML файл:", html_path, "\n")
    } else {
      stop("HTML файл не создан.")
    }
  }

  # 9. Переименовываем в test_index.html
  final_html <- file.path(test_docs_dir, "test_index.html")
  file.copy(html_path, final_html, overwrite = TRUE)
  cat("\nФайл сохранен как:", final_html, "\n")

  # 10. Результат
  message("\n✅ Демо-дашборд успешно создан!")
  message("📁 Файл: ", normalizePath(final_html))

  if (interactive() && file.exists(final_html)) {
    message("📋 Открываю в браузере...")
    utils::browseURL(final_html)
  }

  invisible(final_html)
}
