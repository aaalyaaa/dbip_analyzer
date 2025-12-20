
#'Create HTML Report with DB-IP Data Visualizations with test data
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`test_docs/test_index.html`)
#' @export

make_demo_dashboard <- function() {
  # Создаем временную директорию для рендеринга
  temp_dir <- tempfile("demo_dashboard_")
  dir.create(temp_dir, recursive = TRUE)

  cat("Создаем демо-дашборд...\n")

  # 1. Копируем Quarto шаблон
  quarto_dir <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_dir == "") {
    stop("Не найден шаблон Quarto в пакете")
  }

  file.copy(quarto_dir, temp_dir, recursive = TRUE)
  temp_quarto <- file.path(temp_dir, "quarto")

  # 2. Загружаем встроенные данные и создаем demo.parquet
  data("demo", package = "dbipAnalyzer")

  # Создаем необходимые папки
  data_dir <- file.path(temp_quarto, "data")
  processed_dir <- file.path(temp_quarto, "processed")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  # Сохраняем данные в оба места
  arrow::write_parquet(demo, file.path(data_dir, "demo.parquet"))
  arrow::write_parquet(demo, file.path(processed_dir, "dbip_data.parquet"))

  # 3. Переходим во временную директорию и рендерим
  old_wd <- getwd()
  setwd(temp_quarto)

  cat("Рендеринг дашборда...\n")

  # Рендерим только index1.qmd
  quarto::quarto_render("index1.qmd", as_job = FALSE, quiet = FALSE)

  setwd(old_wd)

  # 4. Создаем папку test_docs
  test_docs_dir <- file.path(getwd(), "test_docs")
  if (!dir.exists(test_docs_dir)) {
    dir.create(test_docs_dir, recursive = TRUE)
  } else {
    # Очищаем существующую папку
    unlink(test_docs_dir, recursive = TRUE)
    dir.create(test_docs_dir, recursive = TRUE)
  }

  # 5. Копируем и переименовываем все созданные файлы с префиксом test_

  # Ищем папку с результатами (обычно docs/ или корень)
  result_dirs <- c(
    file.path(temp_quarto, "docs"),
    temp_quarto
  )

  result_dir <- NULL
  for (dir in result_dirs) {
    if (dir.exists(dir) && length(list.files(dir)) > 0) {
      result_dir <- dir
      break
    }
  }

  if (is.null(result_dir)) {
    stop("Не удалось найти сгенерированные файлы")
  }

  cat("Копируем файлы с префиксом test_...\n")

  # Функция для копирования с переименованием
  copy_with_prefix <- function(from_dir, to_dir, prefix = "test_") {
    all_files <- list.files(from_dir,
                            full.names = TRUE,
                            recursive = TRUE,
                            all.files = TRUE,
                            no.. = TRUE)

    for (file in all_files) {
      # Получаем относительный путь
      rel_path <- substr(file, nchar(from_dir) + 2, nchar(file))

      # Разделяем путь на части
      path_parts <- unlist(strsplit(rel_path, "/"))

      # Добавляем префикс к имени файла (но не к папкам)
      if (length(path_parts) > 0) {
        # Только к последней части (файлу)
        if (!grepl("\\.", path_parts[length(path_parts)])) {
          # Если это папка (без расширения), оставляем как есть
          new_name <- path_parts
        } else {
          # Если это файл, добавляем префикс
          path_parts[length(path_parts)] <- paste0(prefix,
                                                   path_parts[length(path_parts)])
          new_name <- path_parts
        }

        new_rel_path <- paste(new_name, collapse = "/")
        target_file <- file.path(to_dir, new_rel_path)

        # Создаем директорию если нужно
        target_dir <- dirname(target_file)
        if (!dir.exists(target_dir)) {
          dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
        }

        # Копируем файл
        file.copy(file, target_file, overwrite = TRUE)
      }
    }
  }

  # Копируем все файлы
  copy_with_prefix(result_dir, test_docs_dir, "test_")

  # 6. Очищаем временные файлы
  unlink(temp_dir, recursive = TRUE)

  # 7. Показываем пользователю что получилось
  cat("✅ ДЕМО-ДАШБОРД УСПЕШНО СОЗДАН!\n")
  cat("\n📁 Папка: test_docs/\n")
  cat("📋 Созданные файлы:\n")

  # Показываем структуру файлов
  files_in_test_docs <- list.files(test_docs_dir, recursive = TRUE)
  for (file in files_in_test_docs) {
    cat("  - ", file, "\n")
  }

  # Основной HTML файл
  main_html <- file.path(test_docs_dir, "test_index.html")

  if (!file.exists(main_html)) {
    # Ищем любой HTML файл с префиксом test_
    html_files <- list.files(test_docs_dir, pattern = "^test_.*\\.html$",
                             recursive = TRUE, full.names = TRUE)
    if (length(html_files) > 0) {
      main_html <- html_files[1]
    }
  }

  cat("\n📊 Основной файл:", basename(main_html), "\n")
  cat("📍 Полный путь:", main_html, "\n")

  # 8. Автоматически открываем в браузере
  if (interactive() && file.exists(main_html)) {
    cat("\nОткрываю в браузере...\n")
    Sys.sleep(1)
    utils::browseURL(main_html)
  }

  invisible(main_html)
}
