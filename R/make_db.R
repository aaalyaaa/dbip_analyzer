
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
  cat("✅ Файл данных найден\n")

  # 1. Создаем папку docs в рабочей директории
  docs_dir <- file.path(user_dir, "docs")
  if (!dir.exists(docs_dir)) {
    dir.create(docs_dir, recursive = TRUE)
    cat("📁 Создана папка docs\n")
  }

  # 2. Создаем временную директорию для Quarto проекта
  temp_dir <- tempfile("quarto_project_")
  dir.create(temp_dir, recursive = TRUE)
  cat("📁 Временная директория создана\n")

  # 3. Копируем ВЕСЬ Quarto проект из пакета
  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_path == "") {
    stop("Шаблон Quarto не найден в пакете dbipAnalyzer")
  }

  cat("📋 Копируем Quarto проект...\n")

  # Получаем все файлы и папки из quarto директории пакета
  all_files <- list.files(quarto_path, all.files = TRUE, full.names = TRUE,
                          no.. = TRUE)

  # Копируем все содержимое
  file.copy(all_files, temp_dir, recursive = TRUE)

  # 4. Копируем данные пользователя
  temp_processed <- file.path(temp_dir, "processed")
  dir.create(temp_processed, showWarnings = FALSE, recursive = TRUE)
  file.copy(user_data_path, file.path(temp_processed, "dbip_data.parquet"))

  # 5. Проверяем наличие ключевых файлов
  cat("🔍 Проверяем файлы проекта:\n")
  project_files <- list.files(temp_dir)
  print(project_files)

  # 6. Рендерим проект
  old_wd <- getwd()
  on.exit(setwd(old_wd)) # Гарантируем возврат

  setwd(temp_dir)

  cat("🎨 Рендерим Quarto проект...\n")

  # Удаляем старую папку docs если есть
  if (dir.exists("docs")) {
    unlink("docs", recursive = TRUE)
  }

  # Рендерим проект
  quarto::quarto_render(
    input = ".",
    as_job = FALSE,
    quiet = FALSE
  )

  # 7. Проверяем результат рендеринга
  temp_html <- file.path(temp_dir, "docs", "index.html")

  if (!file.exists(temp_html)) {
    # Ищем HTML файл в других местах
    all_html <- list.files(temp_dir, pattern = "\\.html$",
                           recursive = TRUE, full.names = TRUE)

    if (length(all_html) > 0) {
      temp_html <- all_html[1]
    } else {
      stop("HTML файл не был создан при рендеринге Quarto")
    }
  }

  # 8. Копируем ВСЕ сгенерированные файлы в docs пользователя
  cat("📋 Копируем сгенерированные файлы...\n")

  # Получаем все файлы из временной docs папки
  if (dir.exists(file.path(temp_dir, "docs"))) {
    generated_files <- list.files(file.path(temp_dir, "docs"),
                                  full.names = TRUE,
                                  recursive = TRUE,
                                  all.files = TRUE,
                                  no.. = TRUE)

    # Удаляем старые файлы в целевой docs
    if (dir.exists(docs_dir)) {
      unlink(list.files(docs_dir, full.names = TRUE), recursive = TRUE)
    }

    # Копируем каждый файл
    for (file in generated_files) {
      rel_path <- sub(paste0(temp_dir, "/docs/"), "", file)
      target_file <- file.path(docs_dir, rel_path)

      # Создаем директорию для файла
      target_dir <- dirname(target_file)
      if (!dir.exists(target_dir)) {
        dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
      }

      file.copy(file, target_file, overwrite = TRUE)
    }
  } else {
    # Если нет папки docs, копируем просто HTML файл
    file.copy(temp_html, file.path(docs_dir, "index.html"), overwrite = TRUE)
  }

  # 9. Проверяем финальный результат
  final_html <- file.path(docs_dir, "index.html")

  if (file.exists(final_html)) {
    file_size <- file.info(final_html)$size

    cat("\n" + strrep("=", 60) + "\n")
    cat("✅ DASHBOARD УСПЕШНО СОЗДАН!\n")
    cat(strrep("=", 60) + "\n")
    cat("📁 Расположение:  ", normalizePath(docs_dir), "\n")
    cat("📄 Основной файл: ", normalizePath(final_html), "\n")
    cat("📏 Размер файла:  ", round(file_size/1024, 2), "KB\n")

    # Список всех файлов в docs
    if (dir.exists(docs_dir)) {
      cat("📋 Файлы в docs:\n")
      docs_files <- list.files(docs_dir, recursive = TRUE)
      for (f in docs_files) {
        cat("   •", f, "\n")
      }
    }

    # Открываем в браузере
    if (interactive()) {
      cat("\n🌐 Открываю в браузере...\n")
      utils::browseURL(final_html)
    }

  } else {
    cat("\n❌ ОШИБКА: Файл dashboard не был создан\n")
  }

  invisible(final_html)
}

