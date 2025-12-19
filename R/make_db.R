
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

  old_wd <- getwd()
  on.exit(setwd(old_wd))  # Гарантированно вернемся в исходную директорию

  setwd(temp_quarto)

  cat("📁 Current directory for Quarto render:", getwd(), "\n")
  cat("📁 Files in current directory:\n")
  print(list.files())

  # Выполняем рендеринг
  tryCatch({
    result <- quarto::quarto_render(".", quiet = FALSE)
    cat("✅ Quarto render completed\n")
  }, error = function(e) {
    cat("❌ Quarto render error:", e$message, "\n")
    stop(e)
  })

  # Проверяем, что создалось
  cat("📁 Files after render:\n")
  print(list.files(recursive = TRUE))

  # После рендеринга возвращаемся в исходную директорию
  setwd(old_wd)

  # Ищем созданный HTML файл - он должен быть в docs/index.html относительно temp_quarto
  temp_html <- file.path(temp_quarto, "docs", "index.html")

  if (!file.exists(temp_html)) {
    # Проверим другие возможные расположения
    cat("🔍 Searching for HTML file...\n")

    # Ищем все HTML файлы в временной директории
    all_files <- list.files(temp_dir, pattern = "\\.html$",
                            recursive = TRUE, full.names = TRUE,
                            ignore.case = TRUE)

    cat("Found HTML files:\n")
    print(all_files)

    if (length(all_files) > 0) {
      # Берем первый найденный HTML файл
      temp_html <- all_files[1]
      cat("📁 Using HTML file:", temp_html, "\n")
    } else {
      stop("Failed to create dashboard: No HTML files found")
    }
  }

  # Создаем директорию docs в рабочей директории пользователя
  final_docs_dir <- file.path(user_dir, "docs")
  if (!dir.exists(final_docs_dir)) {
    dir.create(final_docs_dir, recursive = TRUE)
    cat("📁 Created docs directory:", final_docs_dir, "\n")
  }

  # Определяем полный путь к целевому файлу
  final_html_path <- file.path(user_dir, "docs", "index.html")

  # Копируем файл
  cat("📋 Copying HTML file...\n")
  cat("From:", temp_html, "\n")
  cat("To:", final_html_path, "\n")

  success <- file.copy(temp_html, final_html_path, overwrite = TRUE)

  if (!success) {
    # Попробуем другой способ копирования
    cat("⚠️ Standard file.copy failed, trying read/write...\n")
    file_content <- readBin(temp_html, "raw", file.info(temp_html)$size)
    writeBin(file_content, final_html_path)
  }

  # Проверяем, что файл создался
  if (file.exists(final_html_path)) {
    # Выводим полный путь к файлу
    cat("✅ Dashboard created\n")
    cat("📄 Full path to report:", normalizePath(final_html_path), "\n")

    # Копируем связанные файлы (CSS, JS и т.д.)
    temp_docs_dir <- dirname(temp_html)
    if (dir.exists(temp_docs_dir)) {
      # Копируем все файлы из директории docs
      other_files <- list.files(temp_docs_dir, full.names = TRUE)
      other_files <- other_files[!grepl("index\\.html$", basename(other_files), ignore.case = TRUE)]

      for (file in other_files) {
        dest_file <- file.path(final_docs_dir, basename(file))
        if (!file.exists(dest_file) || file.info(file)$mtime > file.info(dest_file)$mtime) {
          file.copy(file, dest_file, overwrite = TRUE)
        }
      }
    }

    # Возвращаем путь невидимо
    return(invisible(final_html_path))
  } else {
    stop("Failed to copy HTML file to destination")
  }
}
