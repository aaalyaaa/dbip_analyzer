
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

  # Проверка пакета quarto
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Пакет 'quarto' не установлен. Установите его: install.packages('quarto')")
  }

  # Получаем путь к шаблону quarto
  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_path == "") {
    stop("Не найден шаблон Quarto в пакете dbipAnalyzer")
  }

  # Создаем временную директорию
  temp_dir <- tempfile("dashboard_")
  dir.create(temp_dir)
  file.copy(quarto_path, temp_dir, recursive = TRUE)

  temp_quarto <- file.path(temp_dir, "quarto")

  # Копируем данные
  temp_data_path <- file.path(temp_quarto, "processed", "dbip_data.parquet")
  dir.create(dirname(temp_data_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(user_data_path, temp_data_path, overwrite = TRUE)

  # Сохраняем текущую рабочую директорию
  old_wd <- getwd()
  on.exit(setwd(old_wd)) # Гарантируем возврат даже при ошибке

  # Переходим в директорию с quarto
  setwd(temp_quarto)

  # Рендерим отчет
  tryCatch({
    quarto::quarto_render(".", quiet = FALSE)
  }, error = function(e) {
    stop("Ошибка при рендеринге Quarto: ", e$message)
  })

  # Проверяем, создался ли файл
  temp_html <- file.path(temp_quarto, "docs", "index.html")
  if (!file.exists(temp_html)) {
    # Ищем альтернативные пути
    possible_paths <- list.files(temp_quarto, pattern = "\\.html$",
                                 recursive = TRUE, full.names = TRUE)
    if (length(possible_paths) > 0) {
      temp_html <- possible_paths[1]
      message("Файл найден по альтернативному пути: ", temp_html)
    } else {
      stop("HTML файл не создался при рендеринге Quarto")
    }
  }

  # Создаем целевую директорию
  target_dir <- file.path(old_wd, "docs")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  target_path <- file.path(target_dir, "index.html")

  # Копируем файл
  success <- file.copy(temp_html, target_path, overwrite = TRUE)

  if (success) {
    cat("✅ Dashboard создан успешно!\n")
    cat("📄 Файл сохранен: ", normalizePath(target_path), "\n")

    # Проверяем размер файла
    file_size <- file.info(target_path)$size
  } else {
    stop("Не удалось скопировать файл в ", target_path)
  }

  invisible(target_path)
}
