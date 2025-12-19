
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

  # Вариант A: Используем quarto_render с as_job = FALSE
  quarto::quarto_render(".", as_job = FALSE, quiet = FALSE)

  # ИЛИ Вариант B: Рендерим конкретный файл
  # quarto::quarto_render("index.qmd", as_job = FALSE)

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
        # Пробуем найти в поддиректориях
        all_files <- list.files(temp_dir, recursive = TRUE, full.names = TRUE)
        html_files <- all_files[grep("\\.html$", all_files)]

        if (length(html_files) > 0) {
          created_html <- html_files[1]
        } else {
          stop("HTML файл не был создан. Проверьте логи Quarto.")
        }
      } else {
        created_html <- all_html_files[1]
      }
    }
  }

  # Создаем папку docs в рабочей директории
  user_docs_dir <- file.path(user_dir, "docs")
  if (!dir.exists(user_docs_dir)) {
    dir.create(user_docs_dir, recursive = TRUE)
  }

  # Копируем файл
  target_path <- file.path(user_docs_dir, "index.html")
  file.copy(created_html, target_path, overwrite = TRUE)

  if (file.exists(target_path)) {
    cat("\n✅ УСПЕХ! Dashboard создан!\n")
    cat("📄 Файл:", normalizePath(target_path), "\n")

    # Открываем в браузере
    if (interactive()) {
      utils::browseURL(target_path)
    }
  }

  invisible(target_path)
}
