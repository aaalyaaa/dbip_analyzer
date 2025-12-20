
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

  # Сохраняем данные в оба места (для совместимости с index1.qmd)
  arrow::write_parquet(demo, file.path(data_dir, "demo.parquet"))
  arrow::write_parquet(demo, file.path(processed_dir, "dbip_data.parquet"))

  # 3. Переходим во временную директорию и рендерим
  old_wd <- getwd()
  setwd(temp_quarto)

  cat("Рендеринг дашборда...\n")

  # Рендерим только index1.qmd
  quarto::quarto_render("index1.qmd", as_job = FALSE, quiet = FALSE)

  setwd(old_wd)

  # 4. Ищем созданный HTML файл
  html_candidates <- c(
    file.path(temp_quarto, "index1.html"),
    file.path(temp_quarto, "docs", "index1.html"),
    file.path(temp_quarto, "docs", "index.html"),
    file.path(temp_quarto, "index.html")
  )

  html_source <- NULL
  for (candidate in html_candidates) {
    if (file.exists(candidate)) {
      html_source <- candidate
      break
    }
  }

  if (is.null(html_source)) {
    # Ищем любой HTML файл
    all_html <- list.files(temp_quarto, pattern = "\\.html$",
                           recursive = TRUE, full.names = TRUE)
    if (length(all_html) > 0) {
      html_source <- all_html[1]
    } else {
      stop("Не удалось создать HTML файл")
    }
  }

  # 5. Копируем HTML в удобное место для пользователя
  output_dir <- file.path(getwd(), "docs")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  final_html <- file.path(output_dir, "demo_dashboard.html")
  file.copy(html_source, final_html, overwrite = TRUE)

  # 6. Очищаем временные файлы
  unlink(temp_dir, recursive = TRUE)

  # 7. Информируем пользователя
  cat("\n" + stringr::str_dup("=", 50) + "\n")
  cat("✅ ДЕМО-ДАШБОРД УСПЕШНО СОЗДАН!\n")
  cat("\n📁 Файл:", final_html, "\n")
  cat("📊 Размер:", round(file.info(final_html)$size / 1024, 1), "KB\n")
  cat("\n" + stringr::str_dup("=", 50) + "\n")

  # 8. Автоматически открываем в браузере
  if (interactive()) {
    cat("\nОткрываю в браузере...\n")
    Sys.sleep(1)
    utils::browseURL(final_html)
  }

  invisible(final_html)
}

