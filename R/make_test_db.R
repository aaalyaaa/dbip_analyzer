
#'Create HTML Report with DB-IP Data Visualizations with test data
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`test_docs/test_index.html`)
#' @export

make_demo_dashboard <- function() {
  # Загружаем демо данные из пакета
  data("demo", package = "dbipAnalyzer")

  # Создаем тестовую директорию
  test_dir <- file.path(getwd(), "test_docs")
  if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
  dir.create(test_dir, recursive = TRUE)

  # Сохраняем демо данные как parquet
  arrow::write_parquet(demo, file.path(test_dir, "demo.parquet"))

  # Получаем путь к quarto шаблону
  quarto_dir <- system.file("quarto", package = "dbipAnalyzer")
  qmd_file <- file.path(quarto_dir, "index1.qmd")

  if (!file.exists(qmd_file)) {
    stop("Файл index1.qmd не найден в inst/quarto/")
  }

  # Копируем все файлы из quarto директории
  all_files <- list.files(quarto_dir, full.names = TRUE)
  file.copy(all_files, test_dir, recursive = TRUE)

  # Создаем структуру папок как в make_dashboard()
  processed_dir <- file.path(test_dir, "processed")
  dir.create(processed_dir, recursive = TRUE)
  file.copy(file.path(test_dir, "demo.parquet"),
            file.path(processed_dir, "dbip_data.parquet"))

  # СОЗДАЕМ ПАПКУ data И КОПИРУЕМ demo.rda
  temp_data_dir <- file.path(test_dir, "data")
  dir.create(temp_data_dir, recursive = TRUE, showWarnings = FALSE)

  # Ищем demo.rda
  cat("Ищем demo.rda...\n")

  # Все возможные пути для поиска
  possible_paths <- c(
    # 1. В установленном пакете
    system.file("data", "demo.rda", package = "dbipAnalyzer"),
    # 2. В исходниках (для разработки)
    file.path(dirname(.libPaths()[1]), "dbipAnalyzer", "data", "demo.rda"),
    # 3. В рабочей директории проекта
    file.path(getwd(), "data", "demo.rda"),
    # 4. В родительских директориях
    file.path(dirname(getwd()), "dbipAnalyzer", "data", "demo.rda"),
    file.path(dirname(dirname(getwd())), "dbipAnalyzer", "data", "demo.rda")
  )

  demo_rda_path <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      demo_rda_path <- path
      cat("Найден demo.rda по пути:", path, "\n")
      break
    }
  }

  # Копируем если нашли
  if (!is.null(demo_rda_path) && file.exists(demo_rda_path)) {
    success <- file.copy(demo_rda_path, file.path(temp_data_dir, "demo.rda"))
    if (success) {
      cat("✓ demo.rda скопирован в:", temp_data_dir, "\n")
    } else {
      cat("✗ Не удалось скопировать demo.rda\n")
    }
  } else {
    cat("✗ demo.rda не найден. Создаем файл с текущими данными demo...\n")

    # Сохраняем текущие данные demo как .rda
    save(demo, file = file.path(temp_data_dir, "demo.rda"))
    cat("✓ Создан demo.rda из данных demo\n")
  }

  # Проверяем что файлы на месте
  cat("\nСтруктура test_dir перед рендерингом:\n")
  print(list.files(test_dir, recursive = TRUE))

  old_wd <- getwd()
  setwd(test_dir)

  cat("\nЗапускаем рендеринг index1.qmd...\n")

  # Рендерим quarto
  quarto::quarto_render("index1.qmd", quiet = FALSE)

  setwd(old_wd)

  # Ищем созданный HTML файл
  html_file <- file.path(test_dir, "index1.html")
  docs_html_file <- file.path(test_dir, "docs", "index1.html")
  final_file <- file.path(test_dir, "test_index.html")

  if (file.exists(html_file)) {
    file.copy(html_file, final_file, overwrite = TRUE)
  } else if (file.exists(docs_html_file)) {
    file.copy(docs_html_file, final_file, overwrite = TRUE)
  } else if (file.exists(file.path(test_dir, "docs", "index.html"))) {
    file.copy(file.path(test_dir, "docs", "index.html"), final_file, overwrite = TRUE)
  } else {
    # Ищем любой HTML файл
    html_files <- list.files(test_dir, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
    if (length(html_files) > 0) {
      file.copy(html_files[1], final_file, overwrite = TRUE)
    } else {
      stop("HTML файл не был создан")
    }
  }

  # Очищаем временные файлы, оставляем только нужные
  files_to_keep <- c("test_index.html", "demo.parquet")
  all_current <- list.files(test_dir, full.names = TRUE)

  for (f in all_current) {
    if (!basename(f) %in% files_to_keep && !grepl("^test_index\\.html$|^demo\\.parquet$", basename(f))) {
      if (file.info(f)$isdir) {
        unlink(f, recursive = TRUE)
      } else {
        file.remove(f)
      }
    }
  }

  # Удаляем пустые папки
  folders <- list.dirs(test_dir, recursive = FALSE, full.names = TRUE)
  for (folder in folders) {
    if (length(list.files(folder, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(folder, recursive = TRUE)
    }
  }

  message("\n✓ Дашборд успешно создан и находится в файле: ", final_file)
  message("  Размер файла: ", round(file.info(final_file)$size / 1024, 1), " KB")

  if (interactive()) {
    utils::browseURL(final_file)
  }

  return(invisible(final_file))
}
