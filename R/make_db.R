
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
    stop("Сначала запустите run_etl_pipeline()")
  }

  docs_dir <- file.path(user_dir, "docs")
  if (!dir.exists(docs_dir)) {
    dir.create(docs_dir, recursive = TRUE)
    cat("📁 Создана папка docs в:", normalizePath(docs_dir), "\n")
  }

  temp_dir <- tempfile("quarto_work_")
  dir.create(temp_dir, recursive = TRUE)


  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_path == "") stop("Шаблон Quarto не найден")


  file.copy(list.files(quarto_path, full.names = TRUE),
            temp_dir, recursive = TRUE)


  temp_data_path <- file.path(temp_dir, "processed")
  dir.create(temp_data_path, showWarnings = FALSE)
  file.copy(user_data_path, file.path(temp_data_path, "dbip_data.parquet"))


  old_wd <- getwd()
  setwd(temp_dir)

  cat("🎨 Рендерим dashboard...\n")

  # Способ 1: Используем quarto::quarto_render с output_file
  output_file <- file.path(docs_dir, "index.html")

  # Рендерим прямо в целевую папку
  quarto::quarto_render(
    input = "index.qmd",
    output_file = output_file,
    as_job = FALSE  # Важно! Не фоновый режим
  )

  setwd(old_wd)

  # 5. Проверяем результат
  if (file.exists(output_file)) {
    cat("\n✅ Dashboard успешно создан!\n")
    cat("📁 Расположение:", normalizePath(output_file), "\n")}
  else {cat("ошибка")}
}
