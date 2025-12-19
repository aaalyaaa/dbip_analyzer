
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
  setwd(temp_quarto)

  tryCatch({
    quarto::quarto_render(".", quiet = FALSE)
  }, finally = {
    setwd(old_wd)
  })


  temp_html <- file.path(temp_quarto, "docs", "index.html")
  if (file.exists(temp_html)) {
    if (!dir.exists("docs")) dir.create("docs")
    file.copy(temp_html, "docs/index.html", overwrite = TRUE)
    cat("✅ Dashboard created\n")
  }
}
