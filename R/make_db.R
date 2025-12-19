
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

  cat("📁 User directory:", user_dir, "\n")
  cat("🔍 Checking user data:", user_data_path, "\n")

  if (!file.exists(user_data_path)) {
    stop(
      "❌ Data file not found:\n",
      "   ", user_data_path, "\n\n",
      "To fix:\n",
      "   1. Run: run_etl_pipeline()\n",
      "   2. Or place dbip_data.parquet in processed/ folder"
    )
  }
  cat("✅ User data found\n\n")

  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_path == "" || !dir.exists(quarto_path)) {
    stop("Package 'dbipAnalyzer' not installed correctly")
  }

  docs_in_package <- file.path(quarto_path, "docs")
  if (dir.exists(docs_in_package)) {
    unlink(docs_in_package, recursive = TRUE)
  }

  temp_dir <- tempfile("dashboard_")
  dir.create(temp_dir)
  file.copy(quarto_path, temp_dir, recursive = TRUE)

  temp_quarto <- file.path(temp_dir, "quarto")

  file.copy(user_data_path, file.path(temp_quarto, "dbip_data.parquet"))
  cat("📊 Data copied to Quarto project\n")


  cat("🚀 Rendering dashboard...\n")

  old_wd <- getwd()
  setwd(temp_quarto)

  tryCatch({
    quarto::quarto_render(".", quiet = FALSE)
  }, finally = {
    setwd(old_wd)
  })


  temp_html <- file.path(temp_quarto, "docs", "index.html")
  user_html <- file.path(user_dir, "docs", "index.html")

  if (file.exists(temp_html)) {
    if (!dir.exists(file.path(user_dir, "docs"))) {
      dir.create(file.path(user_dir, "docs"), recursive = TRUE)
    }

    file.copy(temp_html, user_html, overwrite = TRUE)
    cat("✅ Dashboard created:", user_html, "\n")
  } else {
    cat("⚠️  HTML file not created\n")
  }
}
