
#'Create HTML Report with DB-IP Data Visualizations
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`docs/index.html`)
#' @export

make_dashboard <- function() {
  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (!dir.exists("docs")) dir.create("docs")
  quarto::quarto_render(quarto_path)

  if (file.exists("docs/index.html")) {
    message("Файл создан: ", normalizePath("docs/index.html"))
  }
}
