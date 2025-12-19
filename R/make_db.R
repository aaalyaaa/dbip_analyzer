
#'Create HTML Report with DB-IP Data Visualizations
#'
#' Renders an interactive HTML report with maps and charts based on DB-IP data.
#' The report is saved to `docs/index.html` in the current working directory.
#'
#' @return Invisibly returns the path to the created HTML file (`docs/index.html`)
#' @export

make_dashboard <- function() {
  quarto_path <- system.file("quarto", package = "dbipAnalyzer")
  if (quarto_path == "" || !dir.exists(quarto_path)) {
    stop("Package 'dbipAnalyzer' not installed correctly")
  }
  message("Quarto project found: ", quarto_path)
  docs_in_package <- file.path(quarto_path, "docs")
  if (dir.exists(docs_in_package)) {
    warning("Removing existing docs/ from package to avoid conflicts...")
    unlink(docs_in_package, recursive = TRUE)
  }

  message("Rendering dashboard...")
  quarto::quarto_render(
    input = quarto_path,
    quiet = FALSE
  )}
