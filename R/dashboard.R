#' Start HTTP server for dashboard
#'
#' @param port Port for HTTP server
#' @param docs_dir Directory with dashboard HTML
#' @export
.start_server <- function(port = 8080, docs_dir = "docs") {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop("Install httpuv: install.packages('httpuv')")
  }
  
  index_path <- file.path(docs_dir, "index.html")
  if (!file.exists(index_path)) {
    stop(sprintf("Dashboard not found: %s\nRun make_dashboard() first.", index_path))
  }
  
  message(sprintf("Starting server on port %d...", port))
  message(sprintf("   Open: http://localhost:%d", port))
  message("   Press Ctrl+C to stop")
  
  html_content <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
  
  app <- list(
    call = function(req) {
      list(
        status = 200L,
        headers = list('Content-Type' = 'text/html'),
        body = html_content
      )
    }
  )
  
  httpuv::runServer("0.0.0.0", port, app)
}

#' Run complete application
#'
#' @param port Port for HTTP server
#' @export
run_app <- function(port = 8080) {
  message("Running complete DB-IP Analyzer...")
  
  # 1. ETL
  message("Running ETL...")
  run_etl()
  
  # 2. Dashboard
  message("Creating dashboard...")
  make_dashboard()
  
  # 3. Server
  .start_server(port = port)
}
