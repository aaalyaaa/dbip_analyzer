#' Start HTTP server for dashboard
#'
#' @param port Port for HTTP server
#' @param docs_dir Directory with dashboard HTML
#' @export
.start_server <- function(port = 8080, docs_dir = "docs") {
  # Проверяем, доступен ли порт
  check_port <- function(port) {
    tryCatch({
      con <- socketConnection(
        host = "127.0.0.1",
        port = port,
        server = FALSE,
        timeout = 1
      )
      close(con)
      return(FALSE)  # Порт занят
    }, error = function(e) {
      return(TRUE)   # Порт свободен
    })
  }
  
  # Если порт занят, пробуем следующий
  if (!check_port(port)) {
    message(sprintf("Port %d is busy, trying %d...", port, port + 1))
    port <- port + 1
  }
  
  # Добавляем проверку servr для Docker
  if (!requireNamespace("servr", quietly = TRUE)) {
    if (!interactive()) {
      # В Docker автоматически устанавливаем
      install.packages("servr", repos = "https://cloud.r-project.org")
      library(servr)
    } else {
      stop("Install servr: install.packages('servr')")
    }
  }
  
  if (!dir.exists(docs_dir)) {
    stop("Run make_dashboard() first")
  }
  
  url <- sprintf("http://localhost:%d", port)
  message("Server started at: ", url)
  
  servr::httd(dir = docs_dir, port = port, daemon = FALSE)
}

#' Run complete DB-IP Analyzer
#'
#' @param port Port for HTTP server
#' @export
run_app <- function(port = 8080) {
  message("Starting DB-IP Analyzer...")
  
  # 1. ETL
  message("Processing data...")
  run_etl()
  
  # 2. Dashboard
  message("Creating dashboard...")
  make_dashboard()
  
  # 3. Server
  message(sprintf("Starting server on port %d...", port))
  
  # Открываем браузер автоматически
  final_port <- port
  url <- sprintf("http://localhost:%d", final_port)
  
  if (interactive()) {
    message("Opening browser automatically...")
    Sys.sleep(2)
    tryCatch({
      utils::browseURL(url)
    }, error = function(e) {
      message("Could not open browser automatically")
      message("   Please open manually: ", url)
    })
  } else {
    # В Docker контейнере показываем URL
    message("Dashboard URL: ", url)
  }
  
  .start_server(port = final_port)
}