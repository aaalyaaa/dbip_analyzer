#' ETL process for DB-IP data
#' 
#' This function performs the complete ETL process.
#' 
#' @param output_dir Directory for saving processed data
#' @return Invisibly returns processed data
#' @export
run_etl <- function(output_dir = "processed") {
  
  # Extract
  files <- download_dbip()
  
  # Transform and Load
  message("Starting data processing...")
  result <- process_dbip(files, output_dir)
  
  return(invisible(result))
}