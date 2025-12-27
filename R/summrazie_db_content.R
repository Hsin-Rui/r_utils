#' Summarise Database Structure and Content
#'
#' This function generates a text summary of a database connection. It documents
#' the total number of tables, lists all table names, and provides a 5-row preview
#' of every table in the database.
#'
#' @param con A database connection object created by \code{DBI::dbConnect}.
#' @param output_path A character string specifying the file path (including filename)
#'   where the output `.txt` file will be saved.
#'
#' @return This function is called for its side effect of creating a text file
#'   at the specified \code{output_path}. It returns \code{NULL} invisibly.
#'
#' @details
#' The output file is formatted as follows:
#' \enumerate{
#'   \item \strong{Summary Header:} Lists the total table count and names.
#'   \item \strong{Table Previews:} Iterates through each table, fetching the first
#'     5 rows using \code{DBI::dbFetch} (ensuring efficiency even for large tables),
#'     and prints the output.
#' }
#'
#' @note This function requires the \code{DBI} package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a dummy database for testing
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbWriteTable(con, "mtcars", mtcars)
#' DBI::dbWriteTable(con, "iris", iris)
#'
#' # Run the summary function
#' summarise_db_content(con, "db_summary.txt")
#'
#' # Clean up
#' DBI::dbDisconnect(con)
#' }
summarise_db_content <- function(con, output_path) {

  # Check if DBI is available (usually loaded if 'con' exists, but good practice)
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("This function requires the 'DBI' package.")
  }

  # --- PART 1: Get Metadata ---
  # List all tables in the database
  table_names <- DBI::dbListTables(con)
  num_tables <- length(table_names)

  # --- PART 2: Generate Header ---
  header_section <- c(
    "==================================================",
    "DATABASE SUMMARY",
    "==================================================",
    paste("Total Tables:", num_tables),
    "\nTable List:",
    paste(" -", table_names),
    "\n",
    "==================================================",
    "TABLE DATA PREVIEWS (First 5 Rows)",
    "=================================================="
  )

  # --- PART 3: Fetch Data Previews ---
  # We use lapply to loop through tables and capture their output
  data_previews <- lapply(table_names, function(tbl) {

    # 1. Safely quote the table name (handles spaces or special chars in names)
    quoted_name <- DBI::dbQuoteIdentifier(con, tbl)

    # 2. Prepare query to select everything
    # We do NOT use "LIMIT 5" in SQL because syntax varies by DB (SQL Server uses TOP).
    # Instead, we fetch only n=5 rows using R's DBI interface.
    query <- paste("SELECT * FROM", quoted_name)

    # 3. Execute and Fetch
    # Use tryCatch to prevent the whole function from crashing if one table is locked/corrupt
    preview_content <- tryCatch({
      res <- DBI::dbSendQuery(con, query)
      on.exit(DBI::dbClearResult(res)) # Ensure result is cleared even if errors occur

      df_head <- DBI::dbFetch(res, n = 5)

      # Capture the print output of the dataframe to character lines
      capture.output(print(df_head))

    }, error = function(e) {
      return(paste("Error reading table:", e$message))
    })

    # 4. Format the block for this table
    c(
      "\n",
      paste(rep("-", 50), collapse = ""),
      paste("Table:", tbl),
      paste(rep("-", 50), collapse = ""),
      preview_content
    )
  })

  # --- PART 4: Write to File ---
  final_output <- c(header_section, unlist(data_previews))

  # Ensure directory exists
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  writeLines(final_output, output_path)

  message(paste("Successfully generated database summary at:", output_path))
}
