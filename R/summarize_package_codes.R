#' Summarise Package Structure and Source Code
#'
#' This function generates a comprehensive text summary of an R package project.
#' It captures the directory tree structure and consolidates the contents of
#' all `.R` files located in the `R/` directory and `tests/testthat/` directory
#' into a single text file.
#'
#' @param package_path A character string specifying the path to the root of the
#'   R package project.
#' @param output_path A character string specifying the file path (including filename)
#'   where the output `.txt` file will be saved.
#'
#' @return This function is called for its side effect of creating a text file
#'   at the specified \code{output_path}. It returns \code{NULL} invisibly.
#'
#' @details
#' The output file is divided into two sections:
#' \enumerate{
#'   \item \strong{Project Structure:} A visual ASCII tree representation of all files
#'     and directories within the package root (generated via \code{fs::dir_tree}).
#'   \item \strong{Source Code Contents:} The full text content of every \code{.R} file
#'     found in \code{R/} and \code{tests/testthat/}. Each file is separated by a
#'     visual header containing the relative file path.
#' }
#'
#' @note This function requires the \code{fs} package to be installed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run on the current working directory
#' summarise_package_code(
#'   package_path = ".",
#'   output_path = "package_summary.txt"
#' )
#'
#' # Run on a specific package path
#' summarise_package_code(
#'   package_path = "~/projects/my_package",
#'   output_path = "~/Desktop/output.txt"
#' )
#' }

summarise_package_code <- function(package_path, output_path) {

  # Check if 'fs' package is installed
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("This function requires the 'fs' package for tree generation. Please run: install.packages('fs')")
  }

  # Normalize paths to avoid slash issues
  package_path <- fs::path_abs(package_path)

  # --- PART 1: Generate Directory Tree ---
  # capture.output allows us to save the console print of dir_tree to a variable
  tree_structure <- capture.output(fs::dir_tree(path = package_path, recurse = TRUE))

  # --- PART 2: Read Source Code ---
  # Define the specific directories to look for
  target_dirs <- file.path(package_path, c("R", "tests/testthat"))

  # Filter to only directories that actually exist (e.g. if no tests exist)
  existing_dirs <- target_dirs[dir.exists(target_dirs)]

  # Find all .R or .r files in those specific folders
  r_files <- list.files(
    path = existing_dirs,
    pattern = "\\.[rR]$",
    full.names = TRUE,
    recursive = FALSE # We usually don't want deep recursion inside R/ folders, but you can change to TRUE
  )

  code_contents <- character()

  if (length(r_files) > 0) {
    # Loop through files and format the content
    code_contents <- lapply(r_files, function(file_path) {

      # Get path relative to the project root (e.g., "R/utils.R")
      rel_path <- fs::path_rel(path = file_path, start = package_path)

      # Read the file content
      lines <- readLines(file_path, warn = FALSE)

      # Create a separator header
      header <- paste0("\n", paste(rep("-", 50), collapse = ""), "\n",
                       rel_path, "\n",
                       paste(rep("-", 50), collapse = ""), "\n")

      # Combine header and file content
      paste(c(header, lines), collapse = "\n")
    })
  } else {
    code_contents <- "\nNo .R files found in R/ or tests/testthat/."
  }

  # --- PART 3: Combine and Save ---
  final_output <- c(
    "==================================================",
    "PROJECT STRUCTURE",
    "==================================================",
    tree_structure,
    "\n",
    "==================================================",
    "SOURCE CODE CONTENTS",
    "==================================================",
    unlist(code_contents)
  )

  # Ensure the directory for the output file exists
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  # Write the file
  writeLines(final_output, output_path)

  message(paste("Successfully generated summary at:", output_path))
}
