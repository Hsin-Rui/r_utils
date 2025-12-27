#' Update package meta data
#'
#' @description The function updates the version and the date of the DESCRIPTION file, insert the new version
#' and the features/fix messages inside the NEWS.md file, provide informative message.
#'
#' @param fixtures (str) a character vetor of the fixes and features to mention in the NEWS.md file. Example:
#' c("speeking up the `get_data()` function",
#' "fixing the `pull_data()` function",
#' "minor")
#'
#' @return called for the side effect of updating the package meta data in the DESCRIPTION file and inserting
#' release-related information in the NEWS.md
#' @export
#' @examples
#' \dontrun{
#' new_features <- c(
#'  "speeding up the get_data() function",
#'  "fixing the pull_data()",
#'  "minor"
#' )
#'
#' update_package_meta(fixtures = new_features)
#' }
#'
#'@importFrom usethis proj_path
#'@importFrom desc description
#'@importFrom cli cli_alert_success
#'@importFrom cli cli_alert_warning
#'@importFrom cli cli_alert_info
#'@importFrom fs file_create
#'@import usethis
#'

update_package_meta <- function(fixtures) {

  usethis:::check_is_project()

  news_file_path <- usethis::proj_path("NEWS.md")

  news_exists <- file.exists(news_file_path)

  if(!news_exists){
    message("NEWS.md file does not exist, creating it ...")
    fs::file_create(usethis::proj_path("NEWS.md"))
  }

  decision_version <- usethis:::choose_version(message = "Choose the appropriate version number")

  if (is.null(decision_version)) {
    message("OK try again later ...")
    return(NULL)
  }

  new_version <- as.list(decision_version)[[1]]

  description_file_path <- usethis::proj_path("DESCRIPTION")

  if (!file.exists(description_file_path)) {
    stop("DESCRIPTION file does not exist, please create it first")
  }

  desc_file <- desc::description$new(file = description_file_path)

  desc_file$set("Date", Sys.Date())

  cli::cli_alert_success("Date filed in DESCRIPTION set to {Sys.Date()}")

  desc_file$set("Version", new_version)

  cli::cli_alert_success("Version field in DESCRIPTION set to {new_version}")

  desc_file$write(description_file_path)

  actual_content <- readLines(news_file_path)

  new_content <- paste0(
    "# ",
    unname(desc_file$get("Package")),
    " ",
    new_version,
    " -- ",
    Sys.Date(),
    "\n",
    paste("*", fixtures, collapse = "\n"),
    "\n\n"
  )

  final_content_to_write <- c(new_content, actual_content)

  writeLines(final_content_to_write, news_file_path)

  cli::cli_alert_success("Writing into {.emph NEWS.md}")
  cli::cli_alert_warning("Please run devtools::check() before pushing")
  cli::cli_alert_warning("Please create a merge request and avoid pushing directly to production")
  cli::cli_alert_info("If you want, you can copy your fixture text and use it as a commit message (see below)")

  fixtures <- gsub("`", "", fixtures)
  message(paste(fixtures, collapse = " + "))

}

