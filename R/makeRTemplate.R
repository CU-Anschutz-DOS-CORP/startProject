#' Create a .R template
#'
#' @description
#'     `makeRTemplate` will generate a .R file with a header containing
#'    any project information provided in the function call.
#'
#' @param r.dir A character string specifying the file path where the template
#'     will be stored. Defaults to {getwd()}.
#' @param r.name NULL or a character string specifying the name of the .R file.
#'     If NULL, the file will be named [proj.id]_r[currentdate]_v[version] or
#'     r[currentdate].
#' #' @param proj.title A character string providing the full, descriptive project title 
#'     to be used in template headers and memo subject lines.
#' @param proj.id NULL or a short, clean character string providing a project identifier 
#'     to be used as the directory name and as a prefix for template filenames. 
#'     If NULL, a clean identifier will be automatically derived from [proj.title].
#' @param start.date NULL or a character string providing the date to be included
#'     in the template header. Defaults to today's date.
#' @param version NULL or a character string providing the project version to be
#'     included in the template name and/or header. Defaults to "1".
#' @param client NULL or a character string providing the name(s) of client(s) to
#'     be included in the template header.
#' @param client.dept NULL or a character string providing the affiliation of the
#'     client(s) to be included in the template header.
#' @param main.statistician NULL or a character string providing the name of the
#'     primary statistician. This will be used in the template header.
#' @param stats.collab NULL or a character string providing additional 
#'     collaborators to be included as authors in the template header.
#' @param r.purpose NULL or a character string providing the purpose for the
#'     generated template to be included in the template header.
#' @param r.notes NULL or a character string providing additional notes to be
#'     included in the template header.
#' @param r.template.layout A character string specifying the R template layout.
#'     Supported values are "single" and "multi". Defaults to "single".
#' @param r.header.style A character string specifying the R header style.
#'     Supported values are "default" and "simple". Defaults to "default".
#'
#' @details
#' You can append code and comments under the generated header by either
#'    creating a .basefile.R file in your /home/<USERNAME> directory or by modifying
#'     the templates/basefile.R file contained in the package library.
#'     
#'     To find the location of your HOME directory, run
#'     `Sys.getenv("HOME")`.
#'
#'     To find the location of the included template file, run
#'     `system.file("templates", package = "startProject")`.
#'     
#' @examples
#' \dontrun{makeRTemplate(r.name = "rTemplate")}
#'
#' @author Rocio Lopez, \email{Rocio.LopezMoscoso@cuanschutz.edu}
#'
#' @export
makeRTemplate <- function(r.dir = getwd(), r.name = NULL, 
                          proj.id = NULL, proj.title = NULL,
                          start.date = format(Sys.Date(), "%B %d, %Y"),
                          version = "1", client = NULL, client.dept = NULL,
                          main.statistician = NULL, stats.collab = NULL, 
                          r.purpose = NULL, r.notes = NULL,
                          r.template.layout = c("single", "multi"),
                          r.header.style = c("default", "simple"))
  {

  ## r.dir must be provided and exist
  if (is.null(r.dir) | isTRUE(trimws(r.dir) == "")) {
    stop("Parameter 'r.dir' must be specified")
  }
  if (!(dir.exists(r.dir))) {
    stop("Path specified under 'r.dir' does not exist")
  }

  r.template.layout <- match.arg(r.template.layout)
  r.header.style <- match.arg(r.header.style)

  ## Define variables from inputs
  if (is.null(start.date) | isTRUE(trimws(start.date) == "")) {
    start.date <- format(Sys.Date(), "%B %d, %Y")
  }
  if (!(exists("date.stamp"))) {
    date.stamp <- format(Sys.Date(), "%Y%m%d")
  }
  if (is.null(r.name) | isTRUE(trimws(r.name) == "")) {
    if (is.null(version) | isTRUE(trimws(version)) == "") {
      if (!(is.null(proj.id)) | isFALSE(trimws(proj.id) == ""))
        r.name <- paste0("p", proj.id, "_r", date.stamp)
      else
        r.name <- paste0("r", date.stamp)
    }
    else {
      if (!(is.null(proj.id)) | isFALSE(trimws(proj.id) == ""))
        r.name <- paste0("p", proj.id, "_r", date.stamp, "_v", version)
      else
        r.name <- paste0("r", date.stamp, "_v", version)
    }
  }
  homepath <- Sys.getenv("HOME")
  resolve_basefile <- function(basefile_name) {
    candidates <- c(
      file.path(homepath, paste0(".", basefile_name)),
      file.path(homepath, basefile_name),
      system.file("templates", basefile_name, package = "startProject")
    )
    for (candidate in candidates) {
      if (file.exists(candidate)) {
        return(readLines(candidate, warn = FALSE))
      }
    }
    NULL
  }

  build_header <- function(r_file_path, purpose_text, notes_text) {
    if (identical(r.header.style, "simple")) {
      c(
        "#*****************************************************************************",
        paste0("#FILE:     ", basename(r_file_path)),
        paste0("#LOCATION: ", dirname(r_file_path)),
        "#_____________________________________________________________________________",
        "#",
        paste0("#AUTHOR:   ", if (!is.null(main.statistician) && !isTRUE(trimws(main.statistician) == "")) main.statistician else ""),
        paste0("#PROJECT ID:  ", if (!is.null(proj.id) && !isTRUE(trimws(proj.id) == "")) proj.id else ""),
        paste0("#TITLE: ", if (!is.null(proj.title) && !isTRUE(trimws(proj.title) == "")) proj.title else ""),
        paste0("#VERSION:  ", if (!is.null(version) && !isTRUE(trimws(version) == "")) version else "1", " (", date.stamp, ")"),
        "#_____________________________________________________________________________",
        "#",
        paste0("#PURPOSE:  ", purpose_text),
        paste0("#NOTES:    ", notes_text),
        "#_____________________________________________________________________________",
        "#",
        "#CONTENTS:",
        "#*****************************************************************************"
      )
    } else {
      c("#******************************************************************************",
        paste0("#PROJECT ID:  ", if (!is.null(proj.id) && !isTRUE(trimws(proj.id) == "")) proj.id else ""),
        paste0("#TITLE: ", if (!is.null(proj.title) && !isTRUE(trimws(proj.title) == "")) proj.title else ""),
        paste0("#START DATE: ", start.date),
        paste0("#VERSION: ", version),
        paste0("#PROGRAM: ", r_file_path),
        "#_____________________________________________________________________________",
        "#",
        paste0("#PRIMARY STATISTICIAN: ", main.statistician),
        paste0("#STATS COLLABORATORS: ", stats.collab),
        "#_____________________________________________________________________________",
        "#",
        paste0("#CLIENT: ", client),
        paste0("#CLIENT AFFILIATION: ", client.dept),
        "#_____________________________________________________________________________",
        "#",
        paste0("#PURPOSE:", r.purpose),
        paste0("#NOTES: ", r.notes),
        "#_____________________________________________________________________________",
        "#",
        "#DEPENDENCIES:",
        "#_____________________________________________________________________________",
        "#",
        "#CONTENTS:",
        "#******************************************************************************")
    }
  }

  file.permission <- "2770"

  r_files <- if (identical(r.template.layout, "multi")) {
    list(
      list(name = "00_setup", purpose = if (!is.null(r.purpose) && !isTRUE(trimws(r.purpose) == "")) r.purpose else "Defines libraries and global parameters. Required execution for all subsequent scripts.", notes = if (!is.null(r.notes) && !isTRUE(trimws(r.notes) == "")) r.notes else "See root README.md for versioning, dependencies, and execution order.", basefile = "multi_setup_basefile.R"),
      list(name = "01_data_management", purpose = "Creates, cleans, and prepares the analysis data.", notes = if (!is.null(r.notes) && !isTRUE(trimws(r.notes) == "")) r.notes else "See root README.md for versioning, dependencies, and execution order.", basefile = "multi_data_management_basefile.R"),
      list(name = "03_analysis", purpose = "Performs the primary analysis for the project.", notes = if (!is.null(r.notes) && !isTRUE(trimws(r.notes) == "")) r.notes else "See root README.md for versioning, dependencies, and execution order.", basefile = "multi_analysis_basefile.R")
    )
  } else {
    list(list(name = r.name, purpose = if (!is.null(r.purpose) && !isTRUE(trimws(r.purpose) == "")) r.purpose else "", notes = if (!is.null(r.notes) && !isTRUE(trimws(r.notes) == "")) r.notes else "", basefile = "basefile.R"))
  }

  for (r_spec in r_files) {
    r_file_name <- r_spec$name
    r_file_path <- file.path(r.dir, paste0(r_file_name, ".R"))

    if (file.exists(r_file_path)) {
      warning(paste0(r_file_path, " already exists and will not be created"))
      next
    }

    add <- resolve_basefile(r_spec$basefile)
    rFileConn <- file(r_file_path)
    writeLines(c(build_header(r_file_path, r_spec$purpose, r_spec$notes), add), rFileConn)
    close(rFileConn)
    Sys.chmod(r_file_path, mode = file.permission, use_umask = FALSE)
  }

}
