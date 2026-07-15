#' Create a .sas template
#'
#' @description
#'     `makSasTemplate` will generate a .sas file with a header containing
#'    any project information provided in the function call.
#'
#' @param sas.dir A character string specifying the file path where the template
#'     will be stored. Defaults to {getwd()}.
#' @param sas.name NULL or a character string specifying the name of the .sas file.
#'     If NULL, the file will be named p[proj.name]_sas[currentdate]_v[version]
#'     or sas[currentdate].
#' @param proj.name NULL or a character string providing the name of the project
#'     to be included in the template name and/or header.
#' @param start.date NULL or a character string providing the date to be included
#'     in the template header. Defaults to today's date.
#' @param version NULL or a character string providing the project version to be
#'     included in the template name and/or header. Defaults to "1".
#' @param client NULL or a character string providing the name(s) of client(s)
#'     to be included in the template header.
#' @param client.dept NULL or a character string providing the affiliation of the
#'     client(s) to be included in the template header.
#' @param main.statistician NULL or a character string providing the name of the
#'     primary statistician. This will be used in the template header.
#' @param stats.collab NULL or a character string providing additional 
#'     collaborators to be included as authors in the template header.
#' @param sas.purpose NULL or a character string providing the purpose for the
#'     generated template to be included in the template header.
#' @param sas.notes NULL or a character string providing additional notes to be
#'     included in the template header.
#' @param sas.template.layout A character string specifying the SAS template layout.
#'     Supported values are "single" and "multi". Defaults to "single".
#' @param sas.header.style A character string specifying the SAS header style.
#'     Supported values are "default" and "simple". Defaults to "default".
#'
#' @details
#' You can append code and comments under the generated header by either
#'    creating a .basefile.sas file in your /home/<USERNAME> directory or by modifying
#'     the templates/basefile.sas file contained in the package library.
#'     
#'     To find the location of your HOME directory, run
#'     `Sys.getenv("HOME")`.
#'
#'     To find the location of the included template file, run
#'     `system.file("templates", package = "startProject")`.
#'    
#' @examples
#' \dontrun{makeSasTemplate(sas.name = "sasTemplate")}
#'
#' @author Rocio Lopez, \email{Rocio.LopezMoscoso@cuanschutz.edu}
#'
#' @export
makeSasTemplate <- function(sas.dir = getwd(), sas.name = NULL, proj.name = NULL,
                            start.date = format(Sys.Date(), "%B %d, %Y"),
                            version = "1", client = NULL, client.dept = NULL,
                            main.statistician = NULL, stats.collab = NULL, 
                            sas.purpose = NULL, sas.notes = NULL,
                            sas.template.layout = c("single", "multi"),
                            sas.header.style = c("default", "simple"))
  {

  ## sas.dir must be provided and exist
  if (is.null(sas.dir) | isTRUE(trimws(sas.dir) == "")) {
    stop("Parameter 'sas.dir' must be specified")
  }
  if (!(dir.exists(sas.dir))) {
    stop("Path specified under 'sas.dir' does not exist")
  }

  sas.template.layout <- match.arg(sas.template.layout)
  sas.header.style <- match.arg(sas.header.style)

  ## Define variables from inputs
  if (is.null(start.date) | isTRUE(trimws(start.date) == "")) {
    start.date <- format(Sys.Date(), "%B %d, %Y")
  }
  if (!(exists("date.stamp"))) {
    date.stamp <- format(Sys.Date(), "%Y%m%d")
  }
  if (is.null(sas.name) | isTRUE(trimws(sas.name) == "")) {
    if (is.null(version) | isTRUE(trimws(version)) == "") {
      if (!(is.null(proj.name)) | isFALSE(trimws(proj.name) == ""))
        sas.name <- paste0("p", proj.name, "_sas", date.stamp)
      else
        sas.name <- paste0("sas", date.stamp)
    }
    else {
      if (!(is.null(proj.name)) | isFALSE(trimws(proj.name) == ""))
        sas.name <- paste0("p", proj.name, "_sas", date.stamp, "_v", version)
      else
        sas.name <- paste0("sas", date.stamp, "_v", version)
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

  build_header <- function(sas_file_path, purpose_text, notes_text) {
    if (identical(sas.header.style, "simple")) {
      header_lines <- c(
        "/*****************************************************************************",
        paste0("FILE:     ", basename(sas_file_path)),
        paste0("LOCATION: ", dirname(sas_file_path)),
        "______________________________________________________________________________",
        " ",
        paste0("AUTHOR:  ", if (!is.null(main.statistician) && !isTRUE(trimws(main.statistician) == "")) main.statistician else ""),
        paste0("PROJECT: ", if (!is.null(proj.name) && !isTRUE(trimws(proj.name) == "")) proj.name else ""),
        paste0("VERSION: ", if (!is.null(version) && !isTRUE(trimws(version) == "")) version else "1", " (", date.stamp, ")"),
        "______________________________________________________________________________",
        " ",
        paste0("PURPOSE: ", purpose_text),
        paste0("NOTES:   ", notes_text),
        "______________________________________________________________________________",
        " ",
        "CONTENTS:",
        "******************************************************************************/"
      )
    } else {
      header_lines <- c(
        "/******************************************************************************",
        paste0("PROJECT: ", proj.name),
        paste0("START DATE: ", start.date),
        paste0("VERSION: ", version),
        paste0("PROGRAM: ", sas_file_path),
        "______________________________________________________________________________",
        " ",
        paste0("PRIMARY STATISTICIAN: ", main.statistician),
        paste0("STATS COLLABORATORS: ", stats.collab),
        "______________________________________________________________________________",
        " ",
        paste0("CLIENT: ", client),
        paste0("CLIENT AFFILIATION: ", client.dept),
        "______________________________________________________________________________",
        " ",
        paste0("PURPOSE:", sas.purpose),
        paste0("NOTES: ", sas.notes),
        "______________________________________________________________________________",
        " ",
        "DEPENDENCIES:",
        "______________________________________________________________________________",
        " ",
        "CONTENTS:",
        "******************************************************************************/"
      )
    }
    header_lines
  }

  file.permission <- "2770"

  sas_files <- if (identical(sas.template.layout, "multi")) {
    list(
      list(name = "00_setup", purpose = if (!is.null(sas.purpose) && !isTRUE(trimws(sas.purpose) == "")) sas.purpose else "Defines libraries and global parameters. Required execution for all subsequent scripts.", notes = if (!is.null(sas.notes) && !isTRUE(trimws(sas.notes) == "")) sas.notes else "See root README.md for versioning, dependencies, and execution order.", basefile = "multi_setup_basefile.sas"),
      list(name = "01_data_management", purpose = "Creates, cleans, and prepares the analysis data.", notes = if (!is.null(sas.notes) && !isTRUE(trimws(sas.notes) == "")) sas.notes else "See root README.md for versioning, dependencies, and execution order.", basefile = "multi_data_management_basefile.sas"),
      list(name = "03_analysis", purpose = "Performs the primary analysis for the project.", notes = if (!is.null(sas.notes) && !isTRUE(trimws(sas.notes) == "")) sas.notes else "See root README.md for versioning, dependencies, and execution order.", basefile = "multi_analysis_basefile.sas")
    )
  } else {
    list(list(name = sas.name, purpose = if (!is.null(sas.purpose) && !isTRUE(trimws(sas.purpose) == "")) sas.purpose else "", notes = if (!is.null(sas.notes) && !isTRUE(trimws(sas.notes) == "")) sas.notes else "", basefile = "basefile.sas"))
  }

  for (sas_spec in sas_files) {
    sas_file_name <- sas_spec$name
    sas_file_path <- file.path(sas.dir, paste0(sas_file_name, ".sas"))

    if (file.exists(sas_file_path)) {
      warning(paste0(sas_file_path, " already exists and will not be created"))
      next
    }

    add <- resolve_basefile(sas_spec$basefile)

    sasFileConn <- file(sas_file_path)
    writeLines(c(build_header(sas_file_path, sas_spec$purpose, sas_spec$notes), add), sasFileConn)
    close(sasFileConn)
    Sys.chmod(sas_file_path, mode = file.permission, use_umask = FALSE)
  }

}
