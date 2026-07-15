#' Create a snapshot-style project structure analysis subfolder
#'
#' @description
#'     `makeSnapshot()` creates a dated analysis snapshot folder, and template files
#'     placed in the snapshot code directory.
#'
#' @param project.dir A character string specifying the path where the project
#'     root will be created.
#' @param proj.name NULL or a character string providing the name of the project.
#' @param proj.num NULL or a character string providing a project number.
#' @param start.date NULL or a character string providing the date to be included
#'     in templates. Defaults to today's date.
#' @param version NULL or a character string providing the project version to be
#'     included in the templates. Defaults to "1".
#' @param client NULL or a character string providing the name(s) of client(s).
#' @param client.dept NULL or a character string providing the affiliation of the
#'     client(s).
#' @param main.statistician NULL or a character string providing the name of the
#'     primary statistician.
#' @param stats.collab NULL or a character string providing additional
#'     collaborators.
#' @param templates A character string providing a comma delimited list of
#'     templates to be generated. Valid options are memo, R, Rmd and sas.
#' @param memo.name NULL or a character string specifying the name of the .docx
#'     memo template.
#' @param memo.re NULL or a character string providing the subject line for the
#'     memo template.
#' @param r.name NULL or a character string specifying the name of the .R template.
#' @param r.purpose NULL or a character string providing the purpose for the R
#'     file to be included in the R template header.
#' @param r.notes NULL or a character string providing additional notes to be
#'     included in the R template header.
#' @param rmd.name NULL or a character string specifying the name of the .Rmd
#'     template.
#' @param rmd.purpose NULL or a character string providing the purpose for the
#'     Rmd file to be included in the Rmd template header.
#' @param rmd.notes NULL or a character string providing additional notes to be
#'     included in the Rmd template header.
#' @param sas.name NULL or a character string specifying the name of the .sas
#'     template.
#' @param sas.purpose NULL or a character string providing the purpose for SAS
#'     file to be included in the template header.
#' @param sas.notes NULL or a character string providing additional notes to be
#'     included in the SAS template header.
#' @param snapshot.name NULL or a character string providing the snapshot folder
#'     name. If NULL, an analysis_YYYYMMDD folder will be created.
#' @param analysis.subfolders A character vector providing the subfolders to
#'     create inside the analysis snapshot directory. Defaults to the standard
#'     Snapshot layout: orig_data, code, data, output_raw, graphs,
#'     final_styled, and memo.
#' @param sas.template.layout A character string specifying the SAS template layout
#'     to use for Snapshot projects. Supported values are "single" and "multi".
#'     Defaults to "single".
#' @param r.template.layout A character string specifying the R template layout
#'     to use for Snapshot projects. Supported values are "single" and "multi".
#'     Defaults to "single".
#' @param sas.header.style A character string specifying the SAS header style.
#'     Supported values are "default" and "simple". Defaults to "default".
#' @param r.header.style A character string specifying the R header style.
#'     Supported values are "default" and "simple". Defaults to "default".
#'
#' @export
makeSnapshot <- function(project.dir = getwd(), proj.name = NULL, proj.num = NULL,
                         start.date = format(Sys.Date(), "%B %d, %Y"),
                         version = "1", client = NULL, client.dept = NULL,
                         main.statistician = NULL, stats.collab = NULL,
                         templates = "memo, R, Rmd, sas",
                         memo.name = NULL, memo.re = NULL,
                         r.name = NULL, r.purpose = NULL, r.notes = NULL,
                         rmd.name = NULL, rmd.purpose = NULL, rmd.notes = NULL,
                         sas.name = NULL, sas.purpose = NULL, sas.notes = NULL,
                         snapshot.name = NULL,
                         analysis.subfolders = c("orig_data", "code", "data",
                                                 "output_raw", "graphs",
                                                 "final_styled", "memo"),
                         sas.template.layout = c("single", "multi"),
                         r.template.layout = c("single", "multi"),
                         sas.header.style = c("default", "simple"),
                         r.header.style = c("default", "simple")) {

  sas.template.layout <- if (is.null(sas.template.layout)) {
    if (!is.null(template.layout)) template.layout else "single"
  } else {
    match.arg(sas.template.layout, c("single", "multi"))
  }
  r.template.layout <- if (is.null(r.template.layout)) {
    if (!is.null(template.layout)) template.layout else "single"
  } else {
    match.arg(r.template.layout, c("single", "multi"))
  }
  sas.header.style <- match.arg(sas.header.style, c("default", "simple"))
  r.header.style <- match.arg(r.header.style, c("default", "simple"))

  if (is.null(project.dir) || isTRUE(trimws(project.dir) == "")) {
    project.dir <- getwd()
  }

  if (is.null(proj.name) || isTRUE(trimws(proj.name) == "")) {
    proj.name <- basename(project.dir)
  }

  if (is.null(proj.num) || isTRUE(trimws(proj.num) == "")) {
    proj.num <- proj.name
  }

  if (is.null(start.date) || isTRUE(trimws(start.date) == "")) {
    start.date <- format(Sys.Date(), "%B %d, %Y")
  }

  if (!(exists("date.stamp"))) {
    date.stamp <- format(as.Date(start.date, format = "%B %d, %Y"), "%Y%m%d")
    if (is.na(date.stamp)) {
      date.stamp <- format(Sys.Date(), "%Y%m%d")
    }
  }

  if (is.null(snapshot.name) || isTRUE(trimws(snapshot.name) == "")) {
    snapshot.name <- paste0("analysis_", date.stamp)
  }

  project.dir <- normalizePath(project.dir, winslash = "/", mustWork = FALSE)
  proj.root <- file.path(project.dir, proj.name)
  if (!dir.exists(proj.root)) {
    dir.create(proj.root, recursive = TRUE, showWarnings = FALSE)
  }

  snapshot.dir <- file.path(proj.root, snapshot.name)
  if (!dir.exists(snapshot.dir)) {
    dir.create(snapshot.dir, recursive = TRUE, showWarnings = FALSE)
  }

  analysis_folders <- analysis.subfolders
  if (is.null(analysis_folders) || length(analysis_folders) == 0) {
    analysis_folders <- c("orig_data", "code", "data", "output_raw", "graphs",
                          "final_styled", "memo")
  }

  if (length(grep("memo", tolower(templates), fixed = TRUE)) > 0) {
    analysis_folders <- unique(c(analysis_folders, "memo"))
  }
  if (length(grep("r", tolower(templates), fixed = TRUE)) > 0 ||
      length(grep("rmd", tolower(templates), fixed = TRUE)) > 0 ||
      length(grep("sas", tolower(templates), fixed = TRUE)) > 0) {
    analysis_folders <- unique(c(analysis_folders, "code"))
  }

  for (folder in analysis_folders) {
    folder_path <- file.path(snapshot.dir, folder)
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    }
  }

  if (length(templates) > 0) {
    if (length(grep("memo", tolower(templates), fixed = TRUE)) > 0) {
      memo.dir <- file.path(snapshot.dir, "memo")
      makeMemoTemplate(
        memo.dir = memo.dir,
        memo.name = memo.name,
        start.date = start.date,
        version = version,
        client = client,
        client.dept = client.dept,
        main.statistician = main.statistician,
        stats.collab = stats.collab,
        memo.re = memo.re
      )
    }

    if (length(grep("r", tolower(templates), fixed = TRUE)) > 0) {
      r.dir <- file.path(snapshot.dir, "code")
      makeRTemplate(
        r.dir = r.dir,
        r.name = r.name,
        proj.name = proj.num,
        start.date = start.date,
        version = version,
        client = client,
        client.dept = client.dept,
        main.statistician = main.statistician,
        stats.collab = stats.collab,
        r.purpose = r.purpose,
        r.notes = r.notes,
        r.template.layout = r.template.layout,
        r.header.style = r.header.style
      )
    }

    if (length(grep("rmd", tolower(templates), fixed = TRUE)) > 0) {
      rmd.dir <- file.path(snapshot.dir, "code")
      makeRmdTemplate(
        rmd.dir = rmd.dir,
        rmd.name = rmd.name,
        proj.name = proj.num,
        start.date = start.date,
        version = version,
        client = client,
        client.dept = client.dept,
        main.statistician = main.statistician,
        stats.collab = stats.collab,
        rmd.purpose = rmd.purpose,
        rmd.notes = rmd.notes
      )
    }

    if (length(grep("sas", tolower(templates), fixed = TRUE)) > 0) {
      sas.dir <- file.path(snapshot.dir, "code")
      sas_file_name <- if (!is.null(sas.name) && !isTRUE(trimws(sas.name) == "")) {
        sas.name
      } else {
        paste0("p", proj.num, "_sas", format(as.Date(start.date, format = "%B %d, %Y"), "%Y%m%d"))
      }
      makeSasTemplate(
        sas.dir = sas.dir,
        sas.name = sas_file_name,
        proj.name = proj.num,
        start.date = start.date,
        version = version,
        client = client,
        client.dept = client.dept,
        main.statistician = main.statistician,
        stats.collab = stats.collab,
        sas.purpose = sas.purpose,
        sas.notes = sas.notes,
        sas.template.layout = sas.template.layout,
        sas.header.style = sas.header.style
      )
    }
  }

  invisible(proj.root)
}
