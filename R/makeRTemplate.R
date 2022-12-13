#' Create a .R template
#'
#' @description
#'     `makeRTemplate` will generate a .R file with a header containing
#'    any project information provided in the function call.
#'
#' @param r.dir A character string specifying the file path where the template
#'     will be stored. Defaults to {getwd()}.
#' @param r.name NULL or a character string specifying the name of the .R file.
#'     If NULL, the file will be named p[proj.name]_r[currentdate]_v[version] or
#'     r[currentdate].
#' @param proj.name NULL or a character string providing the name of the project
#'    to be included in the template name and/or header.
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
#' @param stats.collab NULL or a character string providing additional QHS
#'     collaborators to be included as authors in the template header.
#' @param r.purpose NULL or a character string providing the purpose for the
#'     generated template to be included in the template header.
#' @param r.notes NULL or a character string providing additional notes to be
#'     included in the template header.
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
makeRTemplate <- function(r.dir = getwd(), r.name = NULL, proj.name = NULL,
                          start.date = format(Sys.Date(), "%B %d, %Y"),
                          version = "1", client = NULL, client.dept = NULL,
                          main.statistician = NULL, stats.collab = NULL, 
                          r.purpose = NULL, r.notes = NULL)
  {

  ## r.dir must be provided and exist
  if (is.null(r.dir) | isTRUE(trimws(r.dir) == "")) {
    stop("Parameter 'r.dir' must be specified")
  }
  if (!(dir.exists(r.dir))) {
    stop("Path specified under 'r.dir' does not exist")
  }

  ## Define variables from inputs
  if (is.null(start.date) | isTRUE(trimws(start.date) == "")) {
    start.date <- format(Sys.Date(), "%B %d, %Y")
  }
  if (!(exists("date.stamp"))) {
    date.stamp <- format(Sys.Date(), "%Y%m%d")
  }
  if (is.null(r.name) | isTRUE(trimws(r.name) == "")) {
    if (is.null(version) | isTRUE(trimws(version)) == "") {
      if (!(is.null(proj.name)) | isFALSE(trimws(proj.name) == ""))
        r.name <- paste0("p", proj.name, "_r", date.stamp)
      else
        r.name <- paste0("r", date.stamp)
    }
    else {
      if (!(is.null(proj.name)) | isFALSE(trimws(proj.name) == ""))
        r.name <- paste0("p", proj.name, "_r", date.stamp, "_v", version)
      else
        r.name <- paste0("r", date.stamp, "_v", version)
    }
  }
  r.file <- paste0(r.dir, "/", r.name, ".R")
  homepath <- Sys.getenv("HOME")

  ## Define folder and file permissions
  file.permission <- "2770"

  ##If file does not exist, define connection and create template file
  if (file.exists(r.file)) {
    warning(paste0(r.file, " already exists and will not be created"))
  }

  else if (!(file.exists(r.file))) {

    ## Bring in .basefile.R if it exists
    add <- NULL
    if (file.exists(paste0(homepath, "/.basefile.r"))) {
      add <- readLines(paste0(homepath, "/.basefile.r"))
    } else if (file.exists(paste0(homepath, "/.basefile.R"))) {
      add <- readLines(paste0(homepath, "/.basefile.R"))
    } else if (file.exists(system.file("templates/basefile.R", package = "startProject"))) {
      add <- readLines(system.file("templates/basefile.R", package = "startProject"))
    }

    ## Open connection to new file
    rFileConn <- file(r.file)

    ## Write lines to file
    writeLines(
        c("#******************************************************************************",
          paste0("#PROJECT: ", proj.name),
          paste0("#START DATE: ", start.date),
          paste0("#VERSION: ", version),
          paste0("#PROGRAM: ", r.file),
          "#_____________________________________________________________________________",
          "#",
          paste0("#PRIMARY STATISTICIAN: ", main.statistician),
          paste0("#STATS COLLABORATORS: ", stats.collab),
          "#_____________________________________________________________________________",
          "#"  ,    
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
          "#******************************************************************************",
          add),
        rFileConn)

    ## Close connection
    close(rFileConn)

    ## Permissions
    Sys.chmod(r.file, mode = file.permission, use_umask = FALSE)

    ## Message
    #message(paste0(r.file, " was successfully created"))
  }

}
