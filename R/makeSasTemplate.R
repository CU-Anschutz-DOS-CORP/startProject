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
#' @param stats.collab NULL or a character string providing additional QHS
#'     collaborators to be included as authors in the template header.
#' @param sas.purpose NULL or a character string providing the purpose for the
#'     generated template to be included in the template header.
#' @param sas.notes NULL or a character string providing additional notes to be
#'     included in the template header.
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
                            sas.purpose = NULL, sas.notes = NULL)
  {

  ## sas.dir must be provided and exist
  if (is.null(sas.dir) | isTRUE(trimws(sas.dir) == "")) {
    stop("Parameter 'sas.dir' must be specified")
  }
  if (!(dir.exists(sas.dir))) {
    stop("Path specified under 'sas.dir' does not exist")
  }

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
  sas.file <- paste0(sas.dir, "/", sas.name, ".sas")
  homepath <- Sys.getenv("HOME")

  ## Define folder and file permissions
  file.permission <- "2770"

  ##If file does not exist, define connection and create template file
  if (file.exists(sas.file)) {
    warning(paste0(sas.file, " already exists and will not be created"))
  }

  else if (!(file.exists(sas.file))) {

    ## Bring in .basefile.sas if it exists
    add <- NULL
    if (file.exists(paste0(homepath, "/.basefile.sas"))) {
      add <- readLines(paste0(homepath, "/.basefile.sas"))
    } else if (file.exists(system.file("templates/basefile.sas", package = "startProject"))) {
      add <- readLines(system.file("templates/basefile.sas", package = "startProject"))
    }

    ## Open connection to new file
    sasFileConn <- file(sas.file)

    ## Write lines to file
    writeLines(
        c("/******************************************************************************",
          paste0("PROJECT: ", proj.name),
          paste0("START DATE: ", start.date),
          paste0("VERSION: ", version),
          paste0("PROGRAM: ", sas.file),
          "______________________________________________________________________________",
          " ",
          paste0("PRIMARY STATISTICIAN: ", main.statistician),
          paste0("STATS COLLABORATORS: ", stats.collab),
          "______________________________________________________________________________",
          " "  ,    
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
          "******************************************************************************/",
          add),
        sasFileConn)

    ## Close connection
    close(sasFileConn)

    ## Permissions
    Sys.chmod(sas.file, mode = file.permission, use_umask = FALSE)

    ## Message
    #message(paste0(sas.file, " was successfully created"))
  }

}
