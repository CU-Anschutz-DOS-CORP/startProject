#' Create a .Rmd template
#'
#' @description
#'     `makRmdTemplate` will generate a .Rmd file with a header containing
#'    any project information provided in the function call.
#'
#' @param rmd.dir A character string specifying the file path where the template
#'     will be stored. Defaults to {getwd()}.
#' @param rmd.name NULL or a character string specifying the name of the .Rmd
#'     file. If NULL, the file will be named p[proj.name]_rmd[currentdate]_v[version]
#'     or rmd[currentdate].
#' @param rmd.output A character string specifying the type of Rmarkdown output
#'     to be created. Choices are: word, html or pdf. Defaults to word.
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
#' @param rmd.purpose NULL or a character string providing the purpose for the
#'     generated template to be included in the template header.
#' @param rmd.notes NULL or a character string providing additional notes to be
#'     included in the template header.
#'
#' @details
#' You can append code and comments under the generated header by either
#'    creating a .basefile.Rmd file in your /home/<USERNAME> directory or by modifying
#'     the templates/basefile.Rmd file contained in the package library.
#'     
#'     To find the location of your HOME directory, run
#'     `Sys.getenv("HOME")`.
#'
#'     To find the location of the included template file, run
#'     `system.file("templates", package = "startProject")`.
#'     
#' @examples
#' \dontrun{makeRmdTemplate(rmd.name = "rmdTemplate")}
#'
#' @author Rocio Lopez, \email{Rocio.LopezMoscoso@cuanschutz.edu}
#'
#' @export
makeRmdTemplate <- function(rmd.dir = getwd(), rmd.name = NULL, rmd.output = "word",
                            proj.name = NULL, start.date = format(Sys.Date(), "%B %d, %Y"),
                            version = "1", client = NULL, client.dept = NULL,
                            main.statistician = NULL, stats.collab = NULL,
                            rmd.purpose = NULL, rmd.notes = NULL)
  {

  ## rmd.dir must be provided and exist
  if (is.null(rmd.dir) | isTRUE(trimws(rmd.dir) == "")) {
    stop("Parameter 'rmd.dir' must be specified")
  }
  if (!(dir.exists(rmd.dir))) {
    stop("Path specified under 'rmd.dir' does not exist")
  }

  ## Define variables from inputs
  if (is.null(start.date) | isTRUE(trimws(start.date) == "")) {
    start.date <- format(Sys.Date(), "%B %d, %Y")
  }
  if (!(exists("date.stamp"))) {
    date.stamp <- format(Sys.Date(), "%Y%m%d")
  }
  if (is.null(rmd.name) | isTRUE(trimws(rmd.name) == "")) {
    if (is.null(version) | isTRUE(trimws(version)) == "") {
      if (!(is.null(proj.name)) | isFALSE(trimws(proj.name) == ""))
        rmd.name <- paste0("p", proj.name, "_rmd", date.stamp)
      else
        rmd.name <- paste0("rmd", date.stamp)
    }
    else {
      if (!(is.null(proj.name)) | isFALSE(trimws(proj.name) == ""))
        rmd.name <- paste0("p", proj.name, "_rmd", date.stamp, "_v", version)
      else
        rmd.name <- paste0("rmd", date.stamp, "_v", version)
    }
  }
  if ((!is.null(main.statistician) | isFALSE(trimws(main.statistician) == "")) &
      (!is.null(stats.collab) | isFALSE(trimws(stats.collab) == ""))) {
    all.authors <- paste0(main.statistician, ", ", stats.collab)
  }
  else if ((!is.null(main.statistician) | isFALSE(trimws(main.statistician) == "")) &
      (is.null(stats.collab) | isTRUE(trimws(stats.collab) == ""))) {
    all.authors <- main.statistician
  }
  else if ((!is.null(main.statistician) | isFALSE(trimws(main.statistician) == "")) &
      (is.null(stats.collab) | isTRUE(trimws(stats.collab) == ""))) {
    all.authors <- stats.collab
  }   else all.authors <- ""
  rmd.file <- paste0(rmd.dir, "/", rmd.name, ".Rmd")
  homepath <- Sys.getenv("HOME")

  ## rmd.output must be word, html or pdf in lower case
  rmd.output <- tolower(rmd.output)
  if(!(rmd.output %in% c("word", "html", "pdf"))) {
    warning("rmd.output must be word, html or pdf. Default value of word will be used.")
    rmd.output <- "word"
  }

  ## Define folder and file permissions
  file.permission <- "2770"

  ## If file does not exist, define connection and create template file
  if (file.exists(rmd.file)) {
    warning(paste0(rmd.file, " already exists and will not be created"))
  }
  else if (!(file.exists(rmd.file))) {

    ## Bring in .basefile.R if it exists
    add <- NULL
    add <- NULL
    if (file.exists(paste0(homepath, "/.basefile.Rmd"))) {
      add <- readLines(paste0(homepath, "/.basefile.Rmd"))
    } else if (file.exists(paste0(homepath, "/.basefile.rmd"))) {
      add <- readLines(paste0(homepath, "/.basefile.rmd"))
    } else if (file.exists(system.file("templates/basefile.Rmd", package = "startProject"))) {
      add <- readLines(system.file("templates/basefile.Rmd", package = "startProject"))
    }

    ## Open connection to new file
    rmdFileConn <- file(rmd.file)

    ## Write lines to file
    writeLines(
      c("---",
        paste0('title: "', proj.name, '"'),
        paste0('author: "', all.authors, '"'),
        'date: "`r format(Sys.time(), "%m/%d/%Y")"',
        paste0('output: ', rmd.output, '_document'),
        "---",
        "",
        "```{r projInfo, eval = FALSE, include=FALSE}",
        "#******************************************************************************",
        paste0("#PROJECT: ", proj.name),
        paste0("#START DATE: ", start.date),
        paste0("#VERSION: ", version),
        paste0("#PROGRAM: ", rmd.file),
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
        paste0("#PURPOSE:", rmd.purpose),
        paste0("#NOTES: ", rmd.notes),
        "#_____________________________________________________________________________",
        "#",
        "#DEPENDENCIES:",
        "#_____________________________________________________________________________",
        "#",
        "#CONTENTS:",
        "#******************************************************************************",
        "```",
        add),
      rmdFileConn)

    ## Close connection
    close(rmdFileConn)

    ## Permissions
    Sys.chmod(rmd.file, mode = file.permission, use_umask = FALSE)

    ## Message
    #message(paste0(rmd.file, " was successfully created"))
  }

}
