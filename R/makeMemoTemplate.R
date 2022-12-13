#' Create a .docx template
#'
#' @description
#'     `makeMemoTemplate()` will generate a .docx file to be used for project
#'     memos. This document will have a header and any project information provided
#'     in the function call.
#'
#' @param memo.dir A character string specifying the file path where the
#'     template will be stored. Defaults to {getwd()}.
#' @param memo.name NULL or a character string specifying the name of the .Rmd
#'     file. If NULL, the file will be named memo[currentdate]_v[version] or
#'     memo[currentdate].
#' @param start.date NULL or a character string providing the date to be included
#'     in the template header. Defaults to current date.
#' @param version NULL or a character string providing the project version to be
#'     included in the template name and/or header. Defaults to "1".
#' @param client NULL or a character string providing the name(s) of client(s)
#'     to be included in the template header.
#' @param client.dept NULL or a character string providing the affiliation of
#'     the client(s) to be included in the template header.
#' @param main.statistician NULL or a character string providing the name of the
#'     primary statistician. This will be used in the template header.
#' @param stats.collab NULL or a character string providing additional QHS
#'     collaborators to be included as authors in the template header.
#' @param memo.re NULL or a character string providing the subject line for the
#'     memo template.
#'
#' @details
#' You can modify the memo template by either creating a .basefile.docx file
#'    under your HOME directory or by modifying the templates/memoTemplate
#'    contained in the package library. Note that the text use as placeholders for
#'    passed information (DATESEC, TOSEC, etc.) should NOT be changed.
#'
#'     To find the location of your HOME directory, run
#'     `Sys.getenv("HOME")`.
#'
#'     To find the location of the included template file, run
#'     `system.file("templates", package = "startProject")`.
#'     
#' @examples
#' \dontrun{makeMemoTemplate(memo.name = "memoTemplate")}
#'
#' @author Rocio Lopez, \email{Rocio.LopezMoscoso@cuanschutz.edu}
#'
#' @export
makeMemoTemplate <- function(memo.dir = getwd(), memo.name = NULL,
                             start.date = format(Sys.Date(), "%B %d, %Y"),
                             version = "1", client = NULL, client.dept = NULL,
                             main.statistician = NULL, stats.collab = NULL, 
                             memo.re = NULL)
  {

  ## memo.dir must be provided and exist
  if (is.null(memo.dir) | isTRUE(trimws(memo.dir) == "")) {
    stop("Parameter 'memo.dir' must be specified")
  }
  if (!(dir.exists(memo.dir))) {
    stop("Path specified under 'memo.dir' does not exist")
  }

  ## Define variables from inputs - within function
  if (is.null(start.date) | isTRUE(trimws(start.date) == "")) {
    start.date <- format(Sys.Date(), "%B %d, %Y")
  }
  if (!(exists("date.stamp"))) {
      date.stamp <- format(Sys.Date(), "%Y%m%d")
  }
  if (is.null(memo.name) | isTRUE(trimws(memo.name) == "")) {
    if (is.null(version) | isTRUE(trimws(version)) == "") {
      memo.name <- paste0("memo", date.stamp)
    }
    else {
      memo.name <- paste0("memo", date.stamp, "_v", version)
    }
  }
  memo.file <- paste0(memo.dir, "/", memo.name, ".docx")
  if (is.null(client)) {
    client <- ""
  }
  if (is.null(client.dept)) {
    client.dept <- ""
    }
  if (is.null(memo.re)) {
    memo.re <- ""
  }
  if ((is.null(main.statistician) & is.null(stats.collab)) |
      (isTRUE(trimws(main.statistician) == "") & isTRUE(trimws(stats.collab) == ""))) {
    from <- ""
    main.statistician <- ""
  } # NULL if running function and "" if running App
  else if (!(is.null(main.statistician)) & is.null(stats.collab) |
           (isFALSE(trimws(main.statistician) == "") & isTRUE(trimws(stats.collab) == ""))) {
    from <- main.statistician
  }
  else if (is.null(main.statistician) & !(is.null(stats.collab)) |
           (isTRUE(trimws(main.statistician) == "") & isFALSE(trimws(stats.collab) == ""))) {
    from <- stats.collab
    main.statistician <- ""
  }
  else if (!(is.null(main.statistician)) & !(is.null(stats.collab)) |
           (isFALSE(trimws(main.statistician) == "") & isFALSE(trimws(stats.collab) == ""))) {
    from <- paste0(main.statistician, ", ", stats.collab)
  }
  homepath <- Sys.getenv("HOME")

  ## if .staffInfo file exists in home directory get name and phone number of author
  if (file.exists(paste0(homepath, "/.staffInfo"))) {
    siFileConn <- paste0(homepath, "/.staffInfo")
    staffInfo <- readLines(siFileConn)
    author <- staffInfo[1]
    if (length(grep("[(]?\\d{3}[ )-]?[ ]?\\d{3}[ -]?\\d{4}", staffInfo)) > 0) {
      phone <- gsub("[^0-9.-]", "", staffInfo[grep("[(]?\\d{3}[ )-]?[ ]?\\d{3}[ -]?\\d{4}", staffInfo)])
    } else phone <- ""
    if (length(grep("@", staffInfo)) > 0) {
        author.email <- regmatches(staffInfo, regexpr("[[:graph:]]+\\@[[:alpha:]]+\\.[[:alpha:]]{3}", staffInfo))
    } else author.email <- ""
  }
  else {
    author <- ""
    phone <- ""
  }

  ## Define folder and file permissions
  file.permission <- "2770"

  ##If file does not exist, define connection and create template file
  if (file.exists(memo.file)) {
    warning(paste0(memo.file, " already exists and will not be created"))
  }
  if (file.exists(paste0(homepath, "/", ".basefile.docx"))) {
    memo.template <- paste0(homepath, "/", ".basefile.docx")
  } else if (file.exists(system.file("templates/memoTemplate.docx", package = "startProject"))) {
    memo.template <- system.file("templates/memoTemplate.docx", package = "startProject")
  } else if ( !(file.exists(paste0(homepath, "/", ".basefile.docx"))) &
              !(file.exists(system.file("templates/memoTemplate.docx", package = "startProject")))) {
    warning(paste0("Memo template document was not found at ",
                   paste0(homepath, "/", ".basefile.docx"), "or ",
                   system.file("templates/memoTemplate.docx", package = "startProject"),
                   ". No memo will be created."))
  }
  if (!(file.exists(memo.file)) &
      (file.exists(paste0(homepath, "/", ".basefile.docx")) |
       file.exists(system.file("templates/memoTemplate.docx", package = "startProject")))) {

    ## Bring in memo template
    memo.doc <- officer::read_docx(memo.template)

    ## Insert user input
    memo.doc <- officer::body_replace_all_text(memo.doc, "DATESEC", start.date, warn = FALSE)
    memo.doc <- officer::body_replace_all_text(memo.doc, "TOSEC", client, warn = FALSE)
    memo.doc <- officer::body_replace_all_text(memo.doc, "DEPTSEC", client.dept, warn = FALSE)
    memo.doc <- officer::body_replace_all_text(memo.doc, "FROMSEC", from, warn = FALSE)
    memo.doc <- officer::body_replace_all_text(memo.doc, "RESEC", memo.re, warn = FALSE)
    memo.doc <- officer:: footers_replace_all_text(memo.doc, "LOCATIONSEC", memo.file, warn = FALSE)
    memo.doc <- officer:: footers_replace_all_text(memo.doc, "CONTACTSEC1", author, warn = FALSE)
    memo.doc <- officer:: footers_replace_all_text(memo.doc, "CONTACTSEC2", paste0("Email: ", author.email), warn = FALSE)
    memo.doc <- officer:: footers_replace_all_text(memo.doc, "CONTACTSEC3", paste0("Phone: ", phone), warn = FALSE)

    ## Save modified template
    print(memo.doc, target = paste0(memo.file))

    ## Permissions
    Sys.chmod(memo.file, mode = file.permission, use_umask = FALSE)

    ## Message
    #message(paste0(memo.file, " was successfully created"))
  }

}
