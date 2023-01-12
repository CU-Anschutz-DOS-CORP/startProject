#' Create a project directory with sub-folders and templates
#'
#' @description
#'     `startProject()` will create a project directory along with the specified
#'    sub-folders and optional .docx, .R, .Rmd and .sas templates containing
#'    headers with project information.
#'
#' @param main.dir A character string specifying the file path where the project
#'     directory will be created. Defaults to {getwd()}.
#' @param proj.name NULL or a character string providing the name of the project
#'     to be included in the templates.
#' @param proj.num NULL or a character string providing a project number in the
#'     directory name and templates. If NULL, [proj.name] will be used instead.
#' @param start.date NULL or a character string providing the date to be included
#'     in templates. Defaults to today's date.
#' @param version NULL or a character string providing the project version to be
#'     included in the templates. Defaults to "1".
#' @param client NULL or a character string providing the name(s) of client(s) to
#'     be included in the templates.
#' @param client.dept NULL or a character string providing the affiliation of the
#'     client(s) to be included in the templates.
#' @param main.statistician NULL or a character string providing the name of the
#'     primary statistician. This will be used in the templates.
#' @param stats.collab NULL or a character string providing additional 
#'     collaborators to be included as authors in the templates.
#' @param subfolders A character string providing a comma delimited list of
#'     sub-folders to be created. Defaults to "communications, data, graphs,
#'     memo, orig_data, others, r, sas, temp".
#' @param templates A character string providing a comma delimited list of
#'     templates to be generated. Valid options are memo, R, Rmd and sas.
#'     Defaults to "memo, R, Rmd, sas".
#' @param memo.name NULL or a character string specifying the name of the .Rmd
#'     file. If NULL, the file will be named p[proj.num]_memo[currentdate]_v[version]
#'     or p[proj.num]_memo[currentdate].
#' @param memo.re NULL or a character string providing the subject line for the
#'     memo template.
#' @param r.name NULL or a character string specifying the name of the .R template.
#'     If NULL, the file will be named p[proj.num]_r[currentdate]_v[version] or
#'     p[proj.num]_r[currentdate].
#' @param r.purpose NULL or a character string providing the purpose for the R
#'     file to be included in the R template header.
#' @param r.notes NULL or a character string providing additional notes to be
#'     included in the R template header.
#' @param rmd.name NULL or a character string specifying the name of the .Rmd
#'     template. If NULL, the file will be named p[proj.num]_rmd[currentdate]_v[version]
#'     or p[proj.num]_rmd_[currentdate]
#' @param rmd.purpose NULL or a character string providing the purpose for the
#'     Rmd file to be included in the Rmd template header.
#' @param rmd.notes NULL or a character string providing additional notes to be
#'     included in the Rmd template header.
#' @param sas.name NULL or a character string specifying the name of the .sas
#'     file. If NULL, the file will be named p[proj.num]_sas[currentdate]_v[version]
#'     or p[proj.num]_sas[currentdate].
#' @param sas.purpose NULL or a character string providing the purpose for SAS
#'     file to be included in the template header.
#' @param sas.notes NULL or a character string providing additional notes to be
#'     included in the SAS template header.
#'
#' @details
#'     If the main directory, sub-folders and/or templates exist, the function
#'     will generate a warning message but continue and will create any components
#'     that do not exist.
#'
#'     Templates other than memo, sas, r and Rmd (case ignored) are not supported.
#'     If any templates are requested, the function will search for memo, rcode 
#'     or r, sascode or sas sub-folders to store the templates; if these are not
#'     found, a templates sub-folder will be created.
#'
#'     You can modify the memo template by either creating a .basefile.docx
#'     file under your HOME directory or by modifying the templates/memoTemplate
#'     contained in the package library. Note that the text use as placeholders
#'     for passed information (DATESEC, TOSEC, etc.) should NOT be changed.
#'
#'     You can also append code and comments under the generated header of any of
#'     the code files (.R, .Rmd, .sas) by either creating a .basefile.R, .basefile.Rmd
#'     and/or .basefile.sas file(s) in your HOME directory or by modifying the
#'     templates/basefile.xxx file contained in the package library.
#'
#'     To find the location of your HOME directory, run
#'     `Sys.getenv("HOME")`.
#'
#'     To find the location of the included template files, run
#'     `system.file("templates", package = "startProject")`.
#'
#' @seealso
#' \code{\link{runApp_startProject}} for how to launch the function as a local 
#'     Shiny app. Functions used to generate templates: 
#'     \code{\link{makeMemoTemplate}}, \code{\link{makeRmdTemplate}}, 
#'     \code{\link{makeRTemplate}}, and \code{\link{makeSasTemplate}}.
#'
#' @examples
#' \dontrun{startProject(proj.name = "Example Project", client = "John Smith",
#'     client.dept = "Test Institute", main.statistician = "My Name Here")}
#'
#' @author Rocio Lopez, \email{Rocio.LopezMoscoso@cuanschutz.edu}
#'
#' @export
startProject <- function(main.dir = getwd(), proj.name = "project name", proj.num = NULL,
                         start.date = format(Sys.Date(), "%B %d, %Y"), version = "1", 
                         client = NULL, client.dept = NULL, 
                         main.statistician = NULL, stats.collab = NULL, 
                         subfolders = "communications, data, graphs, memo, orig_data, others, r, sas, temp",
                         templates = "memo, R, Rmd, sas",
                         memo.name = NULL, memo.re = NULL,
                         r.name = NULL, r.purpose = NULL, r.notes = NULL,
                         rmd.name = NULL, rmd.purpose = NULL, rmd.notes = NULL,
                         sas.name = NULL, sas.purpose = NULL, sas.notes = NULL)
  {

  ## At a minimum user must provide main.dir and proj.name
  if (is.null(main.dir) | isTRUE(trimws(main.dir) == "")) {
    stop("Parameter 'main.dir' must be specified")
  }
  if (is.null(proj.name) | isTRUE(trimws(proj.name) == "")) {
    stop("Parameter 'proj.name' must be specified")
  }

  ## Define variables from inputs
  if (length(grep(":\\\\", main.dir)) > 0) {
      main.dir <- gsub("\\\\", "/", main.dir)
  }
  if (is.null(proj.num) | isTRUE(trimws(proj.num) == "")) {
    proj.num <- proj.name
  }
  if (is.null(start.date) | isTRUE(trimws(start.date) == "")) {
    start.date <- format(Sys.Date(), "%B %d, %Y")
  }
  if (!(exists("date.stamp"))) {
    date.stamp <- format(Sys.Date(), "%Y%m%d")
  }
  if(is.null(memo.name) | isTRUE(trimws(memo.name) == "")) {
    if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
      memo.name <- paste0("p", proj.num, "_memo", date.stamp, "_v", version)
    }
    else {
      memo.name <- paste0("p", proj.num, "_memo", date.stamp)
    }
  }
  if(is.null(r.name) | isTRUE(trimws(r.name) == "")) {
    if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
      r.name <- paste0("p", proj.num, "_r", date.stamp, "_v", version)
    }
    else {
      r.name <- paste0("p", proj.num, "_r", date.stamp)
    }
  }
  if(is.null(rmd.name) | isTRUE(trimws(rmd.name) == "")) {
    if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
      rmd.name <- paste0("p", proj.num, "_rmd", date.stamp, "_v", version)
    }
    else {
      rmd.name <- paste0("p", proj.num, "_rmd", date.stamp)
    }
  }
  if(is.null(sas.name)  | isTRUE(trimws(sas.name) == "")) {
    if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
      sas.name <- paste0("p", proj.num, "_sas", date.stamp, "_v", version)
    }
    else {
      sas.name <- paste0("p", proj.num, "_sas", date.stamp)
    }
  }

  ## If main_dir does not exist, stop
  if (!(dir.exists(main.dir))) {
    stop("Path specified under 'main,dir' does not exist")
  }

  ## Define folder and file permissions
  dir.permission <- "755"
  mail.permission <- "644"

  ## Create project directory
  dir.create(paste0(main.dir, "/", proj.name))
  Sys.chmod(paste0(main.dir, "/", proj.name), mode = dir.permission, use_umask = FALSE)

  ## Loop through subfolders list and create each
  proj.dir <- paste0(main.dir, "/", proj.name)
  folders <- gsub(" ", "", unlist(strsplit(subfolders, ",")))

  for (i in 1:length(folders)) {
    dir.create(paste0(proj.dir, "/", folders[i]))
    Sys.chmod(paste0(proj.dir, "/", folders[i]), mode = dir.permission, use_umask = FALSE)
  }

  ## Create _include folders for r and sas
  if (dir.exists(paste0(proj.dir, "/rcode"))) {
    dir.create(paste0(proj.dir, "/", "rcode/_include"))
    Sys.chmod(paste0(proj.dir, "/", "rcode/_include"), mode = dir.permission, use_umask = FALSE)
  }
  if (dir.exists(paste0(proj.dir, "/r"))) {
    dir.create(paste0(proj.dir, "/", "r/_include"))
    Sys.chmod(paste0(proj.dir, "/", "r/_include"), mode = dir.permission, use_umask = FALSE)
  }
  if (dir.exists(paste0(proj.dir, "/sascode"))) {
    dir.create(paste0(proj.dir, "/", "sascode/_include"))
    Sys.chmod(paste0(proj.dir, "/", "sascode/_include"), mode = dir.permission, use_umask = FALSE)
  }
  if (dir.exists(paste0(proj.dir, "/sas"))) {
    dir.create(paste0(proj.dir, "/", "sas/_include"))
    Sys.chmod(paste0(proj.dir, "/", "sas/_include"), mode = dir.permission, use_umask = FALSE)
  }

  ## If templates requested but expected folders do not exist, create a templates folder for storage
  if (length(templates) > 0) {
    if((length(grep('memo', tolower(templates), fixed = TRUE)) > 0 &
        length(grep('memo', tolower(subfolders), fixed = TRUE)) <= 0) |
       (length(grep('r', tolower(templates), fixed = TRUE)) > 0 &
        length(grep('r', tolower(subfolders), fixed = TRUE)) <= 0 &
        length(grep('rcode', tolower(subfolders), fixed = TRUE)) <= 0) |
       (length(grep('rmd', tolower(templates), fixed = TRUE)) > 0 &
        length(grep('r', tolower(subfolders), fixed = TRUE)) <= 0 &
        length(grep('rcode', tolower(subfolders), fixed = TRUE)) <= 0) |
       (length(grep('sas', tolower(templates), fixed = TRUE)) > 0 &
        length(grep('sas', tolower(subfolders), fixed = TRUE)) <= 0 &
        length(grep('sascode', tolower(subfolders), fixed = TRUE)) <= 0)) {
    dir.create(paste0(proj.dir, "/templates"))
      Sys.chmod(paste0(proj.dir, "/templates"), mode = dir.permission, use_umask = FALSE)
    }
  }

  ## Create memo template, if requested
  # if ("memo" %in% tolower(templates)) {
  if ((length(grep('memo', tolower(templates), fixed = TRUE)) > 0)) {
    if (dir.exists(paste0(proj.dir, "/memo"))) {
      memo.dir <- paste0(proj.dir, "/memo")
    } else {
      memo.dir <- paste0(proj.dir, "/templates")
    }
    makeMemoTemplate(memo.dir = memo.dir, memo.name = memo.name,
                     start.date = start.date, client = client,
                     client.dept = client.dept, main.statistician = main.statistician,
                     stats.collab = stats.collab, memo.re = memo.re)
  }

  ## Create R and Rmd template, if requested
  if ((length(grep('r', tolower(templates), fixed = TRUE)) > 0) |
      (length(grep('rmd', tolower(templates), fixed = TRUE)) > 0)) {
    if (dir.exists(paste0(proj.dir, "/rcode"))) {
      r.dir <- paste0(proj.dir, "/rcode")
    } else if (dir.exists(paste0(proj.dir, "/r"))) {
      r.dir <- paste0(proj.dir, "/r")
    } else {
      r.dir <- paste0(proj.dir, "/templates")
    }
    if (length(grep('r', tolower(templates), fixed = TRUE)) > 0) {
      makeRTemplate(r.dir = r.dir, r.name = r.name, proj.name = proj.num,
                    start.date = start.date, version = version,
                    client = client, client.dept = client.dept,
                    main.statistician = main.statistician, stats.collab = stats.collab,
                    r.purpose = r.purpose, r.notes = r.notes)
    }
    if (length(grep('rmd', tolower(templates), fixed = TRUE)) > 0) {
      makeRmdTemplate(rmd.dir = r.dir, rmd.name = rmd.name,
                      proj.name = proj.num, start.date = start.date,
                      version = version, client = client,
                      client.dept = client.dept, main.statistician = main.statistician,
                      stats.collab = stats.collab,
                      rmd.purpose = rmd.purpose, rmd.notes = rmd.notes)
    }
  }

  ## Create SAS template, if requested
  if ((length(grep('sas', tolower(templates), fixed = TRUE)) > 0)) {
    if (dir.exists(paste0(proj.dir, "/sascode"))) {
      sas.dir <- paste0(proj.dir, "/sascode")
    } else if (dir.exists(paste0(proj.dir, "/sas"))) {
      sas.dir <- paste0(proj.dir, "/sas")
    } else {
      sas.dir <- paste0(proj.dir, "/templates")
    }
    makeSasTemplate(sas.dir = sas.dir, sas.name = sas.name,
                    proj.name = proj.num, start.date = start.date,
                    version = version, client = client,
                    client.dept = client.dept, main.statistician = main.statistician,
                    stats.collab = stats.collab, sas.purpose = sas.purpose,
                    sas.notes = sas.notes)
  }

}
