#' Create a project directory with sub-folders and templates
#'
#' @description
#'     `startProject()` will create a project directory along with the specified
#'    sub-folders and optional .docx, .R, .Rmd and .sas templates containing
#'    headers with project information.
#'
#' @param main.dir A character string specifying the file path where the project
#'     directory will be created. Defaults to {getwd()}.
#' @param proj.title A character string providing the full, descriptive project title 
#'     to be used in template headers and memo subject lines.
#' @param proj.id NULL or a short, clean character string providing a project identifier 
#'     to be used as the directory name and as a prefix for template filenames. 
#'     If NULL, a clean identifier will be automatically derived from [proj.title].
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
#'     sub-folders to be created. If NULL, defaults to "00_protocol_and_irb,
#'     01_data_specs, 02_docs, 03_include, 04_manuscript, 05_presentations,
#'     06_external_resources" if structure="snapshot" and "communications, 
#'     data, graphs, memo, orig_data, others, r, sas, temp" if structure="legacy".
#' @param templates A character string providing a comma delimited list of
#'     templates to be generated. Valid options are memo, readme, R, Rmd and sas.
#'     Defaults to "memo, readme, R, Rmd, sas".
#' @param structure A character string specifying the project layout to create.
#'     Supported values are "legacy" and "snapshot". Defaults to "legacy".
#' @param memo.name NULL or a character string specifying the name of the .Rmd
#'     file. If NULL, the file will be named [proj.id]_memo_[currentdate]_v[version]
#'     or [proj.id]_memo_[currentdate].
#' @param memo.re NULL or a character string providing the subject line for the
#'     memo template. Defults to [proj.title].
#' @param r.name NULL or a character string specifying the name of the .R template.
#'     If NULL, the file will be named [proj.id]_r_[currentdate]_v[version] or
#'     [proj.id]_r_[currentdate].
#' @param r.purpose NULL or a character string providing the purpose for the R
#'     file to be included in the R template header.
#' @param r.notes NULL or a character string providing additional notes to be
#'     included in the R template header.
#' @param rmd.name NULL or a character string specifying the name of the .Rmd
#'     template. If NULL, the file will be named [proj.id]_rmd_[currentdate]_v[version]
#'     or [proj.id]_rmd_[currentdate].
#' @param rmd.purpose NULL or a character string providing the purpose for the
#'     Rmd file to be included in the Rmd template header.
#' @param rmd.notes NULL or a character string providing additional notes to be
#'     included in the Rmd template header.
#' @param sas.name NULL or a character string specifying the name of the .sas
#'     file. If NULL, the file will be named [proj.id]_sas_[currentdate]_v[version]
#'     or [proj.id]_sas_[currentdate].
#' @param sas.purpose NULL or a character string providing the purpose for SAS
#'     file to be included in the template header.
#' @param sas.notes NULL or a character string providing additional notes to be
#'     included in the SAS template header.
#' @param snapshot.name NULL or a character string providing the analysis snapshot 
#'     folder name. If NULL, an analysis_YYYYMMDD folder will be created.
#'     Ignored if structure = "legacy".     
#' @param analysis.subfolders A character vector providing the subfolders to
#'     create inside each analysis snapshot directory. Defaults to the standard
#'     Snapshot layout: orig_data, code, data, output_raw, graphs,
#'     final_styled, and memo.
#'     Ignored if structure = "legacy".   
#' @param sas.template.layout A character string specifying the SAS template layout.
#'     Supported values are "single" and "multi". 
#'     Choose Single file to generate a unified, all-in-one program template ideal 
#'     for straightforward analyses. Choose Multi-file to generate structured, 
#'     separate scripts for setup, data management, and analysis, providing a 
#'     modular framework that is easier to customize and scale with specialized additions.
#'     Defaults to "single".
#' @param r.template.layout A character string specifying the R template layout.
#'     Supported values are "single" and "multi". Defaults to "single".
#' @param sas.header.style A character string specifying the SAS header style.
#'     Supported values are "default" and "simple". 
#'     Choose the Default header for major collaborative or client-facing analysis 
#'     programs to capture comprehensive study metadata, collaborator details, 
#'     and external dependencies. Choose the Simple header for utility, modular, 
#'     or setup scripts that require only lightweight file tracking and execution notes.
#'     Defaults to "default".
#' @param r.header.style A character string specifying the R header style.
#'     Supported values are "default" and "simple".
#'     Choose the Default header for major collaborative or client-facing analysis 
#'     programs to capture comprehensive study metadata, collaborator details, 
#'     and external dependencies. Choose the Simple header for utility, modular, 
#'     or setup scripts that require only lightweight file tracking and execution notes.
#'     Defaults to "default".
#'     
#' @details
#'     If the main directory, sub-folders and/or templates exist, the function
#'     will generate a warning message but continue and will create any components
#'     that do not exist.
#'
#'     Templates other than memo, sas, r and Rmd (case ignored) are not supported.
#'     If any templates are requested, the function will search for memo, rcode 
#'     or r, sascode, sas or code (if snapshot) sub-folders to store the templates; 
#'     if these are not found, a templates sub-folder may be created.
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
#' \dontrun{startProject(proj.title = "Example Project", client = "John Smith",
#'     client.dept = "Test Institute", main.statistician = "My Name Here")}
#'
#' @author Rocio Lopez, \email{Rocio.LopezMoscoso@cuanschutz.edu}
#'
#' @export
startProject <- function(main.dir = getwd(), proj.title = "Project Title", proj.id = NULL,
                         start.date = format(Sys.Date(), "%B %d, %Y"), version = "1", 
                         client = NULL, client.dept = NULL, 
                         main.statistician = NULL, stats.collab = NULL, 
                         subfolders = NULL,
                         templates = "memo, R, Rmd, sas",
                         structure = c("legacy", "snapshot"),
                         snapshot.name = NULL,
                         analysis.subfolders = c("orig_data", "code", "data",
                                                 "output_raw", "graphs",
                                                 "final_styled", "memo"),
                         sas.template.layout = c("single", "multi"),
                         r.template.layout = c("single", "multi"),
                         sas.header.style = c("default", "simple"),
                         r.header.style = c("default", "simple"),
                         memo.name = NULL, memo.re = NULL,
                         r.name = NULL, r.purpose = NULL, r.notes = NULL,
                         rmd.name = NULL, rmd.purpose = NULL, rmd.notes = NULL,
                         sas.name = NULL, sas.purpose = NULL, sas.notes = NULL)
{
    
    ## At a minimum user must provide main.dir and proj.title or proj.id
    if (is.null(main.dir) | isTRUE(trimws(main.dir) == "")) {
        stop("Parameter 'main.dir' must be specified")
    }
    if ((is.null(proj.title) | isTRUE(trimws(proj.title) == "")) 
        && (is.null(proj.id) | isTRUE(trimws(proj.id) == ""))) {
        stop("You must specify at least 'proj.title' or 'proj.id' to create a project.")
    }
    
    structure <- match.arg(structure, c("legacy", "snapshot"))
    
    # Set default subfolders depending on requested structure
    if (is.null(subfolders)) {
        if (structure == "snapshot") {
            subfolders <- "00_protocol_and_irb, 01_data_specs, 02_docs, 03_include, 04_manuscript, 05_presentations, 06_external_resources"
        } else {
            subfolders <- "communications, data, graphs, memo, orig_data, others, r, sas, temp"
        }
    }
    
    sas.template.layout <- if (is.null(sas.template.layout)) { 
        "single"
    } else {
        match.arg(sas.template.layout, c("single", "multi"))
    }
    r.template.layout <- if (is.null(r.template.layout)) {
        "single"
    } else {
        match.arg(r.template.layout, c("single", "multi"))
    }
    sas.header.style <- match.arg(sas.header.style, c("default", "simple"))
    r.header.style <- match.arg(r.header.style, c("default", "simple"))
    
    ## Define variables from inputs
    if (length(grep(":\\\\", main.dir)) > 0) {
        main.dir <- gsub("\\\\", "/", main.dir)
    }
    
    # Generate proj.id if NULL or empty (Slugify logic)
    if (is.null(proj.id) | isTRUE(trimws(proj.id) == "")) {
        clean_id <- tolower(proj.title)
        clean_id <- gsub("[^a-z0-9_ ]", "", clean_id) # remove punctuation/special chars
        clean_id <- gsub("\\s+", "_", trimws(clean_id)) # spaces to underscores
        proj.id <- substr(clean_id, 1, 15) # truncate to keep it highly concise
        
        # Fallback if slugify produces empty string (e.g. only special characters passed)
        if (proj.id == "") {
            proj.id <- "project"
        }
    } else {
        # Ensure a user-provided ID is clean of trailing/leading spaces
        proj.id <- trimws(proj.id)
    }
    
    if (is.null(start.date) | isTRUE(trimws(start.date) == "")) {
        start.date <- format(Sys.Date(), "%B %d, %Y")
    }
    if (!(exists("date.stamp"))) {
        date.stamp <- format(Sys.Date(), "%Y%m%d")
    }
    
    if (is.null(memo.re) | isTRUE(trimws(memo.re) == "")) {
        memo.re <- proj.title
    }
    
    # Define file templates using the new clean proj.id prefix (no hardcoded "p" required)
    if(is.null(memo.name) | isTRUE(trimws(memo.name) == "")) {
        if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
            memo.name <- paste0(proj.id, "_memo_", date.stamp, "_v", version)
        }
        else {
            memo.name <- paste0(proj.id, "_memo_", date.stamp)
        }
    }
    if(is.null(r.name) | isTRUE(trimws(r.name) == "")) {
        if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
            r.name <- paste0(proj.id, "_r_", date.stamp, "_v", version)
        }
        else {
            r.name <- paste0(proj.id, "_r_", date.stamp)
        }
    }
    if(is.null(rmd.name) | isTRUE(trimws(rmd.name) == "")) {
        if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
            rmd.name <- paste0(proj.id, "_rmd_", date.stamp, "_v", version)
        }
        else {
            rmd.name <- paste0(proj.id, "_rmd_", date.stamp)
        }
    }
    if(is.null(sas.name)  | isTRUE(trimws(sas.name) == "")) {
        if (!(is.null(version)) | isFALSE(trimws(version) == "")) {
            sas.name <- paste0(proj.id, "_sas_", date.stamp, "_v", version)
        }
        else {
            sas.name <- paste0(proj.id, "_sas_", date.stamp)
        }
    }
    
    ## If main_dir does not exist, stop
    if (!(dir.exists(main.dir))) {
        stop("Path specified under 'main.dir' does not exist")
    }
    
    ## Define folder and file permissions
    dir.permission <- "755"
    mail.permission <- "644"
    
    ## Create project directory using proj.id instead of proj.name
    dir.create(paste0(main.dir, "/", proj.id))
    Sys.chmod(paste0(main.dir, "/", proj.id), mode = dir.permission, use_umask = FALSE)
    
    proj.dir <- paste0(main.dir, "/", proj.id)
    
    ## Create a README file if requested
    if ("readme" %in% tolower(templates)) {    
        readme_path <- file.path(proj.dir, "README.md")
        
        if (!file.exists(readme_path)) {
            # 1. Safely handle potential NULL or empty metadata strings
            client_val <- if (is.null(client) || isTRUE(trimws(client) == "")) "" else client
            stat_val <- if (is.null(main.statistician) || isTRUE(trimws(main.statistician) == "")) "" else main.statistician
            
            # 2. Construct the dynamic header and summary block
            readme_header <- c(
                "---",
                "title: \"README\"",
                "format: ",
                "  html:",
                "    embed-resources: true",
                "toc: true",
                "toc-depth: 3",
                "toc-location: left",
                "---",
                "",
                "## Project Summary",
                "",
                paste0("**Title:** ", proj.title),
                "",
                paste0("**Directory Location:** `", file.path(main.dir, proj.id), "`"),
                "",
                paste0("**Principal Investigator:** ", client_val),                                
                "",
                paste0("**Lead Analyst:** ", stat_val),
                "",
                "**Objective:** ",
                ""
            )
            
            # 3. Search for a .md template file just like the original logic
            readme_template <- NULL
            homepath <- Sys.getenv("HOME") # Ensured homepath is safely scoped here
            
            if (file.exists(paste0(homepath, "/.basefile.md"))) {
                readme_template <- paste0(homepath, "/.basefile.md")
            } else if (file.exists(system.file("templates/basefile.md", package = "startProject"))) {
                readme_template <- system.file("templates/basefile.md", package = "startProject")
            }     
            
            # 4. If template exists, read and append it; otherwise write header alone
            if (!is.null(readme_template) && file.exists(readme_template)) {
                readme_lines <- readLines(readme_template)
                final_readme <- c(readme_header, readme_lines)
            } else {
                final_readme <- readme_header
            }
            
            writeLines(final_readme, readme_path)
        }   
    }    
    
    ## Create analysis snapshot
    if (structure == "snapshot") {
        # Parse customized or default snapshot folders from subfolders argument
        snapshot_folders <- gsub(" ", "", unlist(strsplit(subfolders, ",")))
        
        for (folder in snapshot_folders) {
            folder_path <- file.path(proj.dir, folder)
            if (!dir.exists(folder_path)) {
                dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
            }
        }
        
        # Passes proj.title and proj.id downstream to makeSnapshot
        makeSnapshot(project.dir = main.dir, proj.title = proj.title, proj.id = proj.id,
                     start.date = start.date, version = version,
                     client = client, client.dept = client.dept,
                     main.statistician = main.statistician, stats.collab = stats.collab,
                     templates = templates, memo.name = memo.name, memo.re = memo.re,
                     snapshot.name = snapshot.name,
                     analysis.subfolders = analysis.subfolders,
                     r.name = r.name, r.purpose = r.purpose, r.notes = r.notes,
                     rmd.name = rmd.name, rmd.purpose = rmd.purpose, rmd.notes = rmd.notes,
                     sas.name = sas.name, sas.purpose = sas.purpose, sas.notes = sas.notes,
                     sas.template.layout = sas.template.layout,
                     r.template.layout = r.template.layout,
                     sas.header.style = sas.header.style,
                     r.header.style = r.header.style)
        return(invisible(NULL))
    }
    
    ## Loop through subfolders list and create each
    folders <- gsub(" ", "", unlist(strsplit(subfolders, ",")))
    
    for (i in 1:length(folders)) {
        dir.create(paste0(proj.dir, "/", folders[i]))
        Sys.chmod(paste0(proj.dir, "/", folders[i]), mode = dir.permission, use_umask = FALSE)
    }
    
    if (structure == "legacy") {
        
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
            clean_templates <- tolower(templates)
            clean_folders <- tolower(folders)
            
            if (("memo" %in% clean_templates && !("memo" %in% clean_folders)) ||
                ("r" %in% clean_templates    && !("r" %in% clean_folders) && !("rcode" %in% clean_folders)) ||
                ("rmd" %in% clean_templates  && !("r" %in% clean_folders) && !("rcode" %in% clean_folders)) ||
                ("sas" %in% clean_templates  && !("sas" %in% clean_folders) && !("sascode" %in% clean_folders))) {
                
                dir.create(paste0(proj.dir, "/templates"))
                Sys.chmod(paste0(proj.dir, "/templates"), mode = dir.permission, use_umask = FALSE)
            }
        }        
        
        ## Create memo template, if requested
        if ("memo" %in% tolower(templates)) {
            if (dir.exists(paste0(proj.dir, "/memo"))) {
                memo.dir <- paste0(proj.dir, "/memo")
            } else {
                memo.dir <- paste0(proj.dir, "/templates")
            }
            # Passes both new metadata variables to memo helper
            makeMemoTemplate(memo.dir = memo.dir, memo.name = memo.name,
                             start.date = start.date, client = client,
                             client.dept = client.dept, main.statistician = main.statistician,
                             stats.collab = stats.collab, memo.re = memo.re)
        }
        
        ## Create R and Rmd template, if requested
        if ("r" %in% tolower(templates) |
            "rmd" %in% tolower(templates)) {
            if (dir.exists(paste0(proj.dir, "/rcode"))) {
                r.dir <- paste0(proj.dir, "/rcode")
            } else if (dir.exists(paste0(proj.dir, "/r"))) {
                r.dir <- paste0(proj.dir, "/r")
            } else {
                r.dir <- paste0(proj.dir, "/templates")
            }
            if ("r" %in% tolower(templates)) {
                # Passes proj.title and proj.id to makeRTemplate
                makeRTemplate(r.dir = r.dir, r.name = r.name, 
                              proj.title = proj.title, proj.id = proj.id,
                              start.date = start.date, version = version,
                              client = client, client.dept = client.dept,
                              main.statistician = main.statistician, stats.collab = stats.collab,
                              r.purpose = r.purpose, r.notes = r.notes,
                              r.template.layout = r.template.layout, r.header.style = r.header.style)
            }
            if ("rmd" %in% tolower(templates)) {
                # Passes proj.title and proj.id to makeRmdTemplate
                makeRmdTemplate(rmd.dir = r.dir, rmd.name = rmd.name,
                                proj.title = proj.title, proj.id = proj.id,
                                start.date = start.date, version = version, 
                                client = client, client.dept = client.dept, 
                                main.statistician = main.statistician, stats.collab = stats.collab,
                                rmd.purpose = rmd.purpose, rmd.notes = rmd.notes)
            }
        }
        
        ## Create SAS template, if requested
        if ("sas" %in% tolower(templates)) {
            if (dir.exists(paste0(proj.dir, "/sascode"))) {
                sas.dir <- paste0(proj.dir, "/sascode")
            } else if (dir.exists(paste0(proj.dir, "/sas"))) {
                sas.dir <- paste0(proj.dir, "/sas")
            } else {
                sas.dir <- paste0(proj.dir, "/templates")
            }
            # Passes proj.title and proj.id to makeSasTemplate
            makeSasTemplate(sas.dir = sas.dir, sas.name = sas.name,
                            proj.title = proj.title, proj.id = proj.id, 
                            start.date = start.date, version = version, 
                            client = client, client.dept = client.dept, 
                            main.statistician = main.statistician, stats.collab = stats.collab, 
                            sas.purpose = sas.purpose, sas.notes = sas.notes, 
                            sas.template.layout = sas.template.layout, sas.header.style = sas.header.style)
        }
    }
}