###############################################################################
# Define function to catch warning and error messages ----
###############################################################################
myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
        tryCatch(expr, error=function(e) {
            err <<- rbind(err, e[1])
            NULL
        }), warning=function(w) {
            warn <<- rbind(warn, w[1])
            invokeRestart("muffleWarning")
        })
    list(warning=warn, error=err)
}

###############################################################################
# Define server logic ----
###############################################################################
server <- function(input, output, session) { # Passed 'session' explicitly to enable dynamic input updates
    
    pkg_root <- if (file.exists("DESCRIPTION")) {
        normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    } else if (file.exists(file.path("..", "DESCRIPTION"))) {
        normalizePath(file.path(".."), winslash = "/", mustWork = TRUE)
    } else {
        NULL
    }
    
    if (!is.null(pkg_root)) {
        template_files <- c(
            "R/makeSnapshot.R",
            "R/makeMemoTemplate.R",
            "R/makeRTemplate.R",
            "R/makeRmdTemplate.R",
            "R/makeSasTemplate.R",
            "R/startProject.R"
        )
        for (template_file in template_files) {
            source_path <- file.path(pkg_root, template_file)
            if (file.exists(source_path)) {
                source(source_path, local = FALSE)
            }
        }
    }
    
    ## Dynamically update default subfolders when Structure changes
    observeEvent(input$structure, {
        if (input$structure == "snapshot") {
            updateTextInput(
                session = session, 
                inputId = "subfolders", 
                value = "00_protocol_and_irb, 01_data_specs, 02_docs, 03_include, 04_manuscript, 05_presentations, 06_external_resources"
            )
        } else {
            updateTextInput(
                session = session, 
                inputId = "subfolders", 
                value = "communications, data, graphs, memo, orig_data, others, r, sas, temp"
            )
        }
    })
    
    ## Dynamically update the snapshot default name when Structure is "snapshot" OR Start Date updates
    observe({
        req(input$structure)
        if (input$structure == "snapshot") {
            date_val <- if (is.null(input$start.date) || isTRUE(is.na(input$start.date))) {
                Sys.Date()
            } else {
                as.Date(input$start.date)
            }
            formatted_date <- format(date_val, "%Y%m%d")
            updateTextInput(
                session = session, 
                inputId = "snapshot.name", 
                value = paste0("analysis_", formatted_date)
            )
        }
    })
    
    ## Manage whether template options are shown or hidden
    observe({
        
        if ("memo" %in% input$templates) {
            shinyjs::show(id = "memoHeader")
            shinyjs::show(id = "memo.name")
            shinyjs::show(id = "memo.re")
        }
        else {
            shinyjs::hide(id = "memoHeader")
            shinyjs::hide(id = "memo.name")
            shinyjs::hide(id = "memo.re")
        }
        
        if ("r" %in% input$templates) {
            shinyjs::show(id = "rHeader")
            shinyjs::show(id = "r.template.layout")
            shinyjs::show(id = "r.header.style")
            if (identical(input$r.template.layout, "multi")) {
                shinyjs::hide(id = "r.name")
                shinyjs::hide(id = "r.purpose")
                shinyjs::hide(id = "r.notes")
            } else {
                shinyjs::show(id = "r.name")
                shinyjs::show(id = "r.purpose")
                shinyjs::show(id = "r.notes")
            }
        }
        else {
            shinyjs::hide(id = "rHeader")
            shinyjs::hide(id = "r.template.layout")
            shinyjs::hide(id = "r.header.style")
            shinyjs::hide(id = "r.name")
            shinyjs::hide(id = "r.purpose")
            shinyjs::hide(id = "r.notes")
        }
        
        if ("rmd" %in% input$templates) {
            shinyjs::show(id = "rmdHeader")
            shinyjs::show(id = "rmd.name")
            shinyjs::show(id = "rmd.purpose")
            shinyjs::show(id = "rmd.notes")
        }
        else {
            shinyjs::hide(id = "rmdHeader")
            shinyjs::hide(id = "rmd.name")
            shinyjs::hide(id = "rmd.purpose")
            shinyjs::hide(id = "rmd.notes")
        }
        
        if ("sas" %in% input$templates) {
            shinyjs::show(id = "sasHeader")
            shinyjs::show(id = "sas.template.layout")
            shinyjs::show(id = "sas.header.style")
            if (identical(input$sas.template.layout, "multi")) {
                shinyjs::hide(id = "sas.name")
                shinyjs::hide(id = "sas.purpose")
                shinyjs::hide(id = "sas.notes")
            } else {
                shinyjs::show(id = "sas.name")
                shinyjs::show(id = "sas.purpose")
                shinyjs::show(id = "sas.notes")
            }
        }
        else {
            shinyjs::hide(id = "sasHeader")
            shinyjs::hide(id = "sas.template.layout")
            shinyjs::hide(id = "sas.header.style")
            shinyjs::hide(id = "sas.name")
            shinyjs::hide(id = "sas.purpose")
            shinyjs::hide(id = "sas.notes")
        }
        
    })
    
    ## Run functions
    message <- NULL
    createProj <- eventReactive(input$run, {
        
        start_date_value <- if (is.null(input$start.date) || isTRUE(is.na(input$start.date))) {
            format(Sys.Date(), "%B %d, %Y")
        } else {
            format(as.Date(input$start.date), "%B %d, %Y")
        }
        
        # Convert analysis.subfolders string to vector: c("orig_data", "code", "data", ...)
        analysis_subfolders_vec <- if (input$structure == "snapshot" && !is.null(input$analysis.subfolders) && nzchar(input$analysis.subfolders)) {
            trimws(unlist(strsplit(input$analysis.subfolders, ",")))
        } else {
            NULL
        }
        
        track <- myTryCatch(
            startProject::startProject(main.dir = input$main.dir, proj.name = input$proj.name,
                                       proj.num = input$proj.num, start.date = start_date_value,
                                       structure = input$structure,
                                       snapshot.name = if(input$structure == "snapshot") input$snapshot.name else NULL,
                                       analysis.subfolders = if(input$structure == "snapshot") input$analysis_subfolders_vec else NULL,
                                       version = input$version, client = input$client,
                                       client.dept = input$client.dept, main.statistician = input$main.statistician,
                                       stats.collab = input$stats.collab,
                                       subfolders = input$subfolders, templates = input$templates,
                                       memo.name = input$memo.name, memo.re = input$memo.re,
                                       r.name = input$r.name, r.purpose = input$r.purpose, r.notes = input$r.notes,
                                       rmd.name = input$rmd.name, rmd.purpose = input$rmd.purpose, rmd.notes = input$rmd.notes,
                                       sas.name = input$sas.name, sas.purpose = input$sas.purpose, sas.notes = input$sas.notes,
                                       sas.template.layout = input$sas.template.layout,
                                       r.template.layout = input$r.template.layout,
                                       sas.header.style = input$sas.header.style,
                                       r.header.style = input$r.header.style)
        )
        
        if (is.null(track$warning) & !(is.null(track$error))) {
            message <- data.frame(matrix(unlist(track$error)))
            names(message) <- c("Error(s)")
            showNotification("Error", duration = 15, closeButton = FALSE, type = "error")
        }
        
        if (!(is.null(track$warning)) & is.null(track$error)) {
            message <- data.frame(matrix(unlist(track$warning)))
            names(message) <- c("Warning(s)")
            showNotification("Warning messages generated", duration = 15, closeButton = FALSE, type = "warning")
        }
        
        if (is.null(track$warning) & is.null(track$error)) {
            message <- data.frame("Project directory was created successfully")
            names(message) <- c("Message")
            showNotification("Project directory was created successfully", duration = 15, closeButton = FALSE, type = "message")
        }
        
        message
        
    })
    
    output$out <- renderTable({
        createProj()
    })
    
    observeEvent(input$close, {
        shinyjs::js$closeWindow()
        stopApp()
    })
    
}