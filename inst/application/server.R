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
server <- function(input, output) {

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
      shinyjs::show(id = "r.name")
      shinyjs::show(id = "r.purpose")
      shinyjs::show(id = "r.notes")
    }
    else {
      shinyjs::hide(id = "rHeader")
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
      shinyjs::show(id = "sas.name")
      shinyjs::show(id = "sas.purpose")
      shinyjs::show(id = "sas.notes")
    }
    else {
      shinyjs::hide(id = "sasHeader")
      shinyjs::hide(id = "sas.name")
      shinyjs::hide(id = "sas.purpose")
      shinyjs::hide(id = "sas.notes")
    }

  })

  ## Run functions
  message <- NULL
  createProj <- eventReactive(input$run, {

    track <- myTryCatch(
      startProject::startProject(main.dir = input$main.dir, proj.name = input$proj.name,
                                 proj.num = input$proj.num, start.date = input$start.date,
                                 version = input$version, client = input$client,
                                 client.dept = input$client.dept, main.statistician = input$main.statistician,
                                 stats.collab = input$stats.collab,
                                 subfolders = input$subfolders, templates = input$templates,
                                 memo.name = input$memo.name, memo.re = input$memo.re,
                                 r.name = input$r.name, r.purpose = input$r.purpose, r.notes = input$r.notes,
                                 rmd.name = input$rmd.name, rmd.purpose = input$rmd.purpose, rmd.notes = input$rmd.notes,
                                 sas.name = input$sas.name, sas.purpose =input$sas.purpose, sas.notes = input$sas.notes)
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
