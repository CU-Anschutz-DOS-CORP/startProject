###############################################################################
# Define home path ----
###############################################################################
homepath <- Sys.getenv("HOME")

###############################################################################
# Define js code to close external window ----
###############################################################################
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

###############################################################################
# Define UI for application ----
###############################################################################
ui <- shiny::shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  title="startProject",
  theme="spacelab.min.css",
  tags$style(type="text/css",
             "label {font-size: 14px;}",
             "h1 {font-size: 32px;}",
             "h2 {font-size: 24px;}",
             "h3 {font-size: 18px;}",
             "p {font-size: 14px;}",
             ".recalculating {opacity: 1.0;}"),

  ## favicon
  tags$head(tags$link(rel="shortcut icon", href="https://www.cuanschutz.edu/CU_favicon_16x16.png")),
  
  ## Application title
  titlePanel(title=div(img(src="logo.png", height='20%', width='20%'), h1("startProject"))),
  p("An application for creating a project directory with memo and code templates"),

  ## Input Info
  hr(),
  tags$h2("Project Information"),

  textInput(
    inputId = 'main.dir',
    label = 'Main directory',
    value = homepath,
    width = '75%',
    placeholder = "Parent directory where project will be created"),

  textInput(
    inputId = 'proj.name',
    label = 'Project name',
    value = "Project Name",
    width = '75%',
    placeholder = "Short name for main project folder"),

  textInput(
    inputId = 'proj.num',
    label = 'Number associated with the project',
    width = '75%',
    placeholder = "If blank, project name used"),

  textInput(
    inputId = 'start.date',
    label = 'Date in which the projects was started',
    value = format(Sys.Date(), "%B %d, %Y"),
    width = '75%',
    placeholder = "If blank, defaults to today"),

  textInput(
    inputId = 'version',
    label = 'Version associated with project',
    value = "1",
    width = '75%',
    placeholder = "Version associated with project"),

  textInput(
    inputId = 'client',
    label = 'Client',
    width = '75%',
    placeholder = "Client/Research collaborators"),

  textInput(
    inputId = 'client.dept',
    label = 'Client department/division',
    width = '75%',
    placeholder = "Research collaborators' insitute, department or division"),

  textInput(
    inputId = 'main.statistician',
    label = 'Main Statistician',
    width = '75%',
    placeholder = "Main statistician for project"),

  textInput(
    inputId = 'stats.collab',
    label = 'Other Statistical collaborators',
    width = '75%',
    placeholder = "Other statistitians and/or statistical programmers collaborating on project"),

  hr(),
  tags$h2("Directory Subfolders & Templates"),

  textInput(
    inputId = 'subfolders',
    label = 'List of subfolders to create (comma delimited)',
    value = "communications, data, graphs, memo, orig_data, others, r, sas, temp",
    width = '75%',
    placeholder = "List of subfolders to create (comma delimited)"),

  checkboxGroupInput(
    inputId = "templates",
    label = "Choose templates to create:",
    choices = c("Memo" = "memo", "R" = "r", "Rmd" = "rmd", "SAS" = "sas"),
    selected = c("memo", "r", "rmd", "sas")),

  tags$h3(id = "memoHeader", "Memo template options"),

  textInput(
    inputId = 'memo.name',
    label = 'Name for memo (.docx) template',
    width = '75%',
    placeholder = "If blank, defaults to p[proj num or name]_memo[currentdate]_v[version]"),

  textInput(
    inputId = 'memo.re',
    label = 'Memo subject',
    width = '75%',
    placeholder = "Subject line for memo"),

  tags$h3(id = "rHeader", "R template options"),

  textInput(
    inputId = 'r.name',
    label = 'Name for R template',
    width = '75%',
    placeholder = "If blank, defaults to p[proj num or name]_r[currentdate]_v[version]"),

  textInput(
    inputId = 'r.purpose',
    label = 'R purpose',
    width = '75%',
    placeholder = "Description of what .R file will be used for"),

  textInput(
    inputId = 'r.notes',
    label = 'R notes',
    width = '75%',
    placeholder = "Additional notes to include in .R template"),

  tags$h3(id = "rmdHeader", "Rmd template options"),

  textInput(
    inputId = 'rmd.name',
    label = 'Name for Rmd template',
    width = '75%',
    placeholder = "If blank, defaults to p[proj num or name]_rmd[currentdate]_v[version]"),

  textInput(
    inputId = 'rmd.purpose',
    label = 'Rmd purpose',
    width = '75%',
    placeholder = "Description of what .Rmd file will be used for"),

  textInput(
    inputId = 'rmd.notes',
    label = 'Rmd notes',
    width = '75%',
    placeholder = "Additional notes to include in .Rmd template"),

  tags$h3(id = "sasHeader", "SAS template options"),

  textInput(
    inputId = 'sas.name',
    label = 'Name for SAS template',
    width = '75%',
    placeholder = "If blank, defaults to p[proj num or name]_sas[currentdate]_v[version]"),

  textInput(
    inputId = 'sas.purpose',
    label = 'SAS purpose',
    width = '75%',
    placeholder = "Description of what .sas file will be used for"),

  textInput(
    inputId = 'sas.notes',
    label = 'SAS notes',
    width = '75%',
    placeholder = "Additional notes to include in .sas template"),

  ## Run button
  actionButton(
    inputId = 'run',
    label = 'Create directory',
    width = '20%'),

  ## Output
  tableOutput("out"),

  ## Close button
  p(""),
  p(""),
  shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
  actionButton(
    inputId = 'close',
    label = "Close",
    width = '20%')

))
