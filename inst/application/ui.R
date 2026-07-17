library(shiny)
library(shinyjs)
library(bslib)

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
ui <- fluidPage(
    # Setup modern styling and dependencies
    theme = bslib::bs_theme(
        version = 5,
        bootswatch = "spacelab",
        primary = "#CFB87C",    # UC Gold
        secondary = "#434343"   # UC Dark Gray
    ),
    
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
    
    tags$head(
        tags$link(rel = "shortcut icon", href = "https://www.cuanschutz.edu/CU_favicon_16x16.png"),
        tags$style(HTML("
      /* Polish spacing and visual elements */
      .card { box-shadow: 0 4px 6px rgba(0,0,0,0.05); border: 1px solid #e3e6f0; margin-bottom: 20px; }
      .card-header { font-weight: bold; }
      .btn-primary { background-color: #002A54; border-color: #002A54; }
      .btn-primary:hover { background-color: #001A33; border-color: #001A33; }
      .control-label { font-weight: 600; margin-bottom: 5px; color: #495057; }
    "))
    ),
    
    # Responsive Header Grid
    div(
        class = "container-fluid py-4 bg-light mb-4 border-bottom",
        div(
            class = "row align-items-center",
            div(
                class = "col-auto",
                img(src = "logo.png", height = "55px", alt = "Logo", style = "margin-right: 15px;")
            ),
            div(
                class = "col",
                h1("startProject", style = "margin: 0; font-weight: 700; color: #434343;"),
                p("An application for creating a project directory with memo and code templates", 
                  style = "margin: 0; color: #6c757d; font-size: 14px;")
            )
        )
    ),
    
    # Main Body Layout
    div(
        class = "container-fluid",
        div(
            class = "row",
            
            # LEFT COLUMN: Project Information (4-wide)
            div(
                class = "col-lg-4 col-md-5",
                div(
                    class = "card",
                    div(class = "card-header bg-primary text-white", "1. Project Metadata"),
                    div(
                        class = "card-body",
                        
                        textInput(
                            inputId = 'main.dir',
                            label = 'Main Directory',
                            value = homepath,
                            width = '100%',
                            placeholder = "Parent directory where project will be created"),
                        
                        textInput(
                            inputId = 'proj.name',
                            label = 'Project Name',
                            # value = "Project Name",
                            width = '100%',
                            placeholder = "Short name used to name main project folder"),
                        
                        textInput(
                            inputId = 'proj.num',
                            label = 'Project Number / ID',
                            width = '100%',
                            placeholder = "If blank, project name used"),
                        
                        dateInput(
                            inputId = 'start.date',
                            label = 'Start Date',
                            value = Sys.Date(),
                            format = 'yyyy-mm-dd',
                            width = '100%'),
                        
                        div(
                            class = "row",
                            div(
                                class = "col-sm-6",
                                radioButtons(
                                    inputId = 'structure',
                                    label = 'Project Structure',
                                    choices = c('Legacy' = 'legacy', 'Snapshot' = 'snapshot'),
                                    selected = 'legacy',
                                    inline = TRUE)
                            ),
                            div(
                                class = "col-sm-6",
                                textInput(
                                    inputId = 'version',
                                    label = 'Current Version',
                                    value = "1",
                                    width = '100%',
                                    placeholder = "e.g., 1, 2, 3")
                            )
                        ),
                        
                        # Dynamic Input: Snapshot Name (Only visible when structure == "snapshot")
                        conditionalPanel(
                            condition = "input.structure == 'snapshot'",
                            textInput(
                                inputId = 'snapshot.name',
                                label = 'Snapshot Name',
                                value = "",
                                width = '100%',
                                placeholder = "analysis_YYYYMMDD")
                        ),

                        textInput(
                            inputId = 'client',
                            label = 'Client / Research Collaborator(s)',
                            width = '100%',
                            placeholder = "Name(s) of key partners"),
                        
                        textInput(
                            inputId = 'client.dept',
                            label = 'Client Department/Division',
                            width = '100%',
                            placeholder = "Institute, department or division"),
                        
                        textInput(
                            inputId = 'main.statistician',
                            label = 'Main Statistician',
                            width = '100%',
                            placeholder = "Lead analyst"),
                        
                        textInput(
                            inputId = 'stats.collab',
                            label = 'Other Statistical Collaborators',
                            width = '100%',
                            placeholder = "Other collaborating team members")
                    )
                )
            ),
            
            # RIGHT COLUMN: Subfolders, Selection & Template Configuration (8-wide)
            div(
                class = "col-lg-8 col-md-7",
                
                # Subfolders Card
                div(
                    class = "card",
                    div(class = "card-header bg-secondary text-white", "2. Directory Framework"),
                    div(
                        class = "card-body",
                        textInput(
                            inputId = 'subfolders',
                            label = 'Subfolders to Create (Comma delimited)',
                            value = "communications, data, graphs, memo, orig_data, others, r, sas, temp",
                            width = '100%',
                            placeholder = "List of subfolders to create (comma delimited)"),
                        
                        # Dynamic Input: Analysis Subfolders (Only visible when structure == "snapshot")
                        conditionalPanel(
                            condition = "input.structure == 'snapshot'",
                            textInput(
                                inputId = 'analysis.subfolders',
                                label = 'Analysis Subfolders to Create (Comma delimited)',
                                value = "orig_data, code, data, output_raw, graphs, final_styled, memo",
                                width = '100%',
                                placeholder = "Subfolders created inside each analysis snapshot folder")
                        ),
                        
                        checkboxGroupInput(
                            inputId = "templates",
                            label = "Choose Templates to Create",
                            choices = c("Memo" = "memo", "R" = "r", "Rmd" = "rmd", "SAS" = "sas"),
                            selected = c("memo", "r", "rmd", "sas"),
                            inline = TRUE)
                    )
                ),
                
                # Templates Options Panel
                div(
                    class = "card",
                    div(class = "card-header bg-secondary text-white", "3. Template Settings"),
                    div(
                        class = "card-body",
                        
                        # MEMO TEMPLATE
                        div(
                            id = "memo_section",
                            tags$h4(id = "memoHeader", class = "text-info border-bottom pb-1", "Memo Template Options"),
                            div(
                                class = "row mb-3",
                                div(class = "col-md-6",
                                    textInput(
                                        inputId = 'memo.name',
                                        label = 'Template File Name',
                                        width = '100%',
                                        placeholder = "Defaults to: p[ID]_memo[date]_v[version]")
                                ),
                                div(class = "col-md-6",
                                    textInput(
                                        inputId = 'memo.re',
                                        label = 'Memo Subject (RE:)',
                                        width = '100%',
                                        placeholder = "Subject line for memo")
                                )
                            )
                        ),
                        
                        # R TEMPLATE
                        div(
                            id = "r_section",
                            tags$h4(id = "rHeader", class = "text-info border-bottom pb-1", "R Template Options"),
                            div(
                                class = "row mb-3",
                                div(class = "col-md-6",
                                    radioButtons(
                                        inputId = 'r.template.layout',
                                        label = 'Layout',
                                        choices = c('Single file' = 'single', 'Multi file' = 'multi'),
                                        selected = 'single',
                                        inline = TRUE)
                                ),
                                div(class = "col-md-6",
                                    radioButtons(
                                        inputId = 'r.header.style',
                                        label = 'Header Style',
                                        choices = c('Default' = 'default', 'Simple' = 'simple'),
                                        selected = 'default',
                                        inline = TRUE)
                                )
                            ),
                            div(
                                class = "row mb-3",
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'r.name',
                                        label = 'File Name',
                                        width = '100%',
                                        placeholder = "Defaults to: p[ID]_r[date]_v[version]")
                                ),
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'r.purpose',
                                        label = 'Code Purpose',
                                        width = '100%',
                                        placeholder = "Description of purpose")
                                ),
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'r.notes',
                                        label = 'Additional Notes',
                                        width = '100%',
                                        placeholder = "Notes to include")
                                )
                            )
                        ),
                        
                        # RMD TEMPLATE
                        div(
                            id = "rmd_section",
                            tags$h4(id = "rmdHeader", class = "text-info border-bottom pb-1", "Rmd Template Options"),
                            div(
                                class = "row mb-3",
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'rmd.name',
                                        label = 'File Name',
                                        width = '100%',
                                        placeholder = "Defaults to: p[ID]_rmd[date]_v[version]")
                                ),
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'rmd.purpose',
                                        label = 'Document Purpose',
                                        width = '100%',
                                        placeholder = "Description of purpose")
                                ),
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'rmd.notes',
                                        label = 'Additional Notes',
                                        width = '100%',
                                        placeholder = "Notes to include")
                                )
                            )
                        ),
                        
                        # SAS TEMPLATE
                        div(
                            id = "sas_section",
                            tags$h4(id = "sasHeader", class = "text-info border-bottom pb-1", "SAS Template Options"),
                            div(
                                class = "row mb-3",
                                div(class = "col-md-6",
                                    radioButtons(
                                        inputId = 'sas.template.layout',
                                        label = 'Layout',
                                        choices = c('Single file' = 'single', 'Multi file' = 'multi'),
                                        selected = 'single',
                                        inline = TRUE)
                                ),
                                div(class = "col-md-6",
                                    radioButtons(
                                        inputId = 'sas.header.style',
                                        label = 'Header Style',
                                        choices = c('Default' = 'default', 'Simple' = 'simple'),
                                        selected = 'default',
                                        inline = TRUE)
                                )
                            ),
                            div(
                                class = "row mb-3",
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'sas.name',
                                        label = 'File Name',
                                        width = '100%',
                                        placeholder = "Defaults to: p[ID]_sas[date]_v[version]")
                                ),
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'sas.purpose',
                                        label = 'Program Purpose',
                                        width = '100%',
                                        placeholder = "Description of purpose")
                                ),
                                div(class = "col-md-4",
                                    textInput(
                                        inputId = 'sas.notes',
                                        label = 'Additional Notes',
                                        width = '100%',
                                        placeholder = "Notes to include")
                                )
                            )
                        )
                        
                    )
                ),
                
                # Execution Panel
                div(
                    class = "card border-primary mb-4",
                    div(
                        class = "card-body bg-light",
                        div(
                            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                            div(
                                actionButton(
                                    inputId = 'run',
                                    label = 'Create Directory',
                                    class = "btn-primary btn-lg px-4"),
                                actionButton(
                                    inputId = 'close',
                                    label = "Close App",
                                    class = "btn-outline-danger btn-lg px-4")
                            ),
                            div(
                                style = "min-width: 250px; text-align: right;",
                                tableOutput("out")
                            )
                        )
                    )
                )
                
            )
        )
    )
)