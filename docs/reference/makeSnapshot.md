# Create a snapshot-style project structure analysis subfolder

\`makeSnapshot()\` creates a dated analysis snapshot folder, and
template files placed in the snapshot code directory.

## Usage

``` r
makeSnapshot(
  project.dir = getwd(),
  proj.title = NULL,
  proj.id = NULL,
  start.date = format(Sys.Date(), "%B %d, %Y"),
  version = "1",
  client = NULL,
  client.dept = NULL,
  main.statistician = NULL,
  stats.collab = NULL,
  templates = "memo, R, Rmd, sas",
  memo.name = NULL,
  memo.re = NULL,
  r.name = NULL,
  r.purpose = NULL,
  r.notes = NULL,
  rmd.name = NULL,
  rmd.purpose = NULL,
  rmd.notes = NULL,
  sas.name = NULL,
  sas.purpose = NULL,
  sas.notes = NULL,
  snapshot.name = NULL,
  analysis.subfolders = c("orig_data", "code", "data", "output_raw", "graphs",
    "final_styled", "memo"),
  sas.template.layout = c("single", "multi"),
  r.template.layout = c("single", "multi"),
  sas.header.style = c("default", "simple"),
  r.header.style = c("default", "simple")
)
```

## Arguments

- project.dir:

  A character string specifying the path where the project root will be
  created.

- proj.title:

  A character string providing the full, descriptive project title to be
  used in template headers and memo subject lines.

- proj.id:

  NULL or a short, clean character string providing a project identifier
  to be used as the directory name and as a prefix for template
  filenames. If NULL, a clean identifier will be automatically derived
  from \[proj.title\].

- start.date:

  NULL or a character string providing the date to be included in
  templates. Defaults to today's date.

- version:

  NULL or a character string providing the project version to be
  included in the templates. Defaults to "1".

- client:

  NULL or a character string providing the name(s) of client(s).

- client.dept:

  NULL or a character string providing the affiliation of the client(s).

- main.statistician:

  NULL or a character string providing the name of the primary
  statistician.

- stats.collab:

  NULL or a character string providing additional collaborators.

- templates:

  A character string providing a comma delimited list of templates to be
  generated. Valid options are memo, R, Rmd and sas.

- memo.name:

  NULL or a character string specifying the name of the .docx memo
  template.

- memo.re:

  NULL or a character string providing the subject line for the memo
  template.

- r.name:

  NULL or a character string specifying the name of the .R template.

- r.purpose:

  NULL or a character string providing the purpose for the R file to be
  included in the R template header.

- r.notes:

  NULL or a character string providing additional notes to be included
  in the R template header.

- rmd.name:

  NULL or a character string specifying the name of the .Rmd template.

- rmd.purpose:

  NULL or a character string providing the purpose for the Rmd file to
  be included in the Rmd template header.

- rmd.notes:

  NULL or a character string providing additional notes to be included
  in the Rmd template header.

- sas.name:

  NULL or a character string specifying the name of the .sas template.

- sas.purpose:

  NULL or a character string providing the purpose for SAS file to be
  included in the template header.

- sas.notes:

  NULL or a character string providing additional notes to be included
  in the SAS template header.

- snapshot.name:

  NULL or a character string providing the snapshot folder name. If
  NULL, an analysis_YYYYMMDD folder will be created.

- analysis.subfolders:

  A character vector providing the subfolders to create inside the
  analysis snapshot directory. Defaults to the standard Snapshot layout:
  orig_data, code, data, output_raw, graphs, final_styled, and memo.

- sas.template.layout:

  A character string specifying the SAS template layout. Supported
  values are "single" and "multi". Choose Single file to generate a
  unified, all-in-one program template ideal for straightforward
  analyses. Choose Multi-file to generate structured, separate scripts
  for setup, data management, and analysis, providing a modular
  framework that is easier to customize and scale with specialized
  additions. Defaults to "single".

- r.template.layout:

  A character string specifying the R template layout. Supported values
  are "single" and "multi". Defaults to "single".

- sas.header.style:

  A character string specifying the SAS header style. Supported values
  are "default" and "simple". Choose the Default header for major
  collaborative or client-facing analysis programs to capture
  comprehensive study metadata, collaborator details, and external
  dependencies. Choose the Simple header for utility, modular, or setup
  scripts that require only lightweight file tracking and execution
  notes. Defaults to "default".

- r.header.style:

  A character string specifying the R header style. Supported values are
  "default" and "simple". Choose the Default header for major
  collaborative or client-facing analysis programs to capture
  comprehensive study metadata, collaborator details, and external
  dependencies. Choose the Simple header for utility, modular, or setup
  scripts that require only lightweight file tracking and execution
  notes. Defaults to "default".
