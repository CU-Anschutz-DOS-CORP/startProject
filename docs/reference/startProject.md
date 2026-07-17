# Create a project directory with sub-folders and templates

\`startProject()\` will create a project directory along with the
specified sub-folders and optional .docx, .R, .Rmd and .sas templates
containing headers with project information.

## Usage

``` r
startProject(
  main.dir = getwd(),
  proj.name = "project name",
  proj.num = NULL,
  start.date = format(Sys.Date(), "%B %d, %Y"),
  version = "1",
  client = NULL,
  client.dept = NULL,
  main.statistician = NULL,
  stats.collab = NULL,
  subfolders = NULL,
  templates = "memo, R, Rmd, sas",
  structure = c("legacy", "snapshot"),
  snapshot.name = NULL,
  analysis.subfolders = c("orig_data", "code", "data", "output_raw", "graphs",
    "final_styled", "memo"),
  sas.template.layout = c("single", "multi"),
  r.template.layout = c("single", "multi"),
  sas.header.style = c("default", "simple"),
  r.header.style = c("default", "simple"),
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
  sas.notes = NULL
)
```

## Arguments

- main.dir:

  A character string specifying the file path where the project
  directory will be created. Defaults to getwd().

- proj.name:

  A short character string providing the name or number to be used as
  the directory name and in template headers.

- proj.num:

  NULL or a character string providing a project number in the directory
  name and templates. If NULL, \[proj.name\] will be used instead.

- start.date:

  NULL or a character string providing the date to be included in
  templates. Defaults to today's date.

- version:

  NULL or a character string providing the project version to be
  included in the templates. Defaults to "1".

- client:

  NULL or a character string providing the name(s) of client(s) to be
  included in the templates.

- client.dept:

  NULL or a character string providing the affiliation of the client(s)
  to be included in the templates.

- main.statistician:

  NULL or a character string providing the name of the primary
  statistician. This will be used in the templates.

- stats.collab:

  NULL or a character string providing additional collaborators to be
  included as authors in the templates.

- subfolders:

  A character string providing a comma delimited list of sub-folders to
  be created. If NULL, defaults to "00_protocol_and_irb, 01_data_specs,
  02_docs, 03_include, 04_manuscript, 05_presentations,
  06_external_resources" if structure="snapshot" and "communications,
  data, graphs, memo, orig_data, others, r, sas, temp" if
  structure="legacy".

- templates:

  A character string providing a comma delimited list of templates to be
  generated. Valid options are memo, R, Rmd and sas. Defaults to "memo,
  R, Rmd, sas".

- structure:

  A character string specifying the project layout to create. Supported
  values are "legacy" and "snapshot". Defaults to "legacy".

- snapshot.name:

  NULL or a character string providing the analysis snapshot folder
  name. If NULL, an analysis_YYYYMMDD folder will be created. Ignored if
  structure = "legacy".

- analysis.subfolders:

  A character vector providing the subfolders to create inside each
  analysis snapshot directory. Defaults to the standard Snapshot layout:
  orig_data, code, data, output_raw, graphs, final_styled, and memo.
  Ignored if structure = "legacy".

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

- memo.name:

  NULL or a character string specifying the name of the .Rmd file. If
  NULL, the file will be named
  p\[proj.num\]\_memo\[currentdate\]\_v\[version\] or
  p\[proj.num\]\_memo\[currentdate\].

- memo.re:

  NULL or a character string providing the subject line for the memo
  template.

- r.name:

  NULL or a character string specifying the name of the .R template. If
  NULL, the file will be named
  p\[proj.num\]\_r\[currentdate\]\_v\[version\] or
  p\[proj.num\]\_r\[currentdate\].

- r.purpose:

  NULL or a character string providing the purpose for the R file to be
  included in the R template header.

- r.notes:

  NULL or a character string providing additional notes to be included
  in the R template header.

- rmd.name:

  NULL or a character string specifying the name of the .Rmd template.
  If NULL, the file will be named
  p\[proj.num\]\_rmd\[currentdate\]\_v\[version\] or
  p\[proj.num\]\_rmd\_\[currentdate\]

- rmd.purpose:

  NULL or a character string providing the purpose for the Rmd file to
  be included in the Rmd template header.

- rmd.notes:

  NULL or a character string providing additional notes to be included
  in the Rmd template header.

- sas.name:

  NULL or a character string specifying the name of the .sas file. If
  NULL, the file will be named
  p\[proj.num\]\_sas\[currentdate\]\_v\[version\] or
  p\[proj.num\]\_sas\[currentdate\].

- sas.purpose:

  NULL or a character string providing the purpose for SAS file to be
  included in the template header.

- sas.notes:

  NULL or a character string providing additional notes to be included
  in the SAS template header.

## Details

If the main directory, sub-folders and/or templates exist, the function
will generate a warning message but continue and will create any
components that do not exist.

Templates other than memo, sas, r and Rmd (case ignored) are not
supported. If any templates are requested, the function will search for
memo, rcode or r, sascode or sas sub-folders to store the templates; if
these are not found, a templates sub-folder will be created.

You can modify the memo template by either creating a .basefile.docx
file under your HOME directory or by modifying the
templates/memoTemplate contained in the package library. Note that the
text use as placeholders for passed information (DATESEC, TOSEC, etc.)
should NOT be changed.

You can also append code and comments under the generated header of any
of the code files (.R, .Rmd, .sas) by either creating a .basefile.R,
.basefile.Rmd and/or .basefile.sas file(s) in your HOME directory or by
modifying the templates/basefile.xxx file contained in the package
library.

To find the location of your HOME directory, run \`Sys.getenv("HOME")\`.

To find the location of the included template files, run
\`system.file("templates", package = "startProject")\`.

## See also

[`runApp_startProject`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/runApp_startProject.md)
for how to launch the function as a local Shiny app. Functions used to
generate templates:
[`makeMemoTemplate`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeMemoTemplate.md),
[`makeRmdTemplate`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeRmdTemplate.md),
[`makeRTemplate`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeRTemplate.md),
and
[`makeSasTemplate`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeSasTemplate.md).

## Author

Rocio Lopez, <Rocio.LopezMoscoso@cuanschutz.edu>

## Examples

``` r
if (FALSE) startProject(proj.name = "Example Project", client = "John Smith",
    client.dept = "Test Institute", main.statistician = "My Name Here") # \dontrun{}
```
