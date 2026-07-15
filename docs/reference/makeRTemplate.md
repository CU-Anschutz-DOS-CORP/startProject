# Create a .R template

\`makeRTemplate\` will generate a .R file with a header containing any
project information provided in the function call.

## Usage

``` r
makeRTemplate(
  r.dir = getwd(),
  r.name = NULL,
  proj.name = NULL,
  start.date = format(Sys.Date(), "%B %d, %Y"),
  version = "1",
  client = NULL,
  client.dept = NULL,
  main.statistician = NULL,
  stats.collab = NULL,
  r.purpose = NULL,
  r.notes = NULL,
  r.template.layout = c("single", "multi"),
  r.header.style = c("default", "simple")
)
```

## Arguments

- r.dir:

  A character string specifying the file path where the template will be
  stored. Defaults to getwd().

- r.name:

  NULL or a character string specifying the name of the .R file. If
  NULL, the file will be named
  p\[proj.name\]\_r\[currentdate\]\_v\[version\] or r\[currentdate\].

- proj.name:

  NULL or a character string providing the name of the project to be
  included in the template name and/or header.

- start.date:

  NULL or a character string providing the date to be included in the
  template header. Defaults to today's date.

- version:

  NULL or a character string providing the project version to be
  included in the template name and/or header. Defaults to "1".

- client:

  NULL or a character string providing the name(s) of client(s) to be
  included in the template header.

- client.dept:

  NULL or a character string providing the affiliation of the client(s)
  to be included in the template header.

- main.statistician:

  NULL or a character string providing the name of the primary
  statistician. This will be used in the template header.

- stats.collab:

  NULL or a character string providing additional collaborators to be
  included as authors in the template header.

- r.purpose:

  NULL or a character string providing the purpose for the generated
  template to be included in the template header.

- r.notes:

  NULL or a character string providing additional notes to be included
  in the template header.

- r.template.layout:

  A character string specifying the R template layout. Supported values
  are "single" and "multi". Defaults to "single".

- r.header.style:

  A character string specifying the R header style. Supported values are
  "default" and "simple". Defaults to "default".

## Details

You can append code and comments under the generated header by either
creating a .basefile.R file in your /home/\<USERNAME\> directory or by
modifying the templates/basefile.R file contained in the package
library.

To find the location of your HOME directory, run \`Sys.getenv("HOME")\`.

To find the location of the included template file, run
\`system.file("templates", package = "startProject")\`.

## Author

Rocio Lopez, <Rocio.LopezMoscoso@cuanschutz.edu>

## Examples

``` r
if (FALSE) makeRTemplate(r.name = "rTemplate") # \dontrun{}
```
