# Create a .sas template

\`makSasTemplate\` will generate a .sas file with a header containing
any project information provided in the function call.

## Usage

``` r
makeSasTemplate(
  sas.dir = getwd(),
  sas.name = NULL,
  proj.name = NULL,
  start.date = format(Sys.Date(), "%B %d, %Y"),
  version = "1",
  client = NULL,
  client.dept = NULL,
  main.statistician = NULL,
  stats.collab = NULL,
  sas.purpose = NULL,
  sas.notes = NULL,
  sas.template.layout = c("single", "multi"),
  sas.header.style = c("default", "simple")
)
```

## Arguments

- sas.dir:

  A character string specifying the file path where the template will be
  stored. Defaults to getwd().

- sas.name:

  NULL or a character string specifying the name of the .sas file. If
  NULL, the file will be named
  p\[proj.name\]\_sas\[currentdate\]\_v\[version\] or
  sas\[currentdate\].

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

- sas.purpose:

  NULL or a character string providing the purpose for the generated
  template to be included in the template header.

- sas.notes:

  NULL or a character string providing additional notes to be included
  in the template header.

- sas.template.layout:

  A character string specifying the SAS template layout. Supported
  values are "single" and "multi". Defaults to "single".

- sas.header.style:

  A character string specifying the SAS header style. Supported values
  are "default" and "simple". Defaults to "default".

## Details

You can append code and comments under the generated header by either
creating a .basefile.sas file in your /home/\<USERNAME\> directory or by
modifying the templates/basefile.sas file contained in the package
library.

To find the location of your HOME directory, run \`Sys.getenv("HOME")\`.

To find the location of the included template file, run
\`system.file("templates", package = "startProject")\`.

## Author

Rocio Lopez, <Rocio.LopezMoscoso@cuanschutz.edu>

## Examples

``` r
if (FALSE) makeSasTemplate(sas.name = "sasTemplate") # \dontrun{}
```
