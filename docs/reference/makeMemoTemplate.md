# Create a .docx template

\`makeMemoTemplate()\` will generate a .docx file to be used for project
memos. This document will have a header and any project information
provided in the function call.

## Usage

``` r
makeMemoTemplate(
  memo.dir = getwd(),
  memo.name = NULL,
  start.date = format(Sys.Date(), "%B %d, %Y"),
  version = "1",
  client = NULL,
  client.dept = NULL,
  main.statistician = NULL,
  stats.collab = NULL,
  memo.re = NULL
)
```

## Arguments

- memo.dir:

  A character string specifying the file path where the template will be
  stored. Defaults to getwd().

- memo.name:

  NULL or a character string specifying the name of the .Rmd file. If
  NULL, the file will be named memo\[currentdate\]\_v\[version\] or
  memo\[currentdate\].

- start.date:

  NULL or a character string providing the date to be included in the
  template header. Defaults to current date.

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

- memo.re:

  NULL or a character string providing the subject line for the memo
  template.

## Details

You can modify the memo template by either creating a .basefile.docx
file under your HOME directory or by modifying the
templates/memoTemplate contained in the package library. Note that the
text use as placeholders for passed information (DATESEC, TOSEC, etc.)
should NOT be changed.

To find the location of your HOME directory, run \`Sys.getenv("HOME")\`.

To find the location of the included template file, run
\`system.file("templates", package = "startProject")\`.

## Author

Rocio Lopez, <Rocio.LopezMoscoso@cuanschutz.edu>

## Examples

``` r
if (FALSE) makeMemoTemplate(memo.name = "memoTemplate") # \dontrun{}
```
