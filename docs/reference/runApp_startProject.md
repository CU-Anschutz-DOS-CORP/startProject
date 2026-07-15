# Launch startProject Shiny App

\`runApp_startProject\` will open a viewer window that will allow you to
run
[`startProject`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/startProject.md)
as a local shiny App.

## Usage

``` r
runApp_startProject(browser = "WindowViewer")
```

## Arguments

- browser:

  A character string that specifies the browser in which the app is
  launched. Valid options are "PaneViewer" to run in RStudio Viewer tab,
  "WindowViewer" to run in RStudio Window or "WindowExternal" to run in
  default web browser (case is ignored). Defaults to "WindowViewer".

## Details

The browser argument only takes effect if running within RStudio.

## See also

[`startProject`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/startProject.md)
for details on inputs.

## Author

Rocio Lopez, <lopezr@ccf.org>

## Examples

``` r
if (FALSE) runApp_startProject(browser = "PaneViewer") # \dontrun{}
```
