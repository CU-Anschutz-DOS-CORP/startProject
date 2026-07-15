# Creating additional templates

After you have created a project directory, you can add additional
templates as needed.

[`makeMemoTemplate()`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeMemoTemplate.md),
[`makeRTemplate()`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeRTemplate.md),
[`makeRmdTemplate()`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeRmdTemplate.md),
and
[`makeSasTemplate()`](https://github.com/CU-Anschutz-DOS-CORP/startProject/reference/makeSasTemplate.md)
will generate .docx, .R, .Rmd and .sas templates, respectively.

Below you can see an example for making an R template; each template
function has a similar call.

``` r

makeRTemplate(r.dir = getwd(), 
              r.name = "myNewRProgram", 
              proj.name = "DemoProject", 
              proj.num = NULL,
              start.date = format(Sys.Date(), "%B %d, %Y"), 
              version = "1.2", 
              client = "John Smith",
              client.dept = "Testing Department", 
              main.statistician = "Rocio Lopez", 
              stats.collab = NULL,
              r.purpose = "Adding x, y and z", 
              r.notes = NULL)
```
