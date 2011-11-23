\name{hwriterPlus-package}
\alias{hwriterPlus-package}
\alias{hwriterPlus}
\docType{package}
\title{
hwriterPlus: Extending the hwriter Package
}
\description{
  This extends the package hwriter providing facilities such as the
  inclusion of output from R, the results of an R session and the
  display of mathematical expressions using LaTeX notation.
}
\details{
\tabular{ll}{
Package: \tab hwriterPlus\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-10-27\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

}
\author{
  David Scott <d.scott@auckland.ac.nz>
}


\seealso{
  See the package \pkg{hwriter}. Also the functions
  \code{\link[R2HTML]{as.latex}} and \code{\link[R2HTML]{HTML.latex}}
  from the package \pkg{R2HTML}.

}
\examples{
### hwriter example: shows many table examples
hwriter:::showExample()

### Some examples of using hwriter to produce tags
### Text in a heading tag
hwrite("Some text", heading = 1)

### Text in a div tag
hwrite("Some text", div = TRUE)

### A link to an external website
hwrite("Auckland University", link = "http://www.auckland.ac.nz")

### A heading with a name so that linking to it is possible
hwrite(hwrite("A Level 2 Heading", name = 'a named heading'),
       heading = 2, center = FALSE, br = TRUE)

### Produce and display a simple html file
### Create a temporary file first
tmpDir <- tempdir()
fileName <- file.path(tmpDir, 'hwriterPlus.html')
### Copy some necessary files
cssDir <- file.path(system.file(package = 'hwriterPlus'), 'css')
file.copy(file.path(cssDir, "BrowserExample.css"), tmpDir)

### Set up some header information
### Open file for writing
p <- newPage(fileName,
             title = "Minimal Document",
             link.css = c("BrowserExample.css"))
hwrite("", p, br = TRUE)
hwrite(paste("<span class = 'title'>",
             "Minimal Document</span>"),
       p, center = TRUE, br = TRUE)
hwrite("<span class = 'subtitle'> Dr. David J. Scott</span>",
       p, center = TRUE, br = TRUE)
hwrite(paste("<span class = 'subtitle'>",
             format(as.Date("2011-11-16"), '\%B \%d, \%Y'), "</span>",
             sep = ""),
       p, center = TRUE, br = TRUE)
hwrite(hwrite("Entering Text", name = "intro"), p,
       heading = 1, center = FALSE, br = TRUE)
hwrite(paste("Ordinary paragraph text can be entering using",
             "<font face = 'monospace' >hwrite</font>.",
             "Aspects of the text such as the font family,",
             "face, size and colour can be altered in line by using",
             "tags or styles."),
       p, center = FALSE, br = TRUE)
hwrite("", p, br = TRUE)
hwrite(paste("For example the font can be changed to sans-serif",
             "<font face = 'Arial, sans-serif'> like this,",
             "</font> italic <i> like this, </i> bold <b> like this,",
             "this,</b>coloured <font color = 'blue'> like this,",
             "</font> or any combination of these",
             "<b><i><font color = 'blue'>like this.</font></i></b>"),
      p, br = TRUE)
hwrite("", p, br = TRUE)

close(p)
### Open a web browser and examine the resulting file
if (interactive()) try(browseURL(fileName))
}

\keyword{package}