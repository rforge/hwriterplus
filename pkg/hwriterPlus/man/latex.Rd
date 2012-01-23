\name{hwriteLatex}
\alias{hwriteLatex}
\alias{as.latex}

\title{
  Insert LaTeX Code into an HTML File
}
\description{
  This makes use of the \code{ASCIIMathML} JavaScript functions.
  Standard LaTeX input will be turned into MathML and displayed through
  any browser extension that can handle MathML (such as MathPlayer).
}
\usage{
as.latex(x, label = NULL,
         inline = ifelse(is.null(label), TRUE, FALSE),
         count = ifelse(is.null(label), FALSE, TRUE))
hwriteLatex(ltx, page = NULL,
            table.attributes = NULL,
            tr.attributes = "",
            td.attributes = NULL, ...)
}

\arguments{
  \item{x}{Character. A string containing a mathematical expression in
    LaTeX notation.}

  \item{label}{Character. A text string giving the label to be displayed
    before the equation.}

  \item{inline}{Logical. Set to \code{TRUE} for a mathematical
    expression to be included inline. Set to \code{FALSE} for a
    displayed mathematical expression.}

  \item{count}{Logical. Set to \code{TRUE} for a numbered equation,
    \code{FALSE} otherwise.}

  \item{ltx}{Either a string containing a mathematical expression in
    LaTeX notation or an object of class \code{latex} as produced by
    \code{as.latex}.}

  \item{page}{Character. An optional connection. A character string
    naming the file to write to or a page object returned by
    \code{\link{newPage}} or \code{openPage}.}

  \item{table.attributes}{Character. A string specifying attributes of
    the table for a displayed equation. See details.}

  \item{tr.attributes}{Character. A string specifying attributes of
    the table row for a displayed equation. Default is "" meaning no
    special attributes.}


  \item{td.attributes}{Character. A string specifying attributes of
    the table data entries for a displayed equation. See details.}

  \item{\dots}{Possible attributes to be attached to the mathematical
    expression.}

}
\details{
  Mathematical expressions in LaTeX format will be translated into
  MathML by the \code{ASCIIMathML} JavaScript program of Peter
  Jipsen. Note that his functions allow translation of ASCII expressions
  into mathematical expressions with a notation simpler than LaTeX which
  does not require the use of backslashes. See his page on
  \code{ASCIIMathML} for details.

  Mathematical expressions can be inserted \emph{inline} (within text)
  or on a single line as a displayed expression, corresponding to the
  use of $...$ and $$...$$ in normal LaTeX.

  One problem with writing LaTeX expressions is that all backslashes
  must be repeated. See the examples.

  For the translation to MathML to work, a reference to the JavaScript
  file must be present within the HTML file \bold{and} the HTML body tag
  has also to include \code{onload="translate()"}. The translation is
  very much browser dependent, but should work with recent versions of
  Firefox and Internet Explorer. Older versions of Internet Explorer
  will require the installation of the MathPlayer plugin.

  See the examples for an example of arguments to be supplied to
  \code{newPage} to ensure mathematical expressions are displayed.

  If \code{ltx} is a string containing a mathematical expression in
  LaTeX notation, or if it is an object of class \code{latex} with
  \code{inline = TRUE} then any values assigned to
  \code{table.attributes} \code{tr.attributes} and \code{td.attributes} are
  ignored. Otherwise the default value of \code{table.attributes} when
  this argument is \code{NULL} depends on whether the mathematical
  expression has an equation number or not, which is determined by
  whether \code{count} in the \code{latex} object is \code{TRUE} or
  \code{FALSE}. If \code{count = FALSE} then \code{table.attributes} is
  assigned the value \code{"border = '0'"}. If \code{count = FALSE} then
  \code{table.attributes} is assigned the value \code{"border = '0'
  width = '90\%'"}.  Similarly the default value of \code{td.attributes}
  depends on whether the mathematical expression has an equation number
  or not. If \code{count = FALSE} then \code{td.attributes} is
  ignored. If \code{count = FALSE} then \code{table.attributes} is
  assigned the value \code{c("width = '50'","align = 'center'",
    "align = 'right' width = '50'")}. The explanation for these
  assignments is that if a displayed mathematical expression has no
  equation number it is constructed as a table comprised of a single row
  with a single entry in that row. If the expression has an equation
  number, it is constructed as a table comprised of a single row with
  three entries: the first is a non-breaking space, the second is the
  mathematical expression itself and the third is the equation number.

  Note that these functions are modified versions of \code{as.latex} and
  \code{HTML.latex} from the package \pkg{R2HTML}. The most notable
  changes are that \code{HTML.latex} does not allow for inclusion of
  table and table data attributes in mathematical expressions and the
  numbering of equations uses \R{} code rather than JavaScript. The
  numbering system for equations adds an entry to the global vector
  \code{hwriterEquationList} whenever a numbered equation is created. If
  a label is specified via the \code{label} argument, by for example,
  \code{label = "anequationlabel"}, then an anchor is created as part of
  the table data entry containing the equation number, using an HTML
  \code{a} element. The actual anchor name is \code{eq:anequationlabel},
  which copies the label categorizing approach used in LaTeX. If no
  label is specified, but the equation is numbered, the name used for
  the anchor is \code{eq:equationxx} where \code{xx} is the equation
  number. Then the number of an equation corresponding to a given label
  may be retrieved using the function \code{eqRef} (intended to be
  reminiscent of the LaTeX \code{\\ref} command).

  To see what attributes are valid for tables, table rows and table
  data, go to \url{http://www.w3schools.com/tags/} and click on the
  desired tag.
}
\value{
  \code{as.latex} returns a list of class \code{latex} with components:
  \item{alt}{A character string containing the A string containing a
    mathematical expression in LaTeX notation.}
  \item{inline}{A logical value.}
  \item{count}{A logical value.}
  \item{label}{If not \code{NULL} a character string.}

  \code{hwriteLatex} produces a character vector containing the output
  HTML code. If \code{page} is specified this text is appended to the
  specified file.
}
\references{
  ASCIIMathML: \url{http://www1.chapman.edu/~jipsen/mathml/asciimath.xml}
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\seealso{
  \code{\link{newPage}}
}
\examples{
as.latex("See what is added by as.latex", label = "a label",
          inline = FALSE, count = TRUE)
### Run this example to see what is written to the file
\dontrun{
### An inline expression will be written to standard output as is
### Note that the backslashes remain duplicated in this case
### I think this is due to a line of code in hwriter::hwriteString
###   if (is.null(page)) txt
### which writes the txt literally rather than using cat
hwrite("Here is an inline mathematical expression:
        `\\int_{-\\infty}^{1}f(x)dx`")
### The same expression but displayed
hwriteLatex(as.latex("\\int_{-\\infty}^{1}f(x)dx",
                     inline = FALSE, count = FALSE),
            table.attributes = "border = '1'",
            tr.attributes = "bgcolor = 'white'")


### The same example written to a file
### Create a temporary file first
tmpDir <- tempdir()
fileName <- file.path(tmpDir, 'hwriteLatex.html')
### Copy some necessary files
cssDir <- file.path(system.file(package = 'hwriterPlus'), 'css')
file.copy(file.path(cssDir, "BrowserExample.css"), tmpDir)
jsDir <- file.path(system.file(package = 'hwriterPlus'), 'javascript')
file.copy(file.path(jsDir, "ASCIIMathML.js"), tmpDir)

### Set up some necessary header information
xmlns <- "xmlns:mml='http://www.w3.org/1998/Math/MathML'"
bodyAttributes <- list(onload = "translate()",
                       bgcolor = "FFFFFF",
                       background = "")
head <- '<object id="mathplayer" classid="clsid:32F66A20-7614-11D4-BD11-00104BD3F987"></object>
<?import namespace="mml" implementation="#mathplayer"?>'
### Open file for writing
p <- newPage(fileName,
             title = "Example of a Document for Display in a Browser",
             doctype = "",
             xmlns = xmlns,
             link.css = c("BrowserExample.css"),
             link.javascript = c("ASCIIMathML.js"),
             body.attributes = bodyAttributes,
             head = head)
hwrite("Here is an inline mathematical expression:
        `\\int_{-\\infty}^{1}f(x)dx`", p, br)
### The same expression but displayed
hwrite("Here is a displayed version of the same expression enclosed in a
       box", p, br)
hwriteLatex(as.latex("\\int_{-\\infty}^{1}f(x)dx",
                     inline = FALSE, count = FALSE),
            p,
            table.attributes = "border = '1'",
            tr.attributes = "bgcolor = 'white'")
### We can also have equation numbers and labels when writing to a file
hwrite("Here is a different expression which has an equation number and
assigned a label", p, br)
hwriteLatex(as.latex("\\{ 26.119 < \\sum_{i=1}^n(X_i-\\bar{X})^2\\}
\\bigcup\\ \\{ 5.629 > \\sum_{i=1}^n (X_i-\\bar{X})^2 \\}.",
                     inline = FALSE, label = "anequationlabel"),
            page = p,
            tr.attributes = "bgcolor = 'gray'",
            td.attributes = c("width = '50'",
                              "align = 'center' bgcolor = 'yellow'",
                              "align = 'right' width = '50'"))
closePage(p)
### Open a web browser and examine the resulting file
if (interactive()) try(browseURL(fileName))
}

}
\keyword{IO}
\keyword{file}
\keyword{print}