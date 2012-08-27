\name{newPage}
\alias{newPage}

\title{
  newPage
}
\description{
  Opens an HTML page/document, allowing a sequential building of an HTML
  page.
}
\usage{
newPage(filename, dirname = NULL, title = filename,
        doctype = "<!DOCTYPE html>\n",
        link.javascript = NULL, link.css = NULL, css = NULL,
        head = NULL, head.attributes = NULL,
        body.attributes = NULL)
}

\arguments{
  \item{filename}{A character string containing the filename or the path
    of the HTML file to be created.}

  \item{dirname}{An optional character string containing the path of the
    directory where the file should be written.}

  \item{title}{An optional character string containing the title of the
    HTML page.}

  \item{doctype}{A character string containing a \code{DOCTYPE}
    statement which declares the document type and level of HTML
    syntax.}

  \item{link.javascript}{An optional character vector containing the URL
    of JavaScripts to be associated with the page.}

  \item{link.css}{An optional character vector containing the URL of CSS
    stylesheets to be associated with the page.}

  \item{css}{An optional character vector containing inline CSS
  stylesheets to be associated with the page.}

  \item{head}{An optional character string containing an HTML fragment
    to be added in the \code{<head>} section of the page.}

  \item{head.attributes}{An optional named list of character strings,
    containing the \code{<head>} attributes.}

  \item{body.attributes}{An optional named list of character strings,
    containing the \code{<body>} attributes.}

}

\details{
  \code{newPage} opens a new file for writing and returns a page handle
  which is used by \code{hwrite} to append HTML elements in a current
  page. Any previous existing file will be overwritten.

  The \code{doctype} argument is used to specify the document type and
  level of HTML syntax. Valid forms may be found at
  \url{http://www.w3.org/QA/2002/04/valid-dtd-list.html}.

  \code{link.javascript} produces A \code{<script>} tag specifying the
  language as JavaScript and stating the location of the
  code. \pkg{hwriterPlus} uses MathJax from
  \url{http://www.mathjax.org/}. The JavaScript may either be loaded
  from the MathJax site, or from a local file. The former option
  requires an active internet connection, while the latter requires that
  MathJax must have been installed locally. The form of the text for
  this argument is \code{"path-to-MathJax/MathJax.js"}. See the examples
  for the appropriate text when loading from MathJax directly.  Details
  of how to ensure MathJax is loaded, for Version 2 of MathJax may be
  found at \url{http://www.mathjax.org/docs/2.0/configuration.html}.

  The argument \code{head} is useful to add extra HTML code in the
  \code{<head>} header code.

  \code{head.attributes} is useful to add extra attributes in the
  \code{<head>} head code. A description of possible head attributes may
  be found at \url{http://www.w3schools.com/tags/tag_body.asp}.

  \code{body.attributes} is useful to add extra attributes in the
  \code{<body>} body code. A description of possible body attributes may
  be found at \url{http://www.w3schools.com/tags/tag_body.asp}.

 To allow the numbering of displayed equations \code{newPage} creates a
 global variable called \code{hwriterEquation} and assigns it the value
 zero. This variable will be updated when a numbered equation is written
 to the HTML file so that it always records the number of the last
 numbered equation. It also creates a a global vector named
 \code{hwriterEquationList}, which is initially empty, but will contain
 the labels of any numbered equations.

}
\value{
  A connection which is a handle to the current HTML page.
}
\references{
  An invaluable source of information concerning valid HTML code and
  options is the website \url{http://www.w3schools.com/}.
}
\author{
 David Scott \email{d.scott@auckland.ac.nz}
}
\note{
  \code{newPage} is a modification of \code{openPage} from \pkg{hwriter}
  which includes a \code{doctype} argument but eliminates the
  \code{charset} and \code{lang} arguments provided by \code{openPage}.
}

\seealso{
  \code{\link[hwriter]{openPage}}
}
\examples{
### The same as the example from openPage, with newPage replacing
### openPage

### Creates a new web page 'test.html' in the R temporary directory
tmpdir <- tempdir()
p <- newPage('test.html', dirname = tmpdir,
              link.css = 'http://www.ebi.ac.uk/~gpau/hwriter/hwriter.css')
hwrite('Iris example', p, center = TRUE, heading = 1)
hwrite('This famous (Fisher\'s or Anderson\'s) iris data set gives the
        measurements in centimeters of the variables sepal length and
        width and petal length and width, respectively, for 50 flowers
        from each of 3 species of iris.', p, class = 'king')
hwrite(iris, p, row.bgcolor = '#ffffaa')
closePage(p)

### Opens a web browser on the web page
if (interactive()) try(browseURL(file.path(tmpdir, 'test.html')))
}


\keyword{interface}
