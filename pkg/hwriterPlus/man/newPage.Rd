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
        doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
        xmlns = "xmlns='http://www.w3.org/1999/xhtml'",
        link.javascript = NULL, link.css = NULL, css = NULL,
        head = NULL, charset = "utf-8", lang = "en",
        head.attributes = NULL, body.attributes = NULL)
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

  \item{xmlns}{A character string specifying an XML namespace to
    import.}

  \item{link.javascript}{An optional character vector containing the URL
    of JavaScripts to be associated with the page.}

  \item{link.css}{An optional character vector containing the URL of CSS
    stylesheets to be associated with the page.}

  \item{css}{An optional character vector containing inline CSS
  stylesheets to be associated with the page.}

  \item{head}{An optional character string containing an HTML fragment
    to be added in the \code{<head>} section of the page.}

  \item{charset}{An optional character string containing the current
  charset. Default is "utf-8".}

  \item{lang}{An optional character string containing the current
    langage. Default is "en".}

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
  \url{http://www.w3.org/QA/2002/04/valid-dtd-list.html}. \pkg{hwriter}
  uses the transitional XHTML 1.0 specification:
  \code{<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">}. For
  incorporation of mathematical expressions omission of the doctype
  statement was found to work fairly reliably and did not seem to cause
  other problems.

  \code{xmlns} allows the importation of XML namespaces. Authoritative
  documentation of this feature may be found at
  \url{http://www.w3.org/TR/REC-xml-names/}. \pkg{hwriter} uses
  \code{xmlns='http://www.w3.org/1999/xhtml'} plus a language
  specification. For incorporation of mathematical expressions,
  \code{xmlns:mml='http://www.w3.org/1998/Math/MathML'} taken from
  \pkg{R2HTML} appears to work fairly reliably.

  \code{link.javascript} produces A \code{<script>} tag specifying the
  language as JavaScript and stating the location of the code. For
  example incorporating mathematical expressions uses MathPlayer in
  Internet Explorer, and the JavaScript required is in the file
  \code{ASCIIMathML.js} and the location of this file must be specified.

  The argument \code{head} is useful to add extra HTML code in the
  \code{<head>} header code.  For incorporation of mathematical
  expressions, the following argument value for \code{head} appears to
  to work fairly reliably:
  \code{<object id="mathplayer"
    classid="clsid:32F66A20-7614-11D4-BD11-00104BD3F987"></object>
    <?import namespace="mml" implementation="#mathplayer"?>}

  \code{head.attributes} is useful to add extra attributes in the
  \code{<head>} head code. A description of possible head attributes may
  be found at \url{http://www.w3schools.com/tags/tag_body.asp}.

  \code{body.attributes} is useful to add extra attributes in the
  \code{<body>} body code. A description of possible body attributes may
  be found at \url{http://www.w3schools.com/tags/tag_body.asp}. For
  incorporation of mathematical expressions it is necessary to run the
  JavaScript code. For display in Internet Explorer, the attribute
  \code{onload = "translate()"} is necessary.

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
  \code{newPage} is an extension of \code{newPage} from \pkg{hwriter}
  which allows more flexibility in the construction of the header
  section of the HTML document.
}

\seealso{
  \code{\link[hwriter]{openPage}}
}
\examples{
### The same as the example from openPage, with newPage replacing
### openPage

### Creates a new web page 'test.html' in the R temporary directory
tmpdir <- tempdir()
p <- newPage('test.html', dirname=tmpdir,
              link.css='http://www.ebi.ac.uk/~gpau/hwriter/hwriter.css')
hwrite('Iris example', p, center=TRUE, heading=1)
hwrite('This famous (Fisher\'s or Anderson\'s) iris data set gives the
        measurements in centimeters of the variables sepal length and
        width and petal length and width, respectively, for 50 flowers
        from each of 3 species of iris.', p, class='king')
hwrite(iris, p, row.bgcolor='#ffffaa')
closePage(p)

### Opens a web browser on the web page
if (interactive()) try(browseURL(file.path(tmpdir, 'test.html')))
}
\keyword{interface}
