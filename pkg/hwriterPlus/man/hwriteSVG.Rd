\name{hwriteSVG}
\alias{hwriteSVG}

\title{
  Write HTML Code to Include an SVG Image
}
\description{
  Write code to include an SVG image in an HTML document which should
  work with most modern browsers.
}
\usage{
hwriteSVG(image.url, page = NULL, width = 500, height = 500,
          attributes = "border = '0'", ...)
}
\arguments{
  \item{image.url}{A character vector or matrix containing the URL or
    the file path of the SVG image.}

  \item{page}{An optional connection. A character string naming the
    file to write to or a page object returned by \code{newPage} or
    \code{openPage}.}

  \item{width, height}{An HTML length unit (in pixels) specifiying the
    width (respectively height) at which the image should be
    rendered. Must be present if browser being used is Internet
    Explorer. If missing, Firefox will use the default image
    width(resp. height).}

  \item{attributes}{Attributes of the SVG image. See details.}

  \item{\dots}{Optional arguments that will be dispatched to the
    underlying \code{hwrite} call.}
}
\details{
  Inserting workable SVG code into an HTML document is difficult because
  the attributes of the \code{<object>} tag required depend on
  the browser used to view the document. \code{hwriteSVG} uses syntax
  taken from support documentation for the SVG Web project. The example
  given in the sample \code{helloworld.html} is
\preformatted{
<!--[if !IE]>-->
  <object data="../svg-files/helloworld.svg" type="image/svg+xml"
          width="200" height="200" id="mySVGObject"> <!--<![endif]-->
<!--[if lt IE 9]>
  <object src="../svg-files/helloworld.svg" classid="image/svg+xml"
          width="200" height="200" id="mySVGObject"> <![endif]-->
<!--[if gte IE 9]>
  <object data="../svg-files/helloworld.svg" type="image/svg+xml"
          width="200" height="200" id="mySVGObject"> <![endif]-->
  </object>}

  Note that recent versions of Firefox should display SVG images without
  problems, and should not require the specification of height and
  width. Internet Explorer is far less foregiving and both height and
  width values must be given or no image will be
  displayed. Experimentation with appropriate values for the arguments
  may be necessary.

  For attributes which may be included in an \code{<object>} tag, see
  the reference for this tag on the website
  \url{http://www.w3schools.com}.
}
\value{
  A character vector containing the output HTML code.
}
\references{
  \url{http://www.w3schools.com/tags/tag_object.asp}
  \url{http://codinginparadise.org/projects/svgweb/docs/QuickStart.html}
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\seealso{
  \code{\link[hwriter]{hwriteImage}}
}
\examples{
examplesDir <- file.path(system.file(package = 'hwriterPlus'),
                         'examples')
catsSVG <- file.path(examplesDir, "cats.svg")
### Raw html produced
hwriteSVG(catsSVG, page = NULL, height = 600, width = 600,
          center = FALSE, br = TRUE)
### Formatted  a little more nicely
svgHTML <- hwriteSVG(catsSVG, page = NULL, height = 600, width = 600,
          center = FALSE, br = TRUE)
unlist(strsplit(svgHTML, "\\n"))
}
\keyword{IO}
\keyword{file}
\keyword{print}
