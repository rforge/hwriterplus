\name{hwriteOutput}
\alias{hwriteOutput}
\alias{hwriteScript}
\title{
Write R Output or an R Script in HTML Format
}
\description{
  
\code{hwriteOutput} is a wrapper function for \code{hwrite} which
formats output from R captured by \code{capture.output} so it
appears correctly when \code{hwrite} is called.

\code{hwriteScript} is a very similar wrapper function for \code{hwrite}
which formats an R session captured by \code{script} so it appears
correctly when \code{hwrite} is called. \code{hwriteScript} also
optionally trims lines added at the beginning and end of the session by
\code{script}. 

}
\usage{
hwriteOutput(output, page = NULL, fontSize = "10pt", ...,
             link = NULL, name = NULL, heading = NULL,
             center = NULL, br = NULL, div = NULL)
hwriteScript(scriptFile, page = NULL, fontSize = "10pt", trim = TRUE, ...,
             link = NULL, name = NULL, heading = NULL,
             center = NULL, br = NULL, div = NULL)
}

\arguments{
  \item{output}{Text. Output produced by R, captured by
    \code{capture.output}
  }

  \item{page}{An optional connection. A character string naming the
    file to write to or a page object returned by \code{newPage} or
    \code{openPage}.}

  \item{fontSize}{Character string. The font size in points in which the
    output will be displayed. A positive number followed by the characters
    \code{"pt"} without a space between. Default is \code{"10pt"}.
  }

  \item{scriptFile}{File. Contains text from an R session captured by
    \code{script}.  
  }
  
  \item{trim}{A logical. If \code{TRUE}, deletes the first and last
    two lines of the lines of text returned by \code{script}.    
  }

  \item{\dots}{Optional arguments. See Details section of
    \code{\link{hwrite}} documentation.
  }

  \item{link}{A character vector containing the URLs the HTML element
    will point to. This argument is the equivalent of the attribute
    \code{href} of the HTML tag <a>.
  }

  \item{name}{A character string naming the HTML element for further
    reference. This is the equivalent of the attribute \code{name} of
    the HTML tag<a>.
  }

  \item{heading}{A numeric containing the heading level style. Valid
    values spans from 1 to 5.
  }

  \item{center}{A logical indicating if \code{x} should be
    centered. Default is \code{FALSE}. This element may interfere with the
    current CSS style. Please consider the use the CSS style attribute
    "text-align" instead.
  }

  \item{br}{A logical specifying if a breakline (carriage return) should
    be appended at the end of \code{x}. Default is \code{FALSE}.
  }

  \item{div}{A logical. If \code{TRUE}, places the HTML element into a
    HTML section, using the <div> HTML tag. This is helpful for styling a
    section. Default is \code{FALSE}.
  }
}

\details{The output obtained from R using \code{capture.output} is
  modified by these functions so that it will appear verbatim in the
  resulting HTML file, as preformatted text.

  The output is first modified by adding characters \code{"\n"} to the
  end of each output line. Then preformatting tags are added to surround
  the output. The opening preformatting tag includes a style definition
  which sets the font size to that specified by \code{fontSize}.

  In addition \code{hwriteScript} can trim off the first line and the
  last two lines of the text returned by a call to \code{script}. The
  first and last lines record the start and end times of the call to
  \code{script}. The second last line typically consists of the text
  \code{> q()} comprised of the R prompt and the call to \code{q()}
  which terminates the recording of the session by \code{script}.
  
}
\value{
  A character vector containing the output HTML code.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
}
\seealso{
  \code{\link{hwrite}}
}
\examples{
### Example results written to standard output stream
### Annette Dobson (1990) "An Introduction to Generalized Linear Models".
### Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
plants <- data.frame(weight = weight, group = group)
strOutput <- capture.output(str(plants))
lm.D9 <- lm(weight ~ group, data = plants)
anovaOutput <- capture.output(anova(lm.D9))
hwrite("Analysis of Weight Data", heading = 1,
       center = FALSE, br = TRUE)
hwriteOutput(strOutput, fontSize = "8pt")
hwriteOutput(anovaOutput)

### Same example but written to a file
tmpDir <- tempdir()
fileName <- file.path(tmpDir, 'hwriterPlus.html')
p <- newPage(fileName)
hwrite("Analysis of Weight Data", p, heading = 1,
       center = FALSE, br = TRUE)
hwriteOutput(strOutput, p, fontSize = "8pt")
hwriteOutput(anovaOutput, p)
closePage(p)
### Open a web browser and examine the resulting file
if (interactive()) try(browseURL(fileName))


### Record of session captured by script written to temporary file
\dontrun{tmpDir <- tempdir()
tmpFile <- file.path(tmpDir, 'script.txt')
script(tmpFile)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
plants <- data.frame(weight = weight, group = group)
lm.D9 <- lm(weight ~ group, data = plants)
q()
sessionOut <- readLines(tmpFile)
sessionOut
hwriteScript(tmpFile, br = TRUE)
hwriteScript(tmpFile, trim = FALSE, fontSize = "8pt", br = TRUE)
}
}

\keyword{IO}
\keyword{file}
\keyword{print}