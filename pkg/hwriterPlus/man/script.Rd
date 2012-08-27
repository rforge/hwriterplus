\name{script}
\alias{script}

\title{
Record an R Session
}
\description{
  Records an R session or part session and saves to a file.

  It is an analog of the Unix script(1) command.
}
\usage{
script(file = "transcript.txt")
}
\arguments{
  \item{file}{Character. The name of the text file where the record of
    the session is to be saved.
  }
}
\details{
To start recording (part of) an R session use
\verb{script(\file{filename})}. To finish recording use \code{q()}. See
the Examples section.
}
\value{
  No value is returned. The function exists for the side effect it
  produces, which is the details of an R session recorded in a file.
}

\author{
Ross Ihaka \email{r.ihaka@auckland.ac.nz}
}
\note{
  This is best regarded as an exercise in getting familar with R's
  condition system and a demonstration of how to write an interpreted
  REPL (Read-Eval-Print-Loop).

  FIXME: Nested calls to "script" are not a good idea.  The boxing-glove
  UI element should probably be used to prevent this.

  BUG/Feature: When a parse is interrupted, nothing appears in the
  transcript.  When a command is interrupted, no output appears.
  This could be fixed with a calling handler, or by shifting the
  interrupt catcher inside repl().

  The function \code{txtStart} from \pkg{TeachingDemos} fullfills a
  similar function, but fails to preserve linebreaks on input of R
  commands.
}


\examples{
\dontrun{
### Record of session captured by script written to temporary file
tmpDir <- tempdir()
tmpFile <- file.path(tmpDir, 'script.txt')
script(tmpFile)
### Annette Dobson (1990) "An Introduction to Generalized Linear Models".
### Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
plants <- data.frame(weight = weight, group = group)
lm.D9 <- lm(weight ~ group, data = plants)
q()
sessionOut <- readLines(tmpFile)
sessionOut
}

}

\keyword{ IO }
\keyword{ utilities }
