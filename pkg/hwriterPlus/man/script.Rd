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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(file = "transcript.txt") {
            cat("Transcript being recorded in \"",
                file, "\"\n", sep = "")
            transcon = file(file, "w")
            cat("# Transcript started", date(), "\n", file = transcon)
            ##  I'm not sure this is right.  However, it works!
            repeat {
                e = tryCatch(repl(sys.parent(), transcon),
                    interrupt = function(x) { cat("\n") })
                if (is.logical(e))
                    break
            }
            close(transcon)
            cat("Transcript recording finished\n")
            invisible()
        }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
