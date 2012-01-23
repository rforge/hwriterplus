\name{ref}
\alias{eqRef}

\title{
  Convenience Functions for Retreiving Reference Numbers
}
\description{
  Given a label as a character string, the vector of labels is searched
  and the row number of the matching vector is returned. This is the
  number that has been assigned to the object with the specified label.
}
\usage{
eqRef(label)
}

\arguments{
  \item{label}{Character. The label of the object whose number is sought.
  }
}
\details{
  The function first adds the required prefix (e.g. \code{eq:} for an
  equation) to the supplied label, then uses \code{\link{which}} to find
  the row number in the vector of labels matching that label. Names of
  vectors storing the labels are based on LaTeX names. For equations,
  the vector is called \code{hwriterEquationList} (even though it is a
  vector not a list).
}
\value{
 The number in the list of labels for that type of object which matches
  the label supplied.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
}
\seealso{
  \code{\link{which}}, \code{\link{hwriteLatex}}
}
\examples{
\dontrun{
eqRef{"anequationlabel"}
}
}
\keyword{ IO }
