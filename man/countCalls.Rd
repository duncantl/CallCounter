\name{countCalls}
\alias{countCalls}
\alias{countMCalls}
\title{Count the number of calls to one or more functions}
\description{
  These functions allow one to count the number of calls
  to one or more functions.
}
\usage{
countCalls(obj, counter = genCounter(), env = globalenv(), print = FALSE)
countMCalls(..., counter = genMultiCounter(), env = globalenv(), print = FALSE,
            funs = substitute(list(...))[-1])
}
\arguments{
  \item{obj,\dots}{the functions either as quoted namesto be }
  \item{counter}{the counter object to use, typically created via \code{genCounter}}
  \item{env}{the environment in which to evaluate the call to \code{\link[base]{trace}}}
  \item{print}{passed on to \code{\link[base]{trace}}}
  \item{funs}{an alternative way to specify the functions whose calls
    are to be counted. This allows the functions to be specified
    collectively in a list or character vector rather than individually
    via the \dots mechanism. }
}
\value{
  These function return the counter.
  This is a list with 3 functions
  \item{value}{function to query the current count(s)}
  \item{reset}{function to reset the count(s) to 0}
  \item{inc}{the function called to increment the counter}
}
\author{
Duncan Temple Lang
}
\seealso{
 \code{\link[base]{trace}}
}
\examples{
ctr = countMCalls(rnorm)
replicate(10, rnorm(0))
ctr$value()


ctr = countMCalls(dnorm, rnorm)
replicate(10, dnorm(0))
replicate(7, rnorm(1))
dnorm(1)
ctr$value()
}
\keyword{programming}
