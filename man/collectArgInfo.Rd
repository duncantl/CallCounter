\name{collectArgInfo}
\alias{collectArgInfo}
\title{Trace a function and collect information about each argument}
\description{
  This function is used to trace calls to a function and
  collect information such as the class, typeof, length and dim
  of each argument in the call.
  The results are collected in a list that can then be
  queried using \code{\link{gatherArgInfo}}.
}
\usage{
collectArgInfo(f, op = getParamInfo, envir = globalenv())
}
\arguments{
  \item{f}{the name of the function to be trace, or the function itself}
  \item{op}{the function to be called for each argument in the call to
    get the information of interest.}
  \item{envir}{the environment in which to find the function to be traced}
}
\value{
  This returns a function which can be called to access the results.
  One can pass it the name of a variable which will call
  \code{\link{gatherArgInfo}}
  for you.
}

\author{
Duncan Temple Lang
}
\seealso{
  \code{\link[base]{trace}}
}
\examples{
g = function(a, b = 2){ invisible(list(length(a), class(b))) }
i = collectArgInfo(g)

g(mtcars, 1:10)
g(3, TRUE)
g(lm, matrix(0, 2, 3))
g("xyz", matrix(letters[1:6], 2, 3))

i()
i("class")
}
\keyword{programming}
\concept{instrumentation}
