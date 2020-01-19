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
collectArgInfo(f, op = getParamInfo, print = FALSE, envir = globalenv(),
               col = genInfoCollectorFun(op, names(formals(f, envir))),
               ...)
}
\arguments{
  \item{f}{the name of the function to be trace, or the function itself}
  \item{op}{the function to be called for each argument in the call to
    get the information of interest.}
  \item{print}{a logical value passed to \code{\link[base]{trace}}
     to control whether it emits a message each time the traced function is called.}
  \item{envir}{the environment in which to find the function to be
    traced}
  \item{col}{a list of functions used in the \code{trace} action.
   This is a parameter so that callers can create it before calling this
  function and manipulate the functions and their enviroment to
  customize the \code{trace} actions. For example, we use this in
  \code{\link{collectCallResults}} to put the results in the same
  environment  as the arguments in all calls. }
  \item{\dots}{passed directly to \code{\link[base]{trace}} allowing the
  caller to, for example, add an \code{exit} or \code{at} argument.}
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
