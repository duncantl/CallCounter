\name{collectCallResults}
\alias{collectCallResults}
\title{Collect arguments, return values and global variables used in
  calls to a function}
\description{
  This function traces a given function and arranges to collect
  all of the arguments and the return value for each call.
  If the function uses global variables, these can also be collected.

  The goal is to be able to use these to test modifications to the
  function to ensure we get the same results.
  So these become unit tests.
}
\usage{
collectCallResults(fn, globals = TRUE, len = 1000L, print = FALSE, ...)
}
\arguments{
  \item{fn}{the function whose calls are to be collected. This should be
  given by name or symbol (character or name), and not computed as it is
  passed to \code{\link[base]{trace}}.}
  \item{globals}{either a scalar logical or a character vector giving
    the names of global variables to also collect with each call.
    The function uses \code{\link[codetools]{findGlobals}} to identify
  the global variables. However, this does not always correctly identify
  global variables (e.g. missing uses before a variable is defined
  locally, including global functions used in, e.g., \code{lapply} calls.)
   One can use the function \code{getGlobals} in
  \code{CodeAnalysis} and pass the names of the global variables it find.
  }
  \item{len}{a positive integer which provides a guess as to how many calls we'll see so that we can preallocate the list into which each call will be inserted.
    #   The goal is to avoid growing the list too often which slows down the computations.}
  \item{print}{passed as the value for \code{\link[base]{trace}}'s
  \code{print} argument.}
  \item{\dots}{additional arguments passed to \code{\link[base]{trace}}.}
}
\value{
A function, which when called, returns the list of the collected call information.
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link[base]{trace}}
}
\examples{
f = function(a, b = 2) a + b
g = function(x)   c(f(x, 3), f(x))     
h = function(x, y)  c(f(x, 3), g(y))     

funNames = c("f", "g", "h")
envs = lapply(funNames, collectCallResults)
names(envs) = funNames

g(10)
h(11, 201)

#########

global1 = 10
global2 = 3.1415
f3 = function(x, y)
        x + y + global1 + global2

k3 = collectCallResults("f3")
f3(10, 20)
f3(101, 9)
}
\keyword{programming}
\keyword{utilities}
\concept{code instrumentation}
\concept{testing code}
