\name{genStackCollector}
\alias{genStackCollector}
\alias{getCallName}
\alias{callNames}
\title{Generate a closure to use with trace to collect information about the call stack.}
\description{
  This function creates two functions that we use to a)
  collect the call stack for calls to a function using
  \code{\link[base]{trace}}
  and b) access the results.

  \code{callNames} returns the names of the functions being called
  in a call stack.
}
\usage{
genStackCollector(fun = NULL, num = 0, expansionFactor = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{the name of the function that we will \code{trace} to find
    the call stacks}
  \item{num}{a guess as to the number of calls to this function so that
  we can preallocate the \code{list} in which we store the call stack
  for each call to the function}
\item{expansionFactor}{a number that indicates by what factor
  to grow the list for storing the results should we have to extend it.}
}
\value{
  A list with 2 elements - an update function and a value function.
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link[base]{trace}}
}
\examples{
z = genStackCollector(`[`)
trace(`[`, z$update)
mtcars[1, 2]
a = 1:10
a[2:3]
f = function(x) x[x > 4]
f(a)

lm(mpg ~ ., mtcars)

untrace(`[`)

z$value()
sapply(z$value(), length)

callNames(z$value()[[1]])

callNames(z$value()[[3]])

callNames(z$value()[[4]])
}
\keyword{programming}
\concept{instrumentation}

