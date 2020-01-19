\name{testFunCall}
\alias{testFunCall}
\title{Call a function and compare the result}
\description{
  This function and its methods uses the inputs
  from an observed call to a function
  to compare the behaviour of a new version
  of that function to determine if it produces
  the same result.
  This uses the arguments and any global
  variables to recreate the call
  and compares the new result to the
  one obtained in the original call.
  
}
\usage{
testFunCall(x, fun, compare = identical, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the \code{FunctionCallResults} or \code{CallResultInfo}
    objects from \code{\link{collectCallResults}}.}
  \item{fun}{the definition of the function with which to compare the
    calls and results in \code{x} to the results from this function.}
  \item{compare}{a function used to compare the results from the
    original call to the result from each of these calls.}
  \item{\dots}{additional arguments passed to the \code{compare} function.}
}
\value{
  Since this is a generic function, the resulting value will be
  different according to the inputs.
  For \code{FunctionCallResults}, this returns a list of values.
  For \code{CallResultInfo}, this returns the value from the
  \code{compare} function.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{collectCallResults}}
}
\examples{
global1 = 10
global2 = 3.1415
f3 = function(x, y)
        x + y + global1 + global2
e3 = collectCallResults("f3")

f3(10, 11)
f3(999, 777)
k = e3()

testFunCall(k, f3)

rm(global1, global2)
testFunCall(k, f3)

testFunCall(k, function(x, y) x + y + global1 + global2)
}
\keyword{programming}
\concept{code insrumentation}
