\name{genFunTimer}
%\alias{genFunTimer}
\alias{timeFun}
\title{Measure the time for calls to a function via \code{trace}}
\description{
  This function arranges to monitor
  all calls (direct or indirect) to the specified function
  and measures the time each call took and stores that value
  which can be accessed at any time via the return value of this
  function.
  This use \code{\link[base]{trace}} to run the
  timing code at the start and end of each call to the function.

  To stop collecting the timings for the function,
  \code{untrace} it.
}
\usage{
timeFun(fun, tmr = genFunTimer(), where = globalenv(), ...)
}
\arguments{
\item{fun}{the function or name of the function to trace}
\item{tmr}{the list of functions closure whose \code{start} and
  \code{end} elements will be used to compute and store the timings.
}
\item{where}{passed to \code{trace}}
\item{\dots}{passed to \code{trace}}
}
\value{
  A list with three elements, allfunctions.
  \item{start}{use with trace to }
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link[base]{trace}}
}
\examples{
tmr = timeFun(model.matrix)
lm(mpg ~ . , mtcars)
tmr$times()
invisible(replicate(10, lm(mpg ~ . , mtcars)))
tmr$times()
}
\keyword{programming}
\concept{code instrumentation}

