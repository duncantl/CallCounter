\name{gatherArgInfo}
\alias{gatherArgInfo}
\title{Extract a variable from argument information across calls}
\description{
  This function extracts one piece of information
  from each of the calls collected via
  \code{\link{collectArgInfo}} via \code{\link[base]{trace}}.
}
\usage{
gatherArgInfo(x, var, paramNames = names(x[[1]]))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the list obtained from each call to the function being traced
    and each element is a list corresponding to each parameter/formal
    argument in that function}
  \item{var}{the name of the variable to extract from the list of call
    information. For the detault information function
    \code{getParamInfo},
    this should be one of \code{class}, \code{type}, \code{length},
    \code{nrow} and \code{ncol}.
  }
  \item{paramNames}{the names of the parameters which is typically taken
  from the call information but can be provided by the caller if appropriate.}
}
\value{
A data frame with as many columns as there are parameters/formal
arguments in the function that was traced.
}
\author{
Duncan Temple Lang
}
\seealso{
\code{\link{collectArgInfo}}
}
%\examples{}
\keyword{programming}
