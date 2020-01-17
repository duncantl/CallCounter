\name{storeEval}
\alias{storeEval}
\alias{timeEval}
\alias{seedChangeEval}
\title{Evaluate R code and time, store results or monitor changes to .Random.seed  for each top-level expression}
\description{
  These functions are similar to \code{\link[base]{source}}
  but evaluate each top-level expression and
  do something for each of these.


 \code{timeEval}  
  times how long each expression took to complete.

  \code{storeEval} saves the result of each top-level as an intermediate result
  that we can then compare as we change the code.
  This helps us identify when and where changes
  give different results.

  \code{seedChangeEval} checks whether the value of \code{.Random.seed}
  changed during the evaluation of each top-level expression.
  Again, this can help us debug changes to the code.
}
\usage{
storeEval(code, envir = globalenv(), verbose = TRUE, store = new.env(), outVars = getOutputVariables(code), all = TRUE, ...)
}
\arguments{
  \item{code}{the R code, as a file name or the text or the parsed code}
  \item{envir}{the environment in which to evaluate each top-level expression.}
  \item{verbose}{a logical value controlling whether each expression is
    displayed before it is evaluated.}
  \item{store}{the environment object in which to store/assign the
    intermediate results.}
  \item{outVars}{the names of all the variables for which we want
    intermediate results}
  \item{all}{a logical value that controls whether we also add the final
  versions of each variable to \code{store}, or just the intermediate
  variables.  The final versions are available in \code{envir}.}
  \item{\dots}{ignore at present}
}
\value{
\code{storeEval} returns the \code{store} environment object containing the intermediate results.

\code{timeEval} returns a list of the results from calling
\code{\link{system.time}} when evaluating each top-level expression.

\code{seedChangeEval} returns a list with the code for top-level
expressions grouped into sub-lists according to whether they were
evaluated  with the same value for \code{.Random.seed}.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link[base]{eval}}
  \code{\link[base]{new.env}}  
}
\examples{
f = system.file("sampleCode", "storeEvalEg.R", package = "CallCounter")
im = storeEval(f)


f = system.file("sampleCode", "timeEvalEg.R", package = "CallCounter")
tms = timeEval(f)
sapply(tms, `[`, 3)


f = system.file("sampleCode", "stochastic.R", package = "CallCounter")
iseed = seedChangeEval(f)
}
\keyword{programming}
\concept{code instrumentation}
