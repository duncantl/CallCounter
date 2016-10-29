
genCounter =
function()
{
    ctr = 0L
    inc = function()
        ctr <<- ctr + 1L
    structure(list(inc = inc, value = function() ctr, reset = function() ctr <<- 0L), class = "CallCounter")
}


genMultiCounter =
function()
{
    ctr = integer()
    inc = function(what) {
             if(what %in% names(ctr))
               ctr[what] <<- ctr[what] + 1L
             else
               ctr[what] <<- 1L
          }
    structure(list(inc = inc, value = function() ctr, reset = function() ctr <<- integer()), class = "CallCounterMultipleFuns")
}


countCalls =
function(obj, counter = genCounter(), env = globalenv(), print = FALSE)
{
    f = counter$inc
    e = substitute(trace(fun, f, print = print), list(fun = deparse(substitute(obj)), f = f, print = print))
    eval(e, env)
    counter
}

countMCalls =
function(..., counter = genMultiCounter(), env = globalenv(), print = FALSE,
         funs = substitute(list(...))[-1])
{
    f = counter$inc
    e = substitute(trace(fun, quote((f)(name)), print = print), list(f = f, print = print))

    for(i in seq(along = funs)) {
        e[[2]] = funs[[i]]
        e[[3]] [[2]][[2]] = as.character(funs[[i]])
        eval(e, env)
    }
    counter
}



if(FALSE) {
 z = genCounter()
 trace(rnorm, z$inc)
 replicate(10, rnorm(1))

 ctr1 = countCalls(dnorm)
 invisible(replicate(10, dnorm(1)))
 ctr1$value()
}


if(FALSE) {
f = function() { trace(dnorm, function() cat("hi\n"))}
f()
dnorm(1)
untrace(dnorm)

f = function(obj) {
     e = substitute(trace(what, function() cat("hi 2\n")), list(what = deparse(substitute(obj))))
browser()     
     eval(e, globalenv())
 }
f(dnorm)
dnorm(1)
}


