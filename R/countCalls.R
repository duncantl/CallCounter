
genCounter =
function()
{
    ctr = 0L
    inc = function()
        ctr <<- ctr + 1L
    structure(list(inc = inc, value = function() ctr, reset = function() ctr <<- 0L), class = "CallCounter")
}


genMultiCounter =
function(names = character())
{
    ctr = integer()
    ctr[names] = 0L
    
    inc = if(length(names)) 
              function(what)
                 ctr[what] <<- ctr[what] + 1L
           else
             function(what) {
                 if(what %in% names(ctr))
                     ctr[what] <<- ctr[what] + 1L
                 else
                     ctr[what] <<- 1L
             }


    
    structure(list(inc = inc,
                   value = function() ctr,
                   reset = function(full = FALSE) {
                               if(full)
                                  ctr <<- integer
                               else
                                   ctr[] <<- 0L
                           }),
              class = "CallCounterMultipleFuns")
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
    #
    # Can just create the initial integer() vector with the names of funs
    # and that simplifies the updating as the element is guaranteed to be there. Updated genMultiCounter() also.
    #
    #
function(..., counter = genMultiCounter(funNames), env = globalenv(), print = FALSE,
         funs = substitute(list(...))[-1],
         funNames = sapply(funs, as.character), hide = FALSE)
{
 #    k = sys.call() m = match.call(countMCalls, k)
    
    f = counter$inc
    e = substitute(trace(fun, quote((f)(name)), print = print), list(f = f, print = print))
    #XXX
    if(!missing(funNames) && (missing(funs) || length(funs) == 0))
        funs = funNames


    if(hide) {
        # This could be dangerous as an error with an options(error = recover) in place will
        # just show
        #    Selection:
        # with  no context and the user will have to call sink(). This is too much to know.
        # tryCatch() will ensure the capture.output exit handlers are called first (?)
        # and then we raise the error again.
        # Not ideal.  Is capture.output() sub-optimally designed, i.e., could it be better?
        tryCatch(
            capture.output( capture.output( setMTraces(funs, e, env) , type = "message"), type = "output" ),
            error = function(e) {
                stop(e)
            })

    } else
        setMTraces(funs, e, env)

    counter$value
}

setMTraces =
function(funs, e, env)
{
    for(i in seq(along = funs)) {
        e[[2]] = funs[[i]]
        e[[3]] [[2]] [[2]] = as.character(funs[[i]])
        eval(e, env)
    }
}


if(FALSE) 
hideOutputEval =
function(e, env)
{
    invisible( capture.output( capture.output( eval(e, env) , type = "message"), type = "output" ))
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


