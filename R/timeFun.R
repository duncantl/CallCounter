genFunTimer =
function()
{
    startTime = NULL
    times = numeric()
    start = function() 
        startTime <<- Sys.time()

    end = function() 
        times[[ length(times) + 1L ]] <<- Sys.time() - startTime

    invisible(list(start = start, end = end, times = function() times, reset = function() times <<- numeric()))
}


timeFun =
function(fun, tmr = genFunTimer(), where = globalenv(), print = FALSE, character.only = FALSE, ...)
{
    funName = if(character.only)
                fun
              else
                as.character(substitute(fun))
    trace(funName, tmr$start, exit = tmr$end, print = print, ..., where = where)
    tmr 
}

timeFunMulti =
function(fun, tmr = genFunTimer(), where = globalenv(), print = FALSE, ncalls = NULL, ...)
{
    if(length(ncalls))
        ncalls = structure(rep(0L, length(fun)), names = fun)
    timers = mapply(timeFun, fun, ncalls, MoreArgs = list(where = where, print = print, ..., character.only = TRUE))    
#    timers = lapply(fun, timeFun, where = where, print = print, ..., character.only = TRUE)
    names(timers) = fun
    function()
        lapply(timers, function(x) x$times())
}



######################################

genMultiFunTimer =
function(funNames, ncalls = NULL)
{
    nfuns = length(funNames)

    startTime = NULL

     # numeric vector won't work. Need the class POSIXct
    reset = function()
        startTime <<- structure(vector("list", nfuns), names = funNames)
    
    reset()
    
    times = structure(vector("list", nfuns), names = funNames)
    
    start = function(fun) {
        startTime[[fun]] <<- Sys.time()
    }
    
    end = function(fun) {
        times[[fun]] [ length(times[[fun]]) + 1L ] <<- Sys.time() - startTime[[fun]]
    }

    list(start = start, end = end, times = function() times, reset = reset)
}


timeFunMulti2 =
function(funNames, tmr = genMultiFunTimer(funNames, ncalls), where = globalenv(), print = FALSE,
         ncalls = NULL,   ...)
{
    for(i in funNames) {

       if(FALSE) {
        start = substitute(fun(funName), list(fun = tmr$start, funName = i))
        end = substitute(fun(funName), list(fun = tmr$end, funName = i))
   } else {
#       force(start); force(end)


           start = quote(f(name))
           start[c(1, 2)] = list(tmr$start, i)
           end = quote(f(name))
           end[c(1, 2)] = list(tmr$end, i)           
       }
#        browser()
        trace(i, start, exit = end, print = print, ..., where = where)
    }
    
    tmr     
}

#lapply(funNames, function(i) {
#          start = substitute(fun(funName), list(fun = tmr$start, funName = i))
#          end = substitute(fun(funName), list(fun = tmr$end, funName = i))
#          trace(i, start, exit = end, print = print, ..., where = where)
#})

##############################





if(FALSE) {

tmr = timeFun(model.matrix)
lm(mpg ~ . , mtcars)
tmr$times()
invisible(replicate(10, lm(mpg ~ . , mtcars)))
tmr$times()


N = 10
xx = replicate(N, rnorm(rpois(1, 4000)),  simplify = FALSE)
a = function(val) {Sys.sleep(1);  median(val [ val > -1 & val < 1 ])}
    
atmr = genFunTimer()
#trace(a, tmr$start, print = FALSE)
#trace(a, quote({tmr$start(); on.exit(tmr$end)}), print = FALSE)
trace(a, atmr$start, exit = atmr$end, print = FALSE)

lapply(xx[1:4], a)

untrace(a)
}
