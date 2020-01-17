genFunTimer =
function()
{
    startTime = NULL
    times = numeric()
    start = function() {
    #    message("start")
        startTime <<- Sys.time()
    }
    end = function() {
    #    message("end")
        times[[ length(times) + 1L ]] <<- Sys.time() - startTime
    }

    list(start = start, end = end, times = function() times, reset = function() times <<- numeric())
}


timeFun =
function(fun, tmr = genFunTimer(), where = globalenv(), ...)
{
    funName = as.character(substitute(fun))
    trace(funName, tmr$start, exit = tmr$end, ..., where = where)
    tmr 
}

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
