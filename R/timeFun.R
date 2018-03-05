genFunTimer =
function()
{
    startTime = NULL
    times = numeric()
    start = function() {
        message("start")
        startTime <<- Sys.time()
    }
    end = function() {
        message("end")
        times[[ length(times) + 1L ]] <<- Sys.time() - startTime
    }

    list(start = start, end = end, times = function() times)
}


timeFun =
function(fun, tmr = genFunTimer(), where = globalenv())
{
  trace(fun, tmr$start, exit = tmr$end, print = TRUE, where = where)
  tmr 
}

if(FALSE) {
tmr = genFunTimer()
#trace(a, tmr$start, print = FALSE)
trace(a, quote({tmr$start(); on.exit(tmr$end())}), print = FALSE)
}
