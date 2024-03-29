collectCallStacks =
function(fun, op = NULL, collector = genStackCollector(op, ncalls), ncalls = 1000L, print = FALSE, ...)
{
   origFun = fun
   fun = deparse(substitute(fun))
   trace(fun, collector$update, print = print, ...)
   collector$value
}

genStackCollector =
function(fun = NULL, num = 0, expansionFactor = 2)
{
    calls = vector("list", num)
    ctr = 0L
    u = function() {
              ctr  <<- ctr + 1
              if(ctr > length(calls))
                  length(calls) <<- length(calls)*2
              k =  sys.calls()
              f = callNames(k)
              k = k[ 1:(which.max(f == ".doTrace")-1) ]
              calls[[ctr]] <<- if(!is.null(fun)) fun(k) else k
    }

    reset = function() {
        length(calls) <<- ctr
        calls
    }
        
    
    list(update = u,
         value = function(reset = FALSE) {
                  ans = calls[seq(length = ctr)]
                  if(reset)
                      reset()
                  ans
    })

}

callNames =
function(calls)        
{
   sapply(calls, getCallName)
}

getCallName = 
function(call)
{
    if(is.call(call) && is.name(call[[1]]))
        as.character(call[[1]])
    else
        paste(deparse(call), collapse = " ")
}
#calls = list(); sysCalls= function(test) { if(length(test) > 1) calls[[length(calls) + 1]] <<- sys.calls()}
#trace(ifelse, quote(sysCalls(test)), print = FALSE)

