
# The idea here is to trace all calls to a given function
# and to capture the arguments and the result and store them
# so we can use these as tests.
# Of course, this will only work for deterministic functions.

#
# In collectArgInfo() and its helper functions, we have code to 
#  process the arguments. We could use that directly.
#


# Have to handle returns from within if() statements and not the final

# And we need the result.

# We need to capture the global variables as well as the arguments.


if(FALSE) {

f = function(a, b = 2) a + b
g = function(x)   c(f(x, 3), f(x))     
h = function(x, y)  c(f(x, 3), g(y))     

col = collectArgInfo(f, function(...) list(...))

results = list()
col = collectArgInfo(f, function(...) list(...), exit = quote(results[[ length(results) + 1L ]] <<- returnValue()))
g(10)
h(11, 201)

length(col())
length(results)


funNames = c("f", "g", "h")
envs = lapply(funNames, collectCallResults)
names(envs) = funNames

g(10)
h(11, 201)
}

if(FALSE) {

global1 = 10
global2 = 3.1415
f2 = function(x)
    x + global1
f3 = function(x, y)
        x + y + global1 + global2
    
}

collectCallResults =
function(fn, globals = TRUE, ...)    
{
    op = function(...) list(...)    
    if(is.logical(globals)) {
        if(globals)
            globals = codetools::findGlobals(get(fn), FALSE)$variables
        else
            globals = character()
    }

    
    if(length(globals)) {
#        browser()        
        e = call("list")
        e[seq(along.with = globals) + 1L] = lapply(globals, as.name)
        names(e) = c("", globals)
        body(op)[[3]] = e
        names(body(op)) = c("", "", ".globals")
    }
        

    col = genInfoCollectorFun(op, names(formals(fn)))
    e = environment(col$collector)


    e$results = list()
    ex = function() results[[ length(results) + 1L ]] <<- returnValue()
    environment(ex) = e
    col2 = collectArgInfo(fn, col = col, exit = substitute(fun(), list(fun = ex)), ...) # , .setTrace = .setTrace)
    e
}




collectCallResults =
function(fn, globals = TRUE, ...)    
{
    op = function(...) list(...)

    calls = list()
    
    op = function() {
        k = list()
        calls[[ length(calls) + 1L ]] <<- k
    }

    ex = function() {
        calls[[ length(calls) ]]$.result <<- returnValue()
    }
    
#browser()
    p = names(formals(fn))
    values = call("list")
    values[seq(along.with = p) + 1L] = lapply(p, as.name)
    names(values) = c("", p)
    
    if(is.logical(globals)) {
        if(globals)
            globals = codetools::findGlobals(get(fn), FALSE)$variables
        else
            globals = character()
    }

    
    if(length(globals)) {
        e = call("list")
        e[seq(along.with = globals) + 1L] = lapply(globals, as.name)
        names(e) = c("", globals)
        values$.globals = e
    }

    body(op)[[2]][[3]] = values
browser()
    start = substitute(fun(), list(fun = op))    
    end = substitute(fun(), list(fun = ex))
    trace(fn, start, exit = end, print = FALSE)
    
    function()
        calls
}


#########################

collectCallResults =
function(fn, globals = TRUE, len = 1000L, ...)    
{
    op = function(...) list(...)

    calls = list()
    
    op = function() {
        k = list()
        calls[[ length(calls) + 1L ]] <<- k
    }

    ex = function() {
        calls[[ length(calls) ]]$.result <<- returnValue()
    }

    ctr = 0L
    calls = vector("list", len)
    update = function(args) {
        if(ctr == length(calls))
            length(calls) = 2*length(calls)
        ctr <<- ctr + 1L
        calls[[ ctr ]] <<- args
        ctr
    }
    
    updateResult = function(val)
        calls[[ ctr ]]$.result <<- val



    # Create a call to list() of the form
    # list(a = a, b = b)
    p = names(formals(fn))
    values = call("list")
    values[seq(along.with = p) + 1L] = lapply(p, as.name)
    names(values) = c("", p)

    # Then add the .globals = list(global1 = global1, global2 = global2)
    if(is.logical(globals)) {
        if(globals)
            globals = codetools::findGlobals(get(fn), FALSE)$variables
        else
            globals = character()
    }

    if(length(globals)) {
        e = call("list")
        e[seq(along.with = globals) + 1L] = lapply(globals, as.name)
        names(e) = c("", globals)
        values$.globals = e
    }

    e = environment(update) # sys.frame(sys.nframe())

    start = substitute(e$update(), list(e = e))
    start[[2]] = values
    end = substitute(e$updateResult(returnValue()), list(e = e))    

    trace(fn, start, exit = end, print = FALSE)
    
    getCalls = function() 
        calls[seq_len(ctr)]

}
