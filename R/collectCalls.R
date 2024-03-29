
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

e3 = collectCallResults("f3")

f3(10, 11)
f3(999, 777)
k = e3()

# Should be identical since same definition of function
testFunCall(k, f3)

testFunCall(k, function(x, y) x + y + global1)
rm(global1)
testFunCall(k, function(x, y) x + y + global1)

rm(global2)
testFunCall(k, function(x, y) x + y + global1 + global2)

testFunCall(k, f3)


# perversely check not equal!
testFunCall(k, f3, compare = `!=`)

testFunCall(k, function(x, y) 1)
}


if(FALSE) {
collectCallResults =
    # Don't build on collectArgInfo() as that applies op to each argument
    # separately not as the entire list of arguments.
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
}

#########################

collectCallResults =
    #
    # fn - the name of the function whose calls we are  going to collect.
    # globals - either a scalar logical, or a character vector giving the names of the global variables
    #    we should collect in the calls
    # len - a guess as to how many calls we'll see so that we can preallocate the list into which each call will be inserted.
    #   The goal is to avoid growing the list too often which slows down the computations.
    #
    #  XXX handle options, environment variables, graphics parameters.
    #
    #
function(fn, globals = TRUE, options = TRUE, environmentVariables = TRUE, graphicsParameters = TRUE, getState = NULL, len = 1000L, print = FALSE, ...)    
{
        # The list into which we will insert each call and a counter that tells us where to insert the next value
    ctr = 0L
    calls = vector("list", len)

    # Function to insert the arguments, called at the start of the function for each call.
    # This grows the calls list if we reach the end, currently by doubling it.
    update = function(args) {
        if(ctr == length(calls))
            length(calls) = 2*length(calls)


        tmp = list(arguments = args)
        if(length(getState))
            tmp$state = getState()
        
        ctr <<- ctr + 1L
        calls[[ ctr ]] <<- tmp
        ctr
    }

    # Function to add the result the current call.
    updateResult = function(val) 
        calls[[ ctr ]]$.result <<- val




    # Now we create the R code to call update() and updateResult() from within the
    # function. So it has access arguments.
    
    # Create a call to list() of the form  list(a = a, b = b) with a named element for each of the parameters.
    p = names(formals(fn))
    values = call("list")
    values[seq(along.with = p) + 1L] = lapply(p, as.name)
    names(values) = c("", p)


     # Check if we are asked to collect the global variables and if so, identify them if necessary.
    if(is.logical(globals)) {
        if(globals)
            globals = codetools::findGlobals(get(fn), FALSE)$variables
        else
            globals = character()
    }

    # Then add the global variables as an element named .globals and it is a single named list with an element for each
    # of the global variables.
    #   i.e.   .globals = list(global1 = global1, global2 = global2)    
    if(length(globals)) {
        e = call("list")
        e[seq(along.with = globals) + 1L] = lapply(globals, as.name)
        names(e) = c("", globals)
        values$.globals = e
    }

     # Now create calls to the update() and updateResult() functions.  These are in the current call frame
     # so we create calls of the form
     #    e$update(args)
     # and insert the value of e directly into the call as e won't be in the call being traced.
    e = environment(update) # sys.frame(sys.nframe())

    start = substitute(e$update(), list(e = e))
    start[[2]] = values
    end = substitute(e$updateResult(returnValue()), list(e = e))    

      # So now we are ready to use these calls to trace the function.
    trace(fn, start, exit = end, print = print, ...)

      # the return value from this function to access the results.
      # It culls the result if trim is TRUE so that it reclaims memory
    getCalls = function(trim = TRUE, addClasses = TRUE) {
        if(ctr == length(calls))
            ans = calls
        else
            ans = calls[seq_len(ctr)]

        if(addClasses)
            ans = lapply(ans, function(x) {class(x) = "CallResultInfo"; x})
        
        class(ans) = "FunctionCallResults"
        if(trim)
           calls <<- ans

        ans
    }
}

#######################

testFunCall =
function(x, fun, compare = identical, ...)    
    UseMethod("testFunCall")

testFunCall.CallResultInfo =
function(x, fun, compare = identical, envir = globalenv(), ...)    
{
    # Get the result, then the global variables and remove each of these
    # leaving only the arguments.
    result = x$.result
    x = x[-length(x)]

    # If there are global variables collected in this call, then we set them in the 
    gvs = x$.globals
    e = new.env(parent = envir)
    if(length(gvs) > 0) {
        mapply(assign, names(gvs), gvs, MoreArgs = list(envir = e))
        x = x[-length(x)] # remove the .globals
    }

    # what if the new fun has a different set of parameters??

    newResult = do.call(fun, x, envir = e)
    compare(newResult, result, ...)
}


testFunCall.FunctionCallResults =
function(x, fun, compare = identical, ...)    
{
  lapply(x, testFunCall, fun, compare, ...)
}
