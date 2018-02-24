When profiling code, the Rprof() function tells you how much time was spent in each function.
Those functions that consume the most time are the most obvious candidates for making faster.
However, it is also useful or important to know how often these functions were called.
If we have two functions A and B, each consuming 60 minutes in the profiling, but A is
called once and B is called 6 million times, often we might focus on A.

To count the number of times a particular function is called, we can use trace(), e.g.,
```
trace(attr, incrementAttrCounter)
```
incrementAttrCounter would be a function that adds one to a variable
that we use to count the number of times attr() is called.
Even better, we define a closure to create a generic, reusable counter
```
mkCounter =
function()
{
   ctr = 0L
   inc = function() ctr <<- ctr + 1L
   list(inc = inc, 
        value = function() ctr, 
        reset = function() ctr <<- 0L)
}
```
Now we can use 
```
attrCtr = mkCounter()
trace(attr, attrCtr$inc)
```
Then we run our code and when it finishes, we query the number of times attr() was called
with
```
attrCtr$value()
```


If we want to count the number of calls to several functions, we can create
a separate counter closure for each.  Alternatively, we can create a single
counter that keeps the counts for all of the functions in a named vector.
Instead of passing the incrementing function to trace(), we call the
incrementing function with the name of the function and then update the corresponding
named element in our vector of counts, e.g.,
```
trace(dnorm, quote(ctr$inc("dnorm")))
trace(rnorm, quote(ctr$inc("rnorm")))
```


Rather than have the user explicitly create the counters
and call trace(), this package provides higher-level functions
to do these steps for you.
For counting calls to a single  function, use `countCalls()`, e.g.,
```
ctr = countMCalls(rnorm)
replicate(10, rnorm(0))
ctr$value()
```

For counting calls to multiple functions, use `countMCalls()`, e.g.,
```
ctr = countMCalls(dnorm, rnorm)
replicate(10, dnorm(0))
replicate(7, rnorm(1))
dnorm(1)
ctr$value()
```

```
ctr = countMCalls(funs = c("dnorm", "rnorm"))
replicate(10, dnorm(0))
replicate(7, rnorm(1))
dnorm(1)
ctr$value()
```


See inst/profilingEg/  for an example of how to use this with profiling information.

