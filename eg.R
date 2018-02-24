f =
function(x)
{

     0L
}

h =
function(x)    
{
  lapply(x, f)
}

g =
function()
{
   f(10)
}

k =
function(n = 100)
{
   replicate(n, h(vector("list", 10)))
}

    


if(FALSE) {
    ctr =  CallCounter:::genCounter()
    trace(f, ctr$inc, print = FALSE)
    invisible (  k()  )
    ctr$value()


    st = genStackCollector(num = 500)
    trace(f, st$update, print = FALSE)
    invisible (  k()  )
    z = st$value()
}
