# Taken from Ross Ihaka's example of profiling and vectorizing code.
rw2d1 =
function(n = 100) {
    xpos = ypos = numeric(n)
    for(i in 2:n) {
          # Decide whether we are moving horizontally or vertically.
      delta = if(runif(1) > .5) 1 else -1
      if (runif(1) > .5) {
        xpos[i] = xpos[i-1] + delta
        ypos[i] = ypos[i-1]
      }
      else {
        xpos[i] = xpos[i-1]
        ypos[i] = ypos[i-1] + delta
      }
    }
    list(x = xpos, y = ypos)
}

if(FALSE) {
 Rprof("rw2d.prof")
 invisible(replicate(10, rw2d1(1000)))
 Rprof(NULL)
 tm = summaryRprof("rw2d.prof")$by.self
 rownames(tm)
 library(CallCounter)
   # We have to remove the "" around the function names.
 ctr = countMCalls(funs = gsub('"', '', rownames(tm)))
 invisible(replicate(10, rw2d1(1000)))
 ctr$value()
}
