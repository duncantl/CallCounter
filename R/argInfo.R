
# See CodeReview/ in the book.

collectArgInfo =
function(fn, op = getParamInfo, print = FALSE, envir = globalenv(), 
         col = genInfoCollectorFun(op, names(formals(fn, envir))),
         ...)
{
    if(!is.character(fn))
       fn = deparse(substitute(fn))
    
    e = mkArgInfoExpr(get(fn, envir), col$collector)
    trace(fn, e,  where = envir, print = print, ...)
    col$info
}


mkArgInfoExpr =
function(fun, col)
{
    p = names(formals(fun))
    args = lapply(p, as.name)
    e = call("x")
    e[[1]] = col
    i = seq(along.with = p) + 1L
    e[i] = args
    names(e)[i] = p
    e
}

genInfoCollectorFun =
function(op, paramNames)
{    
   info = list()
   f = function(...) 
           info[[ length(info) + 1L ]] <<- lapply(list(...), op)

   list(collector = f,
        info = function(var = character()) {
            ans = structure(info, class = "ArgInfoList")
            if(length(var))
                gatherArgInfo(ans, var)
            else
                ans
        })
}

gatherArgInfo =
function(x, var, paramNames = names(x[[1]]))
{
    if(length(x) == 0)
        return(NULL)


    if(var %in% c("nrow", "ncol")) {
        idx = match(var, c("nrow", "ncol"))
        accessor = function(el) {
            tmp = el[["dim"]]
            if(is.null(tmp))
                NA
            else
                tmp[idx]
        }
    } else
        accessor = function(el) el[[var]]

        
    cols = lapply(paramNames,
                      function(p)
                          sapply(x, function(k) accessor(k[[p]])))

    ans = as.data.frame(cols, stringsAsFactors = FALSE)
    names(ans) = paramNames
    ans
}

getParamInfo =
function(x)
   list(class = class(x), type = typeof(x), length = length(x), dim = dim(x))


