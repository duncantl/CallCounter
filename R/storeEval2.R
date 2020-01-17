#library(CodeDepends)
#library(CodeAnalysis)

storeEval =
function(code, envir = globalenv(), verbose = TRUE, store = new.env(),
         outVars = getOutputVariables(code), all = TRUE, ...)
{
    if(is.character(code)) {
        if(file.exists(code))
            code = parse(code)
        else
            code = parse(text = code)
    }

    if(!is.language(code) || is.call(code))
       stop("need a language object")    

     # the number of vars
    numVars = sapply(outVars, length)
    ctrs = structure(rep(1L, length(numVars)), names = names(numVars))
    if(all)
        threshold = 0
    else
        threshold = 1
    im = lapply(code,
                 function(x) {
                    if(verbose)
                       print(deparse(x))
                    obj = eval(x, envir)
                    
                    if(class(x) %in% c("<-", "=")) {
                        var = getVarName(x[[2]])
                        if(numVars[var] - ctrs[var] >= threshold) {
                          varName = paste0(var, ctrs[var])
                          assign(varName, get(var, envir), store) # not obj which could be part of the object, e.g. Best[S, 1] = value would return just the value.
                          ctrs[var] <<- ctrs[var] + 1L
                        }
                    } else
                        ""
                })
    
     store
}



getVarName =
    #
    #
    #
    #
function(expr)
{
    if(is.name(expr))
        return( as.character(expr) )

    if(is.call(expr)) {
        if(is.symbol(expr[[1]]) && (TRUE || as.character(expr[[1]]) %in% c("[", "[[", "$") ))
            return(getVarName(expr[[2]]))
    }

    browser()
}


getOutputVariables =
function(code, info = lapply(code, getInputs))  # as(code, "ScriptInfo")
{
    outs = lapply(info, function(x) c(x@updates, x@outputs))
    bvars = unique(unlist(outs))
    i = lapply(bvars, function(v) which(sapply(outs, function(x) v %in% x)))
    names(i) = bvars
    i
}



compareEnv =
    # version that will work with lists or environments.
function(nw, toy)
{
  vars = intersect(names(nw), names(toy))
  isfun = sapply(vars, function(id) is.function(nw[[id]]))
  vars = vars[!isfun]
  comp = lapply(vars, function(id) all.equal(nw[[id]], toy[[id]]))
  names(comp) = vars
  w = sapply(comp, is.logical)
  comp[!w]
}
