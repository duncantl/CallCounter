seedChangeEval =
function(code, envir = globalenv(), verbose = TRUE, ...)
{
    if(is.character(code)) {
        if(file.exists(code))
            code = parse(code)
        else
            code = parse(text = code)
    }

    if(!is.language(code) || is.call(code))
       stop("need a language object")    

    names(code) = sapply(code, function(x) paste(deparse(x), collapse = " "))
    
    if(!exists(".Random.seed"))
        set.seed(Sys.time())
    
    curSeed = .Random.seed
    
    changes = sapply(code, function(x) {
                    if(verbose)
                       print(deparse(x))
                    system.time(eval(x, envir))
                    ans = !identical(curSeed, .Random.seed)
                    if(ans)
                        curSeed <<- .Random.seed
                    ans
                })
    split(as.list(code), cumsum(changes))
}
