timeEval =
function(code, envir = globalenv(), verbose = TRUE, ...)
{
    if(is.character(code)) {
        if(file.exists(code))
            code = parse(code)
        else
            code = parse(text = code)
    }

    times = lapply(code, function(x) {
                            if(verbose)
                                print(deparse(x))
                            system.time(eval(x, envir))
                        })

    names(times) = sapply(code, function(x) paste(deparse(x), collapse = " "))
    times
}
    
