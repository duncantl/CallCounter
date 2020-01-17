x = 1:10
fib = function(n) if(n < 2) n else fib(n-1) + fib(n -2)
x.fib = sapply(x, fib)
z = rnorm(length(x))
y = 2*x.fib + z
f1 = lm(y ~ x)
f2 = lm(y ~ x.fib)

