a = 2.5
x = 1:10
y = a*x + 3

for(i in 2:5)
    x[i] = x[i-1]*3 + x[i]

y = a*x + 4

y = 2*a*x + 4

y[2] = 4
