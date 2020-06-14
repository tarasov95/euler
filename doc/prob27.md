Euler discovered the remarkable quadratic formula: n2+n+41

It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39. However, when n=40,402+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,41^2+41+41 is clearly divisible by 41.

The incredible formula n2−79n+1601 was discovered, which produces 80 primes for the consecutive values 0≤n≤79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form: n2+an+b, where |a|<1000 and |b|≤1000

where |n| is the modulus/absolute value of n e.g. |11|=11 and |−4|=4

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.

--------------------------------
1. when n=0 ~> 0^2 + a*0 + b ~> b must be prime
2. when n=1 ~> 1^2 + a*1 + b = next/prev prime ~> 1+a+b=prime ~> a => prime-b-1 while a<1000

verify ex1: b=41 (prime); a=1 (43(prime)-41-1)
verify ex2: b=1601 (prime) a=-79 (1523(prime)-1601-1)

-79=(x-1601-1) ~> x = -79+1601+1
--------------------------------
given a = prime1 - b - 1
when n=2 ~> 2^2 + a*2 + b = prime2 ~> 4 + 2a + b  = prime2
4 + 2(prime1 - b - 1) + b  = prime2 ~> 4 + 2prime1 - 2b -2 + b = prime2 ~> 2 + 2prime1 - b  = prime2
--------------------------------
when b = 2
a = prime - 2 -1 = prime - 1
--------------------------------
2. when n=1 ~> 1^2 + a*1 + b ~> 1 + a + b ~> (abs b) must != (abs (+ a 1))
3. when n=2 ~> 2^2 + a*2 + b ~> 4 + 2a + b ~> (abs b) must != (abs (* 2 (+ a 2)))
4. when n=3 ~> 3^2 + a*3 + b ~> 9 + 3a + b ~> (abs b) must != (abs (* 3 (+ a 3)))
5. when n=4 ~> 4^2 + a*4 + b ~> 16 + 4a + b ~> (abs b) must != (abs (* 4 (+ a 4)))

#For (<= (abs b) 20)
b => (-1 -3 -5 -7 -11 -13 -17 -19 1 3 5 7 11 13 17 19)

1. when b=1 ~> a!=1 a!=-1
2. when b=2 ~> 

