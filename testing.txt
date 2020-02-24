--Core Programs for testing the parser:
--1)
main = double 21;
double x = x + x;
g y z s = case a + 1 of <1> a b -> \ g. a b s; <2> -> letrec x=fib; y=fact; z= x*y* (fib fact x y z) * z + y in x y;
f x = h;
h x = (let z = y in x z (a*3));
f a = a + 2 * 4
2)
main = letrec x = x + x; y=z+1; g111 = h*3*x; f12 = case a+1 of <1> -> a+2; <2> -> \a b . a+b
in z (a*3);
f a = h a * 5
3)
main = letrec x = x + x; y=z+1;
g111 = h*3*x;
f12 = case a+1 of <1> -> a+2;
<2> -> \a b . a+b
in z (a*3);
f a = h a * 5;
k a b = Pack{2,1} a;
k1 a b = let a = 2 in letrec b = 4 in a * b *5
4)
main = letrec x = x + x;
y=z+1;
g111 = h*3*x;
f12 = let a= h*3;
b=case a+1 of
<1> -> a+2;
<2> -> \a b . a+b
in z (a*3)
in a*b+c;
f a = h a * 5;
k a b = Pack{2,1} a;
k1 a b = let a = 2 in letrec b = 4 in a * b *5
5)
f a b c = a * b - c | a & b / 2;
g a b = case a of
<1> -> a& b & c;
<2> c d -> a - b * c / d