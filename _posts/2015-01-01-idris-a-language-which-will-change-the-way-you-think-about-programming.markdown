---
layout: post
title:  "Meet Idris, a language that will change the way you think about programming"
date:   2015-01-01
---

Most programming languages barely differ from each other apart from superficial syntax differences, but not Idris. Idris, having a type system supporting dependent types and other innovative ideas, opens up a host of possibilities few other language provides.

When we want to ensure that a piece of code works as intended the usual tool we use is testing. Testing provides plenty of benefits, but wouldn't you prefer to encode your tests right in the type signature and let the compiler decide if your code is correct given those constraints? With Idris you can do exactly that.

Let's investigate how by comparing a very simple list operation, append, also called (++) in Haskell and Idris. Append in Haskell has the following type signature:

{% highlight haskell %}
Prelude> :t (++)
(++) :: [a] -> [a] -> [a]
{% endhighlight %}

Sharing most of the syntax and lot of the type system with Haskell, the append for lists looks very similar in Idris:

{% highlight haskell %}
Idris> :t (++)
Prelude.List.(++) : List a -> List a -> List a
Prelude.Strings.(++) : String -> String -> String
Prelude.Vect.(++) : Vect m a -> Vect n a -> Vect (m + n) a
{% endhighlight %}

When we have a look at the Prelude.List.(++), we can see that it matches the Haskell version - it tells us that it takes two lists and returns a third one - it doesn't say a single word about the lengths of those lists. But if we look at append for the Prelude.Vect type (vectors are a kind of lists with a size), we see the signature describing the size of the vector: 

{% highlight haskell %}
Prelude.Vect.(++) : Vect m a -> Vect n a -> Vect (m + n) a
{% endhighlight %}

In dependent type terminology a vector is a list indexed over its length, or in laymen's terms the type of a vector is a list with a size. This is remotely similar to the concept of fixed sized arrays, (for example, in Go) a [4]int is not the same type as a [5]int. However in Idris, we can not only describe that those lists have different type, but as we can see above we can also express how a given property - in this case the length of a list - changes as a result of a function. As <a href="http://en.wikipedia.org/wiki/Formal_verification">Wikipedia succintly states</a>:

> A promising type-based verification approach is dependently typed programming, in which the types of functions include (at least part of) those functions' specifications, and type-checking the code establishes its correctness against those specifications.

To understand the importance of this it is not enough to inspect the the above functions' type signature. What meets the eye there could be considered documentation. But the real epiphany comes when we start to implement a function after specifying its signature. Let's examine the source code of <a href="https://github.com/idris-lang/Idris-dev/blob/43127b17a765dbab2e7bcb6be2f6f2efc7a42386/libs/base/Data/VectType.idr#L181">append</a>:

{% highlight haskell %}
||| Append two vectors
(++) : Vect m a -> Vect n a -> Vect (m + n) a
(++) []      ys = ys
(++) (x::xs) ys = x :: xs ++ ys
{% endhighlight %}

But what happens if we intentionally break the implementation, for example by appending the first argument to itself and ignoring the second argument entirely? The resulting Vector will have a length of <b>m + m</b>, which is clearly different than the <b>m + n</b> stated in the signature.

{% highlight haskell %}
import Data.Vect

vapp : Vect n a -> Vect m a -> Vect (n + m) a
vapp Nil       ys = ys
vapp (x :: xs) ys = x :: vapp xs xs
{% endhighlight %}
(<a href="https://github.com/idris-lang/Idris-dev/blob/43127b17a765dbab2e7bcb6be2f6f2efc7a42386/test/tutorial006/tutorial006a.idr">link</a>)

The compiler tells exactly what we suspected:

{% highlight haskell %}
Type checking ./vapp.idr
vapp.idr:5:23:When elaborating right hand side of vapp:
When elaborating argument xs to constructor Prelude.Vect.:::
        Can't unify
                Vect (n + n) a
        with
                Vect (plus n m) a
        
        Specifically:
                Can't unify
                        plus n n
                with
                        plus n m
Metavariables: Main.vapp
{% endhighlight %}

The above example shows an interesting quality of Idris' type system: the correctness of the code is checked against any specification in the functions' type. If the implementation does not meet the specification a compile error is triggered. Naturally, any property not described by those specifications will not interest the compiler. Let's say we implement our 'vapp' function the following way:

{% highlight haskell %}
vapp1 : Vect n a -> Vect m a -> Vect (n + m) a
vapp1 Nil               ys = ys
vapp1 (x :: x' :: xs)   ys = x :: x :: vapp xs ys
vapp1 (x :: xs)         ys = x :: vapp xs ys
{% endhighlight %}

This compiling program almost works perfectly but it does have a quirk; the first element from the first argument is present twice, once on the 1st position correctly, and on the second position incorrectly, deleting the second element:

{% highlight haskell %}
*vapp> vapp1 [1,2,3] [4,5,6]
[1, 1, 3, 4, 5, 6] : Vect 6 Integer
{% endhighlight %}

So we have proven that whatever is stated in the type signature is guaranteed to be correct - but nothing else may be. In our append example we state that 'given vector a and vector b, calling append with those vectors as arguments will yield a vector with a length of length a + length b'. It does not state that the resulting vector will contain any element from the argument vectors, or if the order is preserved or anything else! But this is pretty obvious since how could the compiler decide whether a piece code is correct or not if we do not tell it the criteria to judge a piece of code by. Mind reading based programming is not invented yet, unfortunately.

To say the least, dependent types have the potential to become a new useful tool in a programmer's toolbox - and there is a certain idealistic beauty in building on top of formally verified, guaranteed to be correct code.

Idris version used to compile code snippet(s):

{% highlight haskell %}
$ idris -v
0.9.14.2-git:45de9ab
{% endhighlight %}


