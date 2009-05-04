# Haskell Notes

 * Haskell lets you define your own operators.
 * The empty string is written "", AND IS SYNONYMOUS WITH [] ! How bout them apples!
 * _A string is a LIST (of charachters). You can use ++ and : on strings (and you can use other list stuff too)_
 * A TUPLE IS LIKE A .NET ANONYMOUS TYPE, WIHTOUT THE PROPERTY NAMES. ANYTHING THAT HOLDS A (STRING, INT, BOOL), FOR EXAMPLE, USES THAT TUPLE.

## Chapter 2

### OPERATORS ARE *NOT SPECIAL*! For ex, || is NOT built into the language. It is an ORDINARY FUNCTION.

### Haskell uses NON-STRICT evaluation. It computes the values of expressions at the LAST POSSIBLE MINUTE. It merely makes a "promise" to evaluate the expression when it is needed. (Some similarities to the Source concept I did at RenRe). If that value is NEVER NEEDED, then it will NEVER BE EVALUATED!

		NON-STRICT EVALUATION == LAZY EVALUATION
		
### Lazy evaluation DIRECTLY CAUSES short-circuiting. Pretty cool, actually. It is not evaluating ANYTHING until the very last minute, so it can easily know that there is no need to evaluate the short-circuited part.

		Cool, this makes a ton of sense to me. And conceptually it *is* very similar to why I did what I did at RenRe.

### JUMPING AHEAD: *TypeClasses are like C# generic constraints!!* 

		TypeClasses let you say, "this function works for any a whose type supports the Eq functions" (Eq = equality). That's all. You can get very precise with the constraints (much more granular than you can in C#)

#### They have "opened up" things in a way I have imagined and wished for many times! You can say there is a "Class" called Eq ... if you do, then ALL you do is define the set of operations that make up Eq, with a default implementation. This is like an interface, but with a default implementation. THEN, you can make an "instance declaration", where, for example, you might say, "Here is how the Eq operations are implemented on Employee types". They have "split open" the C# concept of types into separate, more granular "things", allowing for greater reuse.

#### And under the hood, TypeClasses are merely implemented using the same primitive concepts that are available to you: Data declarations and pattern matching.

#### As you would expect, one very common use of TypeClasses is the same kinds of things you see mixins used for in Ruby. Equality, Comparison, Ordering, Enumeration, etc.


