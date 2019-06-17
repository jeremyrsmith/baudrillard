# baudrillard

Is it a program, or a type? In the end, maybe what's the difference?

This is the research project around symbolic functions that I demonstrated at ScalaDays 2019.
It's pretty rough right now, but you can go in and see how it all works. Not everything is
implemented, but I'll clean it up a bit soon (and write some actual documentation) – I wanted
to get it posted in case anyone (everyone?) was confused by the slides that I rushed through.

[Slides from the talk](https://jeremyrsmith.github.io/baudrillard/slides/)
[Video of the talk](https://portal.klewel.com/watch/webcast/scala-days-2019/talk/12/) (Note: some sub-slides and the REPL animation aren't working correctly in the video player)

# Takeaway

I should have said this more explicitly. The point I was trying to make *wasn't* that you should be doing something like this
in production (this library is just an experimental party trick right now). The point was more to show two examples of
things the type system can do for you – one simple and useful, and the other extreme but interesting. So there's a lot
of middle ground there, and I hope people will think about what useful and practical things they can accomplish by
"forgetting" a little bit less at the type level.

I've had many people ask if the useful and practical example (the schema types) from the talk will be open source.
It won't be, because I think our use of that idea has too much domain-specific stuff embedded in it. The idea is really
simple, and is probably more valuable than any particular implementation. So the code here is just the code I used in
the symbolic functions example. Keep in mind that it's pretty minimal – the DSL is basically just enough to give the
demo that I wanted to give.

# Explanation

Starting with `SymbolicFunction1`, you can go through the ideas that are used. The expression DSL `Expr` is pretty
straightforward, with the difference that we don't "forget" the specific result type of an operation. We keep it in
the type level to build up a tree of operations.

The constructor for `SymbolicFunction` takes a function `Arg[0] => R`. This `R` will be the recursive type that represents
the entire expression tree, with `Arg[0]` in the place of the argument. So the `Unapply` typeclass traverses this tree
and essentially eta-expands the `Arg[0]` away to make a type constructor. It also allows us to "reapply" the (value-level)
expression tree with a value argument – this is pretty much just for the purposes of pretty-printing, as we don't actually
need the value-level expression tree anywhere (we could conceivably elide it and all of the typeclass derivations if so desired).

Once we have that type-level recursive expression, we can manipulate it in various ways with dependent typeclasses.
These are typeclasses which have a type member, such as `Simplify` and `Derivative`. These are an idea from Shapeless,
so there are many explanations of this kind of machinery if you google that. See if you can figure out how it works,
and try adding some more simplify or differentiation rules, adding more expression operations, and so forth – it should
be an interesting exercise!

There are a few things left unfinished – vector extensions to the DSL, and more arities of symbolic functions. See if you
can finish those things! Another interesting exercise!

There's some special machinery for the DSL in the package object – various ways of using `e` and whatnot. Those are just
for the purposes of the nice DSL.

There is some machinery for piecewise functions. It's a bit of a lie, because we can't *actually* differentiate such
a function in this way without a bunch more work (it might not be decidable at the branch points). But the pretty printing
for it looks kind of nice :)

Hopefully there's some interesting food for thought in here – but I'll reiterate; this isn't intended to be a real
solution for auto-differentiation. It's possible that the idea could be expanded for that, but there are probably better
ways to do it, and a lot of things used here will be obsolete when Scala 3 comes out anyway.
