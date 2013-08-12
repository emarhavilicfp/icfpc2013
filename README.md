Emarhavil Heavy Industries -- ICFPC 2013
========================================

Our entry to this year's ICFP contest, like many others (we expect),
uses a brute-force program enumerator as the basic building block. 
Our language of choice is Standard ML.

I. Enumerator.

The enumerator takes as input a maximum size, and generates a list of
all distinct programs with that many or fewer AST nodes. It also
undoubtedly produces multiple nondistinct programs, but it contains
several "peephole" optimizations to attempt to deduplicate. These are:

* Constant zero and one elimination. Expressions such as "shr16 1" and
  "if0 whatever 0 0" and so on need not be emitted since a semantically
  identical smaller program will also be emitted with "0" there instead.

* Identity function elimination. "or foo foo", "xor foo 0", and so on.

* Distributive function elimination. For all binops except plus,
  "binop (op e1) (op e2)" is redundant with "op (binop e1 e2)".

* Rewriting operators. "plus x x" is the same as "shl1 x",
  "shr1 shr1 shr1 shr1 x" is the same as "shr4 x", and so on. This
  caused considerable grief until we realized we shouldn't omit these if
  the other needed operator isn't in the allowed-operators set...

* Constant folds and ifs. If no bound variable appears in an if0's
  condition, or if the folding expression will always end up as zero
  (e.g., "(fold e1 e2 (lambda (x y) (shr16 y))"), it is redundant.

With the full suite of peephole optimizations we were able to reduce the
number of generated programs by about a factor of 2.

The brute-force enumerator can be found in solve/brute.sml.

II. Solver.

The brute solver proceeds by enumerating and pruning in the following
manner:

1. For a given spec (program size, operator set), use the enumerator
   to generate a (possibly very large) candidate set of programs C.

2. Sample random program input bitvectors.  Keep as X only those that
   actually disambiguate at least two functions in C.

   If the procedure for sampling X yields no points, we simply guess
   an arbitrary function from C and let the server point us to a good
   singleton starter for X.  Quite often, though, we cannot sample a
   set of disambiguating inputs X because our remaining functions C
   are all identical, so the arbitrary guess turns out to be a
   solution, and we're done.

3. Query the server with every point in X to get corresponding output
   bitvectors Y.

4. Keep all functions in C consistent with the server's target function
   (i.e. giving the corresponding outputs in Y) on the inputs in X.

5. If |C| = 1, submit the one remaining function.  Otherwise, keep
   narowing by repeating from step 2.

This entire routine is actually run with increasing program sizes in
step 1 as a form of incredibly naive iterative deepening.  This extra
trick works *really* well on large-instance problems.  As it turns
out, at least in this contest, many large functions are representable
in smaller form.

The general solver can be found in solve/brute-solve.sml.

III. Bonus solver.

The bonus solver is a specialized version of the general solver tuned
for the structure of bonus problems. We noticed that all bonus problems
were shaped like:

    (lambda (x) if0 (and1 CONDITION 1) BRANCHTRUE BRANCHFALSE)

...and furthermore that CONDITION, BRANCHTRUE, and BRANCHFALSE were
"approximately" the same size, and none was ever smaller than a certain
minimum size (5) (which implies that none were ever bigger than a
certain other size (9)). So the bonus solver only asks the brute-forcer
for programs within that range of sizes.

The heart of the bonus solver is perhaps the most clever part of our
submission. ... TODO ...
