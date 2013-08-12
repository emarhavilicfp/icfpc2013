Emarhavil Heavy Industries -- ICFPC 2013
=======================================

Our entry to this year's ICFP contest, like many others (we expect),
uses a brute-force program enumerator as the basic building block. 

I. Enumerator.

The enumerator takes as input a maximum size, and generates a list of
all distinct programs with that many or fewer AST nodes. It also
undoubtedly produces multiple nondistinct programs, but it contains
several "peephole" optimizations to attempt to deduplicate. These are:

... TODO ...


II. Solver.

... TODO ...


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
