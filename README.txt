Emarhavil Heavy Industries -- ICFPC 2013
========================================

Our entry to this year's ICFP contest, like many others (we expect),
uses a brute-force program enumerator as the basic building block. 
Our language of choice is Standard ML.

I. Enumerator.
--------------

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
-----------

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
------------------

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
submission.  To start with, a little bit of terminology --

We shall define the main function under test (i.e., the function that we
wish to submit) as F(x), such that:

  F(x) = if0 f(x)
         then g(x)
         else h(x)

The critical insight was that it is possible to separate f(x), g(x), and
h(x) and solve for them "in a vacuum", considering only the function space
FS(fgh), rather than the master function space FS(F) = FS(f) * FS(fh) *
FS(gh).

We do this first by ignoring f(x), and focusing only on g(x) and h(x).  We
enumerate the function space FS(gh), and propose some test vectors, just
like we do with the standard solver.  However, where we would normally
eliminate /any/ function gh(x) that does not match all test vectors, we
instead construct a matrix of functions gh(x) x test vector success TV_i. 
More concretely, consider that we have only four functions, gh_0 through
gh_3, and we have five test vectors, we can construct the following matrix,
where we place a 't' in a field where the function passes the test vector:

     TV_0  TV_1  TV_2  TV_3  TV_4
gh_0    t     t                 t
gh_1          t     t
gh_2    t                  t    t
gh_3    t           t      t    t

We now have a cover problem: we wish to find pairs of functions gh_i and
gh_j for which forall n in TV_n, at least one of gh_i or gh_j passes TV_n. 
Such pairs gh_i and gh_j are "candidate pairs", since they *could* be the
functions g(x) or h(x).

[Sidebar: implementation.

  We implemented the cover problem initially in the well-known
  "dumb-as-nails" fashion.  Under the assumption that "n^2 is okay if you
  have a small enough n", we simply looked at all gh_i and gh_j pairwise,
  filtering only by which would result in an appropriate program size.  This
  worked surprisingly well for small bonus programs, allowing us to clear
  most of the first two smallest quantities.

  As a surprise to none, this did not scale.  We solved this by partitioning
  on the first column.  Any function gh_i that has TV_n failing can *only* be
  paired with a function gh_j that has TV_n passing (although a function
  gh_i that has TV_n passing can be paired with any gh_j at all).  This
  drastically reduced the "n^2-ness"; the cover matrix is incredibly sparse,
  which means that "most" functions will not have TV_n passing for any given
  TV_n.  So, for any function gh_i that has TV_n failing, only a small
  number need be checked; and conversely, there are only a small number of
  functions gh_i that have TV_n passing, and therefore need to cross-check
  all other functions.
  
  This could, presumably, be extended to a larger partition scheme, but
  doing so seems an exercise only in ego improvement, not performance
  improvement.
  
  ]
  
Once we have determined a list of all candidate pairs {g(x), h(x)}, a
critical question remains: what the f?  As it turns out, once we know a bit
vector of which test vectors pass on each function, we can determine a set
of inputs and outputs for what f(x) must satisfy -- essentially,
synthesizing our own test vectors.  Once we have those, we can narrow down
the function space FS(f) to generate all of the candidate f(x)s for any
given pair {g(x), h(x)}.  (This is, of course, not a pure cross product!)

At this point, we now have a list of program-pieces, {f(x), g(x), and h(x);
these can be simply plugged into F(x) (defined above), and iteratively
tested and narrowed down using the algorithm discussed in II.

One pain point was that the bonus solver only saved us from combinatorial
explosion by a few ply; additionally, it has some n^2 components that
dominate.  Future work might include more advanced test-pattern generation,
allowing to narrow the space FS(gh) more aggressively, generating fewer
{g(x), h{x)} pairs.  (The operation that created a list of potential f(x)s
for a {g(x), h(x)} pair was fairly expensive.)  We began down the path of
requesting new test vectors earlier if it appeared that there was to be a
combinatorial explosion; unfortunately, it was not as successful as we had
hoped, and required a lot of recomputation (mainly as an artifact of how we
implemented it).

IV. Team and configuration.
---------------------------

Emarhavil Heavy Industries this year was composed of:

  Ben Blum <bblum@andrew.cmu.edu>; Carnegie Mellon University; Mozilla
                                   Corporation
  Eric Faust <efaustbmo@gmail.com>; Mozilla Corporation
  Glenn Willen <gwillen@nerdnet.org>
  Joshua Wise <joshua@joshuawise.com>; NVIDIA Corporation
  Kevin Murphy <kemurphy.cmu@gmail.com>; NVIDIA Corporation
  Roy Frostig <rf@cs.stanford.edu>; Stanford

Our thanks to Mozilla Corporation, whose space and food were used during the
production of this entry.

Our primary computational resources were:

  a mid-2012 Retina MacBook Pro with four 2.7GHz Ivy Bridge cores and 16GB
  of RAM;

  a custom-built desktop with 32GB of RAM, and four 3.5GHz Haswell cores.

Some consideration was given to using other large machines available, such
as Amazon EC2 resources ($3.50/hr for 244 GiB of memory, and 88 EC2 Compute
units), or the Carnegie Mellon PDL cluster.  Ultimately, we did not find
time to go as far as to scale our algorithms up to the point where they
could make use of such large machines.

Our tools were written primarily in Standard ML, with some glue written in
Ruby for tasks considered too unpleasant for a functional language.  Please
do not ask about which functional language we chose, or why.  It was the
source of great strife.

We used SML/NJ as a REPL for development, but almost all (if not all) server
submissions took place through MLton.  We stumbled across at least one MLton
bug, in which MLton generated executables that would terminate with SIGBUS
in some circumstances when built with -codegen native on amd64/Darwin;
-codegen c was used to work around this.

We considered using MultiMLton to leverage the multicore parallelism
available, but ultimately did not have time to go beyond an exploratory
phase.  I conject that explicit support for exposing memory-level
parallelism would have helped to accelerate this program.
