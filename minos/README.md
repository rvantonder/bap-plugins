# Minos

Minos generates all static paths between two program points in a program.
Currently, these program points are callsites of a source and sink function,
and will later be extended to handle any program point (tid in BAP).
A source block, for example, could be a block that calls a specific function,
such as a `read()` input function. Alternatively, the source block could also
be set to be the entry block of a function.

Minos achieves static path enumeration by first processing an entire program
graph into subgraphs with respect to source and sink, and further transforms
these subgraphs into DAGs by removing back edges.

## Static checks

A suite of static checks currently exist for Minos. Here's a list of current
checks. Each check drives parameterizes Minos in the following ways:

1. The conditions under which all static paths should be enumerated are encoded in
the `should_produce` function.

2. The checks that should be performed are encoded in the `check_path` function.

3. Direction: whether paths should be enumerated from a sink to a source, or
   vice versa

4. The maximum number of blocks that should be considered for a path (-1
   implies maximum)

The `should_produce` predicate usually guards against generating paths that
are either uninteresting (because arguments to functions are constants) or
because a particular trim produces too many paths in the DAG.

#### A sample check : system

Associated script: `run-test-system.sh`

1. Path generation conditions: Input to `system()` is not a constant (string).

2. Path check predicates:

a) Input is symbolic
b) sprintf or snprintf occurs before the call
c) there is no predicate that the argument is data dependent on
d) the argument dependence does not extend beyond this block

Read the following as "Priority 1 is output if a,b,c,d is satisfied"

* 1 -> a,b,c,d
* 2 -> a,b,c
* 3 -> a,b
* 4 -> a,c
* 0 -> a

3) Path direction: Reverse

4) Default depth of path: 100

5) Default number of paths: 30

## Options

Here are some of the more important options:

* `with_dots` : output dot files of cuts and trims, highlighting the
source and sink blocks. A dot output of a cut will give show you the scope
(context) in which the source and sink block lives, in terms of subroutines. A dot output
of trims will show you the cut with all unreachable nodes between the source and sink
block trimmed out.

* `cuts_only` : produce only the cut groups between a source and sink.

* `trims_only` : produce only trims between a source and sink, after producing cut groups

* `path_counts_only` : print out a path counter as paths are enumerated. Do not process
or analyze the paths

* `output_dot_path` : output a dot of the trim, with the path highlighted. Warning:
this is typically very expensive to run. Useful for debugging.

* `out_dir` : specify an output directory. The default is `./analysis`.

## Usage

Output is found in the analysis directory which is structured as follows
(produced with the `with_dots` option):

```
analysis-test-system-1/
├── cut_groups
│   ├── cuts.txt                                       // summary of cut groups
│   └── valid_cut_0000                                 // instance of cut group
│       └── valid_cut_case_0_%00000162_%00000177.dot   // dot output of cut group
├── meta.txt                                           // overall meta information of this run
├── misc.txt                                           // miscellaneous analysis output
└── trim_groups
    ├── trim_0000_case_0000                            // instance of trim
    │   ├── flagged_0000                               // whether this trim satisifed 'should_produce'
    │   ├── paths                                      // directory with instances of paths produced
    │   │   ├── 0000.path                              // BIR output of path
    │   │   └── priority_0002                          // priority for checking from `check_path`
    │   ├── paths.txt                                  // summary of path information (number of paths)
    │   └── trim_%00000162_%00000177.dot               // dot output of trim
    └── trims.txt                                      // summary of trims
```

## Comments

Minos is suitable for a variety of tasks. For instance, it can answer broad
questions such as:

* How many unique pairs of calls to function X and Y are reachable within a program?
* How many paths exist between reachable pairs of calls to function X and Y in the program? (with respect to a DAG, discounting loops and recursive calls)

It can be used to perform further specific queries such as:

* What is the nature of arguments passed to the `system` call in the program? Are they
  constant strings, or symbolic (stored in a register and known only at runtime)?

* What is the nature of the length argument passed to the `memcpy` call in the program?
Is it a constant value, or symbolic? Is this value checked?

The example queries allude to the fact that Minos currently supports additional
features, such as:

a) Resolving strings in binaries which are referenced by constants
b) Inferring arguments to a number of libc functions
c) Constant propagation and constant folding on paths
d) Determining data dependence of register (i.e. not pointer) arguments to libc
functions
e) Indirect call resolution along paths when possible through constant folding

## Run scripts


## Example

A wonderful example of this in operation is running paths from the `system`
sink backwards, with a given depth and given amount of path samples.

With the following settings:

```
let check : (Check.t) =
  let max_depth = 100 in
  let sample = 10 in  (** Note sample size of 10*)
  let timeout = 3 in
  let reverse = true in
  {should_produce; run;
   reverse;
   max_depth;
   sample;
   timeout}
```
we get the following output:

```
Found system argument "/bin/dslmode"
Found system argument "/bin/ping -f -c 2 google.com"
Found system argument "/bin/ping -f -c 2 216.109.112.135"
Found system argument "/bin/ping -f -c 2 www.linksys.com"
Found system argument "/bin/ping -f -c 2 66.94.234.13"
Found system argument "reboot"
```

Increasing the sample size to `100` results in the same output. But increasing
it to `1000` results in additional calls being detected (not necessarily to
system, but you get the point)

```
Found system argument "/bin/dslmode"
Found system argument "/bin/ping -f -c 2 google.com"
Found system argument "/bin/ping -f -c 2 216.109.112.135"
Found system argument "/bin/ping -f -c 2 www.linksys.com"
Found system argument "/bin/ping -f -c 2 66.94.234.13"
Found system argument "reboot"
s[n]printf arg: "%02X:%02X:%02X:%02X:%02X:%02X"
s[n]printf arg: "%02X:%02X:%02X:%02X:%02X:%02X"
s[n]printf arg: "%d"
s[n]printf arg: "%d"
```
