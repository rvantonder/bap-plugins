# Minos

Minos generates all static paths between a source and sink block. A source
block, for example, could be a block that calls a specific function, such
as a `read()` input function. Alternatively, the source block could also be set
to be the entry block of a function.

A sink block may be defined similarly.

## Static checks

A suite of static checks currently exist for Minos. Here's a list of current
checks. Each check drives Minos in two ways: 1) they define the conditions
under which all static paths should be enumerated (`should_produce`) and
2) they define the check that should be performed on each path (`check_path`).

The `should_produce` predicate usually guards against generating paths that
are either uninteresting (because arguments to functions are constants) or
because a particular trim produces too many paths in the DAG.

#### system

* Associated script: `run-test-system.sh`

Path generation conditions: Input to `system()` is not a constant.
Path check:

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

* Default max number of paths: 1

## Usage

Output is found in the analysis directory which is structured as follows:

```
analysis-test-system-1/
├── cut_groups
│   ├── cuts.txt
│   └── valid_cut_0000
│       └── valid_cut_case_0_%00000162_%00000177.dot
├── meta.txt
├── misc.txt
└── trim_groups
    ├── trim_0000_case_0000
    │   ├── flagged_0000
    │   ├── paths
    │   │   ├── 0000.path
    │   │   └── priority_0002
    │   ├── paths.txt
    │   └── trim_%00000162_%00000177.dot
    └── trims.txt
```

## Comments

Minos is useful when run in conjunction with quarantine. Minos answers the
question "have I covered all paths" and "how many paths are there" for a
given source/sink

## Run scripts


