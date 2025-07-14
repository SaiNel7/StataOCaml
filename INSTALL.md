The only three outside libraries that are required for this system are the [OCaml csv package](https://ocaml.org/p/csv/latest/doc/Csv/index.html), [ANSITerminal](https://chris00.github.io/ANSITerminal/doc/ANSITerminal/ANSITerminal/index.html), and [Batteries](https://ocaml-batteries-team.github.io/batteries-included/hdoc2/).

You can install Csv, ANSITerminal, and Batteries if you haven't already, via:

```
$ opam update
$ opam upgrade
$ opam install csv
$ opam install ansiterminal
$ opam install batteries
```

Use of our system is simple:

Run the command:

```
dune exec bin/main.exe data/baseball.csv
```

Fill in `baseball.csv` with whatever CSV you wish to use in our program. 

Then you can type in commands like the following:
```
help

hist OPS 5

summary SLG

scatter HR H

quit
```

etc.

Note on testing: running ```dune test``` will take approximately 20-30 seconds to run to completion, depending on the hardware. This is because we test our program speed on large files to ensure viable method efficiency.