First of all I'd do a brief tour to the way I organized everything there.
:
I'm starting imandra in this ("CME") folder. All the model-related things
(things that you want to `:load`) are in the "Model" subfolder.  Printers and
helpers (things that you want to `:load_ocaml`) are in the "Printers"
subfolder. All the toplevel imandra scripts (like `testgen.ml` ones) are all in
the "ImandraScripts". There are alsoia bunch of folders related to binary
representation reading/writing.

In order to reproduce the first problem you can jsut do this:

    :load ImandraScripts/stuck.ml

On my machine it gets stuck at the test case #14. I've waited for an hour to
make sure it is really really stuck. The culprit is the `lx86cl64` process
eating 99% of the CPU and 537GB of memory. Denis has reported similar behavior
(it was the test case #23 for him).


