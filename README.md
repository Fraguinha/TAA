# TAA

This repository contains the ocaml code for the TAA project.

## Setting up ocaml

in order to setup an ocaml instalation we will need ocaml's package manager, `opam`.

This can be done using your distribution's package manager or by installing the binaries directly on your system with the script provided by [ocaml.org](https://opam.ocaml.org/doc/Install.html):

```bash
$ bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
```

after opam is installed, the following commands need to be run:

```bash
opam init
```

followed by

```bash
opam switch create default 4.14.0 # latest ocaml version as of May 15, 2022
```

Thats it, all you need to compile/run ocaml code is installed.

## Compiling/Running the code

in order to compile and execute the ocaml code, we will use ocaml's build system, `dune`.

compiling can be done by running the following command at the root of the repository:

```bash
dune build
```

the code can then be run by executing the following command:

```bash
dune exec bin/main.exe
```
