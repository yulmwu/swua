# Swua

-   LLVM Version: 16.0.0

```bash
$ cargo build

$ swua --help

Usage: swua [OPTIONS] <COMMAND>

Commands:
  compile  Compile Swua source code
  run      Jit compile and run Swua source code
  help     Print this message or the help of the given subcommand(s)

Options:
  -l, --llvm-ir  Print LLVM IR
  -h, --help     Print help
  -V, --version  Print version

$ swua compile -i ./examples/test.swua -o ./build/main.ll
$ clang ./build/main.ll -L ./build -l swua -o ./build/main
$ ./build/main
```
