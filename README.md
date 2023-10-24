# Swua

The programming language that compiles to LLVM IR.

# Build and Usage

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

# Syntax

```rust
extern fn print(int) -> int;
extern fn print_str(string) -> int;
extern fn print_array(int[], int) -> int;
// extern fn printf(string) -> int;

fn add(a: int, b: int) -> int {
    return a + b;
}

fn main() -> int {
    let arr = [1, 2, 3, 4, 5];
    print_array(arr, 5);

    let r = if add(arr[2], 5) == 8 {
        return "condition: true";
    } else {
        return "condition: false";
    };
    print_str(r);

    return 0;
}
```

> **Note**
>
> Syntax may change in the future. it is currently based on the syntax of Rust and C-like languages.

# Features and TODOs

-   LLVM Version: 16.0.0

-   [x] Frontend
    -   [x] Lexer
    -   [x] Parser
    -   [x] AST
    -   [x] Type Checker (Semantic Analysis)
-   [x] Backend
    -   [ ] Optimizer
    -   [x] LLVM IR Generator
-   [ ] Standard Library
-   [ ] Documentation
-   [ ] More examples
-   [ ] More tests

... and more
