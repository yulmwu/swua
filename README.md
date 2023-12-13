# Swua

The programming language that compiles to LLVM IR.

```mermaid
graph LR
    A[Source Code] --> C[Parse]
    C --> D[AST]
    D --> E[Type Check]
    E --> F[LLVM Optimize]
    F --> G[LLVM IR Generate]
    G --> H[Clang Build]
    G --> K[JIT execution]
    I[Linking Standard Library] --> H
    I --> K
    H --> J[Object File]
    J --> L[Executable File]
```

# Build and Usage

```bash
$ cargo build
$ swua --help

Usage: swua [OPTIONS] <COMMAND>

Commands:
  run    JIT compile and run Swua source code
  build  Compile Swua source code to native code
  help   Print this message or the help of the given subcommand(s)

Options:
  -o, --optimization-level <OPTIMIZATION_LEVEL>  Optimization level (0-3, default: 0)
      --output-dir <OUTPUT_DIR>                  Build output directory (default: ./build)
  -n, --name <NAME>                              Binary name (default: main)
      --no-verbose                               Don't print verbose information
  -h, --help                                     Print help
  -V, --version                                  Print version

$ swua -n hello build -i ./examples/hello_world.swua -l -a
Compiling ./examples/hello_world.swua (hello) [Unoptimized, Target: aarch64-apple-darwin]
Build Finished in 193 ms, output: ./build/hello

$ ./build/hello
Hello, World!
```

# Syntax

> [!WARNING]
> 
> Rewriting a new parser. the code below doesn't work.

```rust
extern fn print_str(string) -> string;
extern fn concat_str(string, string) -> string;

fn main() -> int {
    print_str(concat_str("Hello, ", "World!"));
    return 0;
}
```

See [examples](./examples) for more details.

> [!NOTE]
>
> Syntax may change in the future. it is currently based on the syntax of Rust and C-like languages.

# Features and TODOs

-   LLVM Version: 16.0.0

-   [x] Frontend
    -   [x] Lexer
    -   [ ] Parser
    -   [x] AST
    -   [x] Type Checker (Semantic Analysis)
-   [x] Backend (WIP)
    -   [ ] Optimizer
    -   [x] LLVM IR Generator
-   [ ] Standard Library (WIP)
-   [ ] Documentation
-   [ ] More examples
-   [ ] More tests

... and more
