<div align="center">

# Swua Programming Language

### Small, insignificant programming language using LLVM as backend. 😃

**English** | [한국어](./README.ko-KR.md) | [Examples](./examples) | [License](./LICENSE)

</div>

# Table of Contents

-   [Overview](#overview)
-   [Usage](#usage)
-   [Tutorial](#tutorial)
-   [Standard Library](#standard-library)
-   [Features and TODO](#features-and-todo)

# Overview

It's not ready yet. translating and reading [Korean documents](./README.ko-KR.md) is also a good option. 😉

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

# Usage

## Build from source

# Tutorial

# Standard Library

# Features and TODO
