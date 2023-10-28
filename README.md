# BrainFudgit: BrainF**k Compiler

A pretty heavily over-engineered compiler built to test LLVM/Custom JIT/Custom AOT compilation & architecture of a compiler.

Uses the [BrainF**k](https://en.wikipedia.org/wiki/Brainfuck) language definition.

## Usage

```bash
Brainf**k compiler/optimizer/JIT/AOT/interpreter

Usage: brainfudgit.exe [OPTIONS] <FILE> [COMMANDS]...

Arguments:
  <FILE>
          The file to operate on

  [COMMANDS]...
          Possible values:
          - lexer:                Run the lexer
          - parser:               Run the parser
          - bytecode:             Output Bytecode
          - ast-interpreter:      Run the AST as is
          - bytecode-interpreter: Run the bytecode interpreter

Options:
  -o, --optimizations <OPTIMIZATIONS>
          Possible values:
          - constant-folding: Fold calculations into a constant

  -d, --default-runtime-size <DEFAULT_RUNTIME_SIZE>
          [default: 30000]

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

## What is BrainF**k?

There is more info [here](https://en.wikipedia.org/wiki/Brainfuck).  But it's an esoteric programming language built with the goal of being the "smallest possible compiler".

The machine model consists of 2 registers; an instruction pointer, and a data pointer which points to the heap.  The heap contains at-least 30,000 bytes (initialized to 0).  There are also 2 external devices (input & output) connected.

The list of instructions are:
- `>`: Increment the `data pointer` by one
- `<`: Decrement the `data pointer` by one
- `+`: Increment the byte at the `data pointer` by one
- `-`: Decrement the byte at the `data pointer` by one
- `.`: Write the byte at the `data pointer` to the `output device`
- `.`: Read the next byte from the `output device` and write it to the `data pointer`
- `[`: If the byte at the `data pointer` is zero, then jump the `instruction pointer` forward to the instruction after the matching `]`
- `]`: If the byte at the `data pointer` is non-zero then jump the `instruction pointer` back to the instruction after the matching `[`

## Optimizations

We also have quite a few optimizations built into this compiler.
... TODO:

## How does it work?

1. We first tokenizer the input stream
2. This is then parsed into a simple top level AST
3. Then if you are running the ast-interpreter it runs off this AST (it won't continue any later steps if doing this)
4. If optimizations are on (for each the bytecode-interpreter, jit-interpreter, or AOT/LLVM) we then run the optimizer
5. Then we'll run the specified mode (bytecode-interpreter, jit-interpreter, or AOT/LLVM)
