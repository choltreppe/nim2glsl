## nim2glsl

`nim2glsl` is a transpiler that allows you to write shaders in nim syntax.
The goal is not to allow you to write shaders that can be tested on the cpu,
but to stay as close to glsl as possible, just with the nim syntax you are used to write (and with kinda type-inference).

The type inference not realy type inference it is more like bestefford type-guessing. It just assumes you dont have any type-errors and on that assumption predicts what should be the type, because this makes things alot simpler and if there is a type-error the openGL compiler will tell you anyways.
To make those openGL type-errors easier to locate `nim2glsl` does its best to put the generated code exactly in the same line as in the source.
If the "type-guessing" cant guess the type or guesses it wrong, you can always annotate the type explicitly

**Have a look at** [tests/testshader.nim](https://github.com/choltreppe/nim2glsl/blob/main/tests/testshader.nim) for an example of the syntax

there are still alot of builtin functions (and types) missing. Mainly for texture stuff (because I dont need them yet), but they are easy to add, and atleast the missing functions arent a big problem because they just make the type-guessing fail. so you just need to annotate explicitly.

### usage

to transpile a shader just do
```bash
nim2glsl path/to/shader.nim
```

to use a different output path
```bash
nim2glsl -o:path/to/output.glsl path/to/shader.nim
```

and to just print transpiled shader to `stdout`
```bash
nim2glsl --stdout path/to/shader.nim
```
