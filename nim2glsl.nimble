version       = "0.1.2"
author        = "Joel Lienhard"
description   = "transpile subset of nim to glsl"
license       = "MIT"
srcDir        = "src"
namedBin      = {"main": "nim2glsl"}.toTable


requires "nim >= 2.2.2"
requires "fusion"


import std/[strformat, strutils]

task test, "transpile test shaders":
  exec "nimble build"
  mkDir "tests/glsl_output"
  for file in listFiles("tests"):
    let outFilename = block:
      let tmp = file.split('/')[^1].split('.')
      join(tmp[0 ..< ^1], ".") & ".glsl"
    exec &"./nim2glsl -o:tests/glsl_output/{outFilename} {file}"