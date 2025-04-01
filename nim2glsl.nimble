version       = "0.1.0"
author        = "Joel Lienhard"
description   = "transpile subset of nim to glsl"
license       = "MIT"
srcDir        = "src"
namedBin      = {"main": "nim2glsl"}.toTable


requires "nim >= 2.2.2"
requires "fusion"


task test, "transpile test shaders":
  exec "nimble build"
  mkDir "tests/glsl_output"
  for file in listFiles("tests"):
    exec "./nim2glsl -o:tests/glsl_output " & file