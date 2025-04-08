const
  x = 1
  y = 5

proc main =
  
  var i = 0
  while i < 10:
    i *= 2

  case x + y
  of 0: finalColor = vec4(vec3(0.0), 1.0)
  of 1: finalColor = vec4(1.0)
  else: finalColor = vec4(0.0, 1.0, 0.8, 1.0)