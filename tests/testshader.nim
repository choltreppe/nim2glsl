{.version: 410.}
precision highp float

var
  fragPosition: in vec2
  finalColor: out vec4
  resolution: uniform vec2

const light = vec3(0.5, 2.0, -1.0).normalize


proc rotateX(angle: float): mat3 =
  var s = sin(angle)
  var c = cos(angle)
  return mat3(
    1.0, 0.0, 0.0,
    0.0, c, s,
    0.0, -s, c
  )

proc opSmoothSubtraction(d1, d2, k: float): float =
  var h = clamp(0.5 - 0.5*(d2+d1)/k, 0.0, 1.0)
  return mix(d2, -d1, h) + k*h*(1.0-h)

proc sdSphere(p, r: float): float =
  return length(p) - r

proc sdBox(p, b: vec3): float =
  var q = abs(p) - b
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0)

proc map(p: vec3): float =
  var box = min(
    sdBox(p - vec3(0.0, 0.3, 0.0), vec3(0.45)),
    sdBox(p - vec3(1.5+sin(1.0*3.0)*0.5, 0.3, 0.0), vec3(0.45))
  )
  var ground = opSmoothSubtraction(box, p.y, 0.4)
  return min(ground, box-0.06)

proc calcNormal(pos: vec3): vec3 =
  var e = vec2(1.0,-1.0)*0.5773
  const eps = 0.0005
  return normalize(
    e.xyy*map( pos + e.xyy*eps ) + 
    e.yyx*map( pos + e.yyx*eps ) + 
    e.yxy*map( pos + e.yxy*eps ) + 
    e.xxx*map( pos + e.xxx*eps )
  )

proc main =
  var p: vec2 = (2.0*fragPosition-resolution.xy)/resolution.y
    
  var r0 = rotateX(0.5) * normalize(vec3(p, 2.0))
  var r = vec3(2.0, 2.0, -4.0)
  var d: float
  for i in 0..<1000:
    d = map(r)
    if d < 0.01: break
    r += r0*d
  if d < 0.01:
    var a = max(0.0, dot(light, calcNormal(r)))
    finalColor = vec4(vec3(a), 1.0)