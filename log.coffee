# This is a simple logging function which prints things a lot prettier than
# console.log in node. The signature is the same.

# The combination of coffeescript + uglify make for a super deoptimized,
# slow-ass logging function (like, 27% total CPU time slow). Just toss it.
log = module.exports = if process.env.NODE_ENV == 'production'
  ->
else
  (args...) ->
    return if log.quiet
    if typeof window is 'object'
      console.log args...
    else
      {inspect} = require 'util'
      f = (a) ->
        if typeof a is 'string'
          a
        else
          inspect a, {depth:5, colors:true}
      console.log args.map(f).join ' '

log.quiet = false
