# This is a simple logging function which prints things a lot prettier than
# console.log in node. The signature is the same.

log = module.exports = (args...) ->
  if require
    {inspect} = require 'util'
    f = (a) ->
      if typeof a is 'string'
        a
      else
        inspect a, {depth:5, colors:true}
    console.log args.map(f).join ' '
  else
    console.log args...

