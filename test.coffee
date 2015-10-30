fs = require 'fs'
Jit = require './jit'
assert = require 'assert'
{printJSONGrid} = require './util'

require('./log').quiet = yes


assert.gridEquals = (expected, actual, step) ->
  try
    @deepEqual expected, actual
  catch e
    console.log "====== EXPECTED ====== (step #{step})"
    printJSONGrid expected
    console.log '======  ACTUAL  ======'
    printJSONGrid actual
    throw e



describe 'jit', ->

  describe 'from test data', ->
    files = fs.readdirSync "#{__dirname}/testdata"

    for filename in files when filename.match /\.json$/
      do (filename) -> it filename, ->
        lines = fs.readFileSync("#{__dirname}/testdata/#{filename}", 'utf8').split '\n'

        initial = JSON.parse lines.shift()

        #console.log "===== #{filename} Initial:"
        #printJSONGrid initial

        s = new Jit initial

        for l,i in lines when l
          expected = JSON.parse l

          s.step()
          actual = s.toJSON()
          assert.gridEquals expected, actual, i

  describe 'from gendata', ->
    if !fs.existsSync "#{__dirname}/gendata"
      console.warn 'Note: Skipping checking previously generated data - data not found. Use gendata to generate'
      return

    files = fs.readdirSync "#{__dirname}/gendata"
    for filename in files when filename.match /\.json$/
      do (filename) -> it filename, ->
        lines = fs.readFileSync("#{__dirname}/gendata/#{filename}", 'utf8').split '\n'

        for l,i in lines when l
          {initial, steps} = JSON.parse l
          jit = new Jit initial
          for expected in steps
            jit.step()
            assert.gridEquals expected, jit.toJSON(), i

  after ->
    console.log Jit.stats
