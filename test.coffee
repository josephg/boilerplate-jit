fs = require 'fs'
Jit = require './jit'
assert = require 'assert'
{printJSONGrid} = require './util'

require('./log').quiet = yes

describe 'jit', ->

  describe 'from test data', ->
    files = fs.readdirSync "#{__dirname}/testdata"

    for filename in files when filename.match /\.json$/
      do (filename) -> it filename, ->
        lines = fs.readFileSync("#{__dirname}/testdata/#{filename}", 'utf8').split '\n'

        initial = JSON.parse lines.shift()

        console.log "===== #{filename} Initial:"
        printJSONGrid initial

        s = new Jit initial

        for l,i in lines when l
          expected = JSON.parse l

          s.step()
          actual = s.toJSON()
          try
            assert.deepEqual expected, actual
          catch e
            console.log "====== EXPECTED ====== (step #{i})"
            printJSONGrid expected
            console.log '======  ACTUAL  ======'
            printJSONGrid actual
            throw e

