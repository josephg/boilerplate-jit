exports.Jit = require './jit'
exports.util = require './util'
exports.Watcher = require './watch'

collections = require './collections2'
exports[k] = collections[k] for k in ['Map2', 'Set2', 'Map3', 'Set3']
