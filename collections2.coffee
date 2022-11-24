{inspect} = require 'util'
assert = require 'assert'
exports.Set2 = Set2 = require 'set2'

Map::getDef = WeakMap::getDef = (k) ->
  v = @get k
  if !v?
    v = @default k
    @set k, v
  return v

Set::map = (fn) ->
  result = new Set
  @forEach (x) ->
    result.add fn(x)
  result

exports.Map2 = class Map2 extends require 'map2'
  constructor: (data) ->
    if typeof data is 'function'
      super()
      @default = data
    else
      super data

  # Get default value
  getDef: (k1, k2) ->
    v = @get k1, k2
    if !v?
      @set k1, k2, v = @default k1, k2
    v

  # This code is all written assuming forEach returns arguments in the sensible order
  forEach: (fn) ->
    super (v, k1, k2) -> fn k1, k2, v

exports.Map3 = class Map3 # A map from (a,b,c) -> d instead of just a->d
  constructor: (data) ->
    @map = new Map
    @size = 0
    if typeof data is 'function'
      @default = data
    else if data
      for [k1, k2, k3, v] in data
        @set k1, k2, k3, v

  get: (k1, k2, k3) ->
    l1 = @map.get k1
    l2 = l1.get k2 if l1
    v = l2.get k3 if l2
    if !v? and @default
      @set k1, k2, k3, v = @default k1, k2
    v

  has: (k1, k2, k3) ->
    l1 = @map.get k1
    l2 = l1.get k2 if l1
    return l2?.has(k3) or false

  set: (k1, k2, k3, v) ->
    l1 = @map.get k1
    if !l1
      l1 = new Map
      @map.set k1, l1

    l2 = l1.get k2
    if !l2
      l2 = new Map
      l1.set k2, l2

    @size -= l2.size
    l2.set k3, v
    @size += l2.size
    return this

  delete: (k1, k2, k3) ->
    l1 = @map.get k1
    l2 = l1.get k2 if l1
    if l2
      deleted = l2.delete k3
      @size-- if deleted
      return deleted
    else
      return no

  forEach: (fn) ->
    @map.forEach (l1, k1) ->
      l1.forEach (l2, k2) ->
        l2.forEach (v, k3) ->
          fn k1, k2, k3, v

  clear: -> @map.clear()

  inspect: (depth, options) ->
    return "[Map3 (#{@size})]" if depth < 0
    return '{[Map3]}' if @size is 0
    entries = []
    @forEach (k1, k2, k3, v) ->
      entries.push "(#{inspect k1, options},#{inspect k2, options},#{inspect k3, options}) : #{inspect v, options}"
    assert entries.length == @size
    "{[Map3] #{entries.join ', '} }"


exports.Set3 = class Set3
  constructor: (data) ->
    @map = new Map
    @size = 0
    if data
      for [v1, v2, v3] in data
        @add v1, v2, v3

  has: (v1, v2, v3) ->
    l1 = @map.get v1
    l2 = l1.get v2 if l1
    return l2?.has(v3) or false

  # # Does it have
  # has2 = (v1, v2) ->
  #   l1 = @map.get v1
  #   return l1?.has(v2) or false
  #
  add: (v1, v2, v3) ->
    l1 = @map.get v1
    if !l1
      l1 = new Map
      @map.set v1, l1

    l2 = l1.get v2
    if !l2
      l2 = new Set
      l1.set v2, l2

    @size -= l2.size
    l2.add v3
    @size += l2.size
    return this

  delete: (v1, v2, v3) ->
    l1 = @map.get v1
    l2 = l1.get v2 if l1
    if l2?.delete v3
      @size--
      if l2.size is 0
        l1.delete v2
        if l1.size is 0
          @map.delete v1
      return yes
    else
      return no

  forEach: (fn) ->
    @map.forEach (l1, v1) ->
      l1.forEach (l2, v2) ->
        l2.forEach (v3) ->
          fn v1, v2, v3

  clear: -> @map.clear()

  inspect: (depth, options) ->
    return "[Set3 (#{@size})]" if depth < 0
    return '{[Set3]}' if @size is 0
    entries = []
    @forEach (v1, v2, v3) ->
      entries.push "(#{inspect v1, options},#{inspect v2, options},#{inspect v3, options})"
    assert entries.length == @size
    "{[Set3] #{entries.join ', '} }"

exports.SetOfPairs = class SetOfPairs extends Set2
  # This stores unordered sets of pairs (a,b) where for any a you can access all values b.
  # If you care about the order of (a,b), use Set2.

  # We don't need a constructor.
  add: (a, b) ->
    super a, b
    super b, a

  delete: (a, b) ->
    if super a, b
      super b, a
      yes
    else
      no

  getAll: (a) -> @map.get a

  deleteAll: (a) ->
    if set = @map.get a
      set.forEach (b) =>
        set2 = @map.get b
        set2.delete a
        @map.delete b if set2.size is 0

      @map.delete a
      @size -= set.size * 2 # Both a->b and b->a relationships
      yes
    else
      no
