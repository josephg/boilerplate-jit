{inspect} = require 'util'
assert = require 'assert'

# Monkey patch inspect functions for Map and Set
Map::inspect ?= (depth, options) ->
  return '[Map]' if depth < 0
  return '{[Map]}' if @size is 0
  entries = []
  @forEach (v, k) -> entries.push "#{inspect k, options} : #{inspect v, options}"
  "{[Map] #{entries.join ', '} }"

Set::inspect ?= (depth, options) ->
  return '[Set]' if depth < 0
  return '{[Set]}' if @size is 0
  entries = []
  @forEach (v) -> entries.push "#{inspect v, options}"
  "{[Set] #{entries.join ', '} }"

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

exports.Map2 = class Map2 # A map from (a,b) -> c instead of just a->c
  constructor: (data) ->
    @levelOne = new Map
    @size = 0
    if typeof data is 'function'
      @makeDefault = data
    else if data
      for [k1, k2, v] in data
        @set k1, k2, v

  get: (k1, k2) ->
    inner = @levelOne.get k1
    v = inner.get k2 if inner
    if !v? and @makeDefault
      @set k1, k2, v = @makeDefault k1, k2
    v

  has: (k1, k2) ->
    inner = @levelOne.get k1
    return inner?.has(k2) or false

  set: (k1, k2, v) ->
    inner = @levelOne.get k1
    if !inner
      inner = new Map
      @levelOne.set k1, inner

    @size -= inner.size
    inner.set k2, v
    @size += inner.size
    return this

  delete: (k1, k2) ->
    inner = @levelOne.get k1
    if inner
      deleted = inner.delete k2
      @size-- if deleted
      return deleted
    else
      return no

  forEach: (fn) ->
    @levelOne.forEach (inner, k1) ->
      inner.forEach (v, k2) ->
        fn k1, k2, v

  clear: -> @levelOne.clear()

  inspect: (depth, options) ->
    return '[Map2]' if depth < 0
    return '{[Map2]}' if @size is 0
    entries = []
    @forEach (k1, k2, v) ->
      entries.push "(#{inspect k1, options},#{inspect k2, options}) : #{inspect v, options}"
    assert entries.length == @size
    "{[Map2] #{entries.join ', '} }"


exports.Map3 = class Map3 # A map from (a,b) -> c instead of just a->c
  constructor: (data) ->
    @map = new Map
    @size = 0
    if typeof data is 'function'
      @makeDefault = data
    else if data
      for [k1, k2, k3, v] in data
        @set k1, k2, k3, v

  get: (k1, k2, k3) ->
    l1 = @map.get k1
    l2 = l1.get k2 if l1
    v = l2.get k3 if l2
    if !v? and @makeDefault
      @set k1, k2, k3, v = @makeDefault k1, k2
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
    return '[Map3]' if depth < 0
    return '{[Map3]}' if @size is 0
    entries = []
    @forEach (k1, k2, k3, v) ->
      entries.push "(#{inspect k1, options},#{inspect k2, options},#{inspect k3, options}) : #{inspect v, options}"
    assert entries.length == @size
    "{[Map3] #{entries.join ', '} }"



exports.Set2 = class Set2 # A set of (a,b) values
  constructor: (data) ->
    @levelOne = new Map
    @size = 0
    if data
      for [v1, v2] in data
        @add v1, v2

  subset: (v1) -> @levelOne.get v1

  has: (v1, v2) ->
    inner = @levelOne.get v1
    return inner?.has(v2) or false

  add: (v1, v2) ->
    inner = @levelOne.get v1
    if !inner
      inner = new Set
      @levelOne.set v1, inner

    @size -= inner.size
    inner.add v2
    @size += inner.size
    return this

  delete: (v1, v2) ->
    inner = @levelOne.get v1
    if inner
      deleted = inner.delete v2
      @size-- if deleted
      return deleted
    else
      return no

  forEach: (fn) ->
    @levelOne.forEach (inner, v1) ->
      inner.forEach (v2) ->
        fn v1, v2

  clear: -> @levelOne.clear()

  inspect: (depth, options) ->
    return '[Set2]' if depth < 0
    entries = []
    @forEach (v1, v2) ->
      entries.push "(#{inspect v1, options},#{inspect v2, options})"

    assert entries.length == @size
    "{[Set2] #{entries.join ', '} }"


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
    if l2
      deleted = l2.delete v3
      @size-- if deleted
      return deleted
    else
      return no

  forEach: (fn) ->
    @map.forEach (l1, v1) ->
      l1.forEach (l2, v2) ->
        l2.forEach (v3) ->
          fn v1, v2, v3

  clear: -> @map.clear()

  inspect: (depth, options) ->
    return '[Set3]' if depth < 0
    return '{[Set3]}' if @size is 0
    entries = []
    @forEach (v1, v2, v3) ->
      entries.push "(#{inspect v1, options},#{inspect v2, options},#{inspect v3, options})"
    assert entries.length == @size
    "{[Set3] #{entries.join ', '} }"

