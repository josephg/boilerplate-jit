
exports.Map2 = class Map2 # A map from (a,b) -> c instead of just a->c
  constructor: (data) ->
    @levelOne = new Map
    if data
      for [k1, k2, v] in data
        @set k1, k2, v

  get: (k1, k2) ->
    inner = @levelOne.get k1
    return inner.get k2 if inner

  has: (k1, k2) ->
    inner = @levelOne.get k1
    return inner?.has(k2) or false

  set: (k1, k2, v) ->
    inner = @levelOne.get k1
    if !inner
      inner = new Map
      @levelOne.set k1, inner

    inner.set k2, v

  delete: (k1, k2) ->
    inner = @levelOne.get k1
    inner.delete k2 if inner

  forEach: (fn) ->
    @levelOne.forEach (inner, k1) ->
      inner.forEach (v, k2) ->
        fn k1, k2, v

  inspect: ->
    data = {}
    @forEach (k1, k2, v) -> data[[k1,k2]] = v
    data


exports.Set2 = class Set2 # A set of (a,b) values
  constructor: (data) ->
    @levelOne = new Map
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

    inner.add v2

  delete: (v1, v2) ->
    inner = @levelOne.get v1
    inner.delete v2 if inner

  forEach: (fn) ->
    @levelOne.forEach (inner, v1) ->
      inner.forEach (v2) ->
        fn v1, v2


