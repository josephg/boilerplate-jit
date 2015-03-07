
chars =
  positive: '+'
  negative: '-'
  nothing: ' '
  thinsolid: 'x'
  shuttle: 'S'
  thinshuttle: 's'
  bridge: 'b'

parseXY = exports.parseXY = (k) ->
  [x,y] = k.split ','
  {x:x|0, y:y|0}

numKeys = exports.numKeys = (obj) ->
  num = 0
  num++ for k of obj
  num

# Flood fill through the grid from a cell
exports.fill = (initial_square, f) ->
  visited = {}
  visited["#{initial_square.x},#{initial_square.y}"] = true
  to_explore = [initial_square]
  hmm = (x,y) ->
    k = "#{x},#{y}"
    if not visited[k]
      visited[k] = true
      to_explore.push {x,y}
  while n = to_explore.shift()
    ok = f n.x, n.y, hmm
    if ok
      hmm n.x+1, n.y
      hmm n.x-1, n.y
      hmm n.x, n.y+1
      hmm n.x, n.y-1
  return

# Find the intersection of list1 and list2. Return null if the intersection is
# empty.
intersectListOrNull = (list1, list2) ->
  intersection = null
  for s,i in list1 when s and list2[i]
    intersection ||= []
    intersection[i] = true
  intersection

# Flood fill through the graph of connected regions. DFS for now - assuming
# there's no path between regions thats bigger than the node stack.
exports.fillRegions = (regions, initialRid, f) ->
  expand = (rid, trace) =>
    region = regions[rid]

    shouldExpand = f rid, trace
    return unless shouldExpand

    for k,{rid:nextRid,sid,inStates} of region.connections
      continue if nextRid in trace.path

      #console.log "connection from #{rid} to #{nextRid} when #{sid} in states #{inStates}"

      prevStates = trace.shuttleStates[sid]

      if !prevStates
        trace.shuttleStates[sid] = inStates
      else
        intersect = intersectListOrNull prevStates, inStates

        # Mutually exclusive states - skip.
        continue if !intersect
        
        trace.shuttleStates[sid] = intersect

      trace.path.push nextRid

      expand nextRid, trace

      trace.path.pop()
      if !prevStates
        delete trace.shuttleStates[sid]
      else
        trace.shuttleStates[sid] = prevStates

  expand initialRid, {path:[initialRid], shuttleStates:{}}

exports.gridExtents = (grid) ->
  # calculate the extents
  top = left = bottom = right = null

  for k, v of grid
    {x,y} = parseXY k
    left = x if left is null || x < left
    right = x if right is null || x > right
    top = y if top is null || y < top
    bottom = y if bottom is null || y > bottom

  {top, left, bottom, right}

exports.printCustomGrid = printCustomGrid = ({top, left, bottom, right}, getFn, stream = process.stdout) ->
  for y in [top-1..bottom+1]
    stream.write ''
    for x in [left-1..right+1]
      v = getFn(x, y)
      stream.write chars[v] || (if v? then ("#{v}")[0] else ';')
    stream.write '\n'

exports.printGrid = (extents, grid, stream = process.stdout) ->
  printCustomGrid extents, ((x, y) -> grid[[x,y]]), stream

exports.printPoint = (extents, grid, px, py) ->
  get = (x, y) -> if x == px and y == py then '%' else grid[[x,y]]
  printCustomGrid extents, get

exports.printEdges = ({top, left, bottom, right}, grid, edgeGrid, stream = process.stdout) ->
  edgeChar = (x, y, isTop) ->
    e = edgeGrid["#{x},#{y},#{isTop}"]
    if e?
      e % 10
    else
      ' '

  for y in [top..bottom+1]
    # tops
    for x in [left..right]
      stream.write " #{edgeChar x, y, true}"
    stream.write '\n'
    # lefts
    if y <= bottom
      for x in [left..right+1]
        stream.write "#{edgeChar x, y, false}"
        if x <= right
          stream.write chars[grid[[x, y]]] || ';'
      stream.write '\n'
  stream.write '\n'


exports.drawRegionGraph = (parserData, filename) ->
  {shuttles, regions} = parserData
  #console.log '\n\n\n'
  g = require('graphviz').graph 'regions'
  g.set 'layout', 'fdp'
  g.set 'K', '1.1'

  drawnRegions = {}
  addRegion = (rid) ->
    r = regions[rid]
    return if drawnRegions[rid]
    drawnRegions[rid] = true

    color = undefined
    label = "r#{rid}"
    if r.pressure > 0
      label += "(+#{r.pressure})"
      color = 'green'
    else if r.pressure < 0
      label += "(#{r.pressure})"
      color = 'red'

    node = g.addNode "r#{rid}", {shape:'box', color}
    node.set 'label', label

  addRegion rid for r,rid in regions when numKeys r.connections

  for s,sid in shuttles
    node = g.addNode "s#{sid}", {shape:'oval', style:'filled', fillcolor:'plum1'}
    node.set 'label', "S#{sid} #{s.type}"

    edges = {}
    addEdge = ({rid, mx, my}) ->
      edge = edges[rid]
      return edge if edge

      addRegion rid
      pressure = (mx||0) + (my||0)
      color = if pressure < 0 then 'red' else if pressure > 0 then 'green' else 'black'
      edges[rid] = edge = g.addEdge "r#{rid}", "s#{sid}"
      edge.set 'color', color
      edge.set 'penwidth', 2
      edge.set "dir", "forward"
      edge

    addEdge p for p in s.pushedBy

    for state, stateid in s.states
      edge = addEdge p for p in state.pushedBy
      #edge.set 'penwidth', 1

  for r,rid in regions when numKeys r.connections
    for k,c of r.connections when c.rid > rid
      edge = g.addEdge "r#{rid}", "r#{c.rid}"

      allowedStates = []
      allowedStates.push stateid for v, stateid in c.inStates when v

      stateList = if allowedStates.length <= 3
        " (#{allowedStates.join(',')})"
      else
        "x#{allowedStates.length}"
      edge.set 'label', "S#{c.sid}#{stateList}"

      #edge = g.addEdge "s#{c.sid}", "r#{rid}"
      #edge.set 'dir', 'forward'

  #console.log g.to_dot()
  g.output filename.split('.')[1], filename
  console.log "generated #{filename}"


