// Model Diagram htmlwidget for BioModME.
//
// Bipartite graph: species nodes (circles) and reaction nodes (squares)
// connected by reactant / product / modifier edges. The widget keeps
// persistent state on its instance (force simulation, node array,
// edge array, SVG selections) so re-renders triggered by R-side reactive
// changes preserve existing node positions instead of restarting the
// force layout from scratch.
//
// Drag handlers and click-to-info come in later commits.

HTMLWidgets.widget({
  name: 'modelDiagram',
  type: 'output',

  factory: function(el, width, height) {

    // ---- Persistent per-instance state -------------------------------------
    var state = {
      svg:               null,
      g:                 null,   // root <g> for content
      hullsG:            null,   // <g> for compartment hulls (later commit)
      edgesG:            null,   // <g> for edges
      nodesG:            null,   // <g> for nodes
      simulation:        null,
      nodes:             [],
      edges:             [],
      compartmentColor:  {},
      width:             width,
      height:            height
    };

    // ---- Setup helpers -----------------------------------------------------
    function ensureSvg() {
      if (state.svg) return;
      var svg = d3.select(el).append('svg')
        .attr('class', 'modelDiagram-svg')
        .attr('width', state.width)
        .attr('height', state.height);

      var defs = svg.append('defs');
      defs.append('marker')
        .attr('id', 'modelDiagram-arrow')
        .attr('viewBox', '0 -5 10 10')
        .attr('refX', 22)
        .attr('refY', 0)
        .attr('markerWidth', 7)
        .attr('markerHeight', 7)
        .attr('orient', 'auto')
        .append('path')
        .attr('class', 'modelDiagram-arrowhead')
        .attr('d', 'M0,-5L10,0L0,5');

      var g = svg.append('g').attr('class', 'modelDiagram-content');
      state.svg    = svg;
      state.g      = g;
      state.hullsG = g.append('g').attr('class', 'modelDiagram-hulls');
      state.edgesG = g.append('g').attr('class', 'modelDiagram-edges');
      state.nodesG = g.append('g').attr('class', 'modelDiagram-nodes');
    }

    function ensureSimulation() {
      if (state.simulation) return;
      state.simulation = d3.forceSimulation()
        .force('link', d3.forceLink()
                          .id(function(d) { return d.id; })
                          .distance(90))
        .force('charge', d3.forceManyBody().strength(-180))
        .force('center', d3.forceCenter(state.width / 2, state.height / 2))
        .force('collide', d3.forceCollide().radius(34));
      state.simulation.alphaDecay(0.05);

      // Auto-pin every node once the simulation finishes settling. This
      // eliminates the "spring bouncing" the user noticed: after the
      // initial layout converges, dragging one node only moves that
      // node. Connected springs no longer tug at neighbors because the
      // neighbors have fx/fy set. New nodes that arrive later (via
      // applyData) are still unpinned at the moment they appear, so the
      // simulation re-runs briefly to place them and then 'end' fires
      // again to pin them too.
      state.simulation.on('end', function() {
        state.nodes.forEach(function(n) {
          if (n.fx == null) n.fx = n.x;
          if (n.fy == null) n.fy = n.y;
        });
      });
    }

    // Clamp x/y to a small inset from the SVG bounds so dragging or
    // simulation forces can't push nodes off-screen.
    function clampToBounds(n) {
      var pad = 30;
      n.x = Math.max(pad, Math.min(state.width  - pad, n.x));
      n.y = Math.max(pad, Math.min(state.height - pad, n.y));
      if (n.fx != null) n.fx = Math.max(pad, Math.min(state.width  - pad, n.fx));
      if (n.fy != null) n.fy = Math.max(pad, Math.min(state.height - pad, n.fy));
    }

    // ---- Drag behavior -----------------------------------------------------
    // Pin-on-drag semantics: dragging sets fx/fy so the node sticks at the
    // user's position. The simulation continues to settle other nodes around
    // it. A future Reset Layout button (commit 8) clears all pins to allow a
    // fresh auto-layout pass.
    function makeDragBehavior() {
      return d3.drag()
        .on('start', function(event, d) {
          // Don't reheat the simulation on drag — we want the rest of
          // the graph to stay fixed. Just set fx/fy on the dragged node.
          d.fx = d.x;
          d.fy = d.y;
        })
        .on('drag', function(event, d) {
          d.fx = event.x;
          d.fy = event.y;
          clampToBounds(d);
          // Manually update this node's transform and any connected
          // edges' endpoints, since the simulation is no longer ticking.
          state.nodesG.selectAll('g.modelDiagram-node')
            .filter(function(nd) { return nd.id === d.id; })
            .attr('transform', 'translate(' + d.fx + ',' + d.fy + ')');
          state.edgesG.selectAll('line.modelDiagram-edge')
            .filter(function(e) {
              return e.source.id === d.id || e.target.id === d.id;
            })
            .attr('x1', function(e) { return e.source.x; })
            .attr('y1', function(e) { return e.source.y; })
            .attr('x2', function(e) { return e.target.x; })
            .attr('y2', function(e) { return e.target.y; });
          // Force-tracked node position must stay in sync with fx/fy
          // so post-drag the data binding has the right coordinates.
          d.x = d.fx;
          d.y = d.fy;
        })
        .on('end', function(event, d) {
          // Keep fx/fy set so the node stays where the user dropped it.
          // Notify R so commit 5's observer can persist it on rv.DIAGRAM.
          if (HTMLWidgets.shinyMode) {
            Shiny.setInputValue('modelDiagram_node_drag', {
              id:    d.id,
              x:     d.fx,
              y:     d.fy,
              fixed: true
            }, { priority: 'event' });
          }
        });
    }

    // ---- Data normalization ------------------------------------------------
    // R's data.frame -> JSON serializes as either an object-of-arrays (default
    // htmlwidgets behavior) or an array-of-objects depending on options. We
    // accept both shapes and pivot to array-of-objects.
    function pivotToArray(raw) {
      if (!raw) return [];
      if (Array.isArray(raw)) return raw.slice();
      var keys = Object.keys(raw);
      if (keys.length === 0) return [];
      var firstKey = keys[0];
      var n = (raw[firstKey] && raw[firstKey].length) ? raw[firstKey].length : 0;
      var out = [];
      for (var i = 0; i < n; i++) {
        var obj = {};
        for (var k = 0; k < keys.length; k++) {
          var key = keys[k];
          obj[key] = raw[key] ? raw[key][i] : null;
        }
        out.push(obj);
      }
      return out;
    }

    // ---- Position preservation across re-renders --------------------------
    function applyData(x) {
      var newNodes = pivotToArray(x.nodes);
      var newEdges = pivotToArray(x.edges);
      var compArr  = pivotToArray(x.compartmentGroups);

      // Compartment color lookup
      var colorMap = {};
      compArr.forEach(function(g) { colorMap[g.compartmentId] = g.color; });
      state.compartmentColor = colorMap;

      // Carry forward (x, y, vx, vy, fx, fy) from any node that survives
      // the diff. New nodes start near the model center with small jitter
      // so the simulation can disperse them; removed nodes drop out.
      var oldById = {};
      state.nodes.forEach(function(n) { oldById[n.id] = n; });
      newNodes.forEach(function(n) {
        var prev = oldById[n.id];
        if (prev) {
          n.x  = prev.x;
          n.y  = prev.y;
          n.vx = prev.vx;
          n.vy = prev.vy;
          if (prev.fx != null) n.fx = prev.fx;
          if (prev.fy != null) n.fy = prev.fy;
        } else {
          n.x = state.width  / 2 + (Math.random() - 0.5) * 80;
          n.y = state.height / 2 + (Math.random() - 0.5) * 80;
        }
      });

      state.nodes = newNodes;
      state.edges = newEdges;
    }

    // ---- DOM update (keyed enter / update / exit) -------------------------
    function update() {
      // Edges
      var edgeSel = state.edgesG.selectAll('line.modelDiagram-edge')
        .data(state.edges, function(d) { return d.id; });
      edgeSel.exit().remove();
      var edgeEnter = edgeSel.enter().append('line')
        .attr('class', function(d) {
          return 'modelDiagram-edge modelDiagram-edge-' + d.role;
        })
        .attr('marker-end', function(d) {
          return d.role === 'modifier' ? null : 'url(#modelDiagram-arrow)';
        });
      var edgeAll = edgeEnter.merge(edgeSel);

      // Nodes
      var nodeSel = state.nodesG.selectAll('g.modelDiagram-node')
        .data(state.nodes, function(d) { return d.id; });
      nodeSel.exit().remove();
      var nodeEnter = nodeSel.enter().append('g')
        .attr('class', function(d) {
          return 'modelDiagram-node modelDiagram-node-' + d.type;
        });

      nodeEnter.each(function(d) {
        var sel = d3.select(this);
        if (d.type === 'species') {
          sel.append('circle').attr('r', 18);
        } else {
          sel.append('rect')
            .attr('width',  30)
            .attr('height', 22)
            .attr('x', -15)
            .attr('y', -11)
            .attr('rx', 3);
        }
        sel.append('text')
          .attr('class', 'modelDiagram-node-label')
          .attr('text-anchor', 'middle')
          .attr('dy', d.type === 'species' ? 32 : 30)
          .text(d.label || d.id);
      });

      // Color species circles by compartment
      nodeEnter.filter(function(d) { return d.type === 'species'; })
        .select('circle')
        .style('fill', function(d) {
          return state.compartmentColor[d.compartmentId] || '#cccccc60';
        });

      // Make new nodes draggable. D3 drag behavior persists on the DOM
      // elements across subsequent re-renders, so this only needs to fire
      // on enter, not on update.
      nodeEnter.call(makeDragBehavior());
      nodeEnter.style('cursor', 'grab');

      var nodeAll = nodeEnter.merge(nodeSel);

      // Tick handler — replaced each update so we close over the right
      // selections. Clamp positions to viewport so nothing flies off
      // screen during the initial settle.
      state.simulation.on('tick', function() {
        state.nodes.forEach(clampToBounds);
        edgeAll
          .attr('x1', function(d) { return d.source.x; })
          .attr('y1', function(d) { return d.source.y; })
          .attr('x2', function(d) { return d.target.x; })
          .attr('y2', function(d) { return d.target.y; });
        nodeAll
          .attr('transform', function(d) {
            return 'translate(' + d.x + ',' + d.y + ')';
          });
      });

      state.simulation.nodes(state.nodes);
      state.simulation.force('link').links(state.edges);
      state.simulation.alpha(0.3).restart();
    }

    function clearEmptyMessage() {
      if (state.svg) state.svg.selectAll('.modelDiagram-empty').remove();
    }

    function showEmptyMessage() {
      if (!state.svg) return;
      clearEmptyMessage();
      state.svg.append('text')
        .attr('class', 'modelDiagram-empty')
        .attr('x', state.width / 2)
        .attr('y', state.height / 2)
        .attr('text-anchor', 'middle')
        .attr('dominant-baseline', 'middle')
        .text('Add species and reactions to see the diagram.');
    }

    // ---- Public API --------------------------------------------------------
    return {
      renderValue: function(x) {
        if (!x) return;
        ensureSvg();
        ensureSimulation();

        var hasNodes = false;
        if (Array.isArray(x.nodes)) {
          hasNodes = x.nodes.length > 0;
        } else if (x.nodes && x.nodes.id) {
          hasNodes = x.nodes.id.length > 0;
        }

        if (!hasNodes) {
          showEmptyMessage();
          state.nodes = [];
          state.edges = [];
          state.simulation.nodes([]);
          state.simulation.force('link').links([]);
          // Also clear the rendered DOM so old nodes don't linger.
          state.edgesG.selectAll('*').remove();
          state.nodesG.selectAll('*').remove();
          return;
        }

        clearEmptyMessage();
        applyData(x);
        update();
      },

      resize: function(newWidth, newHeight) {
        state.width  = newWidth;
        state.height = newHeight;
        if (state.svg) {
          state.svg.attr('width', newWidth).attr('height', newHeight);
        }
        if (state.simulation) {
          state.simulation.force('center',
            d3.forceCenter(newWidth / 2, newHeight / 2));
          state.simulation.alpha(0.3).restart();
        }
      },

      _state: state
    };
  }
});
