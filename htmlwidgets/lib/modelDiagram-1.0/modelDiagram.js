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
      zoom:              null,   // d3.zoom() behavior instance
      nodes:             [],
      edges:             [],
      compartmentColor:  {},
      width:             width,
      height:            height,
      resetToken:        null,   // last seen token; change triggers full re-layout
      hopDepth:          2,      // BFS depth for pathway highlight (0 = off)
      shrinkReactions:   false   // toggled by Shiny — shrinks reaction squares
    };

    // ---- Dimension helpers -------------------------------------------------
    // When the Model Diagram tab is not active, the container element has
    // offsetWidth/Height = 0, so the htmlwidgets runtime passes width=0 to
    // the factory. We fall back to the element's actual bounding rect (or a
    // safe default) whenever state.width/height look wrong.
    function liveWidth()  {
      var w = el.getBoundingClientRect().width  || el.offsetWidth;
      return (w > 10) ? w : (state.width  > 10 ? state.width  : 900);
    }
    function liveHeight() {
      var h = el.getBoundingClientRect().height || el.offsetHeight;
      return (h > 10) ? h : (state.height > 10 ? state.height : 600);
    }

    // ---- Pathway highlight -------------------------------------------------
    // BFS from the selected element out to state.hopDepth. Each node and edge
    // is tagged with the minimum hop distance from the selection (0 = the
    // selected one) and rendered in a hop-specific colour so the user can see
    // at a glance how many steps away each thing is. Inline styles + CSS
    // class fallbacks keep this working even when modelDiagram.css gets
    // served stale from browser cache.
    var FADE_OPACITY = 0.10;

    // Per-hop visuals. Index 0 = selected element. Subsequent entries are
    // the styling used for items found at that BFS distance.
    var HOP_GLOW_NODE = [
      'drop-shadow(0 0 6px rgba(26, 115, 232, 0.85))',  // 0 — vivid blue
      'drop-shadow(0 0 4px rgba(67, 160, 71, 0.70))',   // 1 — green
      'drop-shadow(0 0 4px rgba(251, 140, 0, 0.65))',   // 2 — amber
      'drop-shadow(0 0 3px rgba(233, 30, 99, 0.55))'    // 3 — pink
    ];
    var HOP_GLOW_EDGE = [
      'drop-shadow(0 0 4px rgba(26, 115, 232, 0.65))',
      'drop-shadow(0 0 3px rgba(67, 160, 71, 0.55))',
      'drop-shadow(0 0 3px rgba(251, 140, 0, 0.50))',
      'drop-shadow(0 0 3px rgba(233, 30, 99, 0.45))'
    ];
    var HOP_STROKE       = ['#1a73e8', '#43a047', '#fb8c00', '#e91e63'];
    var HOP_STROKE_WIDTH = ['3px',     '2.5px',   '2.25px',  '2px'];

    function pickIdx(arr, h) { return arr[Math.min(h, arr.length - 1)]; }

    function applyPathwayHighlight(selectedId, kind) {
      var maxHops = state.hopDepth || 0;
      if (maxHops <= 0) { clearPathwayHighlight(); return; }

      // Adjacency map: node id -> [{edge, other}]. Built once so BFS is fast.
      var adj = {};
      state.edges.forEach(function(e) {
        var src = (typeof e.source === 'object') ? e.source.id : e.source;
        var tgt = (typeof e.target === 'object') ? e.target.id : e.target;
        if (!adj[src]) adj[src] = [];
        if (!adj[tgt]) adj[tgt] = [];
        adj[src].push({ edge: e.id, other: tgt });
        adj[tgt].push({ edge: e.id, other: src });
      });

      // BFS — record minimum hop distance to each node and edge.
      var nodeHops = {};
      var edgeHops = {};

      if (kind === 'node') {
        nodeHops[selectedId] = 0;
        var frontier = [selectedId];
        while (frontier.length > 0) {
          var next = [];
          frontier.forEach(function(id) {
            var h = nodeHops[id];
            if (h >= maxHops) return;
            (adj[id] || []).forEach(function(nb) {
              var nh = h + 1;
              if (edgeHops[nb.edge] === undefined || nh < edgeHops[nb.edge]) {
                edgeHops[nb.edge] = nh;
              }
              if (nodeHops[nb.other] === undefined || nh < nodeHops[nb.other]) {
                nodeHops[nb.other] = nh;
                next.push(nb.other);
              }
            });
          });
          frontier = next;
        }
      } else {
        // Edge selected: edge is hop 0, its endpoints are hop 1.
        state.edges.forEach(function(e) {
          if (e.id !== selectedId) return;
          edgeHops[e.id] = 0;
          var src = (typeof e.source === 'object') ? e.source.id : e.source;
          var tgt = (typeof e.target === 'object') ? e.target.id : e.target;
          nodeHops[src] = 1;
          nodeHops[tgt] = 1;
        });
        var frontierE = Object.keys(nodeHops);
        while (frontierE.length > 0) {
          var nextE = [];
          frontierE.forEach(function(id) {
            var h = nodeHops[id];
            if (h >= maxHops) return;
            (adj[id] || []).forEach(function(nb) {
              var nh = h + 1;
              if (edgeHops[nb.edge] === undefined || nh < edgeHops[nb.edge]) {
                edgeHops[nb.edge] = nh;
              }
              if (nodeHops[nb.other] === undefined || nh < nodeHops[nb.other]) {
                nodeHops[nb.other] = nh;
                nextE.push(nb.other);
              }
            });
          });
          frontierE = nextE;
        }
      }

      state.nodesG.selectAll('g.modelDiagram-node')
        .classed('modelDiagram-faded',   function(d) { return nodeHops[d.id] === undefined; })
        .classed('modelDiagram-pathway', function(d) { return nodeHops[d.id] > 0; })
        .style('opacity', function(d) {
          return nodeHops[d.id] === undefined ? FADE_OPACITY : null;
        })
        .style('filter', function(d) {
          var h = nodeHops[d.id];
          if (h === undefined) return null;
          return pickIdx(HOP_GLOW_NODE, h);
        });

      state.edgesG.selectAll('line.modelDiagram-edge')
        .classed('modelDiagram-faded',   function(d) { return edgeHops[d.id] === undefined; })
        .classed('modelDiagram-pathway', function(d) {
          return edgeHops[d.id] !== undefined && d.id !== selectedId;
        })
        .style('opacity', function(d) {
          return edgeHops[d.id] === undefined ? FADE_OPACITY : null;
        })
        .style('filter', function(d) {
          var h = edgeHops[d.id];
          return h === undefined ? null : pickIdx(HOP_GLOW_EDGE, h);
        })
        .style('stroke', function(d) {
          var h = edgeHops[d.id];
          return h === undefined ? null : pickIdx(HOP_STROKE, h);
        })
        .style('stroke-width', function(d) {
          var h = edgeHops[d.id];
          return h === undefined ? null : pickIdx(HOP_STROKE_WIDTH, h);
        });
    }

    // Apply / unapply the shrink-reactions appearance. When state.shrinkReactions
    // is true we shrink each reaction <rect> to a small dot and hide its
    // label so species circles dominate the canvas. Edges are left alone —
    // they still terminate naturally at the (now small) reaction node.
    // Called from the Shiny message handler AND from update() so that any
    // new reaction nodes added by a re-render adopt the current state.
    var RXN_W_DEFAULT = 30, RXN_H_DEFAULT = 22;
    var RXN_W_SHRUNK = 10, RXN_H_SHRUNK = 8;
    function applyShrinkReactions() {
      var shrink = !!state.shrinkReactions;
      if (state.svg) {
        state.svg.classed('modelDiagram-shrink-reactions', shrink);
      }
      if (!state.nodesG) return;
      var rxn = state.nodesG.selectAll('g.modelDiagram-node-reaction');
      if (shrink) {
        rxn.select('rect')
          .attr('width',  RXN_W_SHRUNK)
          .attr('height', RXN_H_SHRUNK)
          .attr('x', -RXN_W_SHRUNK / 2)
          .attr('y', -RXN_H_SHRUNK / 2);
        rxn.select('text.modelDiagram-node-label')
          .style('display', 'none');
      } else {
        rxn.select('rect')
          .attr('width',  RXN_W_DEFAULT)
          .attr('height', RXN_H_DEFAULT)
          .attr('x', -RXN_W_DEFAULT / 2)
          .attr('y', -RXN_H_DEFAULT / 2);
        rxn.select('text.modelDiagram-node-label')
          .style('display', null);
      }
    }

    function clearPathwayHighlight() {
      state.nodesG.selectAll('g.modelDiagram-node')
        .classed('modelDiagram-faded',   false)
        .classed('modelDiagram-pathway', false)
        .style('opacity', null)
        .style('filter',  null);
      state.edgesG.selectAll('line.modelDiagram-edge')
        .classed('modelDiagram-faded',   false)
        .classed('modelDiagram-pathway', false)
        .style('opacity',      null)
        .style('filter',       null)
        .style('stroke',       null)
        .style('stroke-width', null);
    }

    // ---- Setup helpers -----------------------------------------------------
    function ensureSvg() {
      if (state.svg) return;
      var svg = d3.select(el).append('svg')
        .attr('class', 'modelDiagram-svg')
        .attr('width',  state.width)
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

      // Zoom — panning and mouse-wheel zoom transform state.g. The zoom
      // behavior is attached to the SVG so the full area is interactive;
      // node drag events stop propagation so dragging a node doesn't pan.
      state.zoom = d3.zoom()
        .scaleExtent([0.1, 5])
        .on('zoom', function(event) {
          state.g.attr('transform', event.transform);
        });
      svg.call(state.zoom);
      // Disable D3 zoom's built-in double-click-to-zoom; we use dblclick
      // on the background to clear the selection instead.
      svg.on('dblclick.zoom', null);

      // Double-click on the SVG background clears the current selection
      // and the pathway highlight.
      svg.on('dblclick', function() {
        state.nodesG.selectAll('g.modelDiagram-node')
          .classed('modelDiagram-selected', false);
        state.edgesG.selectAll('line.modelDiagram-edge')
          .classed('modelDiagram-selected', false);
        clearPathwayHighlight();
        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue('modelDiagram_node_click',
            { id: null, type: null }, { priority: 'event' });
        }
      });

      // Shiny message handler for the zoom/fit toolbar buttons.
      // Registered once per widget creation; safe for single-instance use.
      if (HTMLWidgets.shinyMode) {
        Shiny.addCustomMessageHandler('modelDiagram_zoom', function(msg) {
          if      (msg.action === 'in')  { zoomBy(1.4); }
          else if (msg.action === 'out') { zoomBy(1 / 1.4); }
          else if (msg.action === 'fit') { fitView(false); }
        });

        // Highlight depth: 0 = off, 1..3 = BFS hops to expand from selection.
        // When the user changes the dropdown while something is selected, we
        // re-run the highlight at the new depth so the change takes effect
        // without forcing them to click the node again.
        Shiny.addCustomMessageHandler('modelDiagram_highlight_hops', function(msg) {
          var hops = (msg && typeof msg.hops === 'number') ? msg.hops : 0;
          state.hopDepth = Math.max(0, Math.min(3, Math.round(hops)));
          if (state.hopDepth === 0) {
            clearPathwayHighlight();
            return;
          }
          // Find the current selection (if any) and re-apply at new depth.
          var selNode = state.nodesG.select('g.modelDiagram-node.modelDiagram-selected');
          if (!selNode.empty()) {
            var nd = selNode.datum();
            if (nd && nd.id) { applyPathwayHighlight(nd.id, 'node'); return; }
          }
          var selEdge = state.edgesG.select('line.modelDiagram-edge.modelDiagram-selected');
          if (!selEdge.empty()) {
            var ed = selEdge.datum();
            if (ed && ed.id) applyPathwayHighlight(ed.id, 'edge');
          }
        });

        // Focus a specific species (driven by the dropdown). Marks it as
        // selected and applies the pathway highlight at the current depth.
        Shiny.addCustomMessageHandler('modelDiagram_highlight_species', function(msg) {
          if (!msg || !msg.id) return;
          state.nodesG.selectAll('g.modelDiagram-node')
            .classed('modelDiagram-selected', function(d) { return d.id === msg.id; });
          state.edgesG.selectAll('line.modelDiagram-edge')
            .classed('modelDiagram-selected', false);
          if (state.hopDepth > 0) applyPathwayHighlight(msg.id, 'node');
        });

        // Apply per-species circle radii from the simulation animation.
        // msg.radii is a plain object: { speciesId: radiusPx, ... }
        Shiny.addCustomMessageHandler('modelDiagram_set_radii', function(msg) {
          if (!msg || !msg.radii) return;
          var radii = msg.radii;
          state.nodesG.selectAll('g.modelDiagram-node-species').each(function(d) {
            var r = radii[d.id];
            if (typeof r !== 'number' || !isFinite(r)) return;
            d3.select(this).select('circle')
              .transition().duration(250)
              .attr('r', r);
          });
        });

        // Reset all species circles back to the default radius. The msg
        // parameter is unused but newer Shiny versions reject custom message
        // handlers whose function arity is zero.
        Shiny.addCustomMessageHandler('modelDiagram_reset_radii', function(msg) {
          state.nodesG.selectAll('g.modelDiagram-node-species').each(function() {
            d3.select(this).select('circle')
              .transition().duration(250)
              .attr('r', 18);
          });
        });

        // Shrink reaction squares to a small dot and hide their labels
        // (or restore defaults). Edges keep terminating at the now-small
        // reaction node so connectivity is still visible.
        Shiny.addCustomMessageHandler('modelDiagram_shrink_reactions', function(msg) {
          state.shrinkReactions = !!(msg && msg.shrink);
          applyShrinkReactions();
        });
      }
    }

    function ensureSimulation() {
      if (state.simulation) return;
      state.simulation = d3.forceSimulation()
        .force('link', d3.forceLink()
                          .id(function(d) { return d.id; })
                          .distance(90))
        .force('charge', d3.forceManyBody().strength(-180))
        .force('center', d3.forceCenter(liveWidth() / 2, liveHeight() / 2))
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

    // ---- Zoom helpers ---------------------------------------------------------
    function zoomBy(factor) {
      if (!state.zoom || !state.svg) return;
      state.svg.transition().duration(300).call(state.zoom.scaleBy, factor);
    }

    // Fit all nodes into view with a 50px margin on each side.
    // Pass instant=true to skip the transition (used after initial layout).
    function fitView(instant) {
      if (!state.nodes.length || !state.zoom || !state.svg) return;
      var W   = liveWidth();
      var H   = liveHeight();
      var xs  = state.nodes.map(function(n) { return n.fx != null ? n.fx : n.x; });
      var ys  = state.nodes.map(function(n) { return n.fy != null ? n.fy : n.y; });
      var pad = 55;
      var minX = Math.min.apply(null, xs) - pad;
      var maxX = Math.max.apply(null, xs) + pad;
      var minY = Math.min.apply(null, ys) - pad;
      var maxY = Math.max.apply(null, ys) + pad;
      var dx   = maxX - minX;
      var dy   = maxY - minY;
      if (dx <= 0 || dy <= 0) return;
      var scale = Math.min(W / dx, H / dy);
      scale = Math.max(0.1, Math.min(scale, 3));
      var t = d3.zoomIdentity
        .translate(W / 2 - scale * (minX + maxX) / 2,
                   H / 2 - scale * (minY + maxY) / 2)
        .scale(scale);
      state.svg.transition().duration(instant ? 0 : 400)
        .call(state.zoom.transform, t);
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
          // No clampToBounds here: with zoom the user should be able to drag
          // nodes into any visible area. Clamping is kept in the simulation
          // tick handler only, to prevent nodes flying off during initial layout.
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

    // ---- Reaction-order initial placement ----------------------------------
    // Lay out fresh nodes using the reaction definition order as a grid
    // backbone. Reactions fill cells left-to-right, top-to-bottom. Each
    // species is placed on the side of its connected reaction that matches
    // its role (reactants left, products right, modifiers above). When a
    // species connects to multiple reactions its position is the average of
    // all those desired positions. The force simulation then refines from
    // this semantically meaningful starting point.
    function reactionOrderLayout(freshNodes, allNodes, edges) {
      var W = liveWidth();
      var H = liveHeight();

      var freshById = {};
      freshNodes.forEach(function(n) { freshById[n.id] = n; });

      // Step 1 — place fresh reaction nodes on a grid.
      // Use the global reaction index (position within allNodes) so that when
      // a new reaction is added incrementally it slots into the next cell and
      // doesn't shift existing pinned reactions.
      var allRxn = allNodes.filter(function(n) { return n.type === 'reaction'; });
      var padL = 90, padT = 80;
      var nCols = Math.max(1, Math.ceil(Math.sqrt(allRxn.length * 1.6)));
      var nRows = Math.max(1, Math.ceil(allRxn.length / nCols));
      var xStep = nCols > 1 ? Math.min(220, (W - padL * 2) / (nCols - 1)) : 0;
      var yStep = nRows > 1 ? Math.min(180, (H - padT * 2) / (nRows - 1)) : 0;

      var rxnPos = {};
      allRxn.forEach(function(n, i) {
        var gx = padL + (i % nCols) * xStep;
        var gy = padT + Math.floor(i / nCols) * yStep;
        if (freshById[n.id]) {
          n.x = gx;
          n.y = gy;
        }
        // Record position for species placement below (prefer pinned position
        // for existing nodes, grid position for fresh ones).
        rxnPos[n.id] = freshById[n.id]
          ? { x: gx, y: gy }
          : { x: n.fx != null ? n.fx : n.x, y: n.fy != null ? n.fy : n.y };
      });

      // Step 2 — place fresh species near their connected reactions.
      // Collect desired positions per species (one per connected reaction edge)
      // then average them so a species shared across reactions lands in between.
      var spcWanted = {};
      freshNodes.forEach(function(n) {
        if (n.type === 'species') spcWanted[n.id] = [];
      });

      var OFFSET = 95;
      edges.forEach(function(e) {
        var src  = typeof e.source === 'object' ? e.source.id : e.source;
        var tgt  = typeof e.target === 'object' ? e.target.id : e.target;
        var role = e.role;
        // reactant: species→reaction  product: reaction→species  modifier: species→reaction
        var rxnId = (role === 'product') ? src : tgt;
        var spcId = (role === 'product') ? tgt : src;
        if (!spcWanted[spcId] || !rxnPos[rxnId]) return;
        var rp = rxnPos[rxnId];
        if (role === 'reactant') {
          spcWanted[spcId].push({ x: rp.x - OFFSET, y: rp.y });
        } else if (role === 'product') {
          spcWanted[spcId].push({ x: rp.x + OFFSET, y: rp.y });
        } else {
          spcWanted[spcId].push({ x: rp.x, y: rp.y - OFFSET });
        }
      });

      freshNodes.forEach(function(n) {
        if (n.type !== 'species') return;
        var wanted = spcWanted[n.id] || [];
        if (wanted.length > 0) {
          var cx = 0, cy = 0;
          wanted.forEach(function(p) { cx += p.x; cy += p.y; });
          // Average + tiny jitter so species sharing a position don't stack.
          n.x = cx / wanted.length + (Math.random() - 0.5) * 25;
          n.y = cy / wanted.length + (Math.random() - 0.5) * 25;
        } else {
          // Disconnected species: scatter near centre.
          n.x = W / 2 + (Math.random() - 0.5) * 200;
          n.y = H / 2 + (Math.random() - 0.5) * 200;
        }
      });
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

      // If the reset token changed, wipe in-memory positions so every node
      // is treated as fresh and gets a clean reaction-order layout.
      if (x.resetToken != null && x.resetToken !== state.resetToken) {
        state.resetToken = x.resetToken;
        state.nodes = [];
      } else if (x.resetToken != null) {
        state.resetToken = x.resetToken;
      }

      // Build lookup for R-side persisted positions (from saved .rds).
      // x.layout is a named object: { nodeId: {x, y}, ... }
      var layoutMap = (x.layout && typeof x.layout === 'object') ? x.layout : {};

      // Carry forward (x, y, vx, vy, fx, fy) from any node that survives
      // the diff. Priority order for new nodes:
      //   1. In-memory position (within-session drag or prior render)
      //   2. R-side persisted layout (restored from saved .rds)
      //   3. Circle placement (fresh auto-layout)
      var oldById = {};
      state.nodes.forEach(function(n) { oldById[n.id] = n; });
      var freshNodes = [];
      newNodes.forEach(function(n) {
        var prev = oldById[n.id];
        if (prev) {
          n.x  = prev.x;
          n.y  = prev.y;
          n.vx = prev.vx;
          n.vy = prev.vy;
          if (prev.fx != null) n.fx = prev.fx;
          if (prev.fy != null) n.fy = prev.fy;
        } else if (layoutMap[n.id]) {
          var saved = layoutMap[n.id];
          n.x = saved.x;  n.y = saved.y;
          n.fx = saved.x; n.fy = saved.y;
        } else {
          freshNodes.push(n);
        }
      });

      // Place fresh nodes using reaction definition order as the grid backbone.
      if (freshNodes.length > 0) {
        reactionOrderLayout(freshNodes, newNodes, newEdges);
      }

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

      // Refresh labels on all nodes so renames from the edit modal show up.
      nodeAll.select('text.modelDiagram-node-label')
        .text(function(d) { return d.label || d.id; });

      // Click handlers — set on nodeAll/edgeAll every render so the closure
      // always references the current selections for selection highlighting.
      nodeAll.on('click', function(event, d) {
        event.stopPropagation();
        state.nodesG.selectAll('g.modelDiagram-node')
          .classed('modelDiagram-selected', false);
        state.edgesG.selectAll('line.modelDiagram-edge')
          .classed('modelDiagram-selected', false);
        d3.select(this).classed('modelDiagram-selected', true);
        if (state.hopDepth > 0) {
          applyPathwayHighlight(d.id, 'node');
        }
        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue('modelDiagram_node_click',
            { id: d.id, type: d.type }, { priority: 'event' });
        }
      });
      // Stop dblclick on nodes from bubbling to the SVG background handler.
      nodeAll.on('dblclick', function(event) { event.stopPropagation(); });
      nodeAll.style('cursor', 'pointer');

      edgeAll.on('click', function(event, d) {
        event.stopPropagation();
        state.nodesG.selectAll('g.modelDiagram-node')
          .classed('modelDiagram-selected', false);
        state.edgesG.selectAll('line.modelDiagram-edge')
          .classed('modelDiagram-selected', false);
        d3.select(this).classed('modelDiagram-selected', true);
        if (state.hopDepth > 0) {
          applyPathwayHighlight(d.id, 'edge');
        }
        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue('modelDiagram_edge_click',
            { reactionId: d.reactionId, role: d.role }, { priority: 'event' });
        }
      });
      edgeAll.on('dblclick', function(event) { event.stopPropagation(); });
      edgeAll.style('cursor', 'pointer');

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

      // If any nodes need placement (no fx/fy yet), run the simulation
      // synchronously to convergence so the layout is ready immediately when
      // the user navigates to the tab — requestAnimationFrame doesn't fire on
      // hidden tabs, so animated settling would never happen.
      var hasUnpinned = state.nodes.some(function(n) { return n.fx == null; });
      if (hasUnpinned) {
        // Also refresh the center force in case width was 0 at init time.
        state.simulation.force('center',
          d3.forceCenter(liveWidth() / 2, liveHeight() / 2));
        state.simulation.stop();
        for (var i = 0; i < 200; i++) {
          state.simulation.tick();
        }
        state.nodes.forEach(function(n) {
          clampToBounds(n);
          if (n.fx == null) { n.fx = n.x; n.fy = n.y; }
        });
        edgeAll
          .attr('x1', function(d) { return d.source.x; })
          .attr('y1', function(d) { return d.source.y; })
          .attr('x2', function(d) { return d.target.x; })
          .attr('y2', function(d) { return d.target.y; });
        nodeAll.attr('transform', function(d) {
          return 'translate(' + d.x + ',' + d.y + ')';
        });
        // Auto-fit the new layout into the viewport.
        fitView(true);
      } else {
        // All nodes already pinned — sync positions once without restarting
        // the simulation. Restarting would run clampToBounds in the tick
        // handler on every frame using the current state.width, which jolts
        // nodes near the SVG edges whenever the container resizes slightly
        // (e.g., a Shiny panel rendering below triggers a resize event).
        edgeAll
          .attr('x1', function(d) { return d.source.x; })
          .attr('y1', function(d) { return d.source.y; })
          .attr('x2', function(d) { return d.target.x; })
          .attr('y2', function(d) { return d.target.y; });
        nodeAll.attr('transform', function(d) {
          return 'translate(' + d.x + ',' + d.y + ')';
        });
      }

      // Re-apply the shrink-reactions sizing so any reactions added by this
      // render pass honour the current toggle state.
      applyShrinkReactions();
    }

    function clearEmptyMessage() {
      if (state.svg) state.svg.selectAll('.modelDiagram-empty').remove();
    }

    function showEmptyMessage() {
      if (!state.svg) return;
      clearEmptyMessage();
      state.svg.append('text')
        .attr('class', 'modelDiagram-empty')
        .attr('x', liveWidth()  / 2)
        .attr('y', liveHeight() / 2)
        .attr('text-anchor', 'middle')
        .attr('dominant-baseline', 'middle')
        .text('Add species and reactions to see the diagram.');
    }

    // ---- Public API --------------------------------------------------------
    return {
      renderValue: function(x) {
        if (!x) return;
        // Refresh state dimensions from the live DOM. The factory receives
        // width=0 when the tab is hidden; pick up the real size now.
        var lw = liveWidth(), lh = liveHeight();
        state.width  = lw;
        state.height = lh;
        ensureSvg();
        if (state.svg) {
          state.svg.attr('width', lw).attr('height', lh);
        }
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
        // Only update SVG dimensions. Do not restart the simulation or call
        // fitView — the user may have manually zoomed/panned, and any Shiny
        // output rendering (e.g. the info panel below) triggers resize() with
        // a trivial size change that must not disturb the current viewport.
        // Use the Fit button to re-center after a genuine window resize.
        state.width  = newWidth;
        state.height = newHeight;
        if (state.svg) {
          state.svg.attr('width', newWidth).attr('height', newHeight);
        }
      },

      _state: state
    };
  }
});
