// Model Diagram htmlwidget for BioModME.
//
// Bipartite graph visualization: species nodes (circles) and reaction
// nodes (squares) connected by reactant / product / modifier edges.
//
// This file is the scaffolding stub committed in commit 1. The actual
// rendering, force simulation, drag handling, and click events land in
// later commits (#3 render, #4 drag, #6 click). Persistent state held
// on the widget instance is set up here so subsequent commits extend
// rather than restructure.

HTMLWidgets.widget({
  name: 'modelDiagram',
  type: 'output',

  factory: function(el, width, height) {
    // Persistent per-instance state. Created once on first renderValue
    // and reused across subsequent re-renders so the force simulation
    // does not restart every time the underlying model changes.
    var state = {
      svg:               null,
      simulation:        null,
      nodes:             [],
      edges:             [],
      compartmentGroups: [],
      initialized:       false
    };

    return {
      renderValue: function(x) {
        // Scaffolding placeholder: the real bipartite render lands in
        // commit #3. For now we draw a centered status message so the
        // tab visibly confirms the widget wired up correctly.
        if (!state.initialized) {
          var svg = d3.select(el).append('svg')
            .attr('class', 'modelDiagram-svg')
            .attr('width', width)
            .attr('height', height);

          svg.append('text')
            .attr('class', 'modelDiagram-placeholder')
            .attr('x', width / 2)
            .attr('y', height / 2)
            .attr('text-anchor', 'middle')
            .attr('dominant-baseline', 'middle')
            .text('modelDiagram scaffolding loaded');

          state.svg = svg;
          state.initialized = true;
        }

        // Stash incoming data; later commits consume it.
        state.nodes             = (x && x.nodes) ? x.nodes : [];
        state.edges             = (x && x.edges) ? x.edges : [];
        state.compartmentGroups = (x && x.compartmentGroups) ? x.compartmentGroups : [];
      },

      resize: function(newWidth, newHeight) {
        if (state.svg) {
          state.svg.attr('width', newWidth).attr('height', newHeight);
        }
      },

      // Exposed for later commits to extend without restructuring.
      _state: state
    };
  }
});
