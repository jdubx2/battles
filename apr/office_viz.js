var margin = {top: 70, right:20, bottom: 20, left: 20},
  width = 700,
  height = 400;

var nodeSize = d3.scaleLinear().range([5,20]);
var edgeSize = d3.scaleLinear().range([1,8]);

var simulation = d3.forceSimulation()
    .force("link", d3.forceLink().id(function(d) { return d.id; }).distance(60))
    .force("charge", d3.forceManyBody().strength(-60))
    .force("center", d3.forceCenter((width + margin.left + margin.right) / 2, (height + margin.bottom) / 2));

var svg = d3.select('#div_graph')
  .append('svg')
    .attr('id', 'svg_graph')
    .attr('width', width + margin.left + margin.right)
    .attr('height', height + margin.top + margin.bottom)
    .append('g')
      .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')')
      .attr('id', 'g_graph');

d3.json("office_graph.json", function(error, data) {
  if (error) throw error;

  console.log(data);

  var edgeCounts = [];
  data.links.forEach(function(d,i) {edgeCounts.push(d['value']);});
  var edgeRange = d3.extent(edgeCounts);
  edgeSize.domain([edgeRange[0], edgeRange[1]]);

  var nodeCounts = [];
  data.nodes.forEach(function(d,i) {nodeCounts.push(d['value']);});
  var nodeRange = d3.extent(nodeCounts);
  nodeSize.domain([nodeRange[0], nodeRange[1]]);

  var tool_tip = d3.tip()
    .attr("class", "d3-tip")
    .offset([-8, 0])
    .html(function(d,i) { return d; });

  svg.call(tool_tip);

  var link = svg.append("g")
    .attr("class", "links")
    .selectAll("line")
    .data(data.links)
    .enter().append("line")
      .attr('class', 'lines')
      .attr("stroke-width", function(d) { return edgeSize(d.value); });

  var node = svg.append("g")
    .attr("class", "nodes")
    .selectAll("circle")
    .data(data.nodes)
    .enter().append("circle")
      .attr('class','circles')
      .attr("r", 0)
      .attr("r", function(d) {return nodeSize(d.value); })
      .attr("fill","black")
      // .attr("fill", function(d) { return colorPicker(d.ing_type, d.aa_group, d.srm); })
      .on('mouseover', function(d,i) {
        tool_tip.show(d.id);})
      .on('mouseout', function(d,i) {
        tool_tip.hide(d.id);})
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));

          node.append("title")
            .text(function(d) { return d.id; });

          simulation.restart();
          simulation.alpha(.5).alphaTarget(0);

          simulation
            .nodes(data.nodes)
            .on("tick", ticked);

          simulation.force("link")
            .links(data.links);

          function ticked() {
            link
                .attr("x1", function(d) { return d.source.x; })
                .attr("y1", function(d) { return d.source.y; })
                .attr("x2", function(d) { return d.target.x; })
                .attr("y2", function(d) { return d.target.y; });

            node
                // .attr("cx", function(d) { return d.x; })
                // .attr("cy", function(d) { return d.y; });
              .attr("cx", function(d) { return d.x = Math.max(nodeSize(d.value), Math.min(width - nodeSize(d.value), d.x)); })
              .attr("cy", function(d) { return d.y = Math.max(nodeSize(d.value), Math.min(height - nodeSize(d.value), d.y)); });
          }

        });

  function dragstarted(d) {
    if (!d3.event.active) simulation.alphaTarget(0.3).restart();
    d.fx = d.x;
    d.fy = d.y;
  }

  function dragged(d) {
    d.fx = d3.event.x;
    d.fy = d3.event.y;
  }

  function dragended(d) {
    if (!d3.event.active) simulation.alphaTarget(0);
    d.fx = null;
    d.fy = null;
  }
