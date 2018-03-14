var margin = {top: 100, right:100, bottom: 100, left: 100},
  width = 400,
  height = 400;

var colors = ["#bdf0ff", "#ddf2ff","#fff0ff","#ffebd1","#ffe49b","#ffcf49","#ffbe28"]
var lengths = [.85,.503,.286,.191,.126,.074,.03]

var tau = 2 * Math.PI;

var svg = d3.select('#arc_div')
  .append('svg')
    .attr('id', 'arc_svg')
    .attr('width', width + margin.left + margin.right)
    .attr('height', height + margin.top + margin.bottom)
    .append('g')
      .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')')
      .attr('id', 'arc_g');

var adj = .46;

var arc = d3.arc()
    .innerRadius(115)
    .outerRadius(120)
    .startAngle(adj);

for (i = 0; i < colors.length; i++) {
  svg.append("path")
    .datum({endAngle: (tau) * lengths[i] +adj})
    .style("fill", colors[i])
    .attr("d", arc);
  }
