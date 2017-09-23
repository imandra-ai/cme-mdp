function htree(cont, data){
    //Defaults
    var width  = 400;
    var height = 500;
    var barHeight = 23;
    var barWidth = width * .9;
    var duration = 400;
    var margin = {top: 30, right: 20, bottom: 30, left: 20};
    var show_internal = true;

    cont.style("height", height + margin.top  + margin.bottom)
        .style("overflow-y", "scroll");

    var svg = cont
              .append("svg")
                  .attr("class", "chart")
                  .attr("width",  width  + margin.left + margin.right )
 
    var grp = svg.append("g")
                 .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var tree = d3.tree().nodeSize([23, 23]);

    data.x0 = 0;
    data.y0 = 0;
    
    var root = d3.hierarchy(data);
    var i = 0;
    root.eachAfter(function(d){
        d.id = i; i += 1;
        if(d.depth < 1) return;
        if(d.children){
            d._children = d.children;
            d.children = null;
        }
    });


    function update(s){
        var i = 0;
        var nodes = tree(root)
            .eachBefore(function(n) {n.x = i * barHeight; i += 1;})
            .descendants();
        var height = nodes.length * barHeight + margin.top + margin.bottom;

        svg.transition()
           .duration(duration)
           .attr("height", height);

        var node = grp.selectAll("g.node")
            .data(nodes, function(d, i) { return d.id || (d.id = ++i); });

        // Enter
        var nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .attr("transform", function(d) { return "translate(" + s.y0 + "," + s.x0 + ")"; })
            .style("opacity", 0.01);

        nodeEnter.transition()
            .duration(duration)
            .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })
            .style("opacity", 1);

        nodeEnter.append("rect")
            .attr("y", -barHeight / 2)
            .attr("height", barHeight)
            .attr("width", barWidth)
            .style("fill", color )
            .on("click", click);

        nodeEnter.append("text")
            .attr("dy", 3.5)
            .attr("dx", 5.5)
            .text(function(d) { return d.data.name; });

        // Update
        node.transition()
            .duration(duration)
            .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })
            .style("opacity", 1)
            .select("rect").style("fill", color );

        // Exit 
        node.exit().transition()
            .duration(duration)
            .attr("transform", function(d) { return "translate(" + s.y + "," + s.x + ")"; })
            .style("opacity", 1e-6)
            .remove();

        // Stash the old positions for transition.
        nodes.forEach(function(d) {
            d.x0 = d.x;
            d.y0 = d.y;
        });
    }

    // Toggle children on click.
    function click(d) {
        if (d.children) {
            d._children = d.children;
            d.children = null;
        } else {
            d.children = d._children;
            d._children = null;
        }
        update(d);
    }

    function chart(){
        update(data);
    };

    function color(d){
       return d._children ? "#9bc2ce" : d.children ? "#f0f5f7" : "#21d8af"; 
    };

    chart.switch_internal = function() {
        if( root.data.name != "Transitions" ) return;
        if( !("withInternal" in root) ) root.withInternal = root.children.slice();
        if( !("withoutInternal" in root) ) 
            root.withoutInternal = root.children.filter(function(d){return d.data.name != "Not Relevant";});
        show_internal = !show_internal;
        if(show_internal) {
            root.children = root.withInternal;        
        } else {
            root.children = root.withoutInternal;        
        }
        update(root);
        return show_internal;
    };

    update(data);
    return chart;
} 
