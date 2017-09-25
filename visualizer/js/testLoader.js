function loadTestList(callback) {
    d3.json("cases/tests.json",function(data) {
        d3.select("select").text("");
        var opts = d3.select("select")  
            .selectAll("option")
            .data(data["tests"]);
        opts.enter().append("option")   
            .attr("value", function(d){return d.file;})
            .text(function(d){return d.name;})
            .on("click", function(d){loadTest(d.file).then(callback);});

        loadTest(data["tests"][0].file).then(callback);
    });
};

function loadTest(file) {
    var ref_a  = new Promise( function(resolve, reject) { 
        d3.json("cases/channel_ref_a_"  + file + ".json", resolve ); }); 
    var ref_b  = new Promise( function(resolve, reject) { 
        d3.json("cases/channel_ref_b_"  + file + ".json", resolve ); });
    var snap_a = new Promise( function(resolve, reject) { 
        d3.json("cases/channel_snap_a_" + file + ".json", resolve ); });
    var snap_b = new Promise( function(resolve, reject) { 
        d3.json("cases/channel_snap_b_" + file + ".json", resolve ); });
    var internal = new Promise( function(resolve, reject) { 
        d3.json("cases/internal_" + file + ".json", resolve); });

    return Promise.all([ref_a, ref_b, snap_a, snap_b, internal]).then(function(values){
        return { 
            channels : {
                 ref_a: values[0],
                 ref_b: values[1],
                snap_a: values[2],
                snap_b: values[3]
            },
            internal : values[4]  
        }
    });
};
