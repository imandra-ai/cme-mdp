function convertRefMessage(msg){
    return { name: [ 
        "Sec #" + msg.SecurityID, 
        "Msg #" + msg.RepSeqNum, 
        msg.MessageType, 
        msg.EntryType,  
        "Price=" + msg.EntryPrice,
        "Size=" + msg.EntrySize,
        "Num=" + msg.NumOrders
    ].join(" ") }
};

function convertSnapMessage(msg){
    var si = { name: [
        "Sec #" + msg.SecurityID,
        "Snap #" + msg.RepSeqNum, 
        "LastMsg=" + msg.LastMsgSeqNumProcessed
    ].join(" ")};
    function processSide(side){
        return side
            .filter(function(d){return d != {};})
            .map(function (ord, i){
                return { name: [
                    "Order #" + i,
                    "Price=" + ord.Price,
                    "Qty=" + ord.Quantity,
                    "Num=" + ord.NumOrders
                ].join(" ")};
            });
    };
    si.children = [
        { name:   "MultiBookBuys"   , children: processSide( msg.MultiBook.Buys    )},
        { name:   "MultiBookSells"  , children: processSide( msg.MultiBook.Sells   )},
        { name: "ImpliedBookBuys"   , children: processSide( msg.ImpliedBook.Buys  )},
        { name: "ImpliedBookBuys"   , children: processSide( msg.ImpliedBook.Sells )}
    ];
    return si;
};

function convertChannels(chan){
    var root = { name: "Channels" };

    function processPackets(packets, fmsg){
        return packets["packets"].map(function(packet){
            return {
                name: "Packet #" + packet.SeqNum,
                children: packet.Messages.map(fmsg)
            };
        });
    };

    root.children = [
        { name: "Incremental Refresh A", children: processPackets (chan.ref_a, convertRefMessage) },
        { name: "Incremental Refresh B", children: processPackets (chan.ref_b, convertRefMessage) },
        { name: "Snapshot Refresh A",    children: processPackets(chan.snap_a, convertSnapMessage) },
        { name: "Snapshot Refresh B",    children: processPackets(chan.snap_b, convertSnapMessage) }
    ];
    return root;
};

