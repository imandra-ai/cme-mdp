function convertTransition(tr) {
    if(tr.ChangeType == "NotRelevant") return {name: "Not_Relevant"};
    var ret = { name: tr.ChangeType + " LastSeqN = " + tr.MessageTime };
    function processSide(side){
        return side
            .filter(function(d){return d.Price != undefined;})
            .map(function (ord, i){
                var txt = [
                    "Order #" + i,
                    "Price=" + ord.Price,
                    "Qty=" + ord.Quantity,
                ];
                if(ord.NumOrders) txt.push("Num=" + ord.NumOrders);
                return { name: txt.join(" ")};
            });
    };
    ret.children = [
        { name: "MultiBuy"    , children: processSide(tr.Books.Multi.Buys)     },
        { name: "MultiSell"   , children: processSide(tr.Books.Multi.Sells)    },
        { name: "ImpliedBuy"  , children: processSide(tr.Books.Implied.Buys)   },
        { name: "ImpliedSell" , children: processSide(tr.Books.Implied.Sells)  },
        { name: "CombinedBuy" , children: processSide(tr.Books.Combined.Buys)  },
        { name: "CombinedSell", children: processSide(tr.Books.Combined.Sells) } 
    ];
    return ret;
};

function convertTransitions(intr) {
    var root = { name: "Transitions" };
    root.children = intr.InternalTransitions.map(convertTransition);
    return root;
};

