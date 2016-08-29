import os.path
import jinja2

class SearchSpaceEvent(object):
    def __init__(self, **args):
        self.type         = args.get('type', None)
        self.constructors = args['constructors']

def precreateBook(bType, sType):

    medianprice = 100 
    events = []
    constructors=['BookAction', 'ST_Add']
    for i in range(1,6):
        oa_data = "(mk_add_data ({0},{1},OrdBuy,{2},{3}))".format(medianprice - 10*i,i,bType,sType) 
        events.append(SearchSpaceEvent( constructors=['BookAction', 'ST_Add', oa_data]))
        oa_data = "(mk_add_data ({0},{1},OrdSell,{2},{3}))".format(medianprice + 10*i,i,bType,sType) 
        events.append(SearchSpaceEvent( constructors=['BookAction', 'ST_Add', oa_data]))
    return events


ordAdd    = SearchSpaceEvent( constructors=['BookAction',     'ST_Add'], type='ord_add_data' )
ordChange = SearchSpaceEvent( constructors=['BookAction',     'ST_Change'], type='ord_change_data' )
makeSnapA = SearchSpaceEvent( constructors=['ExchangeAction', 'ST_Snapshot', 'SecA'])
makeSnapB = SearchSpaceEvent( constructors=['ExchangeAction', 'ST_Snapshot', 'SecA'])
sendInc   = SearchSpaceEvent( constructors=['ExchangeAction', 'ST_DataSendInc'])
sendSnap  = SearchSpaceEvent( constructors=['ExchangeAction', 'ST_DataSendSnap'])



cppck   = SearchSpaceEvent( constructors=['CopyPackets'])
        
events = []
events += precreateBook("Book_Type_Multi", "SecA") 
events += precreateBook("Book_Type_Multi", "SecB")
events += [ 
    sendInc,
    ordChange,
    ordChange,
    ordChange,
    makeSnapA, 
    makeSnapB, 
    sendInc, 
    sendSnap,
    ordChange,
    ordChange,
    ordChange,
    makeSnapA, 
    makeSnapB, 
    sendInc, 
    sendSnap
]


loader = jinja2.FileSystemLoader('.')
env = jinja2.Environment(loader=loader)
template = loader.load(env,"templateTestgen.ml")


outdir = "generated"
if not os.path.exists(outdir):
    os.mkdir(outdir)

print template.render(events=list(enumerate(events)), outdir=outdir)
