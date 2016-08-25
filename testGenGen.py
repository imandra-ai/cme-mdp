import os.path
import jinja2

class SearchSpaceEvent(object):
    def __init__(self, **args):
        self.type         = args.get('type', None)
        self.constructors = args['constructors']

stadd = SearchSpaceEvent( constructors=['BookAction',     'ST_Add'], type='ord_add_data' )
dsinc = SearchSpaceEvent( constructors=['ExchangeAction', 'ST_DataSendInc'])
cppck = SearchSpaceEvent( constructors=['CopyPackets'])
        
events = [
    stadd,
    stadd,
    stadd,
    stadd,
    stadd,
    dsinc,
    stadd,
    stadd,
    cppck
]

loader = jinja2.FileSystemLoader('.')
env = jinja2.Environment(loader=loader)
template = loader.load(env,"templateTestgen.ml")


outdir = "generated"
if not os.path.exists(outdir):
    os.mkdir(outdir)

print template.render(events=list(enumerate(events)), outdir=outdir)
