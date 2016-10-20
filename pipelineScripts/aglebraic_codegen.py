import itertools
import random

class Sequence(object):
    def __init__(self, **args):
        self.seq = [args]
    
    def __rshift__(self, other):
        if hasattr(other, "seq"):
            ret = Sequence()
            ret.seq = self.seq + other.seq 
            return ret
        if hasattr(other, "alt"):
            ret = Sequence()
            ret.seq = self.seq + [other]
            return ret        
    
    def __or__(self, other):
        if hasattr(other, "seq"):
            return Alternative(self, other)
        if hasattr(other, "alt"):
            return other | self
        
    def __mul__(self, a):
        other = Sequence() 
        other.seq = self.seq * a
        return other
    __rmul__ = __mul__

        
class Alternative(object):
    def __init__(self, a, b):
        self.alt = [a, b]

    def __or__(self, other):
        ret = Alternative(0,0)
        if hasattr(other, "alt"):
            ret.alt = self.alt + other.alt 
            return ret
        if hasattr(other, "seq"):
            ret.alt = self.alt + [other]
            return ret
        
    def __rshift__(self, other):
        ret = Sequence()
        if hasattr(other, "seq"):
            ret.seq = [self] + other.seq
            return ret
        if hasattr(other, "alt"):
            ret.seq = [self, other]
            return ret

    def __mul__(self, a):
        other = Sequence()
        other.seq = [self for __ in range(a)]
        return other
    __rmul__ = __mul__
    
    
def expand_sequence(to_expand, append=None, t=False, **args):
    ret = None
    keys = list(to_expand.iterkeys())
    for vals in itertools.product(*list(to_expand.itervalues())):
        s = Sequence(
            values=dict(zip(keys, vals)),
            **args
        )
        if append is not None:
            if not t:
                s = s >> append
            else:
                s = s >> append | s 
        ret = s if ret is None else ret >> s 
    return ret

def expand_alternative(to_expand, append=None, t=False, **args):
    ret = None
    keys = list(to_expand.iterkeys())
    for vals in itertools.product(*list(to_expand.itervalues())):
        s = Sequence(
            values=dict(zip(keys, vals)),
            **args
        )
        if append is not None:
            if not t:
                s = s >> append
            else:
                s = s >> append | s 
        ret = s if ret is None else ret | s 
    return ret


def sample(x):
    if hasattr(x, "alt"):
        return sample(random.choice(x.alt))
    if hasattr(x, "seq"):
        r = []
        for s in x.seq:
            r += sample(s)
        return r
    return [x]
    
# This makes "let make_type" function declarations -- those are very useful 
# in reducing the size fo the generated code.
# The code itself is placed in the "recordMakers" dict.
def create_makers(data,  recordMakers, vsname="x"):
    for entry in data:
        if "types" not in entry: 
            continue
        if "type" not in entry:
            raise KeyError("Both 'type' and 'types' keys shoud be present.")
        if entry["type"] in recordMakers:
            continue # already done                            
        nparams =  len(entry["types"]) + len(entry.get("values",[]))      
        parameters = ",".join([vsname+str(i+1) for i in range(nparams)])

        code = []
        code.append("let make_{0} ({1}) = {{".format( entry["type"], parameters))
        n = 0
        for value in entry.get("values",[]):
            n += 1
            code.append("{0} = {1}{2};".format(value, vsname, n))
        for record in entry["types"]:
            n += 1
            code.append("{0} = {1}{2};".format(record, vsname, n))
        code.append("};;")
        recordMakers[entry["type"]] = "\n    ".join(code)
    return recordMakers


def type_search_space(data, vsname = "m"):
    n = 0
    search_space_code = ["type search_space = {"]
    for entry in data:
        if 'types' not in entry:
            continue    
        for name, tp in entry['types'].iteritems():
            n += 1
            search_space_code.append("{0}{1} : {2};".format(vsname, n, tp))
    return "\n    ".join(search_space_code) + "\n};;"

def search_space_to_list(data, vsname="m"):
    n = 0
    to_list_code = [ "let search_space_to_list x = [" ]
    for entry in data:
        cs = entry["constructors"]
        if 'types' not in entry:
            to_list_code.append("(".join(cs) + ")"*(len(cs)-1) + ";")
            continue  
        if "type" not in entry:
            raise KeyError("Both 'type' and 'types' keys shoud be present.")

        params = []  
        if 'values' in entry:
            params = [ str(value) for tp,value in entry['values'].iteritems()]  

        for name, tp in entry['types'].iteritems():
            n += 1
            params.append("{0}.{1}{2}".format("x",vsname,n))

        mk = "make_{0}({1})".format(entry['type'],",".join(params) )
        to_list_code.append("(".join(cs + [mk]) + ")"*(len(cs)) + ";")

    return "\n    ".join(to_list_code) + "\n];;"

def generate_code(data):
    makers = {}
    create_makers(data, makers)
    gcode = ""
    for code in makers.itervalues():
        gcode += code + "\n"
    gcode += "\n\n"
    gcode += type_search_space(data)
    gcode += "\n\n"
    gcode += search_space_to_list(data)
    return gcode
