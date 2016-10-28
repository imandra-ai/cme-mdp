import json
import argparse

def formatSide(side):
    for o in side:
        if "NumOrders" not in o: 
            continue
        if o["NumOrders"] == {}: 
            o["NumOrders"] = 0 
    side = [ "{Price} {Quantity} {NumOrders}".format(**s) for s in side if s != {} ]
    return str(len(side)) + " " + " ".join(side)


def print_internal_transitions(filename):
    with open(filename, "r") as f:
        jsonDict = json.load(f)

    transitions = jsonDict['InternalTransitions']
    transitions = [t for t in transitions if 'Books' in t]
    print("N {0}".format(len(transitions)))
    for t in transitions:
	books = t['Books']
	print("MB " + formatSide(books['Multi']['Buys']))
	print("MA " + formatSide(books['Multi']['Sells']))
	print("IB " + formatSide(books['Implied']['Buys']))
	print("IA " + formatSide(books['Implied']['Sells']))
	print("CB " + formatSide(books['Combined']['Buys']))
	print("CA " + formatSide(books['Combined']['Sells']))
	print("")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='CME internal transitions JSON printer')
    parser.add_argument('jsonFile', nargs=1)
    args = parser.parse_args()
    print_internal_transitions(args.jsonFile[0])   
