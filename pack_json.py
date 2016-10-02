import argparse
import json
import glob


def get_test_n(fname):
    return int(fname.split(".")[-2].split('_')[-1])

def find_files(path, glb):
    return { get_test_n(f):f for f in glob.glob(path + "/" + glb) }


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Collect test in the given folder into a single json file.  Prints the result to stdout.")
    parser.add_argument('--in_directory', required=True, dest='indir', help='Input directory.')
    args = parser.parse_args()            

    ref_a  = find_files(args.indir, "*_ref_a_test_*.json")
    ref_b  = find_files(args.indir, "*_ref_b_test_*.json")
    snap_a = find_files(args.indir, "*_snap_a_test_*.json")
    snap_b = find_files(args.indir, "*_snap_b_test_*.json")
    states = find_files(args.indir, "state_ch_test_*.txt") 

    def create_test_json(nTest):
        state_lines = map( lambda x: x.strip(), open(states[nTest]).readlines())
        if (len(state_lines)+1) % 8 != 0: raise
        state_messages = [
            state_lines[8*i:8*i+8][:7] for i in range((1+len(state_lines))/8)
        ]
        msgJson = []
        for message in state_messages:
            if message[0][0] != 'N': raise
            msg = { "N": int(message[0].split()[-1])}
            for book in message[1:]:
                bookInfo = book.split()
                norders = int(bookInfo[1])
                msg[bookInfo[0]] = [
                    {"num": bookInfo[2+3*i], "px": bookInfo[3+3*i], "qty": bookInfo[4+3*i], }
                    for i in range(norders) ]
            msgJson.append(msg)

        return {
            "ref_a":  json.load( open(  ref_a[nTest], 'r')),
            "ref_b":  json.load( open(  ref_b[nTest], 'r')),
            "snap_a": json.load( open( snap_a[nTest], 'r')),
            "snap_b": json.load( open( snap_b[nTest], 'r')),
            "states": msgJson
        }

    data = {'testCases':[create_test_json(j) for j in range(len(ref_a))]}
    print json.dumps(data)
