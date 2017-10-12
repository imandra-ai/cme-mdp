import json
import os.path
import sys

def create_jsons(testcase, outdir, name):
    channels = [ 
        ("channel_ref_a_", "RefreshA"), 
        ("channel_ref_b_", "RefreshB"),
        ("channel_snap_a_", "SnapshotA"),
        ("channel_snap_b_", "SnapshotB"),
    ]
    outfile = "internal_" + name + ".json"
    outfile = os.path.join(outdir, outfile)
    with open(outfile, "w") as jsonFile:
        json.dump(testcase['output'], jsonFile)
    
    packets = testcase['input']['packets']
    for fchannel, channel in channels:
        data = [p for p in packets if p['Channel'] == channel]
        data = { "packets": data }
        outfile = fchannel + name + ".json"
        outfile = os.path.join(outdir, outfile)
        with open(outfile, "w") as jsonFile:
            json.dump(data, jsonFile)
        create_jsons.tests.append({
            "name" : "test " + str(len(create_jsons.tests)), "file":name
        })
    
create_jsons.tests = []

def processStream(f, outdir):
    tcs = []
    names = {}
    for line in f.readlines():
        if line.strip(" \n") == "":
            continue
        dic = json.loads(line)
        if dic['status_code'] != 200:
            continue

        results = dic['results'][0]
        if 'testcases' not in results:
            continue
        if 'name' not in results:
            continue

        name = results['name']
        if name in names:
            names[name] += 1
        else:
            names[name] = 1
        name += str(names[name])
        testcases = results['testcases']

        if testcases == []:
            continue
        
        tcs.append( (name, testcases) )

        for testcase in testcases:
            point = str(testcase['point'])
            tcname = name.replace("'","x") + "_" + point
            testcase = json.loads(testcase['text'].decode('string_escape'))
            create_jsons(testcase, outdir, tcname)
            
        with open(os.path.join(outdir, "tests.json"), "w") as jsonTests: 
            json.dump({"tests": create_jsons.tests}, jsonTests)


def main():
    if len(sys.argv) == 2:
        infile = sys.stdin
        outdir = sys.argv[1]
    elif len(sys.argv) == 3:
        infile = open(sys.argv[2], 'rb')
        outdir = sys.argv[1]
    else:
        raise SystemExit(sys.argv[0] + " outdir [infile]")

    if not os.path.exists(outdir):
        os.makedirs(outdir)

    processStream(infile, outdir)
    
    infile.close()

if __name__ == '__main__':
    main()
