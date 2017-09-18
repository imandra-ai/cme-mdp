import json
import os.path
import sys

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
            outfile = outdir + "region_" + name.replace("'","x") + "_" + point + ".json"
            print outfile
            with open(outfile, "w") as jsonFile:
                text = testcase['text'].decode('string_escape')
                jsonFile.write(text.rstrip()[1:-1])


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
