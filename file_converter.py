##	
## 	
##	Aesthetic Integration Ltd.
##  Copyright 2016
##
##	file_converter.py	
## 	
##	

import argparse, sys, ntpath, subprocess, glob
from os.path import isfile, join

## 
## Set the directory where the files are 
## 
parser = argparse.ArgumentParser()
parser.add_argument ('-i', '--input_dir')
parser.add_argument ('-x', '--file_ext')

args = parser.parse_args ()

print args 

if not args.input_dir or not args.file_ext:
	print "Either intput dir or file extension is unspecified!"
	sys.exit(0)


try:
	print args.input_dir + "/" + args.file_ext
	files = glob.glob (args.input_dir + "/" + args.file_ext)

	for f in files:
		if not (isfile (f)): continue

		head, in_file_name = ntpath.split(f)

		elems = in_file_name.split('.')

		out_file_name = elems[0] + ".bin"

#		print 'out_file_name', (args.input_dir + '/' + out_file_name)
		print 'processing', (args.input_dir + '/' + in_file_name)

		try:
#			exec_str = "./json2bin.native -i %s -o %s" % (args.input_dir + '/' + in_file_name, args.input_dir + '/' + out_file_name)
#			print exec_str
			subprocess.call ("./json2bin.native -i %s -o %s" % (args.input_dir + '/' + in_file_name, args.input_dir + '/' + out_file_name), shell=True)

		except Exception, e:
			raise Exception ('Failed on: ' + (args.input_dir + '/' + in_file_name) + ' : ' + str(e))

except Exception, e:
	print "Failed to run the binary conversion script:", e
