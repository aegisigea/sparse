#!/usr/bin/env python

import sys
import getopt as go
import re

import numpy as np
import futhark_data as fd

# /=======\
# | SETUP |
# \=======/

def print_usage():
	program = sys.argv[0]

	print("Usage: python3 {} [OPTION...] DATASET...".format(program))
	print("")
	print("OPTIONS:")
	print("  -b, --binary")
	print("  -h, --help")
	print("  -o, --output=FILENAME")
	print("")
	print("FORMAT:")
	print("  DATASET = [FORM] SIZE... TYPE")
	print("  FORM    = csr")
	print("  SIZE    = [NUMBER]")
	print("  TYPE    = i32|i64|f32|f64")

def get_params():
	inputs = sys.argv[1:]

	try:
		opts, args = go.getopt(inputs, "ho:b", ["help", "output=", "binary"])
	except go.GetoptError as error:
		print(error, "\n")
		print_usage()
		sys.exit(2)

	return opts, args

def parse_opts(opts):
	config = {
		"output" : "test.in",
		"binary" : None,
		"mode"   : "w"
	}

	for opt, val in opts:
		if opt in ["-h", "--help"]:
			print_usage()
			sys.exit(0)
		elif opt in ["-o", "--output"]:
			config["output"] = val
		elif opt in ["-b", "--binary"]:
			config["binary"] = True
			config["mode"] = "wb"
		else:
			print("illegal option parameter", "\n")
			print_usage()
			sys.exit(2)

	return config

def parse_args(args):
	inputs = []

	size_pattern = "\[([1-9]?[0-9]+)\]"
	type_pattern = ".*(?P<dtype>i32|i64|f32|f64)"
	form_pattern = "(?P<dform>csr)?.*"

	for arg in args:
		sizes = re.findall(size_pattern, arg)
		sizes = np.array(list(map(int, sizes)), np.int64)

		dtype = re.match(type_pattern, arg).group("dtype")

		if dtype in ["i32"]:
			dtype = np.int32
		elif dtype in ["i64"]:
			dtype = np.int64
		elif dtype in ["f32"]:
			dtype = np.float32
		elif dtype in ["f64"]:
			dtype = np.float64
		else:
			print("illegal data type", "\n")
			print_usage()
			sys.exit(2)

		dform = re.match(form_pattern, arg).group("dform")

		if dform not in [None, "csr"]:
			print("illegal data format", "\n")
			print_usage()
			sys.exit(2)

		inputs.append((sizes, dtype, dform))

	return inputs

# /===========\
# | FUNCTIONS |
# \===========/

def generate_dense(sizes, dtype):
	rng = np.random.default_rng()

	if dtype in [np.int32, np.int64]:
		values = rng.integers(1, 10, sizes, dtype)
	elif dtype in [np.float32, np.float64]:
		values = rng.uniform(1.0, 10.0, sizes).astype(dtype)
	else:
		print("illegal data type", "\n")
		print_usage()
		sys.exit(2)

	return values

def generate_sparse(sizes, dtype):
	rng = np.random.default_rng()

	if len(sizes) == 2:
		num_row, num_col = sizes

		nnz = (num_row + num_col) // 2

		if dtype in [np.int32, np.int64]:
			values = rng.integers(1, 10, nnz, dtype)
		elif dtype in [np.float32, np.float64]:
			values = rng.uniform(1.0, 10.0, nnz).astype(dtype)
		else:
			print("illegal data type", "\n")
			print_usage()
			sys.exit(2)

		row_ptr = np.insert(np.sort(rng.integers(0, nnz + 1, num_row-1, np.int64)), 0, 0)

		get_len = lambda i: (nnz if i == num_row-1 else row_ptr[i+1]) - row_ptr[i]
		row_len = np.array(list(map(get_len, np.arange(num_row))))

		sel_col = lambda k: rng.choice(num_col, k, False)
		col_idx = np.concatenate(list(map(sel_col, row_len)))
	else:
		print("illegal data format", "\n")
		print_usage()
		sys.exit(2)

	return values, col_idx, row_ptr, num_col

# /======\
# | MAIN |
# \======/

def main():
	opts, args = get_params()

	config = parse_opts(opts)
	inputs = parse_args(args)

	with open(config["output"], config["mode"]) as file:
		for sizes, dtype, dform in inputs:
			if dform in [None]:
				values = generate_dense(sizes, dtype)
				fd.dump(values, file, config["binary"])
			elif dform in ["csr"]:
				values, col_idx, row_ptr, num_col = generate_sparse(sizes, dtype)
				fd.dump(values, file, config["binary"])
				fd.dump(col_idx, file, config["binary"])
				fd.dump(row_ptr, file, config["binary"])
				fd.dump(num_col, file, config["binary"])
			else:
				print("illegal data format", "\n")
				print_usage()
				sys.exit(2)

if __name__ == "__main__":
	main()