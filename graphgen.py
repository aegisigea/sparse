#!/usr/bin/env python

import os
import json
import re
import matplotlib.pyplot as plt
import itertools

test_path = "tst/csr_test.fut"
std_functions = ["scale", "rotate_row", "rotate_col", "transpose"]

def getSize(source):
	m = re.match("((s|d|t)(mat|vec|val)_)+(?P<size>\d*).in", source)
	return int(m.group("size"))

def handle_data(dataset):
	xs = []
	ys = []
	for run in dataset:
		if not isinstance(dataset[run], dict):
			continue
		times = dataset[run]["runtimes"]
		y = round(sum(map(int, times)) / len(times))
		ins = run.split(" ")
		x = getSize(ins[0])
		xs.append(x)
		ys.append(y)
	lists = sorted(zip(*[xs, ys]))
	return list(zip(*lists))

def mk_graph(datasrc, function, folder, versions=["reg","csr"], baseline=None):
	fig, ax = plt.subplots()
	ax.set_xlabel("Input size")
	ax.set_xscale("log")
	ax.set_ylabel("Runtime [µs]")
	ax.set_yscale("log")
	fig.suptitle(function)
	if baseline:
		entryp = "bench_"+baseline+"_reg"
		datasets = datasrc[test_path+':'+entryp]["datasets"]
		plot_x, plot_y = handle_data(datasets)
		line, = ax.plot(plot_x, plot_y)
		line.set_label("reg")
	for ver in versions:
		entryp = "bench_"+function+'_'+ver
		datasets = datasrc[test_path+':'+entryp]["datasets"]
		plot_x, plot_y = handle_data(datasets)
		line, = ax.plot(plot_x, plot_y)
		line.set_label(ver)
	ax.legend()
	ax.grid(linestyle="--")
	plt.savefig("plt/"+folder+'/'+function+".png")
	plt.close()

def parse(jsource):
	destination = jsource.split('.')[0]
	with open(jsource) as file:
		data = json.load(file)
		for func in std_functions:
			mk_graph(data, func, destination)
		# special mat vec mul
		mk_graph(data, "smat_dvec_mul", destination, ["csr_inner_seq", "csr_inner_par"], "dmat_dvec_mul")
		mk_graph(data, "smat_dvec_mulT", destination, ["csr_direct","csr_composed"], "dmat_dvec_mulT")
with os.scandir('.') as cur_dir:
	for entry in cur_dir:
		if entry.name.endswith(".json"):
			destination = "plt/"+entry.name.split('.')[0]
			try:
				os.mkdir(destination)
			except FileExistsError:
				()
			finally:
				parse(entry.name)