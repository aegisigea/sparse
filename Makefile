SEED = $(shell date +%N)

FUTHARK	= futhark
DATASET = $(FUTHARK) dataset -b -s $(SEED)
TEST    = $(FUTHARK) test --exclude=notest
BENCH	  = $(FUTHARK) bench --backend=opencl
DEPS    = $(FUTHARK) pkg add
SYNC    = $(FUTHARK) pkg sync

PYTHON   = python3
DATAGEN  = $(PYTHON) datagen.py
GRAPHGEN = $(PYTHON) graphgen.py
PIP      = pip3 install --user

APPS_DIR = app
CODE_DIR = src
DEPS_DIR = lib
TEST_DIR = tst
PICS_DIR = plt

APPS_SRC = $(wildcard $(APPS_DIR)/*.fut)
CODE_SRC = $(wildcard $(CODE_DIR)/*.fut)
TEST_SRC = $(wildcard $(TEST_DIR)/*.fut)

FUT_SRC = $(APPS_SRC) $(CODE_SRC) $(TEST_SRC)
FUT_BIN = $(patsubst %.fut,%,$(FUT_SRC))

TEST_IN += tmat.in tvec.in tmat_tvec.in tval_tmat.in tmat_tvec_2.in

BENCH_IN += dmat_500.in dmat_1000.in dmat_2000.in dmat_4000.in dmat_8000.in dmat_10000.in
BENCH_IN += dmat_dvec_500.in dmat_dvec_1000.in dmat_dvec_2000.in dmat_dvec_4000.in dmat_dvec_8000.in dmat_dvec_10000.in
BENCH_IN += smat_500.in smat_1000.in smat_5000.in smat_10000.in smat_50000.in
BENCH_IN += smat_100000.in smat_200000.in smat_400000.in smat_500000.in
BENCH_IN += smat_600000.in smat_700000.in smat_800000.in smat_900000.in smat_1000000.in
BENCH_IN += smat_dvec_500.in smat_dvec_1000.in smat_dvec_5000.in smat_dvec_10000.in smat_dvec_50000.in
BENCH_IN += smat_dvec_100000.in smat_dvec_200000.in smat_dvec_400000.in smat_dvec_800000.in smat_dvec_1000000.in

TEST_DATA  = $(patsubst %.in,$(TEST_DIR)/%.in,$(TEST_IN))
BENCH_DATA = $(patsubst %.in,$(TEST_DIR)/%.in,$(BENCH_IN))

JSON_OUT = opencl.json

MODULES  = futhark_data==1.0 numpy==1.19.5 matplotlib==2.1.2
PACKAGES = lib/github.com/diku-dk/sorts lib/github.com/diku-dk/cpprandom lib/github.com/diku-dk/linalg

# /==========\
# | DATASETS |
# \==========/

$(TEST_DIR)/tmat.in:
	$(DATASET) --i32-bounds=-1:1 -g [100][4][6]i32 > $@

$(TEST_DIR)/tvec.in:
	$(DATASET) --i32-bounds=-20:20 -g [100][6]i32 > $@

$(TEST_DIR)/tmat_tvec.in:
	$(DATASET) --i32-bounds=-1:1 -g [100][4][6]i32 --i32-bounds=-20:20 -g [100][6]i32 > $@

$(TEST_DIR)/tmat_tvec_2.in:
	$(DATASET) --i32-bounds=-1:1 -g [100][4][6]i32 --i32-bounds=-20:20 -g [100][4]i32 > $@

$(TEST_DIR)/tval_tmat.in:
	$(DATASET) --i64-bounds=-20:20 -g [100]i64 --i32-bounds=-1:1 -g [100][4][6]i32 > $@

$(TEST_DIR)/dmat_%.in:
	$(DATAGEN) -o $@ -b [$*][$*]i32

$(TEST_DIR)/dmat_dvec_%.in:
	$(DATAGEN) -o $@ -b [$*][$*]i32 [$*]i32

$(TEST_DIR)/smat_%.in:
	$(DATAGEN) -o $@ -b csr[$*][$*]i32

$(TEST_DIR)/smat_dvec_%.in:
	$(DATAGEN) -o $@ -b csr[$*][$*]i32 [$*]i32

test_data: $(TEST_DATA)

bench_data: $(BENCH_DATA)

data: test_data bench_data

# /==========\
# | PACKAGES |
# \==========/

$(DEPS_DIR)/%:
	$(DEPS) $*

deps: $(PACKAGES)
	$(SYNC)
	$(PIP) $(MODULES)

# /===========\
# | UTILITIES |
# \===========/

test_reset:
	$(RM) $(TEST_DATA)

test: deps test_reset test_data
	$(TEST) $(TEST_SRC)

%.json:
	$(BENCH) --json $@ $(TEST_SRC)

bench_reset:
	$(RM) $(JSON_OUT)

bench: deps bench_reset bench_data $(JSON_OUT)

graph: deps bench_data $(JSON_OUT)
	mkdir -p $(PICS_DIR)
	$(GRAPHGEN)

clean:
	$(RM) $(FUT_BIN)
	$(RM) *.pkg *.json $(APPS_DIR)/*.c $(CODE_DIR)/*.c $(TEST_DIR)/*.c $(TEST_DIR)/*.in $(TEST_DIR)/*.actual $(TEST_DIR)/*.expected
	$(RM) -r $(PICS_DIR) $(DEPS_DIR) $(TEST_DIR)/data

.PHONY: test_data bench_data test_reset bench_reset data deps test bench graph clean