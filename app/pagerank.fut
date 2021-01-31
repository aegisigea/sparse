import "../src/csr"

import "../lib/github.com/diku-dk/cpprandom/random"
import "../lib/github.com/diku-dk/linalg/linalg"

-- /=======\
-- # SETUP #
-- \=======/

module f64_csr = mk_csr f64

module f64_field : field with t = f64 = {
  type t = f64
  let (+) = (f64.+)
  let (-) = (f64.-)
  let (*) = (f64.*)
  let (/) = (f64./)
  let neg = f64.negate
  let (<) = (f64.<)
  let abs = f64.abs
  let i32 = f64.i32
  let fma a b c = a + b * c
}
module f64_linalg = mk_linalg f64_field

module rng_engine = minstd_rand
module rand_f64   = uniform_real_distribution f64 rng_engine

-- /=========\
-- # PROGRAM #
-- \=========/

-- Generates a random number between 0 inclusive and 1 exclusive.
let rand = rand_f64.rand (0,1)

-- Dampens the element 'e' with dampening factor 'd' and dimension 'n'.
let dampen d n e = d * e + (1 - d) / (f64.i64 n)

-- Implements the pagerank algorithm using the given dense matrix form directly.
let pagerank_reg [n] (seed : i32) (M : [n][n]f64) (it : i32) (d : f64) : [n]f64 =
	-- generate a random initial vector of the given size
	let v = rng_engine.rng_from_seed [seed]
		|> rng_engine.split_rng n
		|> map rand
		|> unzip
		|> (.1)
	-- normalize the initial vector and dampen the given matrix
	let v_norm = reduce (+) 0 v |> (\sum -> map (\ele -> ele / sum) v)
	let M_hat = map (map (dampen d n)) M
	let result =
		loop v = v_norm for _ in 1...it do
			f64_linalg.matvecmul_row M_hat v
	in result

-- Implements the pagerank algorithm by converting the given dense matrix form
-- to a sparse matrix form internally. Notice that dampening is not viable with
-- this approach, since it unavoidably increases the density of the matrix.
let pagerank_csr [n] (seed : i32) (M : [n][n]f64) (it : i32) : [n]f64 =
	-- generate a random initial vector of the given size
	let v = rng_engine.rng_from_seed [seed]
		|> rng_engine.split_rng n
		|> map rand
		|> unzip
		|> (.1)
	-- normalize the initial vector and convert to sparse matrix form
	let v_norm = reduce (+) 0 v |> (\sum -> map (\ele -> ele / sum) v)
	let M_csr = f64_csr.sparsify M
	let result =
		loop v = v_norm for _ in 1...it do
			f64_csr.smat_dvec_mul M_csr v
	in result :> [n]f64