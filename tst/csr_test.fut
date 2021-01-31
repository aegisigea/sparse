import "../src/csr"

import "../lib/github.com/diku-dk/linalg/linalg"

-- /=======\
-- # SETUP #
-- \=======/

module i32_csr = mk_csr i32

module i32_field : field with t = i32 = {
  type t = i32
  let (+) = (i32.+)
  let (-) = (i32.-)
  let (*) = (i32.*)
  let (/) = (i32./)
  let neg = i32.negate
  let (<) = (i32.<)
  let abs = i32.abs
  let i32 = id
  let fma a b c = a + b * c
}
module i32_linalg = mk_linalg i32_field

-- /=========\
-- # HELPERS #
-- \=========/

let wrap_rotate [n] 't k (arr : [n]t) : [n]t = rotate (k % n) arr

-- /=======\
-- # TESTS #
-- \=======/

-- ==
-- entry: test_sparsify_densify
-- nobench compiled input { empty([0][0]i32) }
-- output { empty([0][0]i32) }
-- nobench compiled input { [[0,0,0],[0,0,0],[0,0,0]] }
-- output { [[0,0,0],[0,0,0],[0,0,0]] }
-- nobench compiled input { [[5,0,0,0],[0,8,0,0],[0,0,3,0],[0,6,0,0]] }
-- output { [[5,0,0,0],[0,8,0,0],[0,0,3,0],[0,6,0,0]] }
entry test_sparsify_densify [m][n]
      (dmat : [m][n]i32) : [m][n]i32 =
  dmat |> i32_csr.sparsify |> i32_csr.densify :> [m][n]i32

-- ==
-- entry: test_gather
-- nobench compiled input { empty([0][0]i32) [0i64] [0i64] }
-- error: csr.fut
-- nobench compiled input { [[5,0,0,0],[0,8,0,0],[0,0,3,0],[0,6,0,0]] [0i64,2,0,3,1] [0i64,2,0,1,0] }
-- output { [5,3,5,6,0] }
entry test_gather [m][n][k]
      (dmat : [m][n]i32) (row_idx : [k]i64) (col_idx : [k]i64) : [k]i32 =
  dmat |> i32_csr.sparsify |> flip i32_csr.gather (zip row_idx col_idx)

-- ==
-- entry: test_scale
-- nobench compiled input { 8 [[0,0,0],[0,0,0],[0,0,0]] }
-- output { [[0,0,0],[0,0,0],[0,0,0]] }
-- nobench compiled input { 2 [[1,2,3],[4,5,6],[7,8,9]] }
-- output { [[2,4,6],[8,10,12],[14,16,18]] }
entry test_scale [m][n]
    (k : i32)  (dmat : [m][n]i32) : [m][n]i32 =
  dmat |> i32_csr.sparsify |> i32_csr.scale k |> i32_csr.densify :> [m][n]i32

-- ==
-- entry: test_rotate_row
-- nobench compiled input { 0i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- nobench compiled input { 1i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80],[10,20,0,0,0,0]] }
-- nobench compiled input { -1i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[0,0,0,0,0,80],[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0]] }
-- nobench compiled input { 10i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[0,0,50,60,70,0],[0,0,0,0,0,80],[10,20,0,0,0,0],[0,30,0,40,0,0]] }
entry test_rotate_row [m][n] k
      (dmat : [m][n]i32) : [m][n]i32 =
  dmat |> i32_csr.sparsify |> i32_csr.rotate_row k |> i32_csr.densify :> [m][n]i32

-- ==
-- entry: test_rotate_col
-- nobench compiled input { 0i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- nobench compiled input { 1i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[20,0,0,0,0,10],[30,0,40,0,0,0],[0,50,60,70,0,0],[0,0,0,0,80,0]] }
-- nobench compiled input { -1i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[0,10,20,0,0,0],[0,0,30,0,40,0],[0,0,0,50,60,70],[80,0,0,0,0,0]] }
-- nobench compiled input { 10i64 [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[0,0,10,20,0,0],[0,0,0,30,0,40],[70,0,0,0,50,60],[0,80,0,0,0,0]] }
entry test_rotate_col [m][n] k
      (dmat : [m][n]i32) : [m][n]i32 =
  dmat |> i32_csr.sparsify |> i32_csr.rotate_col k |> i32_csr.densify :> [m][n]i32

-- ==
-- entry: test_transpose
-- nobench compiled input { [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
-- output { [[10,0,0,0],[20,30,0,0],[0,0,50,0],[0,40,60,0],[0,0,70,0],[0,0,0,80]] }
-- nobench compiled input { [[10,0,0,0],[20,30,0,0],[0,0,50,0],[0,40,60,0],[0,0,70,0],[0,0,0,80]] }
-- output { [[10,20,0,0,0,0],[0,30,0,40,0,0],[0,0,50,60,70,0],[0,0,0,0,0,80]] }
entry test_transpose [m][n]
      (dmat : [m][n]i32) : [n][m]i32 =
  dmat |> i32_csr.sparsify |> i32_csr.transpose |> i32_csr.densify :> [n][m]i32

-- ==
-- entry: test_smat_dvec_mul
-- nobench compiled input { [[1,2,3],[4,5,6],[7,8,9],[10,11,12]] [13,14,15] }
-- output { [86,212,338,464] }
entry test_smat_dvec_mul [m][n]
      (dmat : [m][n]i32) (dvec : [n]i32) : [m]i32 =
  dmat |> i32_csr.sparsify |> flip i32_csr.smat_dvec_mul dvec :> [m]i32

-- ==
-- entry: test_smat_dvec_mul_prime
-- nobench compiled input { [[1,2,3],[4,5,6],[7,8,9],[10,11,12]] [13,14,15] }
-- output { [86,212,338,464] }
entry test_smat_dvec_mul_prime [m][n]
      (dmat : [m][n]i32) (dvec : [n]i32) : [m]i32 =
  dmat |> i32_csr.sparsify |> flip i32_csr.smat_dvec_mul' dvec :> [m]i32

-- ==
-- entry: test_smat_dvec_mulT
-- nobench compiled input { [[1,4,7,10],[2,5,8,11],[3,6,9,12]] [13,14,15] }
-- output { [86,212,338,464] }
entry test_smat_dvec_mulT [m][n]
      (dmat : [m][n]i32) (dvec : [m]i32) : [n]i32 =
  dmat |> i32_csr.sparsify |> flip i32_csr.smat_dvec_mulT dvec :> [n]i32

-- /============\
-- # PROPERTIES #
-- \============/

-- ==
-- entry: test_sparsify_densify_equivalence
-- nobench input @ tmat.in
-- output { true }
entry test_sparsify_densify_equivalence [o][m][n]
      (dmats : [o][m][n]i32) : bool =
	let expected = dmats
  let actual   = map test_sparsify_densify dmats
  in expected == actual

-- ==
-- entry: test_scale_equivalence
-- nobench input @ tval_tmat.in
-- output { true }
entry test_scale_equivalence [o][m][n]
      (ks : [o]i64) (dmats : [o][m][n]i32) : bool =
  let expected = map2 (map <-< map <-< (*) <-< i32.i64) ks dmats
  let actual   = map2 (test_scale <-< i32.i64) ks dmats
  in expected == actual

-- ==
-- entry: test_rotate_row_equivalence
-- nobench input @ tval_tmat.in
-- output { true }
entry test_rotate_row_equivalence [o][m][n]
      (ks : [o]i64) (dmats : [o][m][n]i32) : bool =
  let expected = map2 wrap_rotate ks dmats
  let actual   = map2 test_rotate_row ks dmats
  in expected == actual

-- ==
-- entry: test_rotate_col_equivalence
-- nobench input @ tval_tmat.in
-- output { true }
entry test_rotate_col_equivalence [o][m][n]
      (ks : [o]i64) (dmats : [o][m][n]i32) : bool =
	let expected = map2 (map <-< wrap_rotate) ks dmats
  let actual   = map2 test_rotate_col ks dmats
  in expected == actual

-- ==
-- entry: test_transpose_equivalence
-- nobench input @ tmat.in
-- output { true }
entry test_transpose_equivalence [o][m][n]
      (dmats : [o][m][n]i32) =
	let expected = map transpose dmats
  let actual   = map test_transpose dmats
  in expected == actual

-- ==
-- entry: test_smat_dvec_mul_equivalence
-- nobench input @ tmat_tvec.in
-- output { true }
entry test_smat_dvec_mul_equivalence [o][m][n]
      (dmats : [o][m][n]i32) (dvecs : [o][n]i32) : bool =
  let expected = map2 i32_linalg.matvecmul_row dmats dvecs
  let actual   = map2 test_smat_dvec_mul dmats dvecs
  in expected == actual

-- ==
-- entry: test_smat_dvec_mul_prime_equivalence
-- nobench input @ tmat_tvec.in
-- output { true }
entry test_smat_dvec_mul_prime_equivalence [o][m][n]
      (dmats : [o][m][n]i32) (dvecs : [o][n]i32) =
  let expected = map2 i32_linalg.matvecmul_row dmats dvecs
  let actual   = map2 test_smat_dvec_mul_prime dmats dvecs
  in expected == actual

-- ==
-- entry: test_smat_dvec_mulT_equivalence
-- nobench input @ tmat_tvec_2.in
-- output { true }
entry test_smat_dvec_mulT_equivalence [o][m][n]
      (dmats : [o][m][n]i32) (dvecs : [o][m]i32) =
  let expected = map2 (i32_linalg.matvecmul_row <-< transpose) dmats dvecs
  let actual   = map2 test_smat_dvec_mulT dmats dvecs
  in expected == actual

-- /============\
-- # BENCHMARKS #
-- \============/

-- ==
-- entry: bench_scale_reg
-- notest input @ dmat_500.in   auto output
-- notest input @ dmat_1000.in  auto output
-- notest input @ dmat_2000.in  auto output
-- notest input @ dmat_4000.in  auto output
-- notest input @ dmat_8000.in  auto output
-- notest input @ dmat_10000.in auto output
entry bench_scale_reg [m][n]
      (dmat : [m][n]i32) : [m][n]i32 =
  map (map (*42)) dmat

-- ==
-- entry: bench_scale_csr
-- notest input @ smat_500.in     auto output
-- notest input @ smat_1000.in    auto output
-- notest input @ smat_5000.in    auto output
-- notest input @ smat_10000.in   auto output
-- notest input @ smat_50000.in   auto output
-- notest input @ smat_100000.in  auto output
-- notest input @ smat_200000.in  auto output
-- notest input @ smat_400000.in  auto output
-- notest input @ smat_500000.in  auto output
-- notest input @ smat_600000.in  auto output
-- notest input @ smat_700000.in  auto output
-- notest input @ smat_800000.in  auto output
-- notest input @ smat_900000.in  auto output
-- notest input @ smat_1000000.in auto output
entry bench_scale_csr [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) =
  i32_csr.of_csr vs cs rs n |> i32_csr.scale 42 |> i32_csr.to_csr

-- ==
-- entry: bench_rotate_row_reg
-- notest input @ dmat_500.in   auto output
-- notest input @ dmat_1000.in  auto output
-- notest input @ dmat_2000.in  auto output
-- notest input @ dmat_4000.in  auto output
-- notest input @ dmat_8000.in  auto output
-- notest input @ dmat_10000.in auto output
entry bench_rotate_row_reg [m][n]
      (dmat : [m][n]i32) : [m][n]i32 =
  wrap_rotate 1337 dmat

-- ==
-- entry: bench_rotate_row_csr
-- notest input @ smat_500.in     auto output
-- notest input @ smat_1000.in    auto output
-- notest input @ smat_5000.in    auto output
-- notest input @ smat_10000.in   auto output
-- notest input @ smat_50000.in   auto output
-- notest input @ smat_100000.in  auto output
-- notest input @ smat_200000.in  auto output
-- notest input @ smat_400000.in  auto output
-- notest input @ smat_800000.in  auto output
-- notest input @ smat_1000000.in auto output
entry bench_rotate_row_csr [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) =
  i32_csr.of_csr vs cs rs n |> i32_csr.rotate_row 1337 |> i32_csr.to_csr

-- ==
-- entry: bench_rotate_col_reg
-- notest input @ dmat_500.in   auto output
-- notest input @ dmat_1000.in  auto output
-- notest input @ dmat_2000.in  auto output
-- notest input @ dmat_4000.in  auto output
-- notest input @ dmat_8000.in  auto output
-- notest input @ dmat_10000.in auto output
entry bench_rotate_col_reg [m][n]
      (dmat : [m][n]i32) : [m][n]i32 =
  map (wrap_rotate 1337) dmat

-- ==
-- entry: bench_rotate_col_csr
-- notest input @ smat_500.in     auto output
-- notest input @ smat_1000.in    auto output
-- notest input @ smat_5000.in    auto output
-- notest input @ smat_10000.in   auto output
-- notest input @ smat_50000.in   auto output
-- notest input @ smat_100000.in  auto output
-- notest input @ smat_200000.in  auto output
-- notest input @ smat_400000.in  auto output
-- notest input @ smat_800000.in  auto output
-- notest input @ smat_1000000.in auto output
entry bench_rotate_col_csr [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) =
  i32_csr.of_csr vs cs rs n |> i32_csr.rotate_col 1337 |> i32_csr.to_csr

-- ==
-- entry: bench_transpose_reg
-- notest input @ dmat_500.in   auto output
-- notest input @ dmat_1000.in  auto output
-- notest input @ dmat_2000.in  auto output
-- notest input @ dmat_4000.in  auto output
-- notest input @ dmat_8000.in  auto output
-- notest input @ dmat_10000.in auto output
entry bench_transpose_reg [m][n]
    (dmat : [m][n]i32) : [n][m]i32 =
  transpose dmat

-- ==
-- entry: bench_transpose_csr
-- notest input @ smat_500.in     auto output
-- notest input @ smat_1000.in    auto output
-- notest input @ smat_5000.in    auto output
-- notest input @ smat_10000.in   auto output
-- notest input @ smat_50000.in   auto output
-- notest input @ smat_100000.in  auto output
-- notest input @ smat_200000.in  auto output
-- notest input @ smat_400000.in  auto output
-- notest input @ smat_800000.in  auto output
-- notest input @ smat_1000000.in auto output
entry bench_transpose_csr [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) =
  i32_csr.of_csr vs cs rs n |> i32_csr.transpose |> i32_csr.to_csr

-- ==
-- entry: bench_dmat_dvec_mul_reg
-- notest input @ dmat_dvec_500.in   auto output
-- notest input @ dmat_dvec_1000.in  auto output
-- notest input @ dmat_dvec_2000.in  auto output
-- notest input @ dmat_dvec_4000.in  auto output
-- notest input @ dmat_dvec_8000.in  auto output
-- notest input @ dmat_dvec_10000.in auto output
entry bench_dmat_dvec_mul_reg [m][n]
    (dmat : [m][n]i32) (dvec : [n]i32) : [m]i32 =
  i32_linalg.matvecmul_row dmat dvec

-- ==
-- entry: bench_smat_dvec_mul_csr_inner_seq
-- notest input @ smat_dvec_500.in     auto output
-- notest input @ smat_dvec_1000.in    auto output
-- notest input @ smat_dvec_5000.in    auto output
-- notest input @ smat_dvec_10000.in   auto output
-- notest input @ smat_dvec_50000.in   auto output
-- notest input @ smat_dvec_100000.in  auto output
-- notest input @ smat_dvec_200000.in  auto output
-- notest input @ smat_dvec_400000.in  auto output
-- notest input @ smat_dvec_800000.in  auto output
-- notest input @ smat_dvec_1000000.in auto output
entry bench_smat_dvec_mul_csr_inner_seq [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) (dvec : []i32) =
  i32_csr.of_csr vs cs rs n |> flip i32_csr.smat_dvec_mul dvec

-- ==
-- entry: bench_smat_dvec_mul_csr_inner_par
-- notest input @ smat_dvec_500.in     auto output
-- notest input @ smat_dvec_1000.in    auto output
-- notest input @ smat_dvec_5000.in    auto output
-- notest input @ smat_dvec_10000.in   auto output
-- notest input @ smat_dvec_50000.in   auto output
-- notest input @ smat_dvec_100000.in  auto output
-- notest input @ smat_dvec_200000.in  auto output
-- notest input @ smat_dvec_400000.in  auto output
-- notest input @ smat_dvec_800000.in  auto output
-- notest input @ smat_dvec_1000000.in auto output
entry bench_smat_dvec_mul_csr_inner_par [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) (dvec : []i32) =
  i32_csr.of_csr vs cs rs n |> flip i32_csr.smat_dvec_mul' dvec

-- ==
-- entry: bench_dmat_dvec_mulT_reg
-- notest input @ dmat_dvec_500.in   auto output
-- notest input @ dmat_dvec_1000.in  auto output
-- notest input @ dmat_dvec_2000.in  auto output
-- notest input @ dmat_dvec_4000.in  auto output
-- notest input @ dmat_dvec_8000.in  auto output
-- notest input @ dmat_dvec_10000.in auto output
entry bench_dmat_dvec_mulT_reg [m][n]
    (dmat : [m][n]i32) (dvec : [m]i32) : [n]i32 =
  transpose dmat |> flip i32_linalg.matvecmul_row dvec

-- ==
-- entry: bench_smat_dvec_mulT_csr_direct
-- notest input @ smat_dvec_500.in     auto output
-- notest input @ smat_dvec_1000.in    auto output
-- notest input @ smat_dvec_5000.in    auto output
-- notest input @ smat_dvec_10000.in   auto output
-- notest input @ smat_dvec_50000.in   auto output
-- notest input @ smat_dvec_100000.in  auto output
-- notest input @ smat_dvec_200000.in  auto output
-- notest input @ smat_dvec_400000.in  auto output
-- notest input @ smat_dvec_800000.in  auto output
-- notest input @ smat_dvec_1000000.in auto output
entry bench_smat_dvec_mulT_csr_direct [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) (dvec : [m]i32) =
  i32_csr.of_csr vs cs rs n |> flip i32_csr.smat_dvec_mulT dvec

-- ==
-- entry: bench_smat_dvec_mulT_csr_composed
-- notest input @ smat_dvec_500.in     auto output
-- notest input @ smat_dvec_1000.in    auto output
-- notest input @ smat_dvec_5000.in    auto output
-- notest input @ smat_dvec_10000.in   auto output
-- notest input @ smat_dvec_50000.in   auto output
-- notest input @ smat_dvec_100000.in  auto output
-- notest input @ smat_dvec_200000.in  auto output
-- notest input @ smat_dvec_400000.in  auto output
-- notest input @ smat_dvec_800000.in  auto output
-- notest input @ smat_dvec_1000000.in auto output
entry bench_smat_dvec_mulT_csr_composed [nnz][m]
    (vs : [nnz]i32) (cs : [nnz]i64) (rs: [m]i64) (n : i64) (dvec : [m]i32) =
  i32_csr.of_csr vs cs rs n |> i32_csr.transpose |> flip i32_csr.smat_dvec_mul dvec