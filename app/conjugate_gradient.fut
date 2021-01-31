import "../src/csr"

-- /=======\
-- # SETUP #
-- \=======/

module f64_csr = mk_csr f64

-- Let the symbols &,>,^ correspond to matrices, vectors and scalars respectively.
-- The following operators all have the form: 'op type type', with 'type' being one
-- of the mentioned symbols, and 'op' being an arithmetic operator. Then '*&>' is
-- multiplication of a matrix and a vector, and '+>>' is addition of two vectors.

-- matrix-vector multiplication
let (*&>) = f64_csr.smat_dvec_mul

-- vector-vector operators
let (+>>) = map2 (f64.+)
let (->>) = map2 (f64.-)
let (*>>) = map2 (f64.*)

-- vector-vector dot-product
let (**>>) v = (*>>) v >-> reduce (+) 0.0

--scaling operators
let (*^>) = map <-< (f64.*)
let (*>^) = flip (*^>)
let (*^&) = f64_csr.scale
let (*&^) = flip (*^&)

-- /=========\
-- # PROGRAM #
-- \=========/

-- Determines whether all entries of a given vector are sufficiently small.
let is_suff_small [n] (eps : f64) (v : [n]f64) : bool =
	all f64.(abs >-> (<= eps)) v

-- Implements the conjugate gradient method without transposition, which
-- approximates the numerical solution of a given system of linear equations.
-- Requires that 'A' is symmetric and positive-definite.
let cg_csr [m] (eps : f64) (A : f64_csr.smat f64) (b : [m]f64) : [m]f64 =
	let x0 = replicate m 0.0
	let r0 = b ->> (A *&> x0 :> [m]f64)
	let p0 = r0
	let (x,_,_) =
		loop (xi,ri,pi) = (x0,r0,p0)
		while !(is_suff_small eps ri) do
			let A_pi = A *&> pi :> [m]f64
			let alph = (ri **>> ri) / (pi **>> A_pi)
			let xk = xi +>> alph *^> pi
			let rk = ri ->> alph *^> A_pi
			let beta = (rk **>> rk) / (ri **>> ri)
			let pk = rk +>> beta *^> pi
			in (xk,rk,pk)
	in x

-- Implements the conjugate gradient method with transposition, which
-- approximates the numerical solution of a given system of linear equations.
-- Does not require that 'A' is symmetric and positive-definite.
let cgnr_csr [m] (eps : f64) (A : f64_csr.smat f64) (b : [m]f64) : []f64 =
	let (_,n) = f64_csr.shape A
	let A_t = f64_csr.transpose A
	let x0 = replicate n 0.0
	let r0 = (A_t *&> b :> [n]f64) ->> (A_t *&> (A *&> x0) :> [n]f64)
	let p0 = r0
	let (x,_,_) =
		loop (xi,ri,pi) = (x0,r0,p0)
		while !(is_suff_small eps ri) do
			let A_t_pi = A_t *&> (A *&> pi) :> [n]f64
			let alph = (ri **>> ri) / (pi **>> A_t_pi)
			let xk = xi +>> alph *^> pi
			let rk = ri ->> alph *^> A_t_pi
			let beta = (rk **>> rk) / (ri **>> ri)
			let pk = rk +>> beta *^> pi
			in (xk,rk,pk)
	in x

-- /=========\
-- # ENTRIES #
-- \=========/

-- ==
-- entry: test_cg_csr
-- nobench compiled input { [[4.0,1.0],[1.0,3.0]] [1.0,2.0] }
-- output { [0.0909,0.6364] }
entry test_cg_csr [n] (A : [n][n]f64) (b : [n]f64) : [n]f64 =
  let s = f64_csr.sparsify A
  in cg_csr 0.0001 s b

-- ==
-- entry: test_cg_csr
-- nobench compiled input { [[4.0,1.0],[1.0,3.0]] [1.0,2.0] }
-- output { [0.0909,0.6364] }
entry test_cgnr_csr [n] (A : [n][n]f64) (b : [n]f64) : [n]f64 =
  let s = f64_csr.sparsify A
  in cgnr_csr 0.0001 s b :> [n]f64