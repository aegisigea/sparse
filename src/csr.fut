import "sparse"
import "util"

import "../lib/github.com/diku-dk/sorts/radix_sort"

module mk_csr (T : numeric) : sparse_matrix with t = T.t = {

  -- /=======\
  -- # SETUP #
  -- \=======/

  -- Defines the underlying type as the underlying type of the given numeric.
  type t = T.t

  -- Defines the representation of the compressed sparse row form.
  --
  -- Let 'nnz' denote the number of non-zero elements and 'm' and 'n' the number
  -- of rows and columns respectively. Then 'val_idx' has length 'nnz', while
  -- 'row_ptr' has length 'm'. The former contains value and column index pairs
  -- in row order, while the column order is unspecified. The latter contains
  -- the offsets within 'val_idx' in which each row starts.
  type~ smat 'u = {val_idx : [](u,i64), row_ptr : []i64, num_col : i64}

  -- Defines the zero and unit elements of the given numeric.
  let zero = T.i32 0
  let one  = T.i32 1

  -- /=========\
  -- # HELPERS #
  -- \=========/

  -- Creates a simple flag array from a row pointer with segment offsets.
  -- Remark that multiple writes to the same position are possible, but also
  -- insignificant, since the same value is being written nevertheless.
  --
  -- Work: O(nnz + m)
  -- Span: O(1)
  let mk_flags [m] 'u (nnz : i64) (row_ptr : [m]i64) : [nnz]i64 =
    scatter (replicate nnz 0) row_ptr (replicate m 1)

  -- Creates a row index helper array from a row pointer with segment offsets.
  -- Remark that when multiple writes occur to the same position, we propagate
  -- the maximum in order to propagate the index of the next non-empty row.
  --
  -- Let 'mce' denote the maximum number of consecutive empty rows below.
  -- Remark that as 'nnz' decreases, 'mce' increases probabilistically.
  --
  -- Work: O(nnz + m)
  -- Span: O(mce + log nnz)
  let mk_row_idxs [m] (nnz : i64) (row_ptr : [m]i64) : [nnz]i64 =
    reduce_by_index (replicate nnz 0) i64.max 0 row_ptr (iota m) |> mk_II1

  -- Creates a row length helper array from a row pointer with segment offsets.
  --
  -- Work: O(m)
  -- Span: O(1)
  let mk_row_lens [m] (nnz : i64) (row_ptr : [m]i64) : [m]i64 =
    let rot_ptr = iota m
      |> map (\i -> if i == m-1 then nnz else row_ptr[i+1])
    in map2 (-) rot_ptr row_ptr

  -- /============\
  -- # PROPERTIES #
  -- \============/

  -- Work: O(1)
  -- Span: O(1)
  let shape (mat : smat t) : (i64,i64) =
    (length mat.row_ptr, mat.num_col)

  -- Work: O(1)
  -- Span: O(1)
  let nonzeroes (mat : smat t) : i64 =
    length mat.val_idx

  -- Work: O(1)
  -- Span: O(1)
  let sparsity (mat : smat t) : f64 =
    let (m,n) = shape mat
    let nz = m*n - nonzeroes mat
    in f64.i64 nz / f64.i64 (m*n)

  -- Work: O(1)
  -- Span: O(1)
  let density (mat : smat t) : f64 =
    let (m,n) = shape mat
    let nnz = nonzeroes mat
    in f64.i64 nnz / f64.i64 (m*n)

  -- /==============\
  -- # CONSTRUCTORS #
  -- \==============/

  -- Let 'mrl' denote the maximum row length below, meaning the maximum number
  -- of non-zero elements in a single row. Remark that the work and span of the
  -- used radix sort contain a hidden constant equal to the number of bits.
  --
  -- Work: O(nnz + m)
  -- Span: O(log nnz + mrl + log m)
  let new [nnz] (coords : [nnz](t,i64,i64)) (m : i64, n : i64) : smat t =
    -- sort the coordinate list into row order for compressed sparse row form
    let get_bit i (_v,r,_c) = i64.get_bit i r
    let num_bits = m |> f64.i64 |> f64.log2 |> f64.ceil |> i32.f64
    let (vs, row_idx, col_idx) = radix_sort num_bits get_bit coords |> unzip3
    -- deduce row pointer from row lengths, which are deduced from row indices
    let row_len = reduce_by_index (replicate m 0) (+) 0 row_idx (replicate nnz 1)
    let row_ptr = excl_scan (+) 0 row_len
    in {val_idx = zip vs col_idx, row_ptr, num_col = n}

  -- Work: O(m)
  -- Span: O(1)
  let zeroes (m : i64, n : i64) : smat t =
    {val_idx = [], row_ptr = replicate m 0, num_col = n}

  -- Work: O(n)
  -- Span: O(1)
  let identity (n : i64) : smat t =
    {val_idx = zip (replicate n one) (iota n), row_ptr = iota n, num_col = n}

  -- Work: O(m*n)
  -- Span: O(log m*n)
  let sparsify [m][n] (mat : [m][n]t) : smat t =
    -- create flag matrix which indicates the presence of a non-zero element
    let flg_mat = map (map (\ele -> if ele T.== zero then 0 else 1)) mat
    let row_len = map (reduce (+) 0) flg_mat
    -- filter the data matrix in order to only keep the non-zero elements
    let val_idx = mat
      |> map (\row -> zip row (iota n))
      |> flatten
      |> filter (\(ele, _col) -> ele T.!= zero)
    let row_ptr = excl_scan (+) 0 row_len
    in {val_idx, row_ptr, num_col = n}

  -- Let 'mce' denote the maximum number of consecutive empty rows below.
  --
  -- Work: O(m*n + nnz)
  -- Span: O(mce + log nnz)
  let densify (mat : smat t) : [][]t =
    let (m,n) = shape mat
    let nnz = nonzeroes mat
    -- write values into flattened matrix form before unflattening
    let row_idx = mk_row_idxs nnz (mat.row_ptr :> [m]i64)
    let (vs, col_idx) = unzip (mat.val_idx :> [nnz](t,i64))
    let is = map2 (\r c -> r*n + c) row_idx col_idx
    in scatter (replicate (m*n) zero) is vs |> unflatten m n

  -- /============\
  -- # CONVERTERS #
  -- \============/

  -- Work: O(1)
  -- Span: O(1)
  let of_csr [nnz][m] (vs : [nnz]t) (cs : [nnz]i64) (rs : [m]i64) (n : i64) : smat t =
    {val_idx = zip vs cs, row_ptr = rs, num_col = n}

  -- Work: O(1)
  -- Span: O(1)
  let to_csr (mat : smat t) : ([]t, []i64, []i64, i64) =
    let (vs, col_idx) = unzip mat.val_idx
    in (vs, col_idx, mat.row_ptr, mat.num_col)

  -- /===========\
  -- # ACCESSORS #
  -- \===========/

  -- Let 'mrl' denote the maximum row length below, meaning the maximum number
  -- of non-zero elements in a single row. Remark that the sequential lookup
  -- within each instance halts as soon as the given coordinate pair is found.
  --
  -- Work: O(k * mrl)
  -- Span: O(mrl)
  let gather [k] (mat : smat t) (coords : [k](i64,i64)) : [k]t =
    let (m,_) = shape mat
    let nnz = nonzeroes mat
    let (vs, col_idx) = unzip mat.val_idx
    -- lookup column index within row segment sequentially
    let read (r,c) =
      let start = mat.row_ptr[r]
      let end = if r == m-1 then nnz else mat.row_ptr[r+1]
      let (result, _) =
        loop (res, cur) = (zero, start) while (res T.== zero && cur < end) do
          (if col_idx[cur] == c then vs[cur] else res, cur + 1)
      in result
    in map read coords

  -- Let 'mrl' denote the maximum row length below, meaning the maximum number
  -- of non-zero elements in a single row. Remark that the parallelism which
  -- is exibited here seems irregular, since the segments may vary in lengths.
  --
  -- Work: O(k * mrl)
  -- Span: O(log mrl)
  let gather' [k] (mat : smat t) (coords : [k](i64,i64)) : [k]t =
    let (m,_) = shape mat
    let nnz = nonzeroes mat
    -- lookup column index within row segment in parallel
    let read (r,c) =
      let start = mat.row_ptr[r]
      let end = if r == m-1 then nnz else mat.row_ptr[r+ 1]
      let len = end - start
      let slice = mat.val_idx[start:end] :> [len](t,i64)
      in slice
        |> map (\(v,c') -> if c' == c then v else zero)
        |> reduce (T.+) zero
    in map read coords

  -- /===========\
  -- # OPERATORS #
  -- \===========/

  -- Work: O(nnz)
  -- Span: O(1)
  let scale (k : t) (mat: smat t) : smat t =
    let val_idx = map (\(v,c) -> (k T.* v, c)) mat.val_idx
    in {val_idx, row_ptr = mat.row_ptr, num_col = mat.num_col}

  -- Work: O(nnz + m)
  -- Span: O(1)
  let rotate_row (k : i64) (mat: smat t) : smat t =
    let (m,_) = shape mat
    let nnz = nonzeroes mat
    -- perform the rotation modulo the size of the outer dimension
    -- since the rows are ordered, some reordering is necessary
    let k = k % m
    let rot_amt = mat.row_ptr[k]
    let val_idx = rotate rot_amt mat.val_idx
    let row_ptr = (mat.row_ptr :> [m]i64)
      |> map (\offset -> offset - rot_amt)
      |> map2 (\r offset -> if r >= k then offset else offset + nnz) (iota m)
      |> rotate k
    in {val_idx, row_ptr, num_col = mat.num_col}

  -- Work: O(nnz)
  -- Span: O(1)
  let rotate_col (k : i64) (mat : smat t) : smat t =
    let (_,n) = shape mat
    let (vs, col_idx) = unzip mat.val_idx
    -- perform the rotation modulo the size of the inner dimension
    -- since the columns are unordered, no reordering is necessary
    let col_idx = map (\c -> (c - k) % n) col_idx
    in {val_idx = zip vs col_idx, row_ptr = mat.row_ptr, num_col = mat.num_col}

  -- Let 'mce' denote the maximum number of consecutive empty rows, and let
  -- 'mcl' denote the maximum column length, meaning the maximum number of
  -- non-zero elements in a single column.
  --
  -- Work: O(nnz * log n + m + n)
  -- Span: O(mce + mcl + log nnz * log n)
  let transpose (mat : smat t) : smat t =
    let (m,n) = shape mat
    let nnz = nonzeroes mat
    let (vs, col_idx) = unzip (mat.val_idx :> [nnz](t,i64))
    -- prepare fully instantiated coordinate list before reordering
    let row_idx = mk_row_idxs nnz (mat.row_ptr :> [m]i64)
    let coords = zip3 vs row_idx col_idx
    -- sort the coordinate list into column order for compressed sparse column form
    let get_bit i (_v,_r,c) = i64.get_bit i c
    let num_bits = n |> f64.i64 |> f64.log2 |> f64.ceil |> i32.f64
    let (vs, row_idx, col_idx) = radix_sort num_bits get_bit coords |> unzip3
    -- since columns will become rows, deduce column pointer from column indices
    let col_len = reduce_by_index (replicate n 0) (+) 0 col_idx (replicate nnz 1)
    let col_ptr = excl_scan (+) 0 col_len
    in {val_idx = zip vs row_idx, row_ptr = col_ptr, num_col = m}

  -- Let 'mrl' denote the maximum row length below, meaning the maximum number
  -- of non-zero elements in a single row. Remark that each non-zero element is
  -- only touched once in the process of computing the dot products.
  --
  -- Work: O(m + nnz)
  -- Span: O(mrl)
  let smat_dvec_mul (mat : smat t) (vec : []t) : []t =
    let (m,_) = shape mat
    let nnz = nonzeroes mat
    let (vs, col_idx) = unzip (mat.val_idx :> [nnz](t,i64))
    -- deduce ranges that span the respective segment corresponding to each row
    let ranges = iota m
      |> map (\i -> if i == m-1 then nnz else mat.row_ptr[i+1])
      |> zip (mat.row_ptr :> [m]i64)
    -- compute dot products in parallel across entries, but sequentially within
    let dotprod (i : i64) : t =
      let (start, end) = ranges[i]
      in loop (yi : t) = zero for k in start ..< end do
        yi T.+ vs[k] T.* vec[col_idx[k]]
    in map dotprod (iota m)

  -- Let 'mce' denote the maximum number of consecutive empty rows, and let
  -- 'mrl' denote the maximum row length, meaning the maximum number of
  -- non-zero elements in a single row.
  --
  -- Work: O(nnz + m)
  -- Span: O(mce + log nnz + mrl)
  let smat_dvec_mul' (mat : smat t) (vec : []t) : []t =
    let (m,_) = shape mat
    let nnz = nonzeroes mat
    let (vs, col_idx) = unzip (mat.val_idx :> [nnz](t,i64))
    -- compute dot products in parallel across entries
    let row_idx = mk_row_idxs nnz (mat.row_ptr :> [m]i64)
    let res_val = map2 (\v c -> v T.* vec[c]) vs col_idx
    in reduce_by_index (replicate m zero) (T.+) zero row_idx res_val

  -- Let 'mce' denote the maximum number of consecutive empty rows, and let
  -- 'mcl' denote the maximum column length, meaning the maximum number of
  -- non-zero elements in a single column.
  --
  -- Work: O(nnz + m)
  -- Span: O(mce + log nnz + mcl)
  let smat_dvec_mulT (mat : smat t) (vec : []t) : []t =
    let (m,n) = shape mat
    let nnz = nonzeroes mat
    let (vs, col_idx) = unzip (mat.val_idx :> [nnz](t,i64))
    -- compute dot products in parallel across entries
    let row_idx = mk_row_idxs nnz (mat.row_ptr :> [m]i64)
    let res_val = map2 (\v r -> v T.* vec[r]) vs row_idx
    in reduce_by_index (replicate n zero) (T.+) zero col_idx res_val

}