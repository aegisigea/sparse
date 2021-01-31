-- /=============\
-- | COMBINATORS |
-- \=============/

-- Implements an exclusive scan using an inclusive scan.
-- Assumes that 'op' is associative with 'ne' as its neutral element.
--
-- Work: O(n)
-- Span: O(log n)
let excl_scan [n] 't (op : t -> t -> t) (ne : t) (xs : [n]t) : [n]t =
  let ys = xs |> scan op ne
  in map (\i -> if i == 0 then ne else ys[i-1]) (iota n)

-- Implements a segmented scan using a regular scan.
-- Assumes that 'op' is associative with 'ne' as its neutral element.
--
-- Work: O(n)
-- Span: O(log n)
let segm_scan [n] 't (op: t -> t -> t) (ne: t) (flg : [n]i64) (arr: [n]t) : [n]t =
  let lift_op (v1, f1) (v2, f2) =
    let v = if f2 != 0 then v2 else op v1 v2
    let f = f1 | f2
    in (v, f)
  in zip arr flg
    |> scan lift_op (ne, 0)
    |> unzip
    |> (.0)

-- Implements a segmented reduce using a segmented scan.
-- Assumes that 'op' is associative with 'ne' as its neutral element.
-- Also assumes that 'flags' contains a unit element at each segment.
--
-- Work: O(n)
-- Span: O(log n)
let segm_reduce [n] 't (op: t -> t -> t) (ne: t) (flg : [n]i64) (arr: [n]t) : []t =
    let segm_idx = flg
      |> scan (+) 0
      |> map (\i -> i - 1)
    let end_idx  = rotate 1 flg
    let segm_idx = map2 (\segm end -> if end != 0 then segm else -1) segm_idx end_idx
    let segm_val = segm_scan op ne flg arr
    let num_segm = segm_idx[n-1] + 1
    let tmp_res  = replicate num_segm ne
    in scatter tmp_res segm_idx segm_val

-- /=========\
-- | HELPERS |
-- \=========/

-- Creates a segment index helper array based on a 'flags' array.
-- Assumes that 'flags' contains a segment index at each segment.
--
-- Work: O(n)
-- Span: O(log n)
let mk_II1 [n] (flags : [n]i64) : [n]i64 =
  segm_scan (+) 0 flags flags