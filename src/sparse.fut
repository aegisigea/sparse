module type sparse_matrix = {

  -- /=======\
  -- # SETUP #
  -- \=======/

  -- Constitutes the underlying type of the sparse matrix form.
  type t

  -- Constitutes the representation of the sparse matrix form.
  type~ smat 'u

  -- /============\
  -- # PROPERTIES #
  -- \============/

  -- Extracts the shape of a sparse matrix form.
  val shape : smat t -> (i64,i64)

  -- Extracts the number of non-zero elements from a sparse matrix form.
  val nonzeroes : smat t -> i64

  -- Computes the sparsity of a sparse matrix form.
  val sparsity : smat t -> f64

  -- Computes the density of a sparse matrix form.
  val density : smat t -> f64

  -- /==============\
  -- # CONSTRUCTORS #
  -- \==============/

  -- Creates a sparse matrix form from a coordinate list and a shape.
  -- Assumes that the coordinates are inbounds and without any overlaps.
  val new [nnz] : [nnz](t,i64,i64) -> (i64,i64) -> smat t

  -- Creates a zero matrix with a given shape in sparse matrix form.
  val zeroes : (i64,i64) -> smat t

  -- Creates an identity matrix with a given size in sparse matrix form.
  val identity : i64 -> smat t

  -- Creates a sparse matrix form from a dense matrix form.
  val sparsify [m][n] : [m][n]t -> smat t

  -- Creates a dense matrix form from a sparse matrix form.
  val densify : smat t -> [][]t

  -- /============\
  -- # CONVERTERS #
  -- \============/

  -- Packs the constituents of a compressed sparse row form.
  -- This is meant to be used for testing and benchmarking exclusively.
  val of_csr [nnz][k] : [nnz]t -> [nnz]i64 -> [k]i64 -> i64 -> smat t

  -- Unpacks the constituents of a compressed sparse row form.
  -- This is meant to be used for testing and benchmarking exclusively.
  val to_csr : smat t -> ([]t, []i64, []i64, i64)

  -- /===========\
  -- # ACCESSORS #
  -- \===========/

  -- Reads the elements of a coordinate list from a sparse matrix form.
  -- This version is parallel across coordinate pairs, but sequential within.
  val gather [k] : smat t -> [k](i64,i64) -> [k]t

  -- Reads the elements of a coordinate list from a sparse matrix form.
  -- This version is parallel across coordinate pairs as well as within.
  val gather' [k] : smat t -> [k](i64,i64) -> [k]t

  -- /===========\
  -- # OPERATORS #
  -- \===========/

  -- Scales the elements of a sparse matrix form with a given factor.
  val scale : t -> smat t -> smat t

  -- Rotates the rows of a sparse matrix form with a given amount.
  val rotate_row : i64 -> smat t -> smat t

  -- Rotates the columns of a sparse matrix form with a given amount.
  val rotate_col : i64 -> smat t -> smat t

  -- Transposes the rows and columns of a sparse matrix form.
  val transpose : smat t -> smat t

  -- Multiplies a sparse matrix form with a dense vector.
  -- This version is parallel across rows, but sequential within.
  val smat_dvec_mul [n] : smat t -> [n]t -> []t

  -- Multiplies a sparse matrix form with a dense vector.
  -- This version is parallel across rows as well as within.
  val smat_dvec_mul' [n] : smat t -> [n]t -> []t

  -- Multiplies the transpose of a sparse matrix form with a dense vector.
  -- This version is parallel across rows as well as within.
  val smat_dvec_mulT [m] : smat t -> [m]t -> []t

}