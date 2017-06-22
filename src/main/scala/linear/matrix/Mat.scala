package linear.matrix

import linear.types.Dim

/**
 *
 * @tparam T Type in the matrix
 * @tparam M Number of rows
 * @tparam N Number of columns
 */
trait Mat[+T, M <: Dim, N <: Dim]