#' Explanation: From the input matrix, starting from the second row, each row is shifted one to the right.
#'              all remaining places are filled with a zero
#'
#' Result:      Matrix with same nrows, ncols = ncols(inputmatrix) + nrows(inputmatrix) - 1
#'            
#'
#' Example:   
#' inputmatrix 3x4: -->  outputmatrix 3x6:
#' 1 2 3 4               1 2 3 4 0 0
#' 1 2 3 4               0 1 2 3 4 0
#' 1 2 3 4               0 0 1 2 3 4
#' 
    
shift_rows_right <- function(matrix_df)
{
  # aantal kolommen en rijen in originele matrix. 
  n_rows <- nrow(matrix_df)
  
  # voeg rechts een kolommen toe gevuld met nullen, aantal kolommen = nrow - 1
  matrix_df <- cbind(matrix_df, matrix(0, nrow = n_rows, ncol = n_rows - 1))
  
  # aantallen kolommen en rijen in uitgebreide matrix
  n_cols <- ncol(matrix_df)
  
  # nieuwe matrix: zo groot als uitgebreide matrix, gevuld met nullen
  matrix_shifted <- matrix(0, nrow = n_rows, ncol = n_cols)
  
  # voor elke rij: 
  for (idx in 1:n_rows)
  {
    # aantal kolommen dat elke rij opschuift (0 voor de 1e rij, 1 voor 2e, etc)
    num_shift <- idx -1
    # cat("Rij:", idx," Opschuiving:", num_shift, "\n")
    
    # Lege matrix 
    # [row = rijnummer, kolommen = rijnummer tm n_cols]f)
    # [row = rijnummer, kolom = 1 tm aantal kolommen - numshift]
    # Bijv rij 2: kolommen 2 tm de laaste kolom, vervangen door: eerste dif tm (alle difs - 1)
    # Rij 3: kol 3 tm laatste kolom, vervangen door: eerste dif tm (alle difs -2)
    matrix_shifted[idx, (num_shift + 1):(n_cols)] <- matrix_df[idx, 1:(n_cols - num_shift)]
    
  }
  return(matrix_shifted)
}

print("function shift_rows loaded")