BEGIN {
    PROCINFO["sorted_in"] = "@ind_num_asc"
  }
  {
    A[$1] = 0
  }
  END {
    for (I in A) B[I] = A[I]
    for (I in B) SCRATCH = A[I]
    for (I in A) print I
  }
