# `future_mice()` works locally

    Code
      future_mice(mice::nhanes, maxit = 5L, seed = 1L)
    Warning <rlang_warning>
      Sampling did not converge in 5 iterations
    Message <rlang_message>
      R-hat: NA/NA/1.903/1.903
    Output
      Class: mids
      Number of multiple imputations:  5 
      Imputation methods:
        age   bmi   hyp   chl 
         "" "pmm" "pmm" "pmm" 
      PredictorMatrix:
          age bmi hyp chl
      age   0   1   1   1
      bmi   1   0   1   1
      hyp   1   1   0   1
      chl   1   1   1   0

