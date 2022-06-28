# `future_mice()` works

    Code
      future_mice(mice::nhanes2, seed = 1L, maxit = 5L, minit = 1L)
    Warning <rlang_warning>
      Sampling did not converge in 5 iterations
    Message <rlang_message>
      R-hat: 2.498
    Output
      Class: mids
      Number of multiple imputations:  5 
      Imputation methods:
           age      bmi      hyp      chl 
            ""    "pmm" "logreg"    "pmm" 
      PredictorMatrix:
          age bmi hyp chl
      age   0   1   1   1
      bmi   1   0   1   1
      hyp   1   1   0   1
      chl   1   1   1   0

