# cpue errors when input is not numeric

    Code
      cpue("five", 10)
    Condition
      Error:
      ! Unsupported input type for cpue(): character

# cpue warns when catch and effort lengths differ

    Code
      cpue(c(100, 200, 300), c(10, 20))
    Condition
      Warning in `catch / effort`:
      longer object length is not a multiple of shorter object length
    Output
      CPUE Result
      Records:      3 
      Method:       ratio 
      Gear type:    nordic_gillnet 
      Values:       10 10 30 

# cpue uses verbosity when option set to TRUE

    Code
      cpue(100, 10)
    Message
      Processing 1 records
    Output
      CPUE Result
      Records:      1 
      Method:       ratio 
      Gear type:    nordic_gillnet 
      Values:       10 

# print.cpue_result displays expected output

    Code
      print(result)
    Output
      CPUE Result
      Records:      3 
      Method:       ratio 
      Gear type:    nordic_gillnet 
      Values:       10 10 20 

# cpue.data.frame errors on missing columns

    Code
      cpue(df)
    Condition
      Error:
      ! Column 'catch' not found in data frame.

# cpue.default gives informative error

    Code
      cpue("not valid")
    Condition
      Error:
      ! Unsupported input type for cpue(): character

