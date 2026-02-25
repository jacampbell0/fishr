# biomass_index throws error on invalid input

    Code
      biomass_index("ten", 5)
    Condition
      Error in `cpue * area_swept`:
      ! non-numeric argument to binary operator

---

    Code
      biomass_index(10, "five")
    Condition
      Error in `cpue * area_swept`:
      ! non-numeric argument to binary operator

# biomass_index uses verbosity when option set to TRUE

    Code
      biomass_index(100, 10)
    Message
      Calculating biomass for 1 records
    Output
      [1] 1000

