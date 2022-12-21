# Function returns correct values

    Code
      set.seed(1234567)
      SEResamplingMean(fuzzyValuesA)
    Output
      $mean
      [1] -0.00875  0.61250  1.09250  1.91850
      
      $SE
      [1] 0.05029215
      

---

    Code
      set.seed(1234567)
      SEResamplingMean(fuzzyValuesA, trueMean = c(0, 0.5, 1, 2))
    Output
      $mean
      [1] 0.0 0.5 1.0 2.0
      
      $SE
      [1] 0.02573501
      

---

    Code
      set.seed(1234567)
      SEResamplingMean(fuzzyValuesA, resamplingMethod = "EWMethod")
    Output
      $mean
      [1] -0.1360284  0.6985284  1.1182540  1.9131210
      
      $SE
      [1] 0.04918169
      

---

    Code
      set.seed(1234567)
      SEResamplingMean(fuzzyValuesA, resamplingMethod = "EWMethod", trueMean = c(0,
        0.5, 1, 2))
    Output
      $mean
      [1] 0.0 0.5 1.0 2.0
      
      $SE
      [1] 0.02545883
      

---

    Code
      set.seed(1234567)
      SEResamplingMean(fuzzyValuesIncA, resamplingMethod = "EWMethod", trueMean = fuzzyValuesIncA[
        1, ], increases = TRUE)
    Output
      $mean
           [,1] [,2] [,3] [,4]
      [1,] 0.25  0.5    1 1.25
      
      $SE
      [1] 0.03308151
      

---

    Code
      set.seed(1234567)
      SEResamplingMean(fuzzyValuesA, repetitions = 200)
    Output
      $mean
      [1] 0.0121875 0.6343750 1.1387500 1.9196875
      
      $SE
      [1] 0.03497986
      

---

    Code
      set.seed(1234567)
      SEResamplingMean(fuzzyValuesA, theta = 0.5)
    Output
      $mean
      [1] -0.00875  0.61250  1.09250  1.91850
      
      $SE
      [1] 0.05071039
      

