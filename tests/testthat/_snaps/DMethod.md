# Function returns correct values

    Code
      set.seed(1234567)
      DMethod(fuzzyValuesA)
    Output
            [,1] [,2] [,3] [,4]
      [1,]  0.75  1.0  1.5 3.50
      [2,] -0.50  0.5  1.5 1.70
      [3,]  0.25  0.5  0.5 2.50
      [4,] -1.00  0.0  1.0 1.25

---

    Code
      set.seed(1234567)
      DMethod(fuzzyValuesA, b = 5)
    Output
            [,1] [,2] [,3] [,4]
      [1,]  0.25  0.5  0.5 0.75
      [2,] -0.50  0.5  1.5 1.70
      [3,] -0.25  0.0  0.0 0.20
      [4,]  0.00  1.0  2.0 2.70
      [5,]  0.00  1.0  1.0 1.25

---

    Code
      set.seed(1234567)
      DMethod(fuzzyValuesIncA, increases = TRUE)
    Output
           [,1] [,2] [,3] [,4]
      [1,] 0.25  1.0  1.5 2.00
      [2,] 1.00  0.5  1.5 0.20
      [3,] 0.25  0.5  0.5 2.00
      [4,] 1.00  0.0  1.0 0.25

---

    Code
      set.seed(1234567)
      DMethod(fuzzyValuesIncA, b = 5, increases = TRUE)
    Output
           [,1] [,2] [,3] [,4]
      [1,] 0.25  0.5  0.5 0.25
      [2,] 1.00  0.5  1.5 0.20
      [3,] 0.25  0.0  0.0 0.20
      [4,] 1.00  1.0  2.0 0.70
      [5,] 1.00  1.0  1.0 0.25

