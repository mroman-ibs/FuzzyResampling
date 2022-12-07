# Function returns correct values

    Code
      set.seed(1234567)
      ClassicalBootstrap(fuzzyValuesA)
    Output
            [,1] [,2] [,3] [,4]
      [1,]  0.25  0.5  1.0 1.25
      [2,]  0.00  1.0  2.0 2.20
      [3,]  0.75  1.0  1.5 2.20
      [4,] -1.00  0.0  0.0 2.00

---

    Code
      set.seed(1234567)
      ClassicalBootstrap(fuzzyValuesA, b = 5)
    Output
            [,1] [,2] [,3] [,4]
      [1,]  0.25  0.5  1.0 1.25
      [2,]  0.00  1.0  2.0 2.20
      [3,]  0.75  1.0  1.5 2.20
      [4,] -1.00  0.0  0.0 2.00
      [5,]  0.00  1.0  2.0 2.20

---

    Code
      set.seed(1234567)
      ClassicalBootstrap(fuzzyValuesIncA, increases = TRUE)
    Output
           [,1] [,2] [,3] [,4]
      [1,] 0.25  0.5  1.0 0.25
      [2,] 1.00  1.0  2.0 0.20
      [3,] 0.25  1.0  1.5 0.70
      [4,] 1.00  0.0  0.0 2.00

---

    Code
      set.seed(1234567)
      ClassicalBootstrap(fuzzyValuesIncA, b = 5, increases = TRUE)
    Output
           [,1] [,2] [,3] [,4]
      [1,] 0.25  0.5  1.0 0.25
      [2,] 1.00  1.0  2.0 0.20
      [3,] 0.25  1.0  1.5 0.70
      [4,] 1.00  0.0  0.0 2.00
      [5,] 1.00  1.0  2.0 0.20

