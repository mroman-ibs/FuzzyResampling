# Function returns correct values

    Code
      set.seed(1234567)
      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5, -0.1, 0, 0.5))
    Output
      [1] 0.04

---

    Code
      set.seed(1234567)
      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5, -0.1, 0, 0.5), numberOfSamples = 1000)
    Output
      [1] 0.085

---

    Code
      set.seed(1234567)
      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5, -0.1, 0, 0.5), theta = 1)
    Output
      [1] 0.03

---

    Code
      set.seed(1234567)
      OneSampleCTest(fuzzyValuesA, mu_0 = c(-0.5, -0.1, 0, 0.5), resamplingMethod = "EWMethod")
    Output
      [1] 0.07

---

    Code
      set.seed(1234567)
      OneSampleCTest(fuzzyValuesIncA, mu_0 = c(0.5, -0.1, 0, 0.5), resamplingMethod = "EWMethod",
      increases = TRUE)
    Output
      [1] 0.07

---

    Code
      set.seed(1234567)
      OneSampleCTest(fuzzyValuesMean, mu_0 = c(0.5, 1, 2, 4))
    Output
      [1] 0.74

---

    Code
      set.seed(1234567)
      OneSampleCTest(fuzzyValuesMean, mu_0 = c(0.5, 1, 2, 4), numberOfSamples = 500,
      resamplingMethod = "VAMethod")
    Output
      [1] 0.936

