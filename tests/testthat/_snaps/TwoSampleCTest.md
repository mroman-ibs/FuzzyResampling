# Function returns correct values

    Code
      set.seed(1234567)
      TwoSampleCTest(fuzzyValuesA, fuzzyValuesA)
    Output
      [1] 0.95

---

    Code
      set.seed(1234567)
      TwoSampleCTest(fuzzyValuesA, fuzzyValuesAPlus)
    Output
      [1] 0.28

---

    Code
      set.seed(1234567)
      TwoSampleCTest(fuzzyValuesA, fuzzyValuesAPlus, numberOfSamples = 500)
    Output
      [1] 0.27

---

    Code
      set.seed(1234567)
      TwoSampleCTest(fuzzyValuesA, fuzzyValuesAPlus, theta = 0.5)
    Output
      [1] 0.29

---

    Code
      set.seed(1234567)
      TwoSampleCTest(fuzzyValuesA, fuzzyValuesAPlus, theta = 0.5, resamplingMethod = "VAAMethod")
    Output
      [1] 0.3

---

    Code
      set.seed(1234567)
      TwoSampleCTest(fuzzyValuesIncA, fuzzyValuesIncA, increases = TRUE)
    Output
      [1] 0.95

