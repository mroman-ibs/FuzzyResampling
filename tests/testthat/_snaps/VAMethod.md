# Function returns correct values

    Code
      set.seed(1234567)
      VAMethod(fuzzyValuesA)
    Output
                  [,1]      [,2]      [,3]     [,4]
      [1,]  0.14916461 0.5504177 1.0620908 1.125818
      [2,] -0.05631233 1.0281562 1.7002317 2.799537
      [3,] -0.35418375 1.5520919 1.6242685 1.951463
      [4,] -1.58510920 0.2925546 0.2925546 1.414891

---

    Code
      set.seed(1234567)
      VAMethod(fuzzyValuesA, b = 5)
    Output
                  [,1]       [,2]       [,3]     [,4]
      [1,] -0.17503470  0.7125173  1.0325533 1.184893
      [2,] -0.93093552  1.4654678  1.5891992 3.021602
      [3,]  0.86261630  0.9436919  1.6484310 1.903138
      [4,] -0.40871282 -0.2956436 -0.2956436 2.591287
      [5,] -0.06147704  1.0307385  1.7259828 2.748034

---

    Code
      set.seed(1234567)
      VAMethod(fuzzyValuesIncA, increases = TRUE)
    Output
                [,1]      [,2]      [,3]       [,4]
      [1,] 0.4012531 0.5504177 1.0620908 0.06372751
      [2,] 1.0844685 1.0281562 1.7002317 1.09930480
      [3,] 1.9062756 1.5520919 1.6242685 0.32719435
      [4,] 1.8776638 0.2925546 0.2925546 1.12233619

---

    Code
      set.seed(1234567)
      VAMethod(fuzzyValuesIncA, b = 5, increases = TRUE)
    Output
                 [,1]       [,2]       [,3]      [,4]
      [1,] 0.88755205  0.7125173  1.0325533 0.1523400
      [2,] 2.39640328  1.4654678  1.5891992 1.4324024
      [3,] 0.08107556  0.9436919  1.6484310 0.2547071
      [4,] 0.11306923 -0.2956436 -0.2956436 2.8869308
      [5,] 1.09221555  1.0307385  1.7259828 1.0220516
