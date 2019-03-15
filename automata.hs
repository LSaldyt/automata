-- Lucas Saldyt
-- Wolfram-style cellular automata rules are 4 * 8 bits
-- 

initial = [1]

expand array = [0] ++ array ++ [0]

rule segment = 0

applyauto array = [rule segment | a <- expanded,
                                b <- tail expanded,
                                c <- tail . tail $ expanded,
                                let segment = [a, b, c]] where expanded = expand array

main = print $ applyauto . expand $ initial
