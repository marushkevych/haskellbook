k :: (x, y) -> x
k (x, y) = x
k2 = k ("three", (1+2))

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))