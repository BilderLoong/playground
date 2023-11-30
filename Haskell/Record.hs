data Foo = Foo {bar :: Integer, da :: String}

x = bar $ Foo 1 "s"

y = Just 1 >>= (\x -> Just $ x + 1)
z = Just 1 >> Just 2
z' = Just 1 >>= (\_ -> Just 2)
