-- file: ch03/NestedLets.hs
-- We can nest this, and we get ("foo",1)
bar = let x = 1
      in ((let x = "foo" in x), x)

-- This isn't a good idea to shadow the inside
-- Because the A can be whatever, the output will bee fooeek! no matter what
quux a = let a = "foo"
         in a ++ "eek!"
