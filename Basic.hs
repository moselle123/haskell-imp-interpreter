
-------------------------------------------------------------------
-- HASKELL BASIC LIST AND STRING PROCESSING FUNCTIONS
-- Roy L. Crole and Paula Severi 2025
-------------------------------------------------------------------

module Basic where

import AST

mem :: Eq a => a -> [a] -> Bool
mem x []     = False
mem x (h:t)  = x == h || mem x t

lookUp :: Eq a => [(a,b)] -> a -> Either Error b
lookUp [] v             = Left UninitializedVar
lookUp ((x,y):pairs) v  =
        if x == v then Right y else lookUp pairs v

update :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
update [] v z = [(v, z)]
update ((v,z):pairs) v' z'  =
        if v == v' then (v, z'):pairs else (v, z):update pairs v' z'