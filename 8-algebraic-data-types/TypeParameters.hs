data Maybe a = Nothing | Just a

-- add a typeclass constraint in the data declaration
data (Ord k) => Map k v = ...
-- don't do this because you'll have to put the typeclass constraint into the
-- function type declarations anyways, and some of those functions might not
-- give a damn about the typeclass constraint
