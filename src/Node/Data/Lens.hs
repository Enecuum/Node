module Node.Data.Lens where

import Lens.Micro

idLens :: Lens' a a
idLens = lens id (\_ a -> a)
