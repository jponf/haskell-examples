--
-- ZipTree Applicative 
--
-- Example from the 8th Haskell session of the
-- Lleida Developers Group
-- 

import Control.Applicative

data Tree x = Leaf x | Root x (Tree x) (Tree x) | Empty deriving(Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Root x lx rx) = Root (f x) (fmap f lx) (fmap f rx)

instance Applicative Tree where
    pure x = Root x (pure x) (pure x)
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Leaf f <*> Leaf x = Leaf (f x)
    Leaf f <*> Root x _ _ = Leaf (f x)
    Root f _ _ <*> Leaf x = Leaf (f x)
    Root f lf rf <*> Root x lx rx = Root (f x) (lf <*> lx) (rf <*> rx)
     

t1 = Root 3 (Leaf 5) Empty
t2 = Root 9 (Leaf 8) (Leaf 2)
t3 = Root 3 t1 t2

tf1 = fmap (+2) t1
tf2 = fmap (*2) t2
tf3 = fmap ((-)5) t3

ta1 = (+) <$> t1 <*> t2
ta2 = (*) <$> t2 <*> t1
ta3 = pure ((-)5) <*> t3
