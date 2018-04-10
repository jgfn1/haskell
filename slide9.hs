data Distance = Km Float | Mi Float

data Set t = Set [t]

instance Show Distance where
    show (Km x) = (show x) ++ " Km" 
    show (Mi x) = (show x) ++ " Mi"

instance Eq Distance where
    (==) (Km x) (Mi y) = y == x/1.6 

instance Show (Set x) where
    show (Set x) = show "541"