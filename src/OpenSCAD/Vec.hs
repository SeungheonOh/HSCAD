{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- This flag doesn't work when I put it on cabal. Nonsense
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module OpenSCAD.Vec (
    Vec,
    vec,
    vec',
    unsafeFromList,
    toList,
    shape,
    topWidth,
    OpenSCAD.Vec.head,
    OpenSCAD.Vec.tail,
    OpenSCAD.Vec.last,
    OpenSCAD.Vec.init,
    (!),
    (#.),
    (#*),
    (#+),
    indexMat,
    ConstructVec (..),
    Head,
    Last,
    Tail,
    Init,
    Concat,
    KnownShape,
) where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.TypeLits (ErrorMessage (Text), KnownNat, Nat, TypeError, natVal, type (+), type (-))

class KnownShape (shape :: [Nat]) where
    shapeVal :: Proxy shape -> [Int]

instance (KnownNat x, KnownShape xs) => KnownShape (x ': xs) where
    shapeVal _ =
        fromInteger (natVal (Proxy @x)) : shapeVal (Proxy @xs)

instance KnownShape '[] where
    shapeVal _ = []

newtype Vec (shape :: [Nat])
    = Vec (Vector Double)
    deriving newtype (Show)

class ConstructVec (list :: Type) where
    constructVec :: list -> Maybe ([Double], [Int])

instance forall (x :: Type). ConstructVec x => ConstructVec [x] where
    constructVec xs =
        traverse constructVec xs
            >>= ( \case
                    (xs', y : ys) ->
                        if and $ (y ==) <$> ys
                            then Just (concat xs', length xs' : y)
                            else Nothing
                    (xs', []) -> Just (concat xs', [length xs'])
                )
                . unzip

instance forall (x :: Type). Integral x => ConstructVec x where
    constructVec x = Just ([fromIntegral x], [])

instance ConstructVec Double where
    constructVec x = Just ([x], [])

vec ::
    forall (shape :: [Nat]) (list :: Type).
    ( ConstructVec list
    , KnownShape shape
    ) =>
    list ->
    Vec shape
vec = fromMaybe (error "Shape does not match") . vec'

vec' ::
    forall (shape :: [Nat]) (list :: Type).
    ( ConstructVec list
    , KnownShape shape
    ) =>
    list ->
    Maybe (Vec shape)
vec' xs = do
    (xs', shape') <- constructVec xs
    if shape' /= shapeVal (Proxy @shape)
        then Nothing
        else pure . Vec . V.fromList $ xs'

unsafeFromList ::
    forall shape.
    [Double] ->
    Vec shape
unsafeFromList x =
    Vec $ V.fromList x

toList ::
    forall shape.
    Vec shape ->
    [Double]
toList (Vec v) = V.toList v

singletonShape :: [Int] -> [Int]
singletonShape [] = [0]
singletonShape [0] = [0]
singletonShape a = a

shape ::
    forall (shape :: [Nat]).
    KnownShape shape =>
    Vec shape ->
    [Int]
shape _ = singletonShape $ shapeVal (Proxy @shape)

type family VecOne (shape :: [Nat]) :: [Nat] where
    VecOne '[0] = TypeError ('Text "Vec is empty")
    VecOne '[_] = '[1]
    VecOne (_ ': xs) = xs
    VecOne '[] = TypeError ('Text "")

type family VecRest (shape :: [Nat]) :: [Nat] where
    VecRest '[x] = '[x - 1]
    VecRest (x ': xs) = (x - 1) ': xs
    VecRest '[] = TypeError ('Text "")

rawAt ::
    forall (shape :: [Nat]).
    Vec shape ->
    Int ->
    Double
rawAt (Vec v) idx = v V.! idx

(!) ::
    forall (shape :: [Nat]).
    KnownShape shape =>
    Vec shape ->
    [Int] ->
    Double
v ! (singletonShape -> idx)
    | length s /= length idx = error "Rank mismatch"
    | or $ zipWith (>=) idx s = error "Index out of bound"
    | otherwise = rawAt v (sum $ zipWith (*) idx $ f s)
  where
    s = shape v
    f (_ : xs) = product xs : f xs
    f [] = []

topWidth ::
    forall (shape :: [Nat]).
    KnownShape shape =>
    Vec shape ->
    Int
topWidth _ =
    product . Prelude.tail . shapeVal $ Proxy @shape

head ::
    forall (shape :: [Nat]).
    KnownShape shape =>
    Vec shape ->
    Vec (VecOne shape)
head v@(Vec xs) =
    Vec $ V.take (topWidth v) xs

tail ::
    forall (shape :: [Nat]).
    KnownShape shape =>
    Vec shape ->
    Vec (VecRest shape)
tail v@(Vec xs) =
    Vec $ V.drop (topWidth v) xs

last ::
    forall (shape :: [Nat]).
    KnownShape shape =>
    Vec shape ->
    Vec (VecOne shape)
last v@(Vec xs) =
    Vec $ V.drop (V.length xs - topWidth v) xs

init ::
    forall (shape :: [Nat]).
    KnownShape shape =>
    Vec shape ->
    Vec (VecRest shape)
init v@(Vec xs) =
    Vec $ V.take (V.length xs - topWidth v) xs

type family Head (list :: [k]) :: k where
    Head (x ': _) = x
    Head _ = '[]

type family Last (list :: [k]) :: k where
    Last (x ': '[]) = x
    Last (_ ': xs) = Last xs
    Last '[] = '[]

type family Tail (list :: [k]) :: [k] where
    Tail (_ ': xs) = xs
    Tail '[] = '[]

type family Init (list :: [k]) :: [k] where
    Init '[x] = '[]
    Init (x ': xs) = x ': Init xs
    Init '[] = '[]

type family Concat (list1 :: [k]) (list2 :: [k]) :: [k] where
    Concat (x ': xs) ys = x ': Concat xs ys
    Concat '[] ys = ys

type family Length (list :: [k]) :: Nat where
    Length (x ': xs) = 1 + Length xs
    Length '[] = 0

indexMat ::
    [Int] ->
    [[Int]]
indexMat (x : xs) =
    [ i : indxs
    | i <- [0 .. x - 1]
    , indxs <- indexMat xs
    ]
indexMat [] = [[]]

-- | Matrix multiplication
(#.) ::
    forall (shape1 :: [Nat]) (shape2 :: [Nat]) (shape3 :: [Nat]).
    ( Last shape1 ~ Head shape2
    , KnownShape shape1
    , KnownShape shape2
    , KnownShape shape3
    , Concat (Init shape1) (Tail shape2) ~ shape3
    ) =>
    Vec shape1 ->
    Vec shape2 ->
    Vec shape3
v1 #. v2 =
    let s1 = Prelude.init $ shape v1
        cs = Prelude.head $ shape v2
        s3 = shapeVal $ Proxy @shape3
        at idxs =
            sum $
                [ (v1 ! (Prelude.take (length s1) idxs <> [k]))
                    * (v2 ! (k : Prelude.drop (length s1) idxs))
                | k <- [0 .. cs - 1]
                ]
     in unsafeFromList (at <$> indexMat s3)

-- | Matrix addition
(#+) ::
    forall (shape :: [Nat]).
    Vec shape ->
    Vec shape ->
    Vec shape
(Vec v1) #+ (Vec v2) = Vec $ V.zipWith (+) v1 v2

class CrossProd (dimension :: Nat) where
    (#*) :: Vec '[dimension] -> Vec '[dimension] -> Vec '[dimension]

instance CrossProd 2 where
    (toList -> v1) #* (toList -> v2) =
        unsafeFromList
            [ 0
            , (Prelude.head v1 * v2 Prelude.!! 1)
                - (v1 Prelude.!! 1 * Prelude.head v2)
            ]

instance CrossProd 3 where
    (toList -> v1) #* (toList -> v2) =
        unsafeFromList $
            ( \((a1, b1), (a2, b2)) ->
                (v1 Prelude.!! a1 * v2 Prelude.!! b1)
                    - (v1 Prelude.!! a2 * v2 Prelude.!! b2)
            )
                <$> [((1, 2), (2, 1)), ((2, 0), (0, 2)), ((0, 1), (1, 0))]

-- $> :set -XNoOverloadedLists

-- $> :set -fno-warn-type-defaults

-- $> import OpenSCAD.Vec

-- $> import qualified OpenSCAD.Vec as V

-- $> f x = do { print x; print $ shape x }

-- $>a = vec [[[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[11, 12, 13], [14, 15, 16], [17, 18, 19]]] :: Vec '[2, 3, 3]

-- $>b = vec [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: Vec '[3, 3]

-- $>f $ (vec [1, 2, 3] :: Vec '[3]) #. (vec [4, 5, 6] :: Vec '[3])

-- $>f $ a #. b

-- $> c = vec [2,3,4] :: Vec '[3]

-- $> d = vec [5,6,7] :: Vec '[3]

-- $> f $ c #* d

-- $> f $ c #+ d
