-- |
-- Values.
module Silcc.Value
  ( -- * Values
    Value (..)
  , Value'
  , fields
  , bytes

    -- * Miscellaneous
  , unit
  , bool
  , serialized

    -- * Integers
  , int8
  , int16
  , int32
  , int64
  , word8
  , word16
  , word32
  , word64
  ) where

import Control.Lens (Lens', Prism', _Right, lens, preview, prism')
import Data.ByteString (ByteString)
import Data.Functor.Foldable (Base, Corecursive, Recursive, embed, project)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Serialize (Serialize)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Data.Serialize as Serialize

--------------------------------------------------------------------------------
-- Values

-- |
-- A value consists of fields and bytes. Fields are other values. Bytes are
-- arbitrary.
data Value a = Value (Vector a) ByteString
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- |
-- Constraint for values.
type Value' t = (Base t ~ Value, Corecursive t, Recursive t)

-- |
-- The fields of a value.
fields :: Value' t => Lens' t (Vector t)
fields = lens get set
  where get (project -> Value a _) = a
        set (project -> Value _ a) b = embed $ Value b a
{-# INLINABLE fields #-}

-- |
-- The bytes of a value.
bytes :: Value' t => Lens' t ByteString
bytes = lens get set
  where get (project -> Value _ a) = a
        set (project -> Value a _) b = embed $ Value a b
{-# INLINABLE bytes #-}

--------------------------------------------------------------------------------
-- Miscellaneous

-- |
-- The value with no fields and no bytes.
unit :: Value' t => Prism' t ()
unit = serialized
{-# INLINE unit #-}

bool :: Value' t => Prism' t Bool
bool = serialized
{-# INLINE bool #-}

-- |
-- The value for some data, stored in the bytes.
serialized :: (Value' t, Serialize a) => Prism' t a
serialized = prism' set get
  where set = embed . Value [] . Serialize.encode
        get (project -> Value [] a) = preview _Right $ Serialize.decode a
        get (project -> Value _  _) = Nothing
{-# INLINABLE serialized #-}

--------------------------------------------------------------------------------
-- Integers

int8   :: Value' t => Prism' t Int8
int16  :: Value' t => Prism' t Int16
int32  :: Value' t => Prism' t Int32
int64  :: Value' t => Prism' t Int64
word8  :: Value' t => Prism' t Word8
word16 :: Value' t => Prism' t Word16
word32 :: Value' t => Prism' t Word32
word64 :: Value' t => Prism' t Word64
int8   = serialized
int16  = serialized
int32  = serialized
int64  = serialized
word8  = serialized
word16 = serialized
word32 = serialized
word64 = serialized
{-# INLINE int8   #-}
{-# INLINE int16  #-}
{-# INLINE int32  #-}
{-# INLINE int64  #-}
{-# INLINE word8  #-}
{-# INLINE word16 #-}
{-# INLINE word32 #-}
{-# INLINE word64 #-}
