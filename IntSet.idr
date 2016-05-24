module BitsSet

import Data.Bits
import Data.Fin

-- bitsFromInt : Int -> Bits64
-- bitsFromInt = cast

-- intFromBits : Bits64 -> Int
-- intFromBits = cast

Prefix : Type
Prefix = Bits 64

data MaskInvariant : (n : Nat) -> Bits n -> Type where
  MkMaskInvariant : (pos : Fin n) -> MaskInvariant n (bitAt pos)

Mask : Type
Mask = Subset (Bits 64) (MaskInvariant 64)

BitMap : Type
BitMap = Bits 64

Key : Type
Key = Bits 64

-- Invariant: Nil is never found as a child of Bin. -DONE
-- Invariant: The Mask is a power of 2.  It is the largest bit position at which
--            two elements of the set differ. -DONE
-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit. -DONE
-- Invariant: In Bin prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do. -DONE
-- Invariant: The Prefix is zero for all but the last 5 (on 32 bit arches) or 6
--            bits (on 64 bit arches). The values of the map represented by a tip
--            are the prefix plus the indices of the set bits in the bit map. -TODO
-- Other invariants:
-- - Mask can't have ones in the last 6 bits ; it should be encoded in
-- the invariant (TODO)
-- - a BitMap can't be empty

-- A number in a set is stored as
-- * Prefix (all but last 5-6 bits) and
-- * BitMap (last 5-6 bits stored as a bitmask)
--   Last 5-6 bits are called a Suffix.

-- The BitMap is a set of up to 64 bits
-- 1000000000000000000000000000000000000000000000000000000000100001 means that
-- 0, 5 and 63 are in the sets, which means in turn that
-- 000000, 000101 and 111111 are in the set (ignoring the prefix)
-- the word is: prefix `and` (bitcount bm)

data BitsTree : Prefix -> Type where
  Bin : (pref : Prefix) ->
        (mask : Mask) ->
        (left : BitsTree pref) ->
        (right : BitsTree (pref `and` (getWitness mask))) ->
        BitsTree pref
  Tip : (pref : Prefix) -> (bm : BitMap) -> BitsTree pref
%name BitsTree btree

data BitsSet = Tree (BitsTree pref) | Empty
%name BitsSet bset

-- TODO Understand this

zero : Bits 64 -> Mask -> Bool
zero b (Element mw _) = (b `and` mw) == (MkBits 0)

mask : Bits 64 -> Mask -> Prefix
mask b (Element mw _) = b `and` (complement (mw `minus` (MkBits 1)) `xor` mw)

nomatch : Bits 64 -> Prefix -> Mask -> Bool
nomatch b p m = (mask b m) /= p

match : Bits 64 -> Prefix -> Mask -> Bool
match b p m = (mask b m) == p

shorter : Mask -> Mask -> Bool
shorter (Element mw1 _) (Element mw2 _) = mw1 > mw2

--     1001010101110111000000
--     0101110101010101000000
-- xor 1100100000100010000000

highestBitMask : Bits 64 -> Bits 64

branchMask : Prefix -> Prefix -> Mask
branchMask p1 p2 = Element (highestBitMask (p1 `xor` p2)) ?prf


Semigroup BitsSet where
  (<+>) Empty bset = bset
  (<+>) (Tree btree) Empty = (Tree btree)
  (<+>) (Tree x) (Tree y) = ?plusBits -- TODO

Monoid BitsSet where
  neutral = Empty

empty : BitsSet
empty = Empty

isEmpty : BitsSet -> Bool
isEmpty Empty = True
isEmpty _     = False

member : Key -> BitsSet -> Bool
member k (Tree tree) = findKeyIn tree
  where
    findKeyIn : BitsTree pref -> Bool -- TODO
member k Empty = False

notMember : Key -> BitsSet -> Bool
notMember k = not . member k

-- -- prefixOf : Int -> Prefix
-- -- prefixOf x = x & prefixBitMask

-- -- singleton : Key -> BitsSet
-- -- singleton x = Tip (prefixOf x) (bitmapOf x)

addBitcount : Bits 64 -> Bits 64 -> Bits 64
addBitcount b (MkBits 0) = b
addBitcount b w = addBitcount (b `plus` (MkBits 1)) (w `and` (w `minus` (MkBits 1)))

bitcount : Bits 64 -> Bits 64
bitcount = addBitcount (MkBits 0)

size : BitsSet -> Nat
size (Tree btree) = fromInteger . bitsToInt $ sizeTree btree
  where
    sizeTree : BitsTree pref -> Bits 64
    sizeTree (Bin _ _ l r) = sizeTree l `plus` sizeTree r
    sizeTree (Tip _ bm) = bitcount bm
size Empty = 0
