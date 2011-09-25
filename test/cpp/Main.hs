{-# LANGUAGE CPP #-}
module CPP where

#ifdef DEFINE
f x = x * 2
#endif
