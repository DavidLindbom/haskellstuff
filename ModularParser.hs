--
-- Idea is to have a modular parser where the features is decided by
-- a type list.
--
-- This example is creates a parser that can parse + and one digit
-- numbers
--
-- Only tested in ghci 8.6

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

import Text.Read

--
-- Wrapper around the type level list as it otherwise would have the
-- wrong kind, [*] instead of *
--
data P xs where
  Nil  :: P '[]
  Cons :: a -> P as -> P (a ': as)

--
-- Types for our different parsers/features
--
data Add
data Mul
data Val

--
-- The AST we want to produce
--
data AST
  = Plus AST AST
  | Times AST AST
  | Value Int
  deriving Show


--
-- Alias class to have less type applications later
--
class Parsing ps where
  try :: String -> Maybe (AST, String)

instance Parsing' (P ps) (P ps) => Parsing (P ps) where
  try = try' @(P ps) @(P ps)

--
-- The wrapping parsing class working on two lists of parsers.
--   ps is the parsers left to try on the current input
--   qs is the parsers that will be used on the child expressions
--
class Parsing' ps qs where
  try' :: String -> Maybe (AST, String)

--
-- No parsers left to try
--
instance Parsing' (P '[]) qs where
  try' _ = Nothing

--
-- The recursive step where we try one parser and if it fails then try the next
-- one.
--
instance (Parsing' (P ps) qs, Parser p qs) => Parsing' (P (p ': ps)) qs where
  try' str = case parse @p @qs str of
    Just a -> Just a
    Nothing -> try' @(P ps) @qs str

--
-- The implementation of a parser
--   p is the current parser to try
--   qs is a list of parsers to use on the children
--
class Parser p qs where
  parse :: String -> Maybe (AST, String)

-- 
-- Implemenetation for the value parser
--
instance Parser Val qs where
  parse (x:xs) = do
    v <- readMaybe @Int [x]
    Just (Value v, xs)
  parse _ = Nothing

--
-- Implementation for the plus parser
--
instance Parsing qs => Parser Add qs where
  parse ('+':str) = do
    (v1, str') <- try @qs str
    (v2, str'') <- try @qs str'
    Just (Plus v1 v2, str'')
  parse _ = Nothing

--
-- Examples
--

-- A helper so that we can skip the P constructor
run :: forall xs. Parsing (P xs) => String -> Maybe (AST, String)
run = try @(P xs)

main = print $ run @'[Add, Val] "+23"
