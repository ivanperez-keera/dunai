-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
module Data.MonadicStreamFunction.CausalCommutative where

-- Shows that our arrows are instances of Causal Commutative Arrows
-- When the monads are commutative
-- See "Causal Commutative Arrows and Their Optimization" by
-- Hai Liu, Eric Cheng, Paul Hudak

import Control.Arrow
import Control.Monad.Fix

import Data.MonadicStreamFunction

class ArrowLoop a => ArrowInit a where
    init :: b -> a b b
    -- The following law must be fulfilled:
    -- init i *** init j = init (i,j)

instance MonadFix m => ArrowInit (MStreamF m) where
    init = iPre

class Monad m => CommutativeMonad m where
    -- The following law must be fulfilled:
    -- abx == bax where
    --     abx = do
    --         a' <- a
    --         b' <- b
    --         x a b
    --     bax = do
    --         b' <- b
    --         a' <- a
    --         x a b

class ArrowInit a => CommutativeArrow a where
    -- The following law must be fulfilled:
    -- first f >>> second g == second g >>> first f

instance (CommutativeMonad m, MonadFix m) => CommutativeArrow (MStreamF m) where
    -- Proof:
    --   first f >>> second g
    -- = MStreamF $ \(a,c) -> do
    --     ((b , c'), firstf') <- unMStreamF (first f) (a,c)
    --     ((b', d), secondg') <- unMStreamF (second g) (b,c')
    --     let sf' = firstf' >>> secondg'
    --     return ((b',d), sf')
    -- -- Expand first f & use monad laws
    -- = MStreamF $ \(a,c) -> do
    --     (b, f') <- unMStreamF f a
    --     let c' = c
    --     let firstf' = first f'
    --     ((b', d), secondg') <- unMStreamF (second g) (b,c)
    --     let sf' = firstf' >>> secondg'
    --     return ((b', d), sf')
    -- -- Expand second g & use monad laws (using an easy lemma about second)
    -- = MStreamF $ \(a,c) -> do
    --     (b, f') <- unMStreamF f a
    --     let c' = c
    --     let firstf' = first f'
    --     (d, g') <- unMStreamF g c'
    --     let b' = b
    --     let secondg' = second g
    --     let sf' = firstf' >>> secondg'
    --     return ((b', d), sf')
    -- -- Remove unneccessary lets
    -- = MStreamF $ \(a,c) -> do
    --     (b, f') <- unMStreamF f a
    --     let firstf' = first f'
    --     (d, g') <- unMStreamF g c
    --     let secondg' = second g
    --     let sf' = firstf' >>> secondg'
    --     return ((b, d), sf')
    -- -- Use CommutativeMonad
    -- = MStreamF $ \(a,c) -> do
    --     (d, g') <- unMStreamF g c
    --     let secondg' = second g
    --     (b, f') <- unMStreamF f a
    --     let firstf' = first f'
    --     let sf' = firstf' >>> secondg'
    --     return ((b, d), sf')
    -- Easy exercise: Start with second g >>> first f and arrive at the same expression
