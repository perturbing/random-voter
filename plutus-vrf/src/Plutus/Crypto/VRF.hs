{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module Plutus.Crypto.VRF (
    checkVRF,
    runVRF,
    PubKey (..),
    PrivKey (..),
    Input (..),
    Output (..),
    Proof (..),
) where

import PlutusTx (makeIsDataIndexed)
import PlutusTx.Builtins (
    BuiltinByteString,
    Integer,
    bls12_381_G1_add,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_hashToGroup,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    byteStringToInteger,
    integerToByteString,
    sha2_256,
 )
import PlutusTx.Prelude (
    Bool (..),
    mconcat,
    mempty,
    modulo,
    ($),
    (&&),
    (*),
    (-),
    (.),
    (<$>),
    (<>),
    (==),
 )

import GHC.ByteOrder (ByteOrder (..))

-- [General notes on this file]
-- This file contains the logic of the onchain Verifiable random function.
-- For reference see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-15#name-elliptic-curve-vrf-ecvrf

-- | A type for representing a VRF input
newtype Input = Input BuiltinByteString

makeIsDataIndexed ''Input [('Input, 0)]

-- | A type for representing a VRF output
newtype Output = Output BuiltinByteString

makeIsDataIndexed ''Output [('Output, 0)]

-- | A type for representing a VRF Public key
newtype PubKey = PubKey BuiltinByteString

makeIsDataIndexed ''PubKey [('PubKey, 0)]

-- | A type for representing a VRF Private key
newtype PrivKey = PrivKey BuiltinByteString

makeIsDataIndexed ''PrivKey [('PrivKey, 0)]

{- | A type for representing a VRF Proof.
A proof = (gamma, c, s) where the VRF Output can be derived
from gamma, and c and s are part of a non interactive zero
knowledge proof that gamma and the public key share the same
exponent (the secret key)
-}
data Proof = Proof
    { proofGamma :: BuiltinByteString
    , proofC :: Integer
    , proofS :: Integer
    }

makeIsDataIndexed ''Proof [('Proof, 0)]

{-# INLINEABLE checkVRF #-}
checkVRF :: PubKey -> Input -> Output -> Proof -> Bool
checkVRF (PubKey vrfPubKey) (Input input) (Output output) proof@(Proof gammaBS c s) =
    let pub = bls12_381_G1_uncompress vrfPubKey
        g1 = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
        gamma = bls12_381_G1_uncompress gammaBS
        u = bls12_381_G1_add (bls12_381_G1_scalarMul c pub) (bls12_381_G1_scalarMul s g1)
        h' = bls12_381_G1_hashToGroup input mempty
        v = bls12_381_G1_add (bls12_381_G1_scalarMul c gamma) (bls12_381_G1_scalarMul s h')
     in integerToByteString BigEndian 32 c
            == (sha2_256 . mconcat $ [bls12_381_G1_compressed_generator, bls12_381_G1_compress h', vrfPubKey, gammaBS, bls12_381_G1_compress u, bls12_381_G1_compress v])
            && output
            == sha2_256 gammaBS

{-# NOINLINE runVRF #-}
runVRF :: PrivKey -> Integer -> Input -> (Output, Proof)
runVRF (PrivKey vrfPrivKey) k (Input input) =
    let g1 = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
        priv = byteStringToInteger BigEndian vrfPrivKey
        pub = bls12_381_G1_scalarMul priv g1
        h = bls12_381_G1_hashToGroup input mempty
        gamma = bls12_381_G1_scalarMul priv h
        output = sha2_256 $ bls12_381_G1_compress gamma
        c = byteStringToInteger BigEndian . sha2_256 . mconcat $ bls12_381_G1_compress <$> [g1, h, pub, gamma, bls12_381_G1_scalarMul k g1, bls12_381_G1_scalarMul k h]
        s = (k - c * priv) `modulo` 52435875175126190479447740508185965837690552500527637822603658699938581184513
     in (Output output, Proof (bls12_381_G1_compress gamma) c s)
