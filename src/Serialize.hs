{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
--
--}

{-# LANGUAGE OverloadedStrings #-}

module Serialize (serialize) where

import VirtualMachine
import qualified Data.ByteString as B
import Data.String(IsString(..))

serialize :: Insts -> B.ByteString
serialize (Push value:insts) = B.singleton 0 <> fromString (show value) <> serialize insts
serialize (Call:insts) = B.singleton 1 <> serialize insts
serialize (JmpFalse jmpValue:insts) = B.singleton 2 <> B.singleton (fromIntegral jmpValue) <> serialize insts
serialize (Dup:insts) = B.singleton 3 <> serialize insts
serialize (Ret:insts) = B.singleton 4 <> serialize insts
serialize (PushEnv value:insts) = B.singleton 4 <> fromString value <> serialize insts
serialize [] = B.singleton 0

-- serializeVMValue :: VMValue -> B.ByteString
-- serializeVMValue IntegerValue value = fromString (show value)
-- serializeVMValue AtomValue value = fromString value
-- serializeVMValue 
