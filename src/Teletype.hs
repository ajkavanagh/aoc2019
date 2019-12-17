{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Teletype
    {-(-}
    {-) where-}
    where

import           Data.Maybe      (fromMaybe)

import           Polysemy
import           Polysemy.Input
import           Polysemy.Output


data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()


makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  = runOutputMonoid pure  -- For each WriteTTY in our program, consume an output by appending it to the list in a ([String], a)
  . runInputList i         -- Treat each element of our list of strings as a line of input
  . reinterpret2 \case     -- Reinterpret our effect in terms of Input and Output
      ReadTTY -> fromMaybe "" <$> input
      WriteTTY msg -> output msg


{-echo :: Member Teletype r => Sem r ()-}
{-echo = do-}
  {-i <- readTTY-}
  {-case i of-}
    {-"" -> pure ()-}
    {-_  -> writeTTY i >> echo-}


-- Let's pretend
{-echoPure :: [String] -> Sem '[] ([String], ())-}
{-echoPure = flip runTeletypePure echo-}

{-pureOutput :: [String] -> [String]-}
{-pureOutput = fst . run . echoPure-}


