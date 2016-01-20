{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables, NoImplicitPrelude, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

-- Separating this into a different module because the RE's kill the syntax highlighting


module MyRegexes where

import Text.Regex.PCRE.Heavy as PH (Regex, re, scan)

parodyRegex :: Regex
parodyRegex = [PH.re|\(Parody of \"([\w ]+)\" by ([\w ]+)\)|]
