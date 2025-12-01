{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module SC2.Ids.Ids where

import SC2.Ids.GenIdsTh

$(generateTechTypeStuff)
