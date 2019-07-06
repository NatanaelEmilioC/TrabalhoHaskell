{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Lucid
import Lucid.Base
import Control.Monad (forM_)


mainPage :: Html ()
mainPage = div_ (p_ "hello")