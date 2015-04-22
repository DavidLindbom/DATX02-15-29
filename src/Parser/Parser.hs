module Parser.Parser where

import Parser.ParHopper
import Parser.LayoutHopper
import Parser.AbsHopper
import Utils.ErrM

parse :: String -> Err Module
parse = pModule . resolveLayout True . myLexer
