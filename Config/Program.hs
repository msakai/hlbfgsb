module Config.Program
    ( gfortranProgram
    ) where

import Distribution.Simple.Program

gfortranProgram :: Program
gfortranProgram = simpleProgram "gfortran"
