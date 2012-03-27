-- Shamelessly copied from Cabal-1.14.0 by Ivan Labáth
{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}

#if !(defined(__HUGS__) || (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 610))
#define NEW_EXCEPTION
#endif

module Exception (
     E.IOException,
     onException,
     catchIO,
     catchExit,
     throwIOIO,
     tryIO,
  ) where

import System.Exit
import qualified Control.Exception as E

onException :: IO a -> IO b -> IO a
#ifdef NEW_EXCEPTION
onException = E.onException
#else
onException io what = io `E.catch` \e -> do what
                                                    E.throw e
#endif

throwIOIO :: E.IOException -> IO a
#ifdef NEW_EXCEPTION
throwIOIO = E.throwIO
#else
throwIOIO = E.throwIO . E.IOException
#endif

tryIO :: IO a -> IO (Either E.IOException a)
#ifdef NEW_EXCEPTION
tryIO = E.try
#else
tryIO = E.tryJust E.ioErrors
#endif

catchIO :: IO a -> (E.IOException -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchIO = E.catch
#else
catchIO = E.catchJust E.ioErrors
#endif

catchExit :: IO a -> (ExitCode -> IO a) -> IO a
#ifdef NEW_EXCEPTION
catchExit = E.catch
#else
catchExit = E.catchJust exitExceptions
    where exitExceptions (E.ExitException ee) = Just ee
          exitExceptions _                            = Nothing
#endif

