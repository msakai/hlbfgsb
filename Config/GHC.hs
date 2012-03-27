-- Shamelessly copied from Cabal-1.14.0 by Ivan Labáth
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC
-- Copyright   :  Isaac Jones 2003-2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is a fairly large module. It contains most of the GHC-specific code for
-- configuring, building and installing packages. It also exports a function
-- for finding out what packages are already installed. Configuring involves
-- finding the @ghc@ and @ghc-pkg@ programs, finding what language extensions
-- this version of ghc supports and returning a 'Compiler' value.
--
-- 'getInstalledPackages' involves calling the @ghc-pkg@ program to find out
-- what packages are installed.
--
-- Building is somewhat complex as there is quite a bit of information to take
-- into account. We have to build libs and programs, possibly for profiling and
-- shared libs. We have to support building libraries that will be usable by
-- GHCi and also ghc's @-split-objs@ feature. We have to compile any C files
-- using ghc. Linking, especially for @split-objs@ is remarkably complex,
-- partly because there tend to be 1,000's of @.o@ files and this can often be
-- more than we can pass to the @ld@ or @ar@ programs in one go.
--
-- Installing for libs and exes involves finding the right files and copying
-- them to the right places. One of the more tricky things about this module is
-- remembering the layout of files in the build directory (which is not
-- explicitly documented) and thus what search dirs are used for various kinds
-- of files.

{- Copyright (c) 2003-2005, Isaac Jones
All rights reserved.

Redistribution and use in source and binary forms, with or without
modiication, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Config.GHC (
        configure, getInstalledPackages,
        buildLib, buildExe,
        installLib, installExe,
        libAbiHash,
        registerPackage,
        ghcOptions,
        ghcVerbosityOptions,
        ghcPackageDbOptions,
        ghcLibDir,
 ) where

import Distribution.Simple.GHC (
        configure, getInstalledPackages,
        {-buildLib,-} buildExe,
        installLib, installExe,
        libAbiHash,
        registerPackage,
        ghcOptions,
        ghcVerbosityOptions,
        ghcPackageDbOptions,
        ghcLibDir,
    )

import Config.Program

import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..)
         , Library(..), libModules, hcOptions, allExtensions )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
         , absoluteInstallDirs )
import Distribution.Simple.InstallDirs hiding ( absoluteInstallDirs )
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Package
         ( PackageIdentifier, Package(..) )
import qualified Distribution.ModuleName as ModuleName
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration, ProgArg
         , ProgramLocation(..), rawSystemProgram, rawSystemProgramConf
         , rawSystemProgramStdout, rawSystemProgramStdoutConf
         , requireProgramVersion, requireProgram, getProgramOutput
         , userMaybeSpecifyPath, programPath, lookupProgram, addKnownProgram
         , ghcProgram, ghcPkgProgram, arProgram, ranlibProgram, ldProgram )
import qualified Distribution.Simple.Program.Ar    as Ar
import qualified Distribution.Simple.Program.Ld    as Ld
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), Compiler(..), compilerVersion
         , OptimisationLevel(..) )
import Distribution.Version
         ( Version(..) )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
import Distribution.Text
         ( display )
import Language.Haskell.Extension (Extension(..), KnownExtension(..))

import Control.Monad            ( unless, when )
import Data.Char                ( isSpace )
import Data.Maybe               ( catMaybes )
import System.Directory
         ( removeFile, getDirectoryContents )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension )
import Config.Exception (catchIO)

-- Utilities for fortran


splitUp                   :: String -> [String]
splitUp s                 =  case dropWhile edge s of
                                "" -> []
                                s' -> w : splitUp s''
                                      where (w, s'') =
                                             break edge s'
  where
    edge '\n' = True
    edge ','  = True
    edge  _   = False

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

fSources :: BuildInfo -> [FilePath]
fSources BuildInfo { customFieldsBI = custom } =
    concatMap expand [ paths | ("x-fortran-sources", paths) <- custom ]
  where
    expand = filter (/= "") . map trim . splitUp

constructFortranCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                   -> FilePath -> FilePath -> Verbosity -> Bool -> Bool
                   ->(FilePath,[String])
constructFortranCmdLine lbi bi clbi pref filename verbosity dynamic profiling = (path, args)
  where
    path = pref </> takeDirectory filename
    args =
--         ghcCcOptions lbi bi clbi odir
            (if verbosity >= deafening then ["-v"] else [])
         ++ ["-c",filename]
	 ++ ["-o", pref </> filename `replaceExtension` ".o" ]
         -- Note: When building with profiling enabled, we pass the -prof
         -- option to ghc here when compiling C code, so that the PROFILING
         -- macro gets defined. The macro is used in ghc's Rts.h in the
         -- definitions of closure layouts (Closures.h).
         ++ ["-dynamic" | dynamic]

-- -----------------------------------------------------------------------------
-- Building

-- | Build a library with GHC.
--
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  let pref = buildDir lbi
      pkgid = packageId pkg_descr
      runGhcProg = rawSystemProgramConf verbosity ghcProgram (withPrograms lbi)
      runFortranProg = rawSystemProgramConf verbosity gfortranProgram (withPrograms lbi)
      ifVanillaLib forceVanilla = when (forceVanilla || withVanillaLib lbi)
      ifProfLib = when (withProfLib lbi)
      ifSharedLib = when (withSharedLib lbi)
      ifGHCiLib = when (withGHCiLib lbi && withVanillaLib lbi)
      comp = compiler lbi
      ghcVersion = compilerVersion comp

  libBi <- hackThreadedFlag verbosity
             comp (withProfLib lbi) (libBuildInfo lib)

  let libTargetDir = pref
      forceVanillaLib = EnableExtension TemplateHaskell `elem` allExtensions libBi
      -- TH always needs vanilla libs, even when building for profiling

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recurive modules?
  let ghcArgs =
             "--make"
          :  ["-package-name", display pkgid ]
          ++ constructGHCCmdLine lbi libBi clbi libTargetDir verbosity
          ++ map display (libModules lib)
      ghcArgsProf = ghcArgs
          ++ ["-prof",
              "-hisuf", "p_hi",
              "-osuf", "p_o"
             ]
          ++ ghcProfOptions libBi
      ghcArgsShared = ghcArgs
          ++ ["-dynamic",
              "-hisuf", "dyn_hi",
              "-osuf", "dyn_o", "-fPIC"
             ]
          ++ ghcSharedOptions libBi
  unless (null (libModules lib)) $
    do ifVanillaLib forceVanillaLib (runGhcProg ghcArgs)
       ifProfLib (runGhcProg ghcArgsProf)
       ifSharedLib (runGhcProg ghcArgsShared)

  -- build any C sources
  unless (null (cSources libBi)) $ do
     info verbosity "Building C Sources..."
     sequence_ [do let (odir,args) = constructCcCmdLine lbi libBi clbi pref
                                                        filename verbosity
                                                        False
                                                        (withProfLib lbi)
                   createDirectoryIfMissingVerbose verbosity True odir
                   runGhcProg args
                   ifSharedLib (runGhcProg (args ++ ["-fPIC", "-osuf dyn_o"]))
               | filename <- cSources libBi]

  -- build any fortran sources
  unless (null (fSources libBi)) $ do
     info verbosity "Building fortran Sources..."
     sequence_ [do let (odir,args) = constructFortranCmdLine lbi libBi clbi pref
                                                        filename verbosity
                                                        False
                                                        (withProfLib lbi)
                   createDirectoryIfMissingVerbose verbosity True odir
                   runFortranProg args
                   ifSharedLib (runFortranProg (args ++ ["-fPIC", "-osuf dyn_o"]))
               | filename <- fSources libBi]

  -- link:
  info verbosity "Linking..."
  let cObjs = map (`replaceExtension` objExtension) (cSources libBi ++ fSources libBi)
      cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension)) (cSources libBi ++ fSources libBi)
      vanillaLibFilePath = libTargetDir </> mkLibName pkgid
      profileLibFilePath = libTargetDir </> mkProfLibName pkgid
      sharedLibFilePath  = libTargetDir </> mkSharedLibName pkgid
                                              (compilerId (compiler lbi))
      ghciLibFilePath    = libTargetDir </> mkGHCiLibName pkgid
      libInstallPath = libdir $ absoluteInstallDirs pkg_descr lbi NoCopyDest
      sharedLibInstallPath = libInstallPath </> mkSharedLibName pkgid
                                              (compilerId (compiler lbi))

  stubObjs <- fmap catMaybes $ sequence
    [ findFileWithExtension [objExtension] [libTargetDir]
        (ModuleName.toFilePath x ++"_stub")
    | ghcVersion < Version [7,2] [] -- ghc-7.2+ does not make _stub.o files
    , x <- libModules lib ]
  stubProfObjs <- fmap catMaybes $ sequence
    [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
        (ModuleName.toFilePath x ++"_stub")
    | ghcVersion < Version [7,2] [] -- ghc-7.2+ does not make _stub.o files
    , x <- libModules lib ]
  stubSharedObjs <- fmap catMaybes $ sequence
    [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
        (ModuleName.toFilePath x ++"_stub")
    | ghcVersion < Version [7,2] [] -- ghc-7.2+ does not make _stub.o files
    , x <- libModules lib ]

  hObjs     <- getHaskellObjects lib lbi
                    pref objExtension True
  hProfObjs <-
    if (withProfLib lbi)
            then getHaskellObjects lib lbi
                    pref ("p_" ++ objExtension) True
            else return []
  hSharedObjs <-
    if (withSharedLib lbi)
            then getHaskellObjects lib lbi
                    pref ("dyn_" ++ objExtension) False
            else return []

  unless (null hObjs && null cObjs && null stubObjs) $ do
    -- first remove library files if they exists
    sequence_
      [ removeFile libFilePath `catchIO` \_ -> return ()
      | libFilePath <- [vanillaLibFilePath, profileLibFilePath
                       ,sharedLibFilePath,  ghciLibFilePath] ]

    let staticObjectFiles =
               hObjs
            ++ map (pref </>) cObjs
            ++ stubObjs
        profObjectFiles =
               hProfObjs
            ++ map (pref </>) cObjs
            ++ stubProfObjs
        ghciObjFiles =
               hObjs
            ++ map (pref </>) cObjs
            ++ stubObjs
        dynamicObjectFiles =
               hSharedObjs
            ++ map (pref </>) cSharedObjs
            ++ stubSharedObjs
        -- After the relocation lib is created we invoke ghc -shared
        -- with the dependencies spelled out as -package arguments
        -- and ghc invokes the linker with the proper library paths
        ghcSharedLinkArgs =
            [ "-no-auto-link-packages",
              "-shared",
              "-dynamic",
              "-o", sharedLibFilePath ]
            -- For dynamic libs, Mac OS/X needs to know the install location
            -- at build time.
            ++ (if buildOS == OSX
                then ["-dylib-install-name", sharedLibInstallPath]
                else [])
            ++ dynamicObjectFiles
            ++ ["-package-name", display pkgid ]
            ++ ghcPackageFlags lbi clbi
            ++ ["-l"++extraLib | extraLib <- extraLibs libBi]
            ++ ["-L"++extraLibDir | extraLibDir <- extraLibDirs libBi]

    ifVanillaLib False $ do
      (arProg, _) <- requireProgram verbosity arProgram (withPrograms lbi)
      Ar.createArLibArchive verbosity arProg
        vanillaLibFilePath staticObjectFiles

    ifProfLib $ do
      (arProg, _) <- requireProgram verbosity arProgram (withPrograms lbi)
      Ar.createArLibArchive verbosity arProg
        profileLibFilePath profObjectFiles

    ifGHCiLib $ do
      (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
      Ld.combineObjectFiles verbosity ldProg
        ghciLibFilePath ghciObjFiles

    ifSharedLib $
      runGhcProg ghcSharedLinkArgs

-- | Filter the "-threaded" flag when profiling as it does not
--   work with ghc-6.8 and older.
hackThreadedFlag :: Verbosity -> Compiler -> Bool -> BuildInfo -> IO BuildInfo
hackThreadedFlag verbosity comp prof bi
  | not mustFilterThreaded = return bi
  | otherwise              = do
    warn verbosity $ "The ghc flag '-threaded' is not compatible with "
                  ++ "profiling in ghc-6.8 and older. It will be disabled."
    return bi { options = filterHcOptions (/= "-threaded") (options bi) }
  where
    mustFilterThreaded = prof && compilerVersion comp < Version [6, 10] []
                      && "-threaded" `elem` hcOptions GHC bi
    filterHcOptions p hcoptss =
      [ (hc, if hc == GHC then filter p opts else opts)
      | (hc, opts) <- hcoptss ]

-- when using -split-objs, we need to search for object files in the
-- Module_split directory for each module.
getHaskellObjects :: Library -> LocalBuildInfo
                  -> FilePath -> String -> Bool -> IO [FilePath]
getHaskellObjects lib lbi pref wanted_obj_ext allow_split_objs
  | splitObjs lbi && allow_split_objs = do
        let splitSuffix = if compilerVersion (compiler lbi) <
                             Version [6, 11] []
                          then "_split"
                          else "_" ++ wanted_obj_ext ++ "_split"
            dirs = [ pref </> (ModuleName.toFilePath x ++ splitSuffix)
                   | x <- libModules lib ]
        objss <- mapM getDirectoryContents dirs
        let objs = [ dir </> obj
                   | (objs',dir) <- zip objss dirs, obj <- objs',
                     let obj_ext = takeExtension obj,
                     '.':wanted_obj_ext == obj_ext ]
        return objs
  | otherwise  =
        return [ pref </> ModuleName.toFilePath x <.> wanted_obj_ext
               | x <- libModules lib ]


constructGHCCmdLine
        :: LocalBuildInfo
        -> BuildInfo
        -> ComponentLocalBuildInfo
        -> FilePath
        -> Verbosity
        -> [String]
constructGHCCmdLine lbi bi clbi odir verbosity =
        ghcVerbosityOptions verbosity
        -- Unsupported extensions have already been checked by configure
     ++ ghcOptions lbi bi clbi odir

ghcPackageFlags :: LocalBuildInfo -> ComponentLocalBuildInfo -> [String]
ghcPackageFlags lbi clbi
  | ghcVer >= Version [6,11] []
              = concat [ ["-package-id", display ipkgid]
                       | (ipkgid, _) <- componentPackageDeps clbi ]

  | otherwise = concat [ ["-package", display pkgid]
                       | (_, pkgid)  <- componentPackageDeps clbi ]
    where
      ghcVer = compilerVersion (compiler lbi)

constructCcCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                   -> FilePath -> FilePath -> Verbosity -> Bool -> Bool
                   ->(FilePath,[String])
constructCcCmdLine lbi bi clbi pref filename verbosity dynamic profiling
  =  let odir | compilerVersion (compiler lbi) >= Version [6,4,1] []  = pref
              | otherwise = pref </> takeDirectory filename
                        -- ghc 6.4.1 fixed a bug in -odir handling
                        -- for C compilations.
     in
        (odir,
         ghcCcOptions lbi bi clbi odir
         ++ (if verbosity >= deafening then ["-v"] else [])
         ++ ["-c",filename]
         -- Note: When building with profiling enabled, we pass the -prof
         -- option to ghc here when compiling C code, so that the PROFILING
         -- macro gets defined. The macro is used in ghc's Rts.h in the
         -- definitions of closure layouts (Closures.h).
         ++ ["-dynamic" | dynamic]
         ++ ["-prof" | profiling])

ghcCcOptions :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
             -> FilePath -> [String]
ghcCcOptions lbi bi clbi odir
     =  ["-I" ++ dir | dir <- odir : PD.includeDirs bi]
     ++ ghcPackageDbOptions (withPackageDB lbi)
     ++ ghcPackageFlags lbi clbi
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
     ++ (case withOptimization lbi of
           NoOptimisation -> []
           _              -> ["-optc-O2"])
     ++ ["-odir", odir]

mkGHCiLibName :: PackageIdentifier -> String
mkGHCiLibName lib = "HS" ++ display lib <.> "o"

