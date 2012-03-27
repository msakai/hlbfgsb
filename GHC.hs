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

module GHC (
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
        {-configure,-} getInstalledPackages,
        {-buildLib,-} buildExe,
        {-installLib,-} installExe,
--        libAbiHash,
--        registerPackage,
        ghcOptions,
        ghcVerbosityOptions,
        ghcPackageDbOptions,
        ghcLibDir,
    )

import Distribution.Compat.ReadP
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..)
         , Library(..), libModules, hcOptions, allExtensions )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo_(..) )
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
         , ghcProgram, ghcPkgProgram, hsc2hsProgram
         , arProgram, ranlibProgram, ldProgram
         , gccProgram )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Program.Ar    as Ar
import qualified Distribution.Simple.Program.Ld    as Ld
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..), compilerVersion
         , OptimisationLevel(..), PackageDB(..), PackageDBStack
         , Flag, languageToFlags, extensionsToFlags )
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
import Distribution.Text
         ( display, simpleParse )
import Language.Haskell.Extension (Language(..), Extension(..), KnownExtension(..))

import Control.Monad            ( unless, when, liftM )
import Data.Char                ( isSpace )
import Data.List
import Data.Maybe               ( catMaybes )
import System.Directory
         ( removeFile, getDirectoryContents, doesFileExist
         , getTemporaryDirectory )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension, splitExtension )
import System.IO (hClose, hPutStrLn)
import Exception (catchExit, catchIO)
import System.Process

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration -> IO (Compiler, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf0 = do

  (ghcProg, ghcVersion, conf1) <-
    requireProgramVersion verbosity ghcProgram
      (orLaterVersion (Version [6,4] []))
      (userMaybeSpecifyPath "ghc" hcPath conf0)

  -- This is slightly tricky, we have to configure ghc first, then we use the
  -- location of ghc to help find ghc-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcPkgProg, ghcPkgVersion, conf2) <-
    requireProgramVersion verbosity ghcPkgProgram {
      programFindLocation = guessGhcPkgFromGhcPath ghcProg
    }
    anyVersion (userMaybeSpecifyPath "ghc-pkg" hcPkgPath conf1)

  when (ghcVersion /= ghcPkgVersion) $ die $
       "Version mismatch between ghc and ghc-pkg: "
    ++ programPath ghcProg ++ " is version " ++ display ghcVersion ++ " "
    ++ programPath ghcPkgProg ++ " is version " ++ display ghcPkgVersion

  -- Likewise we try to find the matching hsc2hs program.
  let hsc2hsProgram' = hsc2hsProgram {
                           programFindLocation = guessHsc2hsFromGhcPath ghcProg
                       }
      conf3 = addKnownProgram hsc2hsProgram' conf2

  languages  <- getLanguages verbosity ghcProg
  extensions <- getExtensions verbosity ghcProg

  ghcInfo <- if ghcVersion >= Version [6,7] []
             then do xs <- getProgramOutput verbosity ghcProg ["--info"]
                     case reads xs of
                         [(i, ss)]
                          | all isSpace ss ->
                             return i
                         _ ->
                             die "Can't parse --info output of GHC"
             else return []

  let comp = Compiler {
        compilerId             = CompilerId GHC ghcVersion,
        compilerLanguages      = languages,
        compilerExtensions     = extensions
      }
      conf4 = configureToolchain ghcProg ghcInfo conf3 -- configure gcc and ld
  return (comp, conf4)

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find
-- the corresponding tool; e.g. if the tool is ghc-pkg, we try looking
-- for a versioned or unversioned ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessToolFromGhcPath :: FilePath -> ConfiguredProgram -> Verbosity
                     -> IO (Maybe FilePath)
guessToolFromGhcPath tool ghcProg verbosity
  = do let path              = programPath ghcProg
           dir               = takeDirectory path
           versionSuffix     = takeVersionSuffix (dropExeExtension path)
           guessNormal       = dir </> tool <.> exeExtension
           guessGhcVersioned = dir </> (tool ++ "-ghc" ++ versionSuffix) <.> exeExtension
           guessVersioned    = dir </> (tool ++ versionSuffix) <.> exeExtension
           guesses | null versionSuffix = [guessNormal]
                   | otherwise          = [guessGhcVersioned,
                                           guessVersioned,
                                           guessNormal]
       info verbosity $ "looking for tool " ++ show tool ++ " near compiler in " ++ dir
       exists <- mapM doesFileExist guesses
       case [ file | (file, True) <- zip guesses exists ] of
         [] -> return Nothing
         (fp:_) -> do info verbosity $ "found " ++ tool ++ " in " ++ fp
                      return (Just fp)

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = reverse . takeWhile (`elem ` "0123456789.-") . reverse

        dropExeExtension :: FilePath -> FilePath
        dropExeExtension filepath =
          case splitExtension filepath of
            (filepath', extension) | extension == exeExtension -> filepath'
                                   | otherwise                 -> filepath

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding ghc-pkg, we try looking for both a versioned and unversioned
-- ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessGhcPkgFromGhcPath :: ConfiguredProgram -> Verbosity -> IO (Maybe FilePath)
guessGhcPkgFromGhcPath = guessToolFromGhcPath "ghc-pkg"

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding hsc2hs, we try looking for both a versioned and unversioned
-- hsc2hs in the same dir, that is:
--
-- > /usr/local/bin/hsc2hs-ghc-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs(.exe)
--
guessHsc2hsFromGhcPath :: ConfiguredProgram -> Verbosity -> IO (Maybe FilePath)
guessHsc2hsFromGhcPath = guessToolFromGhcPath "hsc2hs"

-- | Adjust the way we find and configure gcc and ld
--
configureToolchain :: ConfiguredProgram -> [(String, String)]
                                        -> ProgramConfiguration
                                        -> ProgramConfiguration
configureToolchain ghcProg ghcInfo =
    addKnownProgram gccProgram {
      programFindLocation = findProg gccProgram
                              [ if ghcVersion >= Version [6,12] []
                                  then mingwBinDir </> "gcc.exe"
                                  else baseDir     </> "gcc.exe" ],
      programPostConf     = configureGcc
    }
  . addKnownProgram ldProgram {
      programFindLocation = findProg ldProgram
                              [ if ghcVersion >= Version [6,12] []
                                  then mingwBinDir </> "ld.exe"
                                  else libDir      </> "ld.exe" ],
      programPostConf     = configureLd
    }
  . addKnownProgram arProgram {
      programFindLocation = findProg arProgram
                              [ if ghcVersion >= Version [6,12] []
                                  then mingwBinDir </> "ar.exe"
                                  else libDir      </> "ar.exe" ]
    }
  where
    Just ghcVersion = programVersion ghcProg
    compilerDir = takeDirectory (programPath ghcProg)
    baseDir     = takeDirectory compilerDir
    mingwBinDir = baseDir </> "mingw" </> "bin"
    libDir      = baseDir </> "gcc-lib"
    includeDir  = baseDir </> "include" </> "mingw"
    isWindows   = case buildOS of Windows -> True; _ -> False

    -- on Windows finding and configuring ghc's gcc and ld is a bit special
    findProg :: Program -> [FilePath] -> Verbosity -> IO (Maybe FilePath)
    findProg prog locations
      | isWindows = \verbosity -> look locations verbosity
      | otherwise = programFindLocation prog
      where
        look [] verbosity = do
          warn verbosity ("Couldn't find " ++ programName prog ++ " where I expected it. Trying the search path.")
          programFindLocation prog verbosity
        look (f:fs) verbosity = do
          exists <- doesFileExist f
          if exists then return (Just f)
                    else look fs verbosity

    ccFlags        = getFlags "C compiler flags"
    gccLinkerFlags = getFlags "Gcc Linker flags"
    ldLinkerFlags  = getFlags "Ld Linker flags"

    getFlags key = case lookup key ghcInfo of
                   Nothing -> []
                   Just flags ->
                       case reads flags of
                       [(args, "")] -> args
                       _ -> [] -- XXX Should should be an error really

    configureGcc :: Verbosity -> ConfiguredProgram -> IO [ProgArg]
    configureGcc v cp = liftM (++ (ccFlags ++ gccLinkerFlags))
                      $ configureGcc' v cp

    configureGcc' :: Verbosity -> ConfiguredProgram -> IO [ProgArg]
    configureGcc'
      | isWindows = \_ gccProg -> case programLocation gccProg of
          -- if it's found on system then it means we're using the result
          -- of programFindLocation above rather than a user-supplied path
          -- Pre GHC 6.12, that meant we should add these flags to tell
          -- ghc's gcc where it lives and thus where gcc can find its
          -- various files:
          FoundOnSystem {}
           | ghcVersion < Version [6,11] [] ->
              return ["-B" ++ libDir, "-I" ++ includeDir]
          _ -> return []
      | otherwise = \_ _   -> return []

    configureLd :: Verbosity -> ConfiguredProgram -> IO [ProgArg]
    configureLd v cp = liftM (++ ldLinkerFlags) $ configureLd' v cp

    -- we need to find out if ld supports the -x flag
    configureLd' :: Verbosity -> ConfiguredProgram -> IO [ProgArg]
    configureLd' verbosity ldProg = do
      tempDir <- getTemporaryDirectory
      ldx <- withTempFile tempDir ".c" $ \testcfile testchnd ->
             withTempFile tempDir ".o" $ \testofile testohnd -> do
               hPutStrLn testchnd "int foo() {}"
               hClose testchnd; hClose testohnd
               rawSystemProgram verbosity ghcProg ["-c", testcfile,
                                                   "-o", testofile]
               withTempFile tempDir ".o" $ \testofile' testohnd' ->
                 do
                   hClose testohnd'
                   _ <- rawSystemProgramStdout verbosity ldProg
                     ["-x", "-r", testofile, "-o", testofile']
                   return True
                 `catchIO`   (\_ -> return False)
                 `catchExit` (\_ -> return False)
      if ldx
        then return ["-x"]
        else return []

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, Flag)]
getLanguages _ ghcProg
  -- TODO: should be using --supported-languages rather than hard coding
  | ghcVersion >= Version [7] [] = return [(Haskell98,   "-XHaskell98")
                                          ,(Haskell2010, "-XHaskell2010")]
  | otherwise                    = return [(Haskell98,   "")]
  where
    Just ghcVersion = programVersion ghcProg

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Flag)]
getExtensions verbosity ghcProg
  | ghcVersion >= Version [6,7] [] = do

    str <- rawSystemStdout verbosity (programPath ghcProg)
              ["--supported-languages"]
    let extStrs = if ghcVersion >= Version [7] []
                  then lines str
                  else -- Older GHCs only gave us either Foo or NoFoo,
                       -- so we have to work out the other one ourselves
                       [ extStr''
                       | extStr <- lines str
                       , let extStr' = case extStr of
                                       'N' : 'o' : xs -> xs
                                       _              -> "No" ++ extStr
                       , extStr'' <- [extStr, extStr']
                       ]
    let extensions0 = [ (ext, "-X" ++ display ext)
                      | Just ext <- map simpleParse extStrs ]
        extensions1 = if ghcVersion >= Version [6,8]  [] &&
                         ghcVersion <  Version [6,10] []
                      then -- ghc-6.8 introduced RecordPuns however it
                           -- should have been NamedFieldPuns. We now
                           -- encourage packages to use NamedFieldPuns
                           -- so for compatability we fake support for
                           -- it in ghc-6.8 by making it an alias for
                           -- the old RecordPuns extension.
                           (EnableExtension  NamedFieldPuns, "-XRecordPuns") :
                           (DisableExtension NamedFieldPuns, "-XNoRecordPuns") :
                           extensions0
                      else extensions0
        extensions2 = if ghcVersion <  Version [7,1] []
                      then -- ghc-7.2 split NondecreasingIndentation off
                           -- into a proper extension. Before that it
                           -- was always on.
                           (EnableExtension  NondecreasingIndentation, "") :
                           (DisableExtension NondecreasingIndentation, "") :
                           extensions1
                      else extensions1
    return extensions2

  | otherwise = return oldLanguageExtensions

  where
    Just ghcVersion = programVersion ghcProg

-- | For GHC 6.6.x and earlier, the mapping from supported extensions to flags
oldLanguageExtensions :: [(Extension, Flag)]
oldLanguageExtensions =
    let doFlag (f, (enable, disable)) = [(EnableExtension  f, enable),
                                         (DisableExtension f, disable)]
        fglasgowExts = ("-fglasgow-exts",
                        "") -- This is wrong, but we don't want to turn
                            -- all the extensions off when asked to just
                            -- turn one off
        fFlag flag = ("-f" ++ flag, "-fno-" ++ flag)
    in concatMap doFlag
    [(OverlappingInstances       , fFlag "allow-overlapping-instances")
    ,(TypeSynonymInstances       , fglasgowExts)
    ,(TemplateHaskell            , fFlag "th")
    ,(ForeignFunctionInterface   , fFlag "ffi")
    ,(MonomorphismRestriction    , fFlag "monomorphism-restriction")
    ,(MonoPatBinds               , fFlag "mono-pat-binds")
    ,(UndecidableInstances       , fFlag "allow-undecidable-instances")
    ,(IncoherentInstances        , fFlag "allow-incoherent-instances")
    ,(Arrows                     , fFlag "arrows")
    ,(Generics                   , fFlag "generics")
    ,(ImplicitPrelude            , fFlag "implicit-prelude")
    ,(ImplicitParams             , fFlag "implicit-params")
    ,(CPP                        , ("-cpp", ""{- Wrong -}))
    ,(BangPatterns               , fFlag "bang-patterns")
    ,(KindSignatures             , fglasgowExts)
    ,(RecursiveDo                , fglasgowExts)
    ,(ParallelListComp           , fglasgowExts)
    ,(MultiParamTypeClasses      , fglasgowExts)
    ,(FunctionalDependencies     , fglasgowExts)
    ,(Rank2Types                 , fglasgowExts)
    ,(RankNTypes                 , fglasgowExts)
    ,(PolymorphicComponents      , fglasgowExts)
    ,(ExistentialQuantification  , fglasgowExts)
    ,(ScopedTypeVariables        , fFlag "scoped-type-variables")
    ,(FlexibleContexts           , fglasgowExts)
    ,(FlexibleInstances          , fglasgowExts)
    ,(EmptyDataDecls             , fglasgowExts)
    ,(PatternGuards              , fglasgowExts)
    ,(GeneralizedNewtypeDeriving , fglasgowExts)
    ,(MagicHash                  , fglasgowExts)
    ,(UnicodeSyntax              , fglasgowExts)
    ,(PatternSignatures          , fglasgowExts)
    ,(UnliftedFFITypes           , fglasgowExts)
    ,(LiberalTypeSynonyms        , fglasgowExts)
    ,(TypeOperators              , fglasgowExts)
    ,(GADTs                      , fglasgowExts)
    ,(RelaxedPolyRec             , fglasgowExts)
    ,(ExtendedDefaultRules       , fFlag "extended-default-rules")
    ,(UnboxedTuples              , fglasgowExts)
    ,(DeriveDataTypeable         , fglasgowExts)
    ,(ConstrainedClassMethods    , fglasgowExts)
    ]


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

runFortranProg :: [String] -> IO ()
runFortranProg args = do
    print args
    runProcess "gfortran" args Nothing Nothing Nothing Nothing Nothing >>= waitForProcess
    return ()

constructFortranCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                   -> FilePath -> FilePath -> Verbosity -> Bool -> Bool
                   -> [String]
constructFortranCmdLine lbi bi clbi pref filename verbosity dynamic profiling =
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

  mapM_ print $ fSources libBi
  -- build any fortran sources
  unless (null (fSources libBi)) $ do
     info verbosity "Building fortran Sources..."
     sequence_ [do let args = constructFortranCmdLine lbi libBi clbi pref
                                                        filename verbosity
                                                        False
                                                        (withProfLib lbi)
                   createDirectoryIfMissingVerbose verbosity True pref
                   runFortranProg args
--                   ifSharedLib (runFortranProg (args ++ ["-fPIC", "-osuf dyn_o"]))
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

-- | Extracts a String representing a hash of the ABI of a built
-- library.  It can fail if the library has not yet been built.
--
libAbiHash :: Verbosity -> PackageDescription -> LocalBuildInfo
           -> Library -> ComponentLocalBuildInfo -> IO String
libAbiHash verbosity pkg_descr lbi lib clbi = do
  libBi <- hackThreadedFlag verbosity
             (compiler lbi) (withProfLib lbi) (libBuildInfo lib)
  let
      ghcArgs =
             "--abi-hash"
          :  ["-package-name", display (packageId pkg_descr) ]
          ++ constructGHCCmdLine lbi libBi clbi (buildDir lbi) verbosity
          ++ map display (exposedModules lib)
  --
  rawSystemProgramStdoutConf verbosity ghcProgram (withPrograms lbi) ghcArgs


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

-- -----------------------------------------------------------------------------
-- Installing

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic librarys
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib = do
  -- copy .hi files over:
  let copyHelper installFun src dst n = do
        createDirectoryIfMissingVerbose verbosity True dst
        installFun verbosity (src </> n) (dst </> n)
      copy       = copyHelper installOrdinaryFile
      copyShared = copyHelper installExecutableFile
      copyModuleFiles ext =
        findModuleFiles [builtDir] [ext] (libModules lib)
          >>= installOrdinaryFiles verbosity targetDir
  ifVanilla $ copyModuleFiles "hi"
  ifProf    $ copyModuleFiles "p_hi"
  ifShared  $ copyModuleFiles "dyn_hi"

  -- copy the built library files over:
  ifVanilla $ copy builtDir targetDir vanillaLibName
  ifProf    $ copy builtDir targetDir profileLibName
  ifGHCi    $ copy builtDir targetDir ghciLibName
  ifShared  $ copyShared builtDir dynlibTargetDir sharedLibName

  -- run ranlib if necessary:
  ifVanilla $ updateLibArchive verbosity lbi
                               (targetDir </> vanillaLibName)
  ifProf    $ updateLibArchive verbosity lbi
                               (targetDir </> profileLibName)

  where
    vanillaLibName = mkLibName pkgid
    profileLibName = mkProfLibName pkgid
    ghciLibName    = mkGHCiLibName pkgid
    sharedLibName  = mkSharedLibName pkgid (compilerId (compiler lbi))

    pkgid          = packageId pkg

    hasLib    = not $ null (libModules lib)
                   && null (cSources (libBuildInfo lib))
    ifVanilla = when (hasLib && withVanillaLib lbi)
    ifProf    = when (hasLib && withProfLib    lbi)
    ifGHCi    = when (hasLib && withGHCiLib    lbi)
    ifShared  = when (hasLib && withSharedLib  lbi)

-- | On MacOS X we have to call @ranlib@ to regenerate the archive index after
-- copying. This is because the silly MacOS X linker checks that the archive
-- index is not older than the file itself, which means simply
-- copying/installing the file breaks it!!
--
updateLibArchive :: Verbosity -> LocalBuildInfo -> FilePath -> IO ()
updateLibArchive verbosity lbi path
  | buildOS == OSX = do
    (ranlib, _) <- requireProgram verbosity ranlibProgram (withPrograms lbi)
    rawSystemProgram verbosity ranlib [path]
  | otherwise = return ()


-- -----------------------------------------------------------------------------
-- Registering

registerPackage
  :: Verbosity
  -> InstalledPackageInfo
  -> PackageDescription
  -> LocalBuildInfo
  -> Bool
  -> PackageDBStack
  -> IO ()
registerPackage verbosity installedPkgInfo _pkg lbi _inplace packageDbs = do
  let Just ghcPkg = lookupProgram ghcPkgProgram (withPrograms lbi)
  HcPkg.reregister verbosity ghcPkg packageDbs (Right installedPkgInfo)
