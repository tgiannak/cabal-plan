{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | SPDX-License-Identifier: GPL-2.0-or-later
--
-- Implements @cabal-plan license-report@ functionality
module LicenseReport
    ( generateLicenseReport
    ) where

#if defined(MIN_VERSION_Cabal)
import           Cabal.Plan
import qualified Codec.Archive.Tar                      as Tar
import qualified Codec.Archive.Tar.Entry                as Tar
import qualified Codec.Compression.GZip                 as GZip
import           Control.Monad.Compat                   (forM, forM_, guard, unless, when)
import qualified Data.ByteString.Lazy                   as BSL
import qualified Data.ByteString                        as BS
import           Data.Functor.Identity                  (Identity (..))
import           Data.Map                               (Map)
import           Data.List                              (nub)
import qualified Data.Map                               as Map
import           Data.Maybe                             (mapMaybe)
import           Data.Semigroup
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import qualified Data.Text                              as T
import qualified Data.Text.IO                           as T
import qualified Data.Version                           as DV
import           Distribution.SPDX.License (License)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
import           Distribution.Utils.Path
import           Distribution.Pretty
import           System.Directory
import           System.FilePath
import           System.IO                              (stderr)
import           Text.ParserCombinators.ReadP
import           Prelude ()
import           Prelude.Compat

import Cabal.Config (readConfig, Config (..), cfgRepoIndex, hackageHaskellOrg)

#if MIN_VERSION_Cabal(3,2,0)
import          Distribution.Utils.ShortText            (fromShortText)
#endif

-- | Read tarball lazily (and possibly decompress)
readTarEntries :: FilePath -> IO [Tar.Entry]
readTarEntries idxtar = do
    es <- case takeExtension idxtar of
            ".gz"  -> Tar.read . GZip.decompress <$> BSL.readFile idxtar
            ".tar" -> Tar.read                   <$> BSL.readFile idxtar
            ext    -> error ("unknown extension " ++ show ext)

    return (Tar.foldEntries (:) [] (\err -> error ("readTarEntries " ++ show err)) es)

fp2pid :: FilePath -> Maybe PkgId
fp2pid fn0 = do
  [pns,pvs,rest] <- Just (splitDirectories fn0)
  guard (rest == pns <.> "cabal")
  pv <- parseVer pvs
  pure (PkgId (PkgName $ T.pack pns) pv)


parseVer :: String -> Maybe Ver
parseVer str = case reverse $ readP_to_S DV.parseVersion str of
  (ver, "") : _ | not (null (DV.versionBranch ver)), all (>= 0) (DV.versionBranch ver)
      -> Just (Ver $ DV.versionBranch ver)
  _   -> Nothing


readHackageIndex :: FilePath -> IO [(PkgId, BSL.ByteString)]
readHackageIndex indexPath = do
    -- TODO: expose package index configuration as CLI flag
    ents <- readTarEntries indexPath

    pure [ (maybe (error $ show n) id $ fp2pid n,bsl)
         | e@(Tar.Entry { Tar.entryContent = Tar.NormalFile bsl _ }) <- ents
         , let n = Tar.entryPath e
         , takeExtension n == ".cabal"
         ]

getLicenseFiles :: FilePath -> PkgId -> UnitId -> [FilePath] -> IO [BS.ByteString]
getLicenseFiles storeDir compilerId (UnitId uidt) fns = do
  let docDir = storeDir </> T.unpack (dispPkgId compilerId) </> T.unpack uidt </> "share" </> "doc"
  forM fns $ \fn -> BS.readFile (docDir </> fn)

{- WARNING: the code that follows will make you cry; a safety pig is provided below for your benefit.

                         _
 _._ _..._ .-',     _.._(`))
'-. `     '  /-._.-'    ',/
   )         \            '.
  / _    _    |             \
 |  a    a    /              |
 \   .-.                     ;
  '-('' ).-'       ,'       ;
     '-;           |      .'
        \           \    /
        | 7  .__  _.-\   \
        | |  |  ``/  /`  /
       /,_|  |   /,_/   /
          /,_/      '`-'

-}

data LicenseReportInfo = LicenseReportInfo
    { liPkgId :: PkgId
    , liPackageUrl :: T.Text
    , liLicenseInfo :: Maybe LicenseInfo
    , liReverseDependencies :: [PkgName]
    , liUnitType :: UnitType
    }

data LicenseInfo = LicenseInfo
    { liLicenseId :: License
    , liLicenseFiles :: [SymbolicPath PackageDir LicenseFile]
    , liDescription :: T.Text
    }

data DependencyLicenses = DependencyLicenses
    { directDependencyLicenses :: [LicenseReportInfo]
    , indirectDependencyLicenses :: [LicenseReportInfo]
    }

-- Relies on lazy Map to not parse all packages.
getPlanIndexPackageDescriptions :: Config Identity -> PlanJson -> IO (Map PkgId GenericPackageDescription)
getPlanIndexPackageDescriptions cfg plan = do
    indexPath <- maybe (fail "No hackage.haskell.org repository") return $ cfgRepoIndex cfg hackageHaskellOrg
    let pidsOfInterest = Set.fromList (map uPId (Map.elems $ pjUnits plan))
    indexDb <- Map.fromList . filter (flip Set.member pidsOfInterest . fst) <$> readHackageIndex indexPath
    let packageTexts = Map.map BSL.toStrict indexDb
    forM packageTexts $ maybe (fail "parseGenericPackageDescriptionMaybe") pure . parseGenericPackageDescriptionMaybe

genericPackageDescriptionLicenseInfo :: GenericPackageDescription -> LicenseInfo
genericPackageDescriptionLicenseInfo gpd = LicenseInfo
    { liLicenseId = lic
    , liLicenseFiles = lfs
    , liDescription = T.pack desc
    }
  where
    desc = escapeDesc
#if MIN_VERSION_Cabal(3,2,0)
        $ fromShortText
#endif
        $ synopsis $ packageDescription gpd
    lic  = license  $ packageDescription gpd
    lfs  = licenseFiles $ packageDescription gpd

getLicenseInfo :: Map PkgId GenericPackageDescription -> PkgId -> Maybe LicenseInfo
getLicenseInfo pkgDescDb pkgId =
    maybe Nothing (Just . genericPackageDescriptionLicenseInfo) $ Map.lookup pkgId pkgDescDb

-- Assumes that uid0 is mapped by pkgDescDb
-- Includes "rts" in its output, including a non-existent hackage url.
-- https://gitlab.haskell.org/ghc/ghc/-/raw/master/LICENSE
getLicenses :: Map PkgId GenericPackageDescription -> PlanJson -> UnitId -> DependencyLicenses
getLicenses pkgDescDb plan uid0 = DependencyLicenses direct indirect
  where
    -- generally, units belonging to the same package as 'root'
    rootPkgUnits = [ u | u@(Unit { uPId = PkgId pn' _ }) <- Map.elems (pjUnits plan), pn' == pn0 ]
    rootPkgUnitIds = Set.fromList (map uId rootPkgUnits)

    -- the component of interest
    Just root@Unit { uPId = PkgId pn0 _ } = Map.lookup uid0 (pjUnits plan)

    fwdDeps = planJsonIdGraph' plan
    revDeps = invertMap fwdDeps

    transUids = transDeps fwdDeps (uId root) Set.\\ rootPkgUnitIds
    -- immediate reverse dependencies don't include any root units, therefore indirect
    indirectDeps = Set.fromList [ u | u <- Set.toList transUids, Set.null (Map.findWithDefault mempty u revDeps `Set.intersection` rootPkgUnitIds) ]
    -- transitive and not indirect dependency, therefore direct dependency
    directDeps =  transUids Set.\\ indirectDeps

    getLicenseReportInfo :: UnitId -> LicenseReportInfo
    getLicenseReportInfo uid =
      let Just u = Map.lookup uid (pjUnits plan)
          PkgId (PkgName pn) _ = uPId u
          url = case pn of
              "rts" -> "https://gitlab.haskell.org/ghc/ghc/"
              _ -> "http://hackage.haskell.org/package/" <> dispPkgId (uPId u)

          usedBy = Set.fromList [ uPId (Map.findWithDefault undefined unit (pjUnits plan))
                                | unit <- Set.toList (Map.findWithDefault mempty uid revDeps)
                                , unit `Set.member` (directDeps <> indirectDeps)
                                ]
          revDepIds = [ z | PkgId z _ <- Set.toList usedBy,  z /= pn0 ]
      in LicenseReportInfo
          { liPkgId = uPId u
          , liPackageUrl = url
          , liLicenseInfo = getLicenseInfo pkgDescDb (uPId u)
          , liReverseDependencies = revDepIds
          , liUnitType = uType u
          }
    direct = map getLicenseReportInfo $ Set.toList directDeps
    indirect = map getLicenseReportInfo $ Set.toList indirectDeps

showLicenseReportTableRow :: Maybe String -> LicenseReportInfo -> T.Text
showLicenseReportTableRow mlicdir lic = mconcat
    [ if isB then "| **`" else "| `", pn, if isB then "`** | [`" else "` | [`", dispVer pv, "`](", url , ")", " | "
    , licenseEntry , " | ", descEntry, " | "
    , if pn `elem` baseLibs then "*(core library)*"
        else T.intercalate ", " [ T.singleton '`' <> name <> "`" | PkgName name <- usedBy ], " |"
    ]
  where
    usedBy = liReverseDependencies lic
    isB = liUnitType lic == UnitTypeBuiltin
    PkgId (PkgName pn) pv = liPkgId lic
    url = liPackageUrl lic
    -- special core libs whose reverse deps are too noisy
    baseLibs = ["base", "ghc-prim", "integer-gmp", "integer-simple", "rts"]
    licenseEntry = case liLicenseInfo lic of
        Nothing -> " *MISSING* "
        Just licInfo -> liDescription licInfo
    descEntry = case liLicenseInfo lic of
        Nothing -> " *MISSING* "
        Just licInfo ->
            let licurl = case liLicenseFiles licInfo of
                    [] -> liPackageUrl lic
                    (l:_) | Just licdir <- mlicdir, liUnitType lic == UnitTypeGlobal ->
                            T.pack (licdir </> T.unpack (dispPkgId (liPkgId lic)) </> takeFileName (getSymbolicPath l))
                          | otherwise                                                ->
                            liPackageUrl lic <> "/src/" <> T.pack (getSymbolicPath l)
            in
            mconcat [T.pack (prettyShow (liLicenseId licInfo)), "`](", licurl , ")"]

showLicenseReportInfoWarning :: LicenseReportInfo -> Maybe T.Text
showLicenseReportInfoWarning LicenseReportInfo{..} = case liLicenseInfo of
    Nothing -> Just $ "WARNING: couldn't find metadata for " <> dispPkgId liPkgId
    Just _ -> Nothing

-- copyLicenseFiles :: FilePath -> PlanJson -> UnitId -> CompName -> IO ()
-- copyLicenseFiles mlicdir plan uid cn0 = _
-- runLicenseReport :: Maybe FilePath -> PlanJson -> UnitId -> CompName -> IO ()
-- runLicenseReport mlicdir plan uid0 cn0 = do
--     cfg <- readConfig :: _
--     indexPkgDescDb <- getPlanIndexPackageDescriptions cfg plan
--     let storeDir = runIdentity (cfgStoreDir cfg)
--     let DependencyLicenses directDeps indirectDeps = getLicenses indexPkgDescDb plan uid0

-- TODO: emit report to Text or Text builder
generateLicenseReport :: Maybe FilePath -> PlanJson -> UnitId -> CompName -> IO ()
generateLicenseReport mlicdir plan uid0 cn0 = do
    -- find and read ~/.cabal/config
    cfg <- readConfig
    indexPkgDescDb <- getPlanIndexPackageDescriptions cfg plan
    let storeDir = runIdentity (cfgStoreDir cfg)
    let DependencyLicenses directDeps indirectDeps = getLicenses indexPkgDescDb plan uid0
    -- the component of interest
    let Just root@Unit { uPId = PkgId pn0 _ } = Map.lookup uid0 (pjUnits plan)

    let printInfo :: LicenseReportInfo -> IO ()
        printInfo licReportInfo =
            let (PkgId (PkgName pn) _) = (liPkgId licReportInfo)
            in
                unless ("rts" == pn ) $ do
                    mapM_ (T.hPutStrLn stderr) (showLicenseReportInfoWarning licReportInfo)
                    T.putStrLn (showLicenseReportTableRow mlicdir licReportInfo)

              -- -- print (pn, pv, prettyShow lic, cr, lfs, [ j | PkgId (PkgName j) _ <- Set.toList usedBy ])
    -- let copyLicenseFiles :: FilePath -> LicenseInfo -> IO ()
    --     copyLicensefiles licdir licenseInfo = _
              -- forM_ mlicdir $ \licdir -> do

              --   case liUnitType licReportInfo of
              --     UnitTypeGlobal -> do
              --       let lfs' = nub (map (takeFileName . getSymbolicPath) (liLicenseFiles licReportInfo))

              --       when (length lfs' /= length lfs) $ do
              --         T.hPutStrLn stderr ("WARNING: Overlapping license filenames for " <> dispPkgId (uPId u))

              --       crdat <- getLicenseFiles storeDir (pjCompilerId plan) uid lfs'

              --       forM_ (zip lfs' crdat) $ \(fn,txt) -> do
              --         let d = licdir </> T.unpack (dispPkgId (uPId u))
              --         createDirectoryIfMissing True d
              --         BS.writeFile (d </> fn) txt

              --       -- forM_ crdat $ print
              --       pure ()

              --     -- TODO:
              --     --   UnitTypeBuiltin
              --     --   UnitTypeLocal
              --     --   UnitTypeInplace

              --     UnitTypeBuiltin -> T.hPutStrLn stderr ("WARNING: license files for " <> dispPkgId (uPId u) <> " (global/GHC bundled) not copied")
              --     UnitTypeLocal   -> T.hPutStrLn stderr ("WARNING: license files for " <> dispPkgId (uPId u) <> " (project-local package) not copied")
              --     UnitTypeInplace -> T.hPutStrLn stderr ("WARNING: license files for " <> dispPkgId (uPId u) <> " (project-inplace package) not copied")

              --   unless (length lfs == Set.size (Set.fromList lfs)) $
              --     fail ("internal invariant broken for " <> show (uPId u))


    T.putStrLn "# Dependency License Report"
    T.putStrLn ""
    T.putStrLn ("Bold-faced **`package-name`**s denote standard libraries bundled with `" <> dispPkgId (pjCompilerId plan) <> "`.")
    T.putStrLn ""

    T.putStrLn ("## Direct dependencies of `" <> unPkgN pn0 <> ":" <> dispCompNameTarget pn0 cn0 <> "`")
    T.putStrLn ""
    T.putStrLn "| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |"
    T.putStrLn "| --- | --- | --- | --- | --- |"
    forM_ directDeps $ printInfo
    T.putStrLn ""

    T.putStrLn "## Indirect transitive dependencies"
    T.putStrLn ""
    T.putStrLn "| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |"
    T.putStrLn "| --- | --- | --- | --- | --- |"
    forM_ indirectDeps $ printInfo
    T.putStrLn ""

    pure ()

escapeDesc :: String -> String
escapeDesc []          = []
escapeDesc ('\n':rest) = ' ':escapeDesc rest
escapeDesc ('|':rest)  = '\\':'|':escapeDesc rest
escapeDesc (x:xs)      = x:escapeDesc xs

unPkgN :: PkgName -> T.Text
unPkgN (PkgName t) = t

planItemAllLibDeps :: Unit -> Set.Set UnitId
planItemAllLibDeps Unit{..} = mconcat [ ciLibDeps | (cn,CompInfo{..}) <- Map.toList uComps, wantC cn ]
  where
    wantC (CompNameSetup)   = False
    wantC (CompNameTest _)  = False
    wantC (CompNameBench _) = False
    wantC _                 = True

planJsonIdGraph':: PlanJson -> Map UnitId (Set UnitId)
planJsonIdGraph' PlanJson{..} = Map.fromList [ (uId unit, planItemAllLibDeps unit) | unit <- Map.elems pjUnits ]



invertMap :: Ord k => Map k (Set k) -> Map k (Set k)
invertMap m0 = Map.fromListWith mappend [ (v, Set.singleton k) | (k,vs) <- Map.toList m0, v <- Set.toList vs ]

transDeps :: Map UnitId (Set UnitId) -> UnitId -> Set UnitId
transDeps g n0 = go mempty [n0]
  where
    go :: Set UnitId -> [UnitId] -> Set UnitId
    go acc [] = acc
    go acc (n:ns)
      | Set.member n acc = go acc ns
      | otherwise = go (Set.insert n acc) (ns ++ Set.toList (Map.findWithDefault undefined n g))

#else

----------------------------------------------------------------------------
import           Cabal.Plan
import           System.Exit
import           System.IO

generateLicenseReport :: Maybe FilePath -> PlanJson -> UnitId -> CompName -> IO ()
generateLicenseReport _ _ _ _ = do
  hPutStrLn stderr "ERROR: `cabal-plan license-report` sub-command not available! Please recompile/reinstall `cabal-plan` with the `license-report` Cabal flag activated."
  exitFailure

#endif
