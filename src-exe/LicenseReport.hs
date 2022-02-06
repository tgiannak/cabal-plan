{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | SPDX-License-Identifier: GPL-2.0-or-later
--
-- Implements @cabal-plan license-report@ functionality
module LicenseReport
    ( runLicenseReport
    , LicenseReportFormat(..)
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
import           Data.Maybe                             (mapMaybe, fromMaybe)
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

data LicenseReportFormat = Md | Csv

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

data LicenseReportInfo = LicenseReportInfo
    { liUnit :: Unit
    , liPkgUrl :: T.Text
    , liLicenseInfo :: Maybe LicenseInfo
    , liReverseDependencies :: [PkgName]
    }

liPkgId :: LicenseReportInfo -> PkgId
liPkgId = uPId . liUnit

liUId :: LicenseReportInfo -> UnitId
liUId = uId . liUnit

liUnitType :: LicenseReportInfo -> UnitType
liUnitType = uType . liUnit

liPkgName :: LicenseReportInfo -> T.Text
liPkgName lri = let (PkgId (PkgName pn) _) = liPkgId lri in pn

liPkgVersion :: LicenseReportInfo -> Ver
liPkgVersion lri = let (PkgId (PkgName _) pv) = liPkgId lri in pv

data LicenseInfo = LicenseInfo
    { liLicenseId :: License
    , liLicenseFiles :: [SymbolicPath PackageDir LicenseFile]
    , liDescription :: T.Text
    }

data DependencyLicenses = DependencyLicenses
    { directDependencyLicenses :: [LicenseReportInfo]
    , indirectDependencyLicenses :: [LicenseReportInfo]
    }

allDependencyLicenses :: DependencyLicenses -> [LicenseReportInfo]
allDependencyLicenses DependencyLicenses{..} =
    directDependencyLicenses <> indirectDependencyLicenses

-- | Get all of the package descriptions for packages enumeratd in the plan.
getPlanIndexPackageDescriptions :: Config Identity -> PlanJson -> IO (Map PkgId GenericPackageDescription)
getPlanIndexPackageDescriptions cfg plan = do
    indexPath <- maybe (fail "No hackage.haskell.org repository") return $ cfgRepoIndex cfg hackageHaskellOrg
    let pidsOfInterest = Set.fromList (map uPId (Map.elems $ pjUnits plan))
    indexDb <- Map.fromList . filter (flip Set.member pidsOfInterest . fst) <$> readHackageIndex indexPath
    let packageTexts = Map.map BSL.toStrict indexDb
    forM packageTexts $ maybe (fail "parseGenericPackageDescriptionMaybe") pure . parseGenericPackageDescriptionMaybe

-- | Get the license information from a package description.
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

-- | Get the license info for a package from the database of package descriptions.
getLicenseInfo :: Map PkgId GenericPackageDescription -> PkgId -> Maybe LicenseInfo
getLicenseInfo pkgDescDb pkgId =
    maybe Nothing (Just . genericPackageDescriptionLicenseInfo) $ Map.lookup pkgId pkgDescDb

-- | Get all of the license information from the package descriptoin database and the plan.
--
-- Assumes that uid0 and all of its dependencies are mapped by pkgDescDb.
-- Includes "rts" in its output, even though it isn't a real package. The URL
-- for rts is set to https://gitlab.haskell.org/ghc/ghc instead of a location on
-- Hackage.
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
          { liUnit = u
          , liPkgUrl = url
          , liLicenseInfo = getLicenseInfo pkgDescDb (uPId u)
          , liReverseDependencies = revDepIds
          }
    direct = map getLicenseReportInfo $ Set.toList directDeps
    indirect = map getLicenseReportInfo $ Set.toList indirectDeps

-- | Show a warning if the license report info doesn't include any license
-- information.
showLicenseReportInfoWarning :: LicenseReportInfo -> Maybe T.Text
showLicenseReportInfoWarning lri = case liLicenseInfo lri of
    Nothing -> Just $ "WARNING: couldn't find metadata for " <> dispPkgId (liPkgId lri)
    Just _ -> Nothing

-- | Copy license files for the package to a specific directory.
copyLicenseFiles ::
    -- | The directory to which to copy the files
    FilePath ->
    -- | The path to the Cabal store
    FilePath ->
    -- | The compiler ID
    PkgId ->
    -- | The LicenseReportInfo for the package whose files to copy
    LicenseReportInfo ->
    IO ()
copyLicenseFiles licdir storeDir compilerId licReportInfo = do
    let pkgid = liPkgId licReportInfo
    let pkgIdText = dispPkgId (liPkgId licReportInfo)
    let uid = liUId licReportInfo
    case (liUnitType licReportInfo, liLicenseInfo licReportInfo) of
        (UnitTypeGlobal, Just licenseInfo) -> do
            let lfs = liLicenseFiles licenseInfo
            let lfs' = nub (map (takeFileName . getSymbolicPath) lfs)

            when (length lfs' /= length lfs) $ do
                T.hPutStrLn stderr ("WARNING: Overlapping license filenames for " <> pkgIdText)

            crdat <- getLicenseFiles storeDir compilerId uid lfs'

            forM_ (zip lfs' crdat) $ \(fn,txt) -> do
                let d = licdir </> T.unpack (dispPkgId (liPkgId licReportInfo))
                createDirectoryIfMissing True d
                BS.writeFile (d </> fn) txt

            unless (length lfs == Set.size (Set.fromList lfs)) $
                fail ("internal invariant broken for " <> show pkgid)
            -- forM_ crdat $ print
            pure ()

        -- TODO:
        --   UnitTypeBuiltin
        --   UnitTypeLocal
        --   UnitTypeInplace

        (UnitTypeGlobal, Nothing) -> T.hPutStrLn stderr ("WARNING: license files for " <> pkgIdText <> " (missing metadata) not copied")
        (UnitTypeBuiltin, _) -> T.hPutStrLn stderr ("WARNING: license files for " <> pkgIdText <> " (global/GHC bundled) not copied")
        (UnitTypeLocal, _)  -> T.hPutStrLn stderr ("WARNING: license files for " <> pkgIdText <> " (project-local package) not copied")
        (UnitTypeInplace, _) -> T.hPutStrLn stderr ("WARNING: license files for " <> pkgIdText <> " (project-inplace package) not copied")


-- | Creates a Commonmark formatted license report.
--
-- Some entries reverse dependencies are omitted from the output because they
-- are too common to be informative. The rts package is from the output omitted
-- entirely. Builtin libraries are shown in bold.
generateCommonmarkLicenseReport ::
    -- | If not Nothing, used as base directory for license files for global
    -- units instead of giving a Hackage URL.
    Maybe FilePath ->
    -- | The name of the package the report is for.
    PkgName ->
    -- | The name of the component the report is for.
    CompName ->
    -- | The ID of the compiler.
    PkgId ->
    -- | The dependency license info to include in the report.
    DependencyLicenses ->
    T.Text
generateCommonmarkLicenseReport mlicdir pn0 cn0 compilerId dependencyLicenses =
    T.unlines $
        [ "# Dependency License Report"
        , ""
        , ("Bold-faced **`package-name`**s denote standard libraries bundled with `" <> dispPkgId compilerId <> "`.")
        , ""
        , ("## Direct dependencies of `" <> unPkgN pn0 <> ":" <> dispCompNameTarget pn0 cn0 <> "`")
        , ""
        , "| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |"
        , "| --- | --- | --- | --- | --- |"
        ]
        <>
        mapMaybe showInfo directDeps
        <>
        [ ""
        , "## Indirect transitive dependencies"
        , ""
        , "| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |"
        , "| --- | --- | --- | --- | --- |"
        ]
        <>
        mapMaybe showInfo indirectDeps
        <>
        [""]
  where
    DependencyLicenses directDeps indirectDeps = dependencyLicenses

    showInfo :: LicenseReportInfo -> Maybe T.Text
    showInfo licReportInfo = if ("rts" == liPkgName licReportInfo)
        then Nothing
        else Just $ showLicenseReportTableRow licReportInfo

    showLicenseReportTableRow :: LicenseReportInfo -> T.Text
    showLicenseReportTableRow lic = mconcat
        [ if isB then "| **`" else "| `", pn, if isB then "`** | [`" else "` | [`", dispVer pv, "`](", liPkgUrl lic, ")", " | "
        , licenseEntry , " | ", descEntry, " | "
        , if pn `elem` baseLibs then "*(core library)*"
            else T.intercalate ", " [ T.singleton '`' <> name <> "`" | PkgName name <- liReverseDependencies lic ], " |"
        ]
      where
        isB = liUnitType lic == UnitTypeBuiltin
        PkgId (PkgName pn) pv = liPkgId lic
        -- special core libs whose reverse deps are too noisy
        baseLibs = ["base", "ghc-prim", "integer-gmp", "integer-simple", "rts"]
        licenseEntry = case liLicenseInfo lic of
            Nothing -> " *MISSING* "
            Just licInfo -> liDescription licInfo
        descEntry = case liLicenseInfo lic of
            Nothing -> " *MISSING* "
            Just licInfo ->
                let licurl = case liLicenseFiles licInfo of
                        [] -> liPkgUrl lic
                        (l:_) | Just licdir <- mlicdir, liUnitType lic == UnitTypeGlobal ->
                                T.pack (licdir </> T.unpack (dispPkgId (liPkgId lic)) </> takeFileName (getSymbolicPath l))
                              | otherwise ->
                                liPkgUrl lic <> "/src/" <> T.pack (getSymbolicPath l)
                in
                mconcat [T.pack (prettyShow (liLicenseId licInfo)), "`](", licurl , ")"]

runLicenseReport :: Maybe FilePath -> PlanJson -> UnitId -> CompName -> Maybe LicenseReportFormat -> IO ()
runLicenseReport mlicdir plan uid0 cn0 fmt = do
    let fmt' = fromMaybe Md fmt
    -- find and read ~/.cabal/config
    cfg <- readConfig
    indexPkgDescDb <- getPlanIndexPackageDescriptions cfg plan
    let storeDir = runIdentity (cfgStoreDir cfg)
    let compilerId = pjCompilerId plan
    -- the component of interest
    let Just Unit { uPId = PkgId pn0 _ } = Map.lookup uid0 (pjUnits plan)

    let dependencyLicenses = getLicenses indexPkgDescDb plan uid0
    forM_ (allDependencyLicenses dependencyLicenses) $ \licReportInfo ->
        let (PkgId (PkgName pn) _) = (liPkgId licReportInfo)
        in unless ("rts" == pn ) $ mapM_ (T.hPutStrLn stderr) (showLicenseReportInfoWarning licReportInfo)

    forM_ mlicdir $ \licdir ->
        forM_ (allDependencyLicenses dependencyLicenses) $ copyLicenseFiles licdir storeDir compilerId
    case fmt' of
        Md -> T.putStrLn $ generateCommonmarkLicenseReport mlicdir pn0 cn0 compilerId dependencyLicenses
        Csv -> T.putStrLn $ generateCsvLicenseReport (allDependencyLicenses dependencyLicenses)

-- | Creates a license report in CSV format including: the package name,
-- version, URL, and a list of the licenses.
--
-- The CSV format may have a variable number of columns in each row, since all
-- license file URLs are appended to the end of the row. Unlike the Commonmark
-- report, this does not use local file paths even if licenses were copied.
generateCsvLicenseReport :: [LicenseReportInfo] -> T.Text
generateCsvLicenseReport lris = T.unlines $ mapMaybe generateCsvLicenseLine lris
  where
    generateCsvLicenseLine :: LicenseReportInfo -> Maybe T.Text
    generateCsvLicenseLine lri = case liLicenseInfo lri of
        Just licInfo -> Just . T.intercalate "," $
            [ liPkgName lri
            , dispVer $ liPkgVersion lri
            , liPkgUrl lri
            , T.pack . prettyShow $ liLicenseId  licInfo
            ]
            <>
            nub (map (\l -> liPkgUrl lri <> "/src/" <> T.pack (getSymbolicPath l)) . liLicenseFiles $ licInfo)
        Nothing -> Nothing

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

data LicenseReportFormat = Md | Csv

runLicenseReport :: Maybe FilePath -> PlanJson -> UnitId -> CompName -> Maybe LicenseReportFormat -> IO ()
runLicenseReport _ _ _ _ _ = do
  hPutStrLn stderr "ERROR: `cabal-plan license-report` sub-command not available! Please recompile/reinstall `cabal-plan` with the `license-report` Cabal flag activated."
  exitFailure

#endif
