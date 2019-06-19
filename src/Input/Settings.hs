{-# LANGUAGE RecordWildCards, PatternGuards #-}


-- | Module for reading settings files.
module Input.Settings(
    Settings(..), loadSettings
    ) where

import Control.Exception (catch, throwIO)
import Data.List.Extra
import Data.Maybe
import Language.Haskell.TH.Syntax (lift, runIO)
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.IO.Extra
import qualified Data.Map.Strict as Map
import Paths_hoogle


-- | Settings values. Later settings always override earlier settings.
data Setting
    = -- | Given a Cabal tag/author rename it from the LHS to the RHS.
      --   If the RHS is blank, delete the tag.
      RenameTag String String
    | -- | Change the priority of a module. Given package name, module name, new priority.
      --   Use * for wildcard matches. All un-reordered modules are 0
      ReorderModule String String Int
    deriving Read


data Settings = Settings
    {renameTag :: String -> String -- ^ Rename a cabal tag
    ,reorderModule :: String -> String -> Int
    }


readFileSettings :: FilePath -> String -> IO [Setting]
readFileSettings file backup = do
    src <- readFileUTF8 file `catch` \e ->
        if isDoesNotExistError e
            then return backup
            else throwIO e
    return $ concat $ zipWith f [1..] $ map trim $ lines src
    where
        f i s | null s = []
              | "--" `isPrefixOf` s = []
              | [(x,"")] <- reads s = [x]
              | otherwise = error $ file ++ ":" ++ show i ++ ": Failure to parse, got: " ++ s



-- | Fix bad names in the Cabal file.
loadSettings :: IO Settings
loadSettings = do
    dataDir <- getDataDir
    let backup =
            "-- A list of settings, installed as a data file on the users machine.\n\
            \\n\
            \-- Applied to cabal fields when the same semantic value is used multiple times with\n\
            \-- typos/names/capitalisation.\n\
            \RenameTag \"Silk-B.V.\" \"Silk\"\n\
            \RenameTag \"Silk.-B.V.\" \"Silk\"\n\
            \RenameTag \"Michael-snoyman\" \"Michael-Snoyman\"\n\
            \RenameTag \"Apache-2.0\" \"Apache\"\n\
            \RenameTag \"GPL-3\" \"GPL\"\n\
            \RenameTag \"LGPL-2.1\" \"LGPL\"\n\
            \RenameTag \"LGPL-3\" \"LGPL\"\n\
            \RenameTag \"graphics\" \"Graphics\"\n\
            \RenameTag \"math\" \"Math\"\n\
            \RenameTag \"Unclassified\" \"\"\n\
            \RenameTag \"data\" \"Data\"\n\
            \RenameTag \"Edward-A.-Kmett\" \"Edward-Kmett\"\n\
            \RenameTag \"Jose-Pedro-Magalhaes\" \"José-Pedro-Magalhães\"\n\
            \RenameTag \"AUTHORS\" \"\"\n\
            \RenameTag \"contributors-see-README\" \"\"\n\
            \RenameTag \"author\" \"\"\n\
            \RenameTag \"http://www.cse.chalmers.se/~nad/\" \"Nils Anders Danielsson\"\n\
            \RenameTag \"many-others\" \"\"\n\
            \RenameTag \"Error-handling\" \"Error-Handling\"\n\
            \RenameTag \"Daniel-SchÃ¼ssler\" \"Daniel Schüssler\"\n\
            \RenameTag \"Various\" \"\"\n\
            \RenameTag \"Various;-see-individual-modules\" \"\"\n\
            \\n\
            \-- Reorder modules so the common things come first\n\
            \ReorderModule \"base\" \"Prelude\"        1009\n\
            \ReorderModule \"base\" \"Data.List\"      1008\n\
            \ReorderModule \"base\" \"Data.Maybe\"     1007\n\
            \ReorderModule \"base\" \"Data.Function\"  1006\n\
            \ReorderModule \"base\" \"Control.Monad\"  1005\n\
            \ReorderModule \"base\" \"GHC.*\"        (-1000)"
    src <- readFileSettings (dataDir </> "misc/settings.txt") backup
    return $ createSettings src

createSettings :: [Setting] -> Settings
createSettings xs = Settings{..}
    where
        renameTag = \x -> fromMaybe x $ f x
            where f = literals [(a,b) | RenameTag a b <- xs]

        reorderModule = \pkg -> case f pkg of
                                    [] -> const 0
                                    xs -> let f = wildcards xs
                                          in \mod -> last $ 0 : f mod
            where f = wildcards [(a,(b,c)) | ReorderModule a b c <- xs]


---------------------------------------------------------------------
-- SPECIAL LOOKUPS

literals :: [(String, a)] -> String -> Maybe a
literals xs = \x -> Map.lookup x mp
    where mp = Map.fromList xs

wildcards :: [(String, a)] -> String -> [a]
wildcards xs x = [b | (a,b) <- xs, matchWildcard a x]

matchWildcard :: String -> String -> Bool
matchWildcard ['*'] ys = True -- special common case
matchWildcard ('*':xs) ys = any (matchWildcard xs) $ tails ys
matchWildcard (x:xs) (y:ys) = x == y && matchWildcard xs ys
matchWildcard [] [] = True
matchWildcard _ _ = False
