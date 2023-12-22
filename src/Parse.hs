-- | Parses specified *.hs files and returns a list of all data declarations from them
module Parse (parseFiles) where

import Language.Haskell.Exts
import Data.Data (Data)
import Data.Generics.PlateData (universeBi)
import Control.Monad (liftM)
import System.Exit (exitFailure)
import Debug.Trace

parseFiles :: Maybe [Extension] -> [FilePath] -> IO [Decl SrcSpanInfo]
parseFiles exts = liftM concat . mapM parseFile'
  where
    parser = case exts of
      Nothing -> parseFile
      Just es -> \f -> parseFileWithMode defaultParseMode{ parseFilename = f, extensions = es} f
      
    parseFile' fname = do
      res <- parser fname
      case res of
        ParseOk m -> return $ collectDeclarations m
        ParseFailed srcLoc message -> do
          putStrLn $ unlines [ prettyPrint srcLoc
                             , message
                             ]
          exitFailure

collectDeclarations :: Data l => Module l -> [Decl l]
collectDeclarations (Module _ _ _ _ decls) =
  [ x | x <- universeBi decls, isDeclaration x]
  where
    isDeclaration (DataDecl {}) = True
    isDeclaration (TypeDecl {}) = True
    isDeclaration _ = False
collectDeclarations _ = []
