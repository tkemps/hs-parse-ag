{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty
import Language.Haskell.AG.SyntaxExt
import Language.Haskell.AG.Objects
import System.Environment (getArgs)
import DataTreePrint
import Text.PrettyPrint

data X = X1 Int | X2 String
type Y a = a Int

f ∷ String → IO ()
f s = putStrLn s

g ∷ Int
g = 42

main ∷ IO ()
main = do
  args ← getArgs
  let fn = head args
  putStrLn $ "Parse file "++fn
  res ← parseFile fn
  case res of
    ParseOk modul@(Module loca mModuleHead modulePragmas
             importDecls decls) → do
      mapM_ (\d → do
                putStrLn $ prettyPrint d
                putStrLn $ showTree d
                putStrLn "\n")
            decls
      putStrLn "Parse OK"
      print (objects modul)
    ParseOk (XmlPage _ _ _ _ _ _ _) → do
      putStrLn "Parse OK with XmlPage"
    ParseOk (XmlHybrid _ _ _ _ _ _ _ _ _) → do
      putStrLn "Parse OK with XmlPage"
    ParseFailed loca msg → do
      putStrLn $ "Parse failed at "++show loca++" with message:"
      print msg

objects ∷ Module l -> [Object]
objects modul = objs_Syn_Module (wrap_Module (sem_Module modul) Inh_Module)
