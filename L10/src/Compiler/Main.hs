module Compiler.Main where

--import Compiler.Syntax     
import Compiler.Parser      (parse)
import Compiler.Interpreter (interp)
import Compiler.Machine     (exec)
import Compiler.Compiler    (compile)

main = do
  source <- getContents
  let prog = parse source
  putStrLn "interpreted:"
  print (interp prog)
  putStrLn "compiled:"
  print (exec (compile prog))
