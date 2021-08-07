module Repld.Ansi where

import qualified System.Console.ANSI as Console

style :: [Console.SGR] -> String -> String
style code s =
  Console.setSGRCode code ++ s ++ Console.setSGRCode [Console.Reset]

vivid :: Console.Color -> Console.ConsoleLayer -> Console.SGR
vivid color layer =
  Console.SetColor layer Console.Vivid color

black :: Console.Color
black =
  Console.Black

white :: Console.Color
white =
  Console.White

bg :: Console.ConsoleLayer
bg =
  Console.Background

fg :: Console.ConsoleLayer
fg =
  Console.Foreground
