module Haskellorls.Config.Option.Color
  ( colorParser,
    extraColorParser,
  )
where

import qualified Haskellorls.Config.Option.When as W
import Options.Applicative

colorParser :: Parser W.WHEN
colorParser = noArg <|> withArg
  where
    noArg = flag' W.ALWAYS (long "color")
    withArg =
      option W.reader $
        long "color"
          <> metavar "WHEN"
          <> value W.NEVER
          <> help "When use output with color. If omits =WHEN, then the value assumes 'always'. (default is 'never')."
          <> completeWith ["never", "always", "auto"]

extraColorParser :: Parser Bool
extraColorParser =
  switch $
    long "extra-color"
      <> help "Enable extra coloring which is incompatible for GNU ls."
