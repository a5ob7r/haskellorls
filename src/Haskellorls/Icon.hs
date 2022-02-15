{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.Icon
  ( lookupIcon,
    lookupColoredIcon,
    defaultConfig,
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import Haskellorls.LsColor.Util as Color
import qualified Haskellorls.Name.Decorator as Name
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT
import qualified System.FilePath.Posix as Posix

newtype IconDictSource = IconDictSource {getIconDictSource :: [(T.Text, T.Text)]}

data Config = Config
  { fileIcon :: T.Text,
    directoryIcon :: T.Text,
    symlinkIcon :: T.Text,
    symlinkDirIcon :: T.Text,
    pipeIcon :: T.Text,
    socketIcon :: T.Text,
    blockDeviceIcon :: T.Text,
    charDeviceIcon :: T.Text,
    orphanedIcon :: T.Text,
    iconDirectory :: Color.LsColorDict
  }

defaultConfig :: Config
defaultConfig =
  Config
    { fileIcon = defaultIcon,
      directoryIcon = "\xf115",
      symlinkIcon = "\xf481",
      symlinkDirIcon = "\xf482",
      pipeIcon = "\xfce3",
      socketIcon = "\xf6a7",
      blockDeviceIcon = "\xfc29",
      charDeviceIcon = "\xe601",
      orphanedIcon = "\xf127",
      iconDirectory = defaultIconMap
    }

defaultIcon :: T.Text
defaultIcon = "\xf016"

lookupIconDictionary :: T.Text -> Color.LsColorDict -> T.Text
lookupIconDictionary = Color.findLsColorWithDefault defaultIcon

lookupIcon :: Node.NodeInfo -> Config -> [WT.WrappedText]
lookupIcon node config =
  [ WT.toWrappedText icon,
    WT.toWrappedText margin
  ]
  where
    icon = lookupIcon' node config

lookupColoredIcon :: Color.Config -> Node.NodeInfo -> Config -> [WT.WrappedText]
lookupColoredIcon cConfig node config =
  [ Color.toWrappedText cConfig selector icon,
    WT.toWrappedText margin
  ]
  where
    icon = lookupIcon' node config
    selector = Name.lookupEscSeq node

lookupIcon' :: Node.NodeInfo -> Config -> T.Text
lookupIcon' node config@Config {..} = case Node.getNodeLinkInfo node of
  Nothing -> lookupIconForFileInfo node config
  Just (Right _) -> lookupIconForLinkInfo node config
  Just (Left _) -> orphanedIcon

lookupIconForFileInfo :: Node.NodeInfo -> Config -> T.Text
lookupIconForFileInfo node Config {..} = case Node.pfsNodeType $ Node.getNodeStatus node of
  Node.Directory -> directoryIcon
  Node.NamedPipe -> pipeIcon
  Node.Socket -> socketIcon
  Node.BlockDevise -> blockDeviceIcon
  Node.CharDevise -> charDeviceIcon
  Node.Orphan -> orphanedIcon
  _ -> lookupIconDictionary filename iconDirectory
  where
    filename = T.pack . Posix.takeFileName $ Node.getNodePath node

lookupIconForLinkInfo :: Node.NodeInfo -> Config -> T.Text
lookupIconForLinkInfo node Config {..} = case Node.pfsNodeType $ Node.getNodeStatus node of
  Node.Directory -> symlinkDirIcon
  _ -> symlinkIcon

defaultIconMap :: Color.LsColorDict
defaultIconMap = Color.LsColorDict . M.fromList . getIconDictSource $ capitalizeSourceKeys iconMapSource

capitalizeSourceKeys :: IconDictSource -> IconDictSource
capitalizeSourceKeys IconDictSource {..} = IconDictSource $ map capKey getIconDictSource
  where
    capKey (key, val) = (T.toUpper key, val)

iconMapSource :: IconDictSource
iconMapSource =
  IconDictSource
    [ (".txt", "\xf0f6"),
      (".json", "\xfb25"),
      (".lock", "\xf023"),
      ("tags", "\xf02c"),
      ("license", "\xe60a"),
      (".pdf", "\xf1c1"),
      (".iso", "\xfaed"),
      -- Adobe
      (".ai", "\xe7b4"),
      (".ps", "\xe7b8"),
      -- brew
      (".brewfile", "\xe615"),
      ("brewfile", "\xe615"),
      -- build
      ("Makefile", "\xf0f7"),
      (".am", "\xf0f7"),
      -- c
      (".c", "\xe61e"),
      (".h", "\xe61e"),
      (".cpp", "\xfb71"),
      -- clojure
      (".clj", "\xe768"),
      (".cljs", "\xe768"),
      (".cljx", "\xe768"),
      (".cljc", "\xe768"),
      -- compressed
      (".Z", "\xf410"),
      (".gz", "\xf410"),
      (".bz2", "\xf410"),
      (".zip", "\xf410"),
      (".tgz", "\xf410"),
      (".zstd", "\xf410"),
      -- config
      (".editorconfig", "\xe615"),
      (".conf", "\xe615"),
      (".yml", "\xe615"),
      (".yaml", "\xe615"),
      (".toml", "\xe615"),
      (".ini", "\xe615"),
      -- c sharp
      (".cs", "\xf81a"),
      -- css
      (".css", "\xe749"),
      (".scss", "\xe749"),
      (".sass", "\xe749"),
      (".less", "\xe758"),
      (".styl", "\xe759"),
      (".stylus", "\xe759"),
      -- d lang
      (".d", "\xe7af"),
      -- dir colors
      (".dir_colors", "\xe22b"),
      (".dircolors", "\xe22b"),
      ("dircolors", "\xe22b"),
      -- docker
      (".Dockerfile", "\xf308"),
      ("Dockerfile", "\xf308"),
      ("Containerfile", "\xf308"),
      (".dockerignore", "\xf308"),
      -- elixir
      (".exs", "\xe62d"),
      -- elm
      (".elm", "\xe62c"),
      -- erlang
      (".erl", "\xe7b1"),
      -- f sharp
      (".fs", "\xe7a7"),
      (".fsi", "\xe7a7"),
      (".fsx", "\xe7a7"),
      (".fsscript", "\xe7a7"),
      -- git
      (".gitconfig", "\xf1d3"),
      (".gitignore", "\xf1d3"),
      (".gitattributes", "\xf1d3"),
      (".gitmodules", "\xf1d3"),
      -- go
      (".go", "\xe626"),
      -- image
      (".svg", "\xfc1f"),
      (".jpg", "\xf1c5"),
      (".jpeg", "\xf1c5"),
      (".png", "\xf1c5"),
      (".gif", "\xf1c5"),
      (".tif", "\xf1c5"),
      -- haskell
      (".hs", "\xe61f"),
      (".lhs", "\xe61f"),
      (".cabal", "\xe61f"),
      (".nix", "\xf313"),
      -- history
      ("history", "\xf7d9"),
      (".lesshst", "\xf7d9"),
      -- html
      (".html", "\xe736"),
      (".xhtml", "\xe60e"),
      (".xht", "\xe60e"),
      -- java
      (".java", "\xe256"),
      (".jav", "\xe256"),
      (".scala", "\xe737"),
      (".groovy", "\xe775"),
      -- javascript
      (".js", "\xe60c"),
      (".javascript", "\xe60c"),
      (".es", "\xe60c"),
      (".mjs", "\xe60c"),
      (".cjs", "\xe60c"),
      (".jsx", "\xe60c"),
      (".coffee", "\xe61b"),
      (".ts", "\xe628"),
      (".tsx", "\xe628"),
      (".vue", "\xfd42"),
      (".dart", "\xe798"),
      (".drt", "\xe798"),
      -- julia
      (".jl", "\xe624"),
      -- lua
      (".lua", "\xe620"),
      -- markdown
      (".md", "\xf48a"),
      (".markdown", "\xf48a"),
      (".mdown", "\xf48a"),
      (".mkd", "\xf48a"),
      (".mkdn", "\xf48a"),
      (".mdwn", "\xf48a"),
      -- movie
      (".mp4", "\xf1c8"),
      (".webm", "\xf1c8"),
      -- MS Office
      (".doc", "\xf1c2"),
      (".docx", "\xf1c2"),
      (".xls", "\xf1c3"),
      (".xlsx", "\xf1c3"),
      (".ppt", "\xf1c4"),
      (".pptx", "\xf1c4"),
      -- music
      (".mp3", "\xf722"),
      (".m4a", "\xf722"),
      -- perl
      (".pl", "\xe769"),
      (".pm", "\xe769"),
      (".pl6", "\xe769"),
      (".p6", "\xe769"),
      (".pm6", "\xe769"),
      (".raku", "\xe769"),
      (".rakumod", "\xe769"),
      -- php
      (".php", "\xe73d"),
      -- prolog
      (".pdb", "\xe7a1"),
      -- python
      (".py", "\xe235"),
      (".pyw", "\xe235"),
      (".pythonstartup", "\xe235"),
      (".pythonrc", "\xe235"),
      ("Pipfile", "\xe615"),
      -- r
      (".R", "\xfcd2"),
      -- ruby
      (".rb", "\xe21e"),
      (".rbw", "\xe21e"),
      (".gemspec", "\xe21e"),
      (".rbs", "\xe21e"),
      (".erb", "\xe21e"),
      (".ru", "\xe21e"),
      ("Gemfile", "\xe21e"),
      ("Rakefile", "\xe21e"),
      (".rake", "\xe21e"),
      -- rust
      (".rs", "\xe7a8"),
      -- shell
      (".sh", "\xf489"),
      (".bash", "\xf489"),
      (".bashrc", "\xf489"),
      (".bash_profile", "\xf489"),
      (".bash_logout", "\xf489"),
      (".profile", "\xf489"),
      (".zsh", "\xf489"),
      (".zshenv", "\xf489"),
      (".zprofile", "\xf489"),
      (".zshrc", "\xf489"),
      (".zlogin", "\xf489"),
      (".zlogout", "\xf489"),
      ("zshenv", "\xf489"),
      ("zprofile", "\xf489"),
      ("zshrc", "\xf489"),
      ("zlogin", "\xf489"),
      ("zlogout", "\xf489"),
      -- swift
      (".swift", "\xfbe3"),
      -- vim
      (".vim", "\xe62b"),
      (".vimrc", "\xe62b"),
      (".gvimrc", "\xe62b"),
      ("gvimrc", "\xe62b"),
      ("vimrc", "\xe62b"),
      (".viminfo", "\xe62b"),
      -- windows
      (".bat", "\xe629"),
      (".cmd", "\xf17a"),
      (".ps1", "\xf17a"),
      (".scpt", "\xf179"),
      -- xml
      (".xml", "\xfabf")
    ]

margin :: T.Text
margin = " "
