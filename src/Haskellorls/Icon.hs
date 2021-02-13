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
lookupIcon' node config@Config {..} = case node of
  Node.FileInfo {} -> lookupIconForFileInfo node config
  Node.LinkInfo {} -> lookupIconForLinkInfo node config
  Node.OrphanedLinkInfo {} -> orphanedIcon

lookupIconForFileInfo :: Node.NodeInfo -> Config -> T.Text
lookupIconForFileInfo node Config {..} = case Name.nodeTypeOf $ Node.nodeInfoStatus node of
  Name.Directory -> directoryIcon
  Name.NamedPipe -> pipeIcon
  Name.Socket -> socketIcon
  Name.BlockDevise -> blockDeviceIcon
  Name.CharDevise -> charDeviceIcon
  Name.Orphan -> orphanedIcon
  _ -> lookupIconDictionary filename iconDirectory
  where
    filename = T.pack . Posix.takeFileName $ Node.nodeInfoPath node

lookupIconForLinkInfo :: Node.NodeInfo -> Config -> T.Text
lookupIconForLinkInfo node Config {..} = case Name.nodeTypeOf $ Node.nodeInfoStatus node of
  Name.Directory -> symlinkDirIcon
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
    [ (".hs", "\xe61f"),
      (".lhs", "\xe61f"),
      (".cabal", "\xe61f"),
      (".nix", "\xf313"),
      (".md", "\xf48a"),
      (".markdown", "\xf48a"),
      (".vim", "\xe62b"),
      (".vimrc", "\xe62b"),
      (".gvimrc", "\xe62b"),
      ("gvimrc", "\xe62b"),
      ("vimrc", "\xe62b"),
      (".rb", "\xe21e"),
      (".erb", "\xe21e"),
      (".ru", "\xe21e"),
      ("Gemfile", "\xe21e"),
      ("Rakefile", "\xe21e"),
      (".lock", "\xf023"),
      (".Dockerfile", "\xf308"),
      ("Dockerfile", "\xf308"),
      (".js", "\xe60c"),
      (".gitignore", "\xf1d3"),
      (".gitattributes", "\xf1d3"),
      (".gitmodules", "\xf1d3"),
      (".txt", "\xf0f6"),
      (".json", "\xfb25"),
      (".editorconfig", "\xe615"),
      (".conf", "\xe615"),
      (".yml", "\xe615"),
      (".yaml", "\xe615"),
      (".toml", "\xe615"),
      (".css", "\xe749"),
      (".scss", "\xe749"),
      (".sass", "\xe749"),
      (".html", "\xe736"),
      (".svg", "\xfc1f"),
      (".jpg", "\xf1c5"),
      (".jpeg", "\xf1c5"),
      (".png", "\xf1c5"),
      (".gif", "\xf1c5"),
      (".rs", "\xe7a8"),
      ("history", "\xf7d9"),
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
      (".c", "\xe61e"),
      (".h", "\xe61e"),
      (".cpp", "\xfb71"),
      ("Makefile", "\xf0f7")
    ]

margin :: T.Text
margin = " "
