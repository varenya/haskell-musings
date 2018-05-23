{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty

import           Data.Monoid                   (mconcat)
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Web.Scotty                    as S

main :: IO ()
main =
  scotty 3000 $ do
    get "/" $ do
      S.html . renderHtml $ do H.h1 "My Todo List"
