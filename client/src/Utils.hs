{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Utils
    ( BemClass    (..)
    , ElementModifier ()
    , FormMark    ()
    , Page        ()
    , PagesCount  ()
    , SetIx       ()
    , UnitIx      ()
    , bemClass
    , darkMode'
    , formData
    , listedStep
    , pagesCount
    , paginate
    , set'
    , setResultsIsDone
    ) where

import Control.Lens ((^.), (^..), (^?))
import Control.Lens.Combinators (_head, each, ix, non, to)
import Control.Lens.Prism (_Just,)
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Traversal (traversed)
import Data.Aeson (FromJSON ())
import Language.Javascript.JSaddle (eval)
import Miso.String (MisoString (), ms)
import Miso (JSM (), parse)

import Common (Set (), SharedSet (SharedSet))

import qualified Model.Model as MM


makeFieldsNoPrefix ''MM.LiteSet
makeFieldsNoPrefix ''MM.Memorizing
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''Set

data BemClass
    = BemClass ParentName [BlockModifier] [ElementModifier]
    deriving stock (Eq, Show)

bemClass :: BlockName -> BemClass -> MisoString
bemClass blockName (BemClass parentName blockModifiers elemModifiers) = ms
    $ blockName ++ " " ++ parentName ++ "-" ++ blockName
        ++ bemModifiers blockName blockModifiers
        ++ bemModifiers (parentName ++ "-" ++ blockName) elemModifiers

bemModifiers :: Name -> [Modifier] -> String
bemModifiers _     []         = ""
bemModifiers name' modifiers  =
    ' ' : (unwords . map ((name' ++ "_") ++) . filter (not . null) $ modifiers)

darkMode' :: MM.Model -> String
darkMode' model = model ^. settings.darkMode.to (maybe "" $ const "darkMode")

formData :: FromJSON a => FormMark -> Reseting -> JSM [a]
formData formMark reseting = parse =<< eval js
  where
    js = " \
        \\n (() => { \
        \\n   const selector = `form[data-mark='" <> formMark <> "']` \
        \\n   const forms = document.querySelectorAll(selector) \
        \\n   const objs = [] \
        \\n   forms.forEach(form => { \
        \\n     const formData = new FormData(form) \
        \\n     " <> (if reseting then "form.reset()" else "\n") <> " \
        \\n     const obj = Object.fromEntries(formData) \
        \\n     for (const key in obj) { \
        \\n       if (key[key.length - 1] === '+') { \
        \\n         obj[key.slice(0, -1)] = formData.getAll(key) \
        \\n         delete obj[key] \
        \\n       } \
        \\n     } \
        \\n     objs.push(obj) \
        \\n   }) \
        \\n   return JSON.stringify(objs) \
        \\n })() \
        \ "
type Reseting = Bool

listedStep :: MM.Memorizing -> a -> [a]
listedStep memorizing' step =
    if memorizing' ^. initLiteSetsLen
            == memorizing' ^.. liteSets.each.unitIxs.each ^. to length
        || memorizing' ^. pause
    then []
    else [ step ]

pagesCount :: PageCount -> PaginatedLength -> PagesCount
pagesCount _         (-1)         = 0
pagesCount pageCount paginatedLen =
    div paginatedLen pageCount + if mod paginatedLen pageCount == 0 then 0 else 1
type PageCount       = Int
type PaginatedLength = Int

paginate :: Page -> PagesCount -> [a] -> [a]
paginate _    0           xs = xs
paginate page pagesCount' xs = take pagesCount' $ drop (page * pagesCount') xs

set' :: Either Set (Either SharedSet (Either MM.BeingDownloadedSet MM.DownloadedSet)) -> Set
set' (Left  set)                                                          = set
set' (Right (Left (SharedSet _ _ _ set)))                                 = set
set' (Right (Right (Left (MM.BeingDownloadedSet (SharedSet _ _ _ set))))) = set
set' (Right (Right (Right (MM.DownloadedSet (SharedSet _ _ _ set)))))     = set

setResultsIsDone :: MM.Model -> Bool
setResultsIsDone model =
    and $ model ^? statistics._head.traversed.to (\(MM.SetResult _ steps') ->
        steps' ^. to length == model ^? sets.ix 0.to set'.units._Just ^. non [].to length)

type BlockModifier   = Modifier
type BlockName       = Name
type Modifier        = String
type Name            = String
type ParentName      = Name

type ElementModifier = Modifier
type FormMark        = MisoString
type Page            = Int
type PagesCount      = Int
type SetIx           = Int
type UnitIx          = Int
