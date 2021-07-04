{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Utils
    ( BemClass   (..)
    , FormMark   ()
    , Page       ()
    , PagesCount ()
    , SetIx      ()
    , UnitIx     ()
    , bemClass
    , formData
    , pagesCount
    , paginate
    ) where

import Data.Aeson (FromJSON ())
import Language.Javascript.JSaddle (eval)
import Miso.String (MisoString (), ms)
import Miso (JSM (), parse)


data BemClass
    = BemClass ParentName [BlockModifier] [ElementModifier]
    deriving stock (Eq, Show)

bemClass :: BlockName -> BemClass -> MisoString
bemClass blockName (BemClass parentName blockModifiers elemModifiers) = ms
    $ blockName ++ " " ++ parentName ++ "-" ++ blockName
        ++ bemModifiers blockName blockModifiers
        ++ bemModifiers (parentName ++ "-" ++ blockName) elemModifiers

bemModifiers :: Name -> [Modifier] -> String
bemModifiers _    []         = ""
bemModifiers name modifiers  =
    ' ' : (unwords . map ((name ++ "_") ++) . filter (not . null) $ modifiers)

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

pagesCount :: PageCount -> PaginatedLength -> PagesCount
pagesCount _         (-1)         = 0
pagesCount pageCount paginatedLen =
    div paginatedLen pageCount + if mod paginatedLen pageCount == 0 then 0 else 1
type PageCount       = Int
type PaginatedLength = Int

paginate :: Page -> PagesCount -> [a] -> [a]
paginate _    0           xs = xs
paginate page pagesCount' xs = take pagesCount' $ drop (page * pagesCount') xs

type BlockModifier   = Modifier
type BlockName       = Name
type ElementModifier = Modifier
type Modifier        = String
type Name            = String
type ParentName      = Name

type FormMark   = MisoString
type Page       = Int
type PagesCount = Int
type SetIx      = Int
type UnitIx     = Int
