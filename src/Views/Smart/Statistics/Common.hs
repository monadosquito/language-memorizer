{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}


module Views.Smart.Statistics.Common
    ( statistics'
    ) where

import Control.Lens ((^.), (^?))
import Control.Lens.Combinators (_Just, ix, non, to)
import Control.Lens.TH (makeFieldsNoPrefix)

import Model.Action (Action (), Paginated (Statistics))
import Views.Dumb.PageSwitcher.Common (pageSwitcher)
import Utils (BemClass (BemClass), bemClass, paginate, setResultsIsDone)

import qualified Miso as M

import qualified Model.Model as MM


makeFieldsNoPrefix ''MM.Pages
makeFieldsNoPrefix ''MM.Pagination
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.SetResultStep
makeFieldsNoPrefix ''MM.Set
makeFieldsNoPrefix ''MM.SetResult
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''MM.Unit

statistics' :: BemClass -> MM.Model -> M.View Action
statistics' bemClass_ model = M.main_
    [ M.class_ $ bemClass "Statistics" bemClass_
    ]
    [ M.ul_
        [ M.class_ . bemClass "SetResultListList"
            $ BemClass
                "Statistics"
                []
                [ if model ^. pagination.statistics.count < 2 then "" else "paginated"
                ]
        ]
        $ zipWith (\setResults paginatedSetResultsIx -> M.li_
            [ M.class_ . bemClass "SetResults" $ BemClass
                "Statistics"
                []
                [
                    if paginatedSetResultsIx == 0
                        && model ^. pagination.statistics.current == 0
                        && not (setResultsIsDone model)
                    then "undone"
                    else ""
                ]
            ]
            [ M.ul_
                [ M.class_ . bemClass "SetResultList" $ BemClass "Statistics" [] []
                ]
                $ map
                    (\(MM.SetResult setIx' steps') -> M.li_ []
                        [ M.span_ []
                            [ M.text $ model ^? sets.ix setIx'.name ^. non ""
                            ]
                        , M.ul_
                            [ M.class_ . bemClass "StepList" $ BemClass "Statistics" [] []
                            ]
                            $ map (\(MM.SetResultStep success' unitIx') ->
                                let unit = model
                                        ^? sets.ix setIx'.units._Just.ix unitIx'
                                        ^. non (MM.Unit "" [])
                                in M.li_
                                    [ M.class_ . bemClass "Form" $ BemClass
                                        "Statistics"
                                        []
                                        [ if success' then "succeeded" else "" ]
                                    ]
                                    [ M.span_
                                        [ M.class_ . bemClass "FormField"
                                            $ BemClass "Form" [] []
                                        ]
                                        [ M.text $ unit ^. text
                                        ]
                                    , M.ul_
                                        [ M.class_ . bemClass "FieldList"
                                            $ BemClass "Form" [] []
                                        ]
                                        . map (\translate -> M.li_ []
                                            [ M.span_
                                                [ M.class_ . bemClass "FormField"
                                                    $ BemClass "Form" [] []
                                                ]
                                                [ M.text translate
                                                ]
                                            ])
                                        $ unit ^. translates
                                    ])
                            steps'
                    ])
                    setResults
            ])
        paginatedStatistics
        [ 0..length paginatedStatistics - 1 ]
    , pageSwitcher (BemClass "Statistics" [] []) Statistics
        $ model ^. pagination.statistics.count
    ]
  where
    paginatedStatistics = paginate
        (model ^. pagination.statistics.current)
        (model ^. settings.statisticsPageCount.to read)
        $ model ^. statistics
