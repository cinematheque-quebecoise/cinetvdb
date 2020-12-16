{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module EntityLinking.Fonction.Wikidata
  ( roleWdLinking
  )
where

import           Import

import qualified Data.Map as Map

roleWdLinking :: Map Int64 [Text]
roleWdLinking = Map.fromList
  [ -- Interprétation
    ( 15
    , [ "http://www.wikidata.org/entity/Q2259451"
      , "http://www.wikidata.org/entity/Q10800557"
      , "http://www.wikidata.org/entity/Q10798782"
      , "http://www.wikidata.org/entity/Q33999"
      ]
    )

    -- Scénario
    -- > screenwriter
  , ( 31
    , ["http://www.wikidata.org/entity/Q28389"]
    )

    -- Réalisation
    -- > film director
  , ( 1
    , ["http://www.wikidata.org/entity/Q2526255"]
    )

    -- Assistant réalisation
    -- > film director
  , ( 3
    , ["http://www.wikidata.org/entity/Q2526255"]
    )

    -- Producteur
    -- > film producer
  , ( 28
    , ["http://www.wikidata.org/entity/Q3282637"]
    )

    -- Producteur exécutif
    -- > film producer
  , ( 48
    , ["http://www.wikidata.org/entity/Q3282637"]
    )

    -- Producteur délégué
    -- > film producer
  , ( 29
    , ["http://www.wikidata.org/entity/Q3282637"]
    )

    -- Direction de production
    -- > film producer
  , ( 11
    , ["http://www.wikidata.org/entity/Q3282637"]
    )

    -- Montage image
  , ( 19
    , ["http://www.wikidata.org/entity/Q7042855"]
    )

    -- Image
  , ( 14
    , ["http://www.wikidata.org/entity/Q7042855"]
    )

    -- Musique
    -- > composer, singer, record producer, musician
  , ( 21
    , [ "http://www.wikidata.org/entity/Q36834"
      , "http://www.wikidata.org/entity/Q177220"
      , "http://www.wikidata.org/entity/Q183945"
      , "http://www.wikidata.org/entity/Q639669"
      ]
    )

    -- Interprète musique
  , ( 16
    , [ "http://www.wikidata.org/entity/Q36834"
      , "http://www.wikidata.org/entity/Q177220"
      , "http://www.wikidata.org/entity/Q183945"
      , "http://www.wikidata.org/entity/Q639669"
      ]
    )

    -- Montage sonore
  , ( 20
    , [ "http://www.wikidata.org/entity/Q36834"
      , "http://www.wikidata.org/entity/Q177220"
      , "http://www.wikidata.org/entity/Q183945"
      , "http://www.wikidata.org/entity/Q639669"
      ]
    )

    -- Mixage
  , ( 18
    , [ "http://www.wikidata.org/entity/Q36834"
      , "http://www.wikidata.org/entity/Q177220"
      , "http://www.wikidata.org/entity/Q183945"
      , "http://www.wikidata.org/entity/Q639669"
      ]
    )

    -- Prise de son
  , ( 27
    , [ "http://www.wikidata.org/entity/Q36834"
      , "http://www.wikidata.org/entity/Q177220"
      , "http://www.wikidata.org/entity/Q183945"
      , "http://www.wikidata.org/entity/Q639669"
      ]
    )

    -- Conception sonore
  , ( 7
    , [ "http://www.wikidata.org/entity/Q36834"
      , "http://www.wikidata.org/entity/Q177220"
      , "http://www.wikidata.org/entity/Q183945"
      , "http://www.wikidata.org/entity/Q639669"
      ]
    )

    -- Direction artistique
  , ( 10
    , []
    )

     -- Scripte
  , ( 32
    , []
    )

    -- Photographe de plateau
  , ( 26
    , []
    )

    -- Maquillage
  , ( 17
    , []
    )

    -- Costumes
    -- > Costume designer
  , ( 8
    , ["http://www.wikidata.org/entity/Q1323191"]
    )

    -- Recherche
  , ( 38
    , []
    )

    -- Animation
  , ( 2
    , ["http://www.wikidata.org/entity/Q266569"]
    )

    -- Décors
  , ( 9
    , []
    )

    -- Narrateur
  , ( 23
    , []
    )

    -- Narration
  , ( 22
    , []
    )

    -- Source originale
  , ( 33
    , []
    )

    -- Participant
  , ( 24
    , []
    )

    -- Régie
  , ( 30
    , []
    )

    -- Effets spéciaux
  , ( 12
    , []
    )

    -- Coiffure
  , ( 6
    , []
    )

    -- Conception
  , ( 44
    , []
    )

    -- Animateur d'émission
  , ( 35
    , []
    )

    -- Société de distribution
  , ( 39
    , []
    )

    -- Société de production
  , ( 34
    , []
    )

    -- Générique additionnel
  , (13, [])
  ]
