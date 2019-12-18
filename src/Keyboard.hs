module Keyboard
( model
) where

import qualified Data.Set as Set
import Graphics.OpenSCAD

import qualified Keyboard.Components.KeyPlate  as KeyPlate
import qualified Keyboard.Components.KeySwitch as KeySwitch

switchWidth = 18.6
switchClearance = 2
keyplateDepth = 2.5

keyhole = translate (-fullWidth/2, -fullWidth/2, -keyplateDepth) (box fullWidth fullWidth keyplateDepth)
    where
        fullWidth = switchWidth + 2 * switchClearance

inv (x,y,z) = (-x,-y,-z)

rotateAroundPoint
    :: Vector3d -- | point to rotate about
    -> Vector3d -- | rotation vector
    -> Model Vector3d -- | model to rotate
    -> Model Vector3d -- | rotated model
rotateAroundPoint v1 v2
    = translate v1
    . rotate v2
    . translate (inv v1)

rotatedKeyPlate = rotateAroundPoint (0,0,10) (0,45,0) KeyPlate.model

type KeyAddress = (Int, Int)
type KeyAddresses = Set.Set KeyAddress
type LocationFunction = Model Vector3d -> KeyAddress -> Model Vector3d
data KeyCluster = KeyCluster
    { addresses :: KeyAddresses
    , locationFunction :: LocationFunction
    }

data Orientation
    = Horizontal
    | Vertical

adjacentKeys :: Orientation -> KeyAddresses -> Set.Set (KeyAddress, KeyAddress)
adjacentKeys Horizontal addresses =
    Set.map
        (\key -> (key, addressRightOf key))
        (Set.filter (\key -> (addressRightOf key) `Set.member` addresses) addresses)
adjacentKeys Vertical addresses =
    Set.map
        (\key -> (key, addressAbove key))
        (Set.filter (\key -> (addressAbove key) `Set.member` addresses) addresses)

mainCluster = KeyCluster 
    { addresses = Set.delete (6,0) $ Set.fromList
        [ (i,j)
        | i <- [0..6]
        , j <- [0..3]
        ]
    , locationFunction = locateMainClusterKey
    }

locateMainClusterKey :: LocationFunction
locateMainClusterKey model (xInt, yInt)
    = translate (0,0,2)
    . rotateAroundPoint (0,0,255) (    0,-4.2*x,-0.85*x)
    . rotateAroundPoint (0,0,155) (6.5*y,     0,      0)
    . translate (0,0,depthAdjust xInt)
    $ model
        where
            x = fromIntegral xInt
            y = fromIntegral yInt
 
            depthAdjust :: Int -> Double
            depthAdjust 0 = 5.2
            depthAdjust 1 = 3.2
            depthAdjust 2 = 0
            depthAdjust 3 = -3
            depthAdjust 4 = 0
            depthAdjust columnNumber = 2 * ((fromIntegral columnNumber) - 4)


baselineTilt = 7.5 -- degrees
splay = 2.3 -- degrees
    
clusterFlange
    :: KeyCluster
    -> Model Vector3d
clusterFlange cluster = difference
    ( union
        [ upperFlanges
        , lowerFlanges
        , leftFlanges
        , rightFlanges
        , upperWebs
        , lowerWebs
        , leftWebs
        , rightWebs
        ]
    )
    (translate (-200,-200,-400) (box 400 400 400))
  where
        locs = addresses cluster
        locate = locationFunction cluster
        upperFlanges = union
            [ locate (hull [KeyPlate.ulFlangePost, KeyPlate.urFlangePost]) address
            | address <- topmostKeys cluster
            ]
        lowerFlanges = union
            [ locate (hull [KeyPlate.llFlangePost, KeyPlate.lrFlangePost]) address
            | address <- bottommostKeys cluster
            ]
        leftFlanges = union
            [ locate (hull [KeyPlate.ulFlangePost, KeyPlate.llFlangePost]) address
            | address <- leftmostKeys cluster
            ]
        rightFlanges = union
            [ locate (hull [KeyPlate.lrFlangePost, KeyPlate.urFlangePost]) address
            | address <- rightmostKeys cluster
            ]
        upperWebs = union
            [ hull
                [ locate KeyPlate.urFlangePost address
                , locate KeyPlate.ulFlangePost (addressRightOf address)
                ]
            | address <- Set.toList locs
            , and
                [ (addressRightOf address) `elem` locs
                , or
                    [ (addressAbove address) `notElem` locs
                    , ((addressAbove . addressRightOf) address) `notElem` locs
                    ]
                ]
            ]
        lowerWebs = union
            [ hull
                [ locate KeyPlate.lrFlangePost address
                , locate KeyPlate.llFlangePost (addressRightOf address)
                ]
            | address <- Set.toList locs
            , and
                [ (addressRightOf address) `elem` locs
                , or
                    [ (addressBelow address) `notElem` locs
                    , ((addressRightOf . addressBelow) address) `notElem` locs
                    ]
                ]
            ]
        leftWebs = union
            [ hull
                [ locate KeyPlate.ulFlangePost address
                , locate KeyPlate.llFlangePost (addressAbove address)
                ] 
            | address <- Set.toList locs
            , and
                [ (addressAbove address) `elem` locs
                , or
                    [ (addressLeftOf address) `notElem` locs
                    , ((addressLeftOf . addressAbove) address) `notElem` locs
                    ]
                ]
            ]
        rightWebs = union
            [ hull
                [ locate KeyPlate.urFlangePost address
                , locate KeyPlate.lrFlangePost (addressAbove address)
                ]
            | address <- Set.toList locs
            , and
                [ (addressAbove address) `elem` locs
                , or
                    [ (addressRightOf address) `notElem` locs
                    , ((addressAbove . addressRightOf) address) `notElem` locs
                    ]
                ]
            ]

topmostKeys :: KeyCluster -> [KeyAddress]
topmostKeys (KeyCluster addresses _) = 
    [ address
    | address <- Set.toList addresses
    , addressAbove address `notElem` addresses
    ]

bottommostKeys :: KeyCluster -> [KeyAddress]
bottommostKeys (KeyCluster addresses _) = 
    [ address
    | address <- Set.toList addresses
    , addressBelow address `notElem` addresses
    ]

leftmostKeys :: KeyCluster -> [KeyAddress]
leftmostKeys (KeyCluster addresses _) = 
    [ address
    | address <- Set.toList addresses
    , addressLeftOf address `notElem` addresses
    ]

rightmostKeys :: KeyCluster -> [KeyAddress]
rightmostKeys (KeyCluster addresses _) = 
    [ address
    | address <- Set.toList addresses
    , addressRightOf address `notElem` addresses
    ]

addressAbove   (x,y) = (x,y+1)
addressBelow   (x,y) = (x,y-1)
addressLeftOf  (x,y) = (x-1,y)
addressRightOf (x,y) = (x+1,y)

clusterWalls
    :: KeyCluster
    -> Model Vector3d
clusterWalls cluster = difference
    ( union
        [ upperWalls
        , lowerWalls
        , leftWalls
        , rightWalls
        , upperWebWalls
        , lowerWebWalls
        , leftWebWalls
        , rightWebWalls
        ]
    )
    (translate (-200,-200,-400) (box 400 400 400))
  where
        locs = addresses cluster
        locate = locationFunction cluster
        longPin = union
            [ translate (0,0,-100) (cylinder 1 100 (fn 20))
            , sphere 1 (fn 20)
            ]
        upperWalls = union
            [ minkowski
                [ longPin
                , locate (hull [KeyPlate.ulPin, KeyPlate.urPin]) address
                ]
            | address <- topmostKeys cluster
            ]
        lowerWalls = union
            [ minkowski
                [ longPin
                , locate (hull [KeyPlate.llPin, KeyPlate.lrPin]) address
                ]
            | address <- bottommostKeys cluster
            ]
        leftWalls = union
            [ minkowski
                [ longPin
                , locate (hull [KeyPlate.ulPin, KeyPlate.llPin]) address
                ]
            | address <- leftmostKeys cluster
            ]
        rightWalls = union
            [ minkowski
                [ longPin
                , locate (hull [KeyPlate.urPin, KeyPlate.lrPin]) address
                ]
            | address <- rightmostKeys cluster
            ]
        upperWebWalls = union  
            [ minkowski
                [ longPin
                , hull
                    [ locate KeyPlate.urPin address
                    , locate KeyPlate.ulPin (addressRightOf address)
                    ]
                ]
            | address <- Set.toList locs
            , and
                [ (addressRightOf address) `elem` locs
                , or
                    [ (addressAbove address) `notElem` locs
                    , ((addressAbove . addressRightOf) address) `notElem` locs
                    ]
                ]
            ]
        lowerWebWalls = union  
            [ minkowski
                [ longPin
                , hull
                    [ locate KeyPlate.lrPin address
                    , locate KeyPlate.llPin (addressRightOf address)
                    ]
                ]
            | address <- Set.toList locs
            , and
                [ (addressRightOf address) `elem` locs
                , or
                    [ (addressBelow address) `notElem` locs
                    , ((addressRightOf . addressBelow) address) `notElem` locs
                    ]
                ]
            ]
        leftWebWalls = union
            [ minkowski
                [ longPin
                , hull 
                    [ locate KeyPlate.ulPin address
                    , locate KeyPlate.llPin (addressAbove address)
                    ]
                ]
            | address <- Set.toList locs
            , and
                [ (addressAbove address) `elem` locs
                , or
                    [ (addressLeftOf address) `notElem` locs
                    , ((addressAbove . addressLeftOf) address) `notElem` locs
                    ]
                ]
            ]
        rightWebWalls = union
            [ minkowski 
                [ longPin
                , hull
                    [ locate KeyPlate.urPin address
                    , locate KeyPlate.lrPin (addressAbove address)
                    ]
                ]
            | address <- Set.toList locs
            , and
                [ ((addressAbove address) `elem` locs)
                , or
                    [ (addressRightOf address) `notElem` locs
                    , ((addressAbove . addressRightOf) address) `notElem` locs
                    ]
                ]
            ]

clusterPlate
    :: KeyCluster
    -> Model Vector3d
clusterPlate cluster = difference
    ( union
        [ keyPlates
        , horizontalWebs
        , verticalWebs
        , centerBits
        ]
    )
    (translate (-200,-200,-400) (box 400 400 400))
        where
            locs = addresses cluster
            locate = locationFunction cluster
            keyPlates = union $ fmap (locate KeyPlate.model) (Set.toList locs)
            horizontalWebs = union
                [ hull
                    [ locate KeyPlate.urShortPost address               
                    , locate KeyPlate.lrShortPost address               
                    , locate KeyPlate.ulShortPost (addressRightOf address)
                    , locate KeyPlate.llShortPost (addressRightOf address)
                    ] 
                | address <- Set.toList locs
                , addressRightOf address `elem` locs
                ]
            verticalWebs = union
                [ hull
                    [ locate KeyPlate.ulShortPost address             
                    , locate KeyPlate.urShortPost address             
                    , locate KeyPlate.llShortPost (addressAbove address)
                    , locate KeyPlate.lrShortPost (addressAbove address)
                    ]
                | address <- Set.toList locs
                , addressAbove address `elem` locs
                ]
            centerBits = union
                [ hull
                    [ locate KeyPlate.urShortPost address                                  
                    , locate KeyPlate.ulShortPost (addressRightOf address)                 
                    , locate KeyPlate.lrShortPost (addressAbove address)                   
                    , locate KeyPlate.llShortPost ((addressAbove . addressRightOf) address)
                    ]
                | address <- Set.toList locs
                , and
                    [ (addressAbove address) `elem` locs
                    , (addressRightOf address) `elem` locs
                    , ((addressAbove . addressRightOf) address) `elem` locs
                    ]
                ]

clusterKeys
    :: KeyCluster
    -> Model Vector3d
clusterKeys cluster = union $ fmap (locate KeySwitch.model) (Set.toList locs)
    where
        locs = addresses cluster
        locate = locationFunction cluster

clusterKeyBlockages
    :: KeyCluster
    -> Model Vector3d
clusterKeyBlockages cluster = union $ fmap (locate KeySwitch.blockage) (Set.toList locs)
    where
        locs = addresses cluster
        locate = locationFunction cluster

model = 
    union
        [ difference
            ( union
                [ clusterPlate mainCluster
                , clusterFlange mainCluster
                , clusterWalls mainCluster
                ]
            )
            (clusterKeyBlockages mainCluster)
        , clusterKeys mainCluster            
        ] 
