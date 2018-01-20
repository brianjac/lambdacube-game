module CreaturesLib where
import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL
import Codec.Picture as Juicy
import qualified Data.Map as Map
import qualified Data.Vector as V

-- CreatureData stores the information necessary to create a creature in an IO context.
data CreatureData = CreatureData { cd_name :: String
                                 , cd_idle_animation :: [String]
                                 , cd_run_animation :: [String]
                                 , cd_mesh :: LambdaCubeGL.Mesh}
data Creature = Creature { name :: String
                         , x :: Float
                         , y :: Float
                         , idle_animation :: [IO TextureData]
                         , run_animation :: [IO TextureData]
                         , mesh :: IO Object}

-- Creature Functions

-- Need to update this to use lenses, probably
updatePosition :: Creature -> Float -> Float -> Creature
updatePosition c dx dy = c { x = (x c) + dx } {y = (y c) + dy}

get_animation_frame c framecnt =
  let anim = idle_animation c in
  head $ drop ((framecnt `quot` 5) `mod` (length anim)) (idle_animation c)


{- Sonic -}
cr_sonic :: CreatureData
cr_sonic =
  let xscale = 0.5 in
  let yscale = 0.5 in
    CreatureData
    "Sonic"
    [ "sprites/idle_0.png","sprites/idle_1.png","sprites/idle_2.png",
      "sprites/idle_3.png","sprites/idle_4.png","sprites/idle_5.png",
      "sprites/idle_6.png"]
    [ "sprites/run_0.png","sprites/run_1.png","sprites/run_2.png",
      "sprites/run_3.png","sprites/run_4.png","sprites/run_5.png"]
    Mesh { mAttributes   = Map.fromList
                           [ ("position",  A_V2F $ V.fromList [V2 (xscale *   1)  (yscale *   1),
                                                               V2 (xscale *   1)  (yscale * (-1)),
                                                               V2 (xscale * (-1)) (yscale * (-1)),
                                                               V2 (xscale *   1)  (yscale *   1),
                                                               V2 (xscale * (-1)) (yscale * (-1)),
                                                               V2 (xscale * (-1)) (yscale *   1)])
                           , ("uv",        A_V2F $ V.fromList [V2 1 0, V2 1 1, V2 0 1, V2 0 0, V2 (-1) 1, V2 (-1) 0])
                           ]
         , mPrimitive    = P_Triangles
         }
