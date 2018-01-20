{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Data.Map as Map
import qualified Data.Vector as V

import LambdaCube.GL as LambdaCubeGL -- renderer
import LambdaCube.GL.Mesh as LambdaCubeGL

import Codec.Picture as Juicy

import Data.Aeson
import qualified Data.ByteString as SB

-- Local imports
import CreaturesLib
import Constants

main :: IO ()
main = do


    win <- initWindow "LambdaCube 3D DSL Hello World" 640 640

    let inputSchema = makeSchema $ do
          defObjectArray "objects" Triangles $ do
            "position"       @: Attribute_V2F
            "uv"             @: Attribute_V2F
          defUniforms $ do
            "position"       @: V2F
            "time"           @: Float
            "diffuseTexture" @: FTexture2D

    storage <- LambdaCubeGL.allocStorage inputSchema


    -- Initialization for creatures, etc.
    cr_sonic_p <- return $ uploadCreature storage cr_sonic


    Just pipelineDesc <- decodeStrict <$> SB.readFile "hello.json"
    renderer <- LambdaCubeGL.allocRenderer pipelineDesc


    LambdaCubeGL.setStorage renderer storage >>= \case
      Just err -> putStrLn err
      Nothing  -> do
        Just init_time <- GLFW.getTime
        loop 0 init_time
        where loop framecnt init_time = do

                (w, h) <- GLFW.getWindowSize win 
                LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)

                
                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k
                escape <- keyIsPressed Key'Escape

                {-LambdaCubeGL.updateUniforms storage $ do
                  "diffuseTexture" @= get_animation_frame cr_sonic_p framecnt-}

                (mesh cr_sonic_p) >>= \ m -> LambdaCubeGL.updateObjectUniforms m $ do
                  "diffuseTexture" @= get_animation_frame cr_sonic_p framecnt

                LambdaCubeGL.renderFrame renderer
                GLFW.swapBuffers win
                GLFW.pollEvents
                
                Just curr_time <- GLFW.getTime
                if escape then return () else loop (truncate ((curr_time - init_time) * const_framerate)) init_time

    LambdaCubeGL.disposeRenderer renderer
    LambdaCubeGL.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate


uploadCreature :: GLStorage -> CreatureData -> Creature
uploadCreature storage c =
  Creature (cd_name c) 0 0
  (map (\ s -> do Right img <- Juicy.readImage s ; LambdaCubeGL.uploadTexture2DToGPU img) (cd_idle_animation c))
  (map (\ s -> do Right img <- Juicy.readImage s ; LambdaCubeGL.uploadTexture2DToGPU img) (cd_run_animation c))
  (LambdaCubeGL.uploadMeshToGPU (cd_mesh c) >>= LambdaCubeGL.addMeshToObjectArray storage "objects" ["diffuseTexture"])

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win 
