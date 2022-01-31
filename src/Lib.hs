{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Lib
  ( convert,
  )
where

import Codec.Picture (readImageWithMetadata)
import Codec.Picture.Metadata (Keys (Width))
import qualified Codec.Picture.Metadata as J
import Control.Applicative (optional, (<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Tiled as Tiled
import Data.Attoparsec.Text as Atto ((<?>))
import qualified Data.Attoparsec.Text as Atto
import Data.Bits ((.|.))
import Data.Either (fromRight, lefts, rights)
import Data.Foldable (find, traverse_)
import Data.Functor (($>))
import Data.Int (Int16, Int32)
import Data.List (sort, sortBy, sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (fromList)
import Data.Word (Word32)
import GHC.Generics (Generic)
import System.Directory (listDirectory)
import Text.Read (readMaybe)

fLIPPED_HORIZONTALLY_FLAG :: Word32
fLIPPED_HORIZONTALLY_FLAG = 0x80000000

fLIPPED_VERTICALLY_FLAG :: Word32
fLIPPED_VERTICALLY_FLAG = 0x40000000

fLIPPED_DIAGONALLY_FLAG :: Word32
fLIPPED_DIAGONALLY_FLAG = 0x20000000

rOTATED_HEXAGONAL_120_FLAG :: Word32
rOTATED_HEXAGONAL_120_FLAG = 0x10000000

data Common = Common
  { commonUniqueId :: Int,
    commonPosition :: Position,
    commonLayer :: Int
  }
  deriving (Eq, Show)

data Position = Position
  { positionX :: Int,
    positionY :: Int
  }
  deriving (Eq, Show)

data SpriteFrame = SpriteFrame
  { spriteFrameName :: Text,
    spriteFrameIndex :: Int,
    spriteFrameWidth :: Int,
    spriteFrameHeight :: Int,
    spriteFramePath :: Text
  }
  deriving stock (Eq, Ord, Show)

newtype Sprite a = Sprite {unSprite :: a}
  deriving newtype (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

type SpriteFrameIndex = Int

type InitialFrameIndex = Int

type SpriteName = Text

type UnloadedSprite = Sprite (SpriteName, InitialFrameIndex)

type LoadedSprite a = Sprite (SpriteFrame, [a])

data Normalized

-- obj,Region,7086,1008,1224,11,-999999,caseScript,2,1,char,0,++,0=216,1=192,p2=0,
-- TODO

-- obj,Scenery,1893,712,-48,10,-999999,++,0=spr_c_FloorPatchBig,1=1,2=0,3=0,k=0,p=-4,fp=0,4=0,5=0,f=1,l=0,
-- 3=animated
-- k=draw_priority?
-- p=draw_on_top_of_gid
-- fp=symmetry over left center
-- 5=foreground
-- f=floor
-- l=level
data Scenery a = Scenery
  { scenerySprite :: a,
    scenerySpriteAnimationSpeed :: Float,
    scenerySpriteAnimated :: Bool,
    scenerySpriteDrawPriority :: Bool,
    scenerySpriteDrawParentId :: Maybe Int,
    scenerySpriteFlip :: Bool,
    scenerySpriteForeground :: Bool,
    scenerySpriteFloor :: Bool,
    scenerySpriteLevel :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Functor, Foldable, Traversable)

data Destructable = Destructable
  { destructableOriginSprite :: Maybe UnloadedSprite,
    destructableRespawnTime :: Int,
    destructablePopSprite :: UnloadedSprite
  }
  deriving stock (Eq, Show)

-- obj,Ambience,1801,1252,784,13,-999999,++,g=0,r=0,ra=0,rs=150,s=MUS_AMB_MONO_MACHINE_2,v=8,min=25,max=100,
data Ambience = Ambience
  { ambienceGlobal :: Bool,
    ambienceR :: Bool,
    ambienceRA :: Bool,
    ambienceRS :: Int,
    ambienceMusic :: Text,
    ambienceVolume :: Int,
    ambienceMin :: Int,
    ambienceMax :: Int
  }
  deriving (Eq, Show)

-- obj,HealthPlant,3937,1136,552,3,-999999,++,0=spr_HealthPlantCut,1=0,2=0,3=spr_CrateDebri,5=1,
data HealthPlant = HealthPlant
  { healthPlantSprite :: UnloadedSprite,
    healthPlantRespawnTime :: Int,
    healthPlantPopSprite :: UnloadedSprite
  }
  deriving stock (Eq, Show)

-- obj,Spawner,981,1040,48,4,-999999,++,-1=GearbitCrate,-2=-999999,-4=1,-5=0,-6=-1,-7=0,-8=0,
data Spawner a = Spawner
  { spawnerDelay :: Int,
    spawnerObject :: HLDObject a
  }
  deriving stock (Eq, Show, Generic)
  deriving (Functor, Foldable, Traversable)

-- obj,NPCGeneric,1730,1072,120,2,-999999,++,wlb=1,wl=-999999,32=spr_none,300=spr_NPC_northgrannybird_idleLook,301=spr_NPC_northgrannybird_idleStroke,302=spr_NPC_northgrannybird_idleLook,310=spr_NPCBirdman_Idle,xs=1,bi=0,tr=0,tg=0,

data HLDObject a
  = HLDScenery (Scenery a)
  | HLDRegion ()
  | HLDDestructable Destructable
  | HLDAmbience Ambience
  | HLDHealthPlant HealthPlant
  | HLDSpawner (Spawner a)
  | HLDUnsupported Text
  deriving stock (Eq, Show, Generic)
  deriving (Functor, Foldable, Traversable)

data HLDObjectRoot a b
  = HLDObjectRoot
      Common
      (HLDObject a)
  deriving stock (Eq, Show, Generic)
  deriving (Functor, Foldable, Traversable)

parseBool :: Atto.Parser Bool
parseBool = do
  c <- Atto.anyChar
  pure $ case c of
    '0' -> False
    '1' -> True
    _ -> error "impossible"

parseCommon :: Atto.Parser Common
parseCommon = do
  uniqueId <- Atto.decimal
  _ <- Atto.char ','
  x <- Atto.signed Atto.decimal
  _ <- Atto.char ','
  y <- Atto.signed Atto.decimal
  _ <- Atto.char ','
  Common uniqueId (Position x y) <$> Atto.decimal

parsePopSprite :: Atto.Parser UnloadedSprite
parsePopSprite = do
  _ <- Atto.string "3=" <?> "popSprite"
  sprite <- Atto.takeTill (== ',')
  _ <- Atto.char ',' <?> "popSprite.2"
  _ <- Atto.string "5=" <?> "popSpriteFrame"
  Sprite . (sprite,) <$> Atto.decimal

parseSprite :: Atto.Parser UnloadedSprite
parseSprite = do
  _ <- Atto.string "0=" <?> "sprite"
  sprite <- Atto.takeTill (== ',') <?> "sprite.1"
  _ <- Atto.char ',' <?> "sprite.2"
  _ <- Atto.string "1=" <?> "spriteFrame"
  Sprite . (sprite,) . round . toRational <$> Atto.scientific

parseMaybeSprite :: Atto.Parser (Maybe UnloadedSprite)
parseMaybeSprite = do
  _ <- Atto.string "0=" <?> "maybeSprite"
  sprite <- Atto.takeTill (== ',')
  _ <- Atto.char ',' <?> "maybeSprite.1"
  _ <- Atto.string "1=" <?> "maybeSpriteFrame.1"
  frame <- Atto.decimal <?> "maybeSpriteFrame.2"
  case sprite of
    "<undefined>" -> pure Nothing
    _ -> pure $ Just $ Sprite (sprite, frame)

parseObject :: Text -> Atto.Parser (HLDObject UnloadedSprite)
parseObject "region" = pure $ HLDRegion ()
-- -- obj,destructable,1899,656,832,9,-999999,++,0=<undefined>,1=0,2=3,3=spr_GrassDebris,5=1,
parseObject "destructable" = do
  sprite <- parseMaybeSprite
  _ <- Atto.char ','
  _ <- Atto.string "2=" <?> "destructableRespawn"
  respawn <- Atto.decimal
  _ <- Atto.char ','
  popSprite <- parsePopSprite <?> "destructable.popSprite"
  pure $ HLDDestructable $ Destructable sprite respawn popSprite

--obj,Spawner,4544,840,504,3,-999999,++,-1=destructable,-2=-999999,-4=1,-5=0,-6=-1,-7=0,-8=0,0=spr_C_Cut_Shrub,1=0,2=2,3=spr_ShrubDebris,5=1,
parseObject "spawner" = do
  _ <- Atto.string "-1="
  spawnObjectType <- T.toLower <$> Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.string "-4="
  respawn <- Atto.decimal
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  object <- parseObject spawnObjectType
  pure $ HLDSpawner $ Spawner respawn object

-- obj,HealthPlant,3937,1136,552,3,-999999,++,0=spr_HealthPlantCut,1=0,2=0,3=spr_CrateDebri,5=1,
parseObject "healthplant" = do
  sprite <- parseSprite <?> "healthPlant.parseSprite"
  _ <- Atto.char ',' <?> "1"
  _ <- Atto.string "2=" <?> "healthPlantRespawn.1"
  respawn <- Atto.decimal <?> "healPlantRespawn.2"
  _ <- Atto.char ',' <?> "1"
  popSprite <- parsePopSprite <?> "healthPlant.popSprite"
  pure $ HLDHealthPlant $ HealthPlant sprite respawn popSprite

-- obj,Ambience,1801,1252,784,13,-999999,++,g=0,r=0,ra=0,rs=150,s=MUS_AMB_MONO_MACHINE_2,v=8,min=25,max=100,
parseObject "ambience" = do
  _ <- Atto.string "g="
  global <- parseBool
  _ <- Atto.char ','
  _ <- Atto.string "r="
  r <- parseBool
  _ <- Atto.char ','
  _ <- Atto.string "ra="
  ra <- parseBool
  _ <- Atto.char ','
  _ <- Atto.string "rs="
  rs <- Atto.decimal
  _ <- Atto.char ','
  _ <- Atto.string "s="
  music <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.string "v="
  v <- Atto.decimal
  _ <- Atto.char ','
  _ <- Atto.string "min="
  mini <- Atto.decimal
  _ <- Atto.char ','
  _ <- Atto.string "max="
  HLDAmbience . Ambience global r ra rs music v mini <$> Atto.decimal

-- obj,RecessingScenery,2863,928,152,0,1,3231,caseScript,3,1,-999999,0,++,0=spr_WLabBlock16,1=0,2=0,3=0,k=0,p=-4,fp=0,4=0,5=0,f=0,l=0,
-- parseObject "recessingscenery" = do

{-
-- obj,Scenery,1893,712,-48,10,-999999,++,0=spr_c_FloorPatchBig,1=1,2=0,3=0,k=0,p=-4,fp=0,4=0,5=0,f=1,l=0,
0=sprite name
1=start frame
2=animation speed
3=animated
k=draw_priority?
p=draw_on_top_of_gid
fp=symmetry over left center
4=???????
5=foreground
f=floor
l=level
-}
parseObject "scenery" = do
  sprite <- parseSprite
  Atto.char ','
  _ <- Atto.string "2="
  animationSpeed <- fromRational . toRational <$> Atto.scientific <?> "animationSpeed"
  Atto.char ','
  _ <- Atto.string "3="
  animated <- parseBool <?> "animated"
  Atto.char ','
  _ <- Atto.string "k="
  drawPriority <- parseBool <?> "drawPriority"
  _ <- Atto.char ','
  _ <- Atto.string "p="
  drawPriorityParentId <-
    Just <$> Atto.decimal <|> Atto.string "-4" $> Nothing
  _ <- Atto.char ','
  _ <- Atto.string "fp="
  flip <- parseBool <?> "flip"
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.string "5="
  foreground <- parseBool <?> "foreground"
  _ <- Atto.char ','
  _ <- Atto.string "f="
  floor <- parseBool <?> "floor"
  _ <- Atto.char ','
  _ <- Atto.string "l="
  level <- parseBool <?> "level"
  pure $
    HLDScenery $
      Scenery
        sprite
        animationSpeed
        animated
        drawPriority
        drawPriorityParentId
        flip
        foreground
        floor
        level
parseObject x = pure $ HLDUnsupported x

parseRoot :: Atto.Parser (HLDObjectRoot UnloadedSprite ())
parseRoot = do
  _ <- Atto.skipSpace
  _ <- Atto.string "obj"
  _ <- Atto.char ','
  objectType <- T.toLower <$> Atto.takeTill (== ',')
  _ <- Atto.char ','
  common <- parseCommon
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  _ <- Atto.takeTill (== ',')
  _ <- Atto.char ','
  object <- parseObject objectType
  pure $ HLDObjectRoot common object

{-
==================== Scenery ================
foreground || floor => y = y + sprite_height
k => y = y + (sprite_height / 2)
flip =>  x -= sprite_width && horizontal inversion
=============================================
-}
normalize ::
  HLDObjectRoot (LoadedSprite SpriteFrame) () ->
  HLDObjectRoot (LoadedSprite SpriteFrame) Normalized
normalize (HLDObjectRoot c (HLDScenery scenery)) =
  let Sprite (SpriteFrame _ _ width height _, _) = scenerySprite scenery
      halfWidth = div width 2
      halfHeight = div height 2
      foreground = scenerySpriteForeground scenery
      floor = scenerySpriteFloor scenery
      prio = scenerySpriteDrawPriority scenery
      flip = scenerySpriteFlip scenery
      applyFlip p =
        if flip
          then p {positionX = positionX p - width}
          else p
      applyForegroundFloor p =
        if floor
          then p {positionY = positionY p + height}
          else p
      applyPriority p =
        if prio
          then p {positionY = positionY p + 20}
          else p
   in HLDObjectRoot
        c
          { commonPosition = applyForegroundFloor . applyPriority . applyFlip $ commonPosition c
          }
        (HLDScenery scenery)
normalize (HLDObjectRoot c o) = HLDObjectRoot c o

loadSprite ::
  HLDObjectRoot UnloadedSprite a ->
  IO (HLDObjectRoot (LoadedSprite SpriteFrame) a)
loadSprite (HLDObjectRoot c o) = HLDObjectRoot c <$> traverse go o
  where
    frameOf :: Text -> Int
    frameOf x =
      let frame = head $ T.split (== '.') (T.split (== '_') x !! 1)
       in fromMaybe (error "impossible") $ readMaybe $ T.unpack frame

    loadFrame :: Text -> Int -> Text -> IO SpriteFrame
    loadFrame sprite index path = do
      (_, meta) <- fromRight (error "impossible") <$> readImageWithMetadata (T.unpack path)
      let width = fromMaybe (error "impossible") $ J.lookup J.Width meta
          height = fromMaybe (error "impossible") $ J.lookup J.Height meta
      pure $ SpriteFrame sprite index (fromIntegral width) (fromIntegral height) path

    go :: UnloadedSprite -> IO (LoadedSprite SpriteFrame)
    go (Sprite (sprite, frame)) = do
      let spritePath = "..\\Maps\\assets\\" <> sprite
      sprites <-
        sortOn frameOf . fmap T.pack
          <$> listDirectory (T.unpack spritePath)
      sprites' <-
        traverse (uncurry (loadFrame sprite)) $
          zip [0 ..] $
            ((spritePath <> "\\") <>) <$> sprites
      pure $ Sprite (sprites' !! frame, sprites')

toTiles ::
  [HLDObjectRoot (LoadedSprite SpriteFrame) a] ->
  M.Map (Either SpriteName SpriteFrame) Tiled.Tile
toTiles = snd . foldr (\x (i, m) -> let (i', m') = go i x in (i', M.union m m')) (0, mempty)
  where
    go :: Int -> HLDObjectRoot (LoadedSprite SpriteFrame) a -> (Int, M.Map (Either SpriteName SpriteFrame) Tiled.Tile)
    go i (HLDObjectRoot _ (HLDScenery s)) =
      let sprites = zip [i ..] $ snd $ unSprite $ scenerySprite s
          anim = scenerySpriteAnimated s
          (_, SpriteFrame sprite _ _ _ firstFramePath) = head sprites
          tile oid (SpriteFrame sprite _ _ _ path) =
            Tiled.Tile
              (Tiled.LocalId oid)
              mempty
              (Just $ Aeson.String path)
              mempty
              mempty
          frames =
            foldr
              (\(i, f) m -> M.insert (Right f) (tile i f) m)
              mempty
              sprites
          animated =
            Tiled.Tile
              (Tiled.LocalId $ length sprites + i)
              mempty
              (Just $ Aeson.String firstFramePath)
              mempty
              $ Just $ Tiled.Frame 100 . Tiled.tileId <$> fromList (M.elems frames)
       in (length sprites + i, frames <> if anim then M.singleton (Left sprite) animated else mempty)
    go i _ = (i, mempty)

toObject ::
  M.Map (Either SpriteName SpriteFrame) Tiled.Tile ->
  HLDObjectRoot (LoadedSprite SpriteFrame) Normalized ->
  Maybe (Int, Tiled.Object)
toObject sprites (HLDObjectRoot (Common _ (Position x y) layer) (HLDScenery scenery)) =
  let Sprite (frame@(SpriteFrame sprite _ _ _ _), xs) = scenerySprite scenery
      anim = scenerySpriteAnimated scenery
      flip = scenerySpriteFlip scenery
      applyFlip x =
        if flip
          then fLIPPED_HORIZONTALLY_FLAG .|. x
          else x
      o =
        Tiled.Object
          0
          0
          0
          ""
          ""
          mempty
          True
          (fromIntegral x)
          (fromIntegral y)
          0.0
          ( Just $
              Tiled.GlobalId $
                fromIntegral $
                  applyFlip $
                    fromInteger $
                      toInteger $
                        (+ 1) $
                          Tiled.unLocalId $
                            Tiled.tileId $
                              fromMaybe (error "impossible") $
                                if anim
                                  then M.lookup (Left sprite) sprites
                                  else M.lookup (Right frame) sprites
          )
          False
          mempty
          mempty
          mempty
   in Just (if layer > 9 then 2 else 1, o)
toObject _ _ = Nothing

toTiledLayer :: [HLDObjectRoot (LoadedSprite SpriteFrame) Normalized] -> ([Tiled.Layer], Tiled.Tileset)
toTiledLayer objects =
  let tiledTiles = toTiles objects
      tiledObjects = catMaybes $ toObject tiledTiles <$> objects
      layersObjects = foldr (\(layer, x) -> M.insertWith (<>) layer (pure x)) mempty tiledObjects
      layer objs =
        Tiled.Layer
          48
          48
          "Scenery"
          "objectgroup"
          True
          0
          0
          Nothing
          (Just $ fromList objs)
          mempty
          1.0
          "topdown"
      tileset =
        Tiled.Tileset
          (Tiled.GlobalId 1)
          ""
          "Map"
          1
          1
          0
          0
          mempty
          mempty
          0
          0
          mempty
          mempty
          0
          (length objects)
          (M.foldrWithKey (\_ v m -> M.insert (Tiled.tileId v) v m) mempty tiledTiles)
   in (layer <$> fmap snd (sortBy (\(l, _) (m, _) -> compare m l) (M.assocs layersObjects)), tileset)

toTiledMap :: [([Tiled.Layer], Tiled.Tileset)] -> Tiled.Tiledmap
toTiledMap t =
  Tiled.Tiledmap 1 "1.7.2" 48 48 0 0 "orthogonal" (fromList $ fst =<< t) (fromList $ snd <$> t) Nothing "right-down" mempty 10000

convert :: IO ()
convert = do
  x <- fmap (Atto.parseOnly parseRoot) . T.lines <$> T.readFile "rm_C_Central.lvl"
  normalized <- traverse (fmap normalize . loadSprite) (rights x)
  Aeson.encodeFile "rm_C_Central.json" $ toTiledMap $ pure $ toTiledLayer normalized
