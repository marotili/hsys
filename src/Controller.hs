{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Controller
( World (..)
, Component (..)
, Position (..)
, Rotation (..)
, MovableEntity
, DefaultWorld
, DefaultWorldDelta (..)
, DefaultDeltaEvent (..)
, DeriveDefaultWorldPred (..)
, DeriveDefaultWorld (..)
, HTrue
, HFalse
, newSession
, stepSession
, runWire
, newDefaultWorld
, WorldMonad
, WorldWire
, spawnMovable
-- , spawnMovable'
, applyDelta'
, getEntityByName
, mkMovable
, defaultWorld
, move
, rotate
, EntityId
, getStuff
)
where

import           Control.DeepSeq
import           Control.DeepSeq.Generics    (genericRnf)
import           Control.Lens
import qualified Control.Lens                as L
import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Control.Wire                hiding (at, id, stepSession, (.))
import qualified Control.Wire                as W
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           GHC.Generics

instance Functor a => Monoid (Free a ()) where
    mempty = Pure ()
    mappend a b = let x = a >> b in x `seq` x

type WorldMonad w = RWS w (Free (Delta w) ()) ()
-- type WorldMonad w = RWS w (Delta w) (Identity ())

type Time = (W.Timed W.NominalDiffTime())
type WorldWire w a b = W.Wire Time () (WorldMonad w) a b


type EntityId = Int
     
type family InvDeltaEvent d

class World w where
    type Delta d :: * -> *
    type DeltaEvent de
    applyDelta :: Free (Delta w) n -> State w [DeltaEvent w]
    -- | Initialize a new world (the first parameter is a dummy parameter for the inference engine)
--    newWorld :: w -> w
    -- | Return the starting wire for the world (the first parameter is a dummy parameter for the inference engine)
--    startWire :: w -> WorldWire w () ()

newSession :: (Applicative m, MonadIO m) => Session m Time
newSession = W.clockSession_

stepSession :: (Applicative m, MonadIO m) => Session m Time -> m (Time, Session m Time)
stepSession = W.stepSession

runWire :: (World w, Functor (Delta w)) => Time -> w -> WorldWire w () ()
        -> ((Either () (), WorldWire w () ()), (), Free (Delta w) ())
runWire dt world wire = runRWS (stepWire wire dt (Right ())) world ()

-- run :: (World w) => w -> IO ()
-- run world = do
--    let session = newSession
--    let loop session world wire = do
--         (dt, session') <- stepSession session
--         let ((_, newWire), st, freeList)  = runWire dt world wire -- runRWS (stepWire wire dt (Right ())) world ()
--         let newWorld = execState (mapM_ applyDelta freeList) world

--         loop session' newWorld newWire

--    loop session (newWorld world) (startWire world)


class World w => Component w c where
    type EntityComp w c :: * -> *
    setComponent :: w -> (EntityComp w c s) -> c -> (Free (Delta w) ())
    getComponent :: (EntityComp w c s) -> w -> c
    getEntity :: w -> c -> EntityId -> Maybe (EntityComp w c s)
    readComponent :: (World w, MonadReader w m) => (EntityComp w c s) -> m c
    readComponent eId = do
      w <- ask
      return $ getComponent eId w

    readEntity :: (World w, MonadReader w m) => c -> w -> EntityId -> m (Maybe (EntityComp w c s))
    readEntity _ _ eId = do
      w <- ask
      return $ getEntity w (undefined::c) eId

instance Monoid Float where
         mempty = 0.0
         mappend a b = a + b
newtype Position = Position (Float, Float) deriving (Show, Monoid, Generic, Eq, Ord)
newtype Rotation = Rotation Float deriving (Show, Monoid, Generic, Eq, Ord)

instance NFData Position where rnf = genericRnf
instance NFData Rotation where rnf = genericRnf

data DefaultWorld = DefaultWorld
    { _wPositions    :: !(Map.Map EntityId Position)
    , _wRotations    :: !(Map.Map EntityId Rotation)
    , _wNextEntityId :: !EntityId
    , _wEntityNames  :: !(Map.Map EntityName EntityId)
    } deriving (Show, Generic)

instance NFData DefaultWorld where rnf = genericRnf

-- data WorldExtends = WorldExtends
--     { _wRotate :: Map.Map EntityId Rotation
--     , _wMain   :: W1
--     }

type EntityName = String
newtype MovableEntity s = MovableEntity EntityId deriving (Show)
        
data DefaultDeltaEvent =
  EventMove EntityId Position
  | EventRotate EntityId Rotation
  | EventSpawn EntityId EntityName
  deriving (Show, Eq, Ord)

data DefaultWorldDelta next =
    Move !EntityId !Position !next
    | Rotate !EntityId !Rotation !next
    | SetPosition !EntityId !Position !next
    | SetRotation !EntityId !Rotation !next
    | SpawnMovable !EntityName !Position !Rotation !(EntityId -> next)
    | GetEntity !EntityName !(Maybe EntityId -> next)
    | forall a. (Component DefaultWorld a) => GetComp !EntityId (a -> next)
    | forall s. MkMovable !EntityId !(Maybe (MovableEntity s) -> next)

instance Functor DefaultWorldDelta where
    fmap f (Move eId pos n) = Move eId pos (f n)
    fmap f (Rotate eId rot n) = Rotate eId rot (f n)
    fmap f (SetPosition eId pos n) = SetPosition eId pos (f n)
    fmap f (SetRotation eId rot n) = SetRotation eId rot (f n)
    fmap f (SpawnMovable name pos rot g) = SpawnMovable name pos rot (f . g)
    fmap f (GetEntity name g) = GetEntity name (f . g)
    fmap f (MkMovable eId g) = MkMovable eId (f . g)
    fmap f (GetComp eId g) = GetComp eId (f . g)

instance Show (DefaultWorldDelta a) where
    show _ = "DefaultWorldDelta {..}"

-- data WorldExtendsDelta next =
--     MainDelta (W1Delta next)
--     | Rotated EntityId Rotation next
--       deriving (Show, Functor)

makeLenses ''DefaultWorld
makeLenses ''DefaultWorldDelta

getStuff eId = liftF (GetComp eId id)

spawnMovable :: String -> (Float, Float) -> Float -> Free (Delta DefaultWorld) EntityId
spawnMovable name pos rot = liftF $ SpawnMovable name (Position pos) (Rotation rot) id

-- spawnMovable' :: (DeriveDefaultWorld w) => w -> String -> (Float, Float) -> Float -> Free (Delta w) EntityId
-- spawnMovable' w name pos rot = getDefaultDelta w $ liftF $ SpawnMovable name (Position pos) (Rotation rot) id

spawnEntity :: State DefaultWorld EntityId
spawnEntity = do
            eId <- use $ wNextEntityId
            wNextEntityId += 1
            return eId

defaultWorld :: (DeriveDefaultWorld w) => w -> (Free DefaultWorldDelta a) -> Free (Delta w) a
defaultWorld w f = getDefaultDelta w f

getEntityByName name = liftF $ GetEntity name id
mkMovable eId = liftF $ MkMovable eId id

newDefaultWorld = DefaultWorld { _wPositions = Map.empty
                               , _wRotations = Map.empty
                               , _wNextEntityId = 1
                               , _wEntityNames = Map.empty
                               }

liftS f = do
  world <- use getDefaultWorldL
  let (ret, newWorld) = runState f world
  getDefaultWorldL .= newWorld
  return ret

applyDelta' :: (DeriveDefaultWorld w)
            => w -> DefaultWorldDelta (Free (Delta w) a)
            -- -> (Free DefaultWorldDelta a -> Free (Delta w) a)
            -> State w [DeltaEvent w]
applyDelta' w ((Move eId pos n)) = do
           liftS $ wPositions.at eId._Just %= mappend pos
           fmap (getDefaultDeltaEvent w (EventMove eId pos) :) $ applyDelta (n)

applyDelta' _ ((SetPosition eId pos n)) = do
           liftS $ wPositions.at eId .= Just pos
           applyDelta (n)

applyDelta' w ((Rotate eId rot n)) = do
           liftS $ wRotations.at eId._Just %= mappend rot
           fmap (getDefaultDeltaEvent w (EventRotate eId rot) :) $ applyDelta (n)

applyDelta' _ ((SetRotation eId rot n)) = do
           liftS $ wRotations.at eId .= Just rot
           applyDelta (n)

applyDelta' w ((SpawnMovable name pos rot g)) = do
  eId <- liftS $ do
           eId <- spawnEntity
           wEntityNames.at name .= Just eId
           wPositions.at eId .= Just pos
           wRotations.at eId .= Just rot
           return eId
  fmap (getDefaultDeltaEvent w (EventSpawn eId name) :) $ applyDelta ((g eId))

applyDelta' _ (GetEntity name g) = do
  eId <- liftS $ use $ wEntityNames.at name
  applyDelta $ g eId

applyDelta' _ (MkMovable eId g) = do
  isMovable <- liftS $ do
           mPos <- use $ wPositions.at eId
           mRot <- use $ wRotations.at eId
           return $ isJust mPos && isJust mRot
  applyDelta $ g (if isMovable then Just $ MovableEntity eId else Nothing)

applyDelta' _ (GetComp eId g) = do
  w' <- get
  let w = getDefaultWorld w'
  let x = getComponent undefined w
  applyDelta (g x)

type instance InvDeltaEvent (DefaultDeltaEvent) = DefaultWorld
instance World DefaultWorld where
    type Delta DefaultWorld = DefaultWorldDelta
    type DeltaEvent DefaultWorld = DefaultDeltaEvent
    applyDelta (Free f) = applyDelta' (undefined::DefaultWorld) f
    applyDelta (Pure a) = return []

-- instance World WorldExtends where
--     type Delta WorldExtends = Free WorldExtendsDelta

getEntityByName' :: DefaultWorld -> EntityName -> Maybe EntityId
getEntityByName' world name = world^.wEntityNames.at name

getMovable :: DefaultWorld -> EntityName -> Maybe (EntityComp DefaultWorld Position s)
getMovable w name = getEntityByName' w name >>= getEntity w (undefined::Position)

instance Component' HFalse DefaultWorld Position where
    type EntityComp' HFalse DefaultWorld Position = MovableEntity
    setComponent' (undefined) w (MovableEntity eId) pos = Free (Move eId pos (Pure ()))
    getComponent' (undefined) (MovableEntity eId) w = fromJust $ w^.wPositions.at eId
    getEntity' (undefined) w _ eId = case w^.wPositions.at eId of Just _ -> Just $ MovableEntity eId; Nothing -> Nothing

instance Component' HFalse DefaultWorld Rotation where
    type EntityComp' HFalse DefaultWorld Rotation = MovableEntity
    setComponent' (undefined) w (MovableEntity eId) rot = Free (Rotate eId rot (Pure ()))
    getComponent' (undefined) (MovableEntity eId) w = fromJust $ w^.wRotations.at eId
    getEntity' (undefined) w _ eId = case w^.wRotations.at eId of Just _ -> Just $ MovableEntity eId; Nothing -> Nothing

class (World w, Functor (Delta w)) => DeriveDefaultWorld w where
      getDefaultWorld :: w -> DefaultWorld
      getDefaultWorldL :: Lens' w DefaultWorld
      getDefaultDelta :: w -> Free (Delta DefaultWorld) a -> Free (Delta w) a
      getDefaultDeltaEvent :: w -> DefaultDeltaEvent -> DeltaEvent w

instance DeriveDefaultWorld DefaultWorld where
    getDefaultWorld = id
    getDefaultWorldL = lens id const --(\w1 w2 -> w2) -- FIXME: is this the right one?
    getDefaultDelta _ = id
    getDefaultDeltaEvent _ = id

class Component' flag w c where
    type EntityComp' flag w c :: * -> *
    setComponent' :: flag -> w -> (EntityComp w c s) -> c -> (Free (Delta w) ())
    getComponent' :: flag -> (EntityComp w c s) -> w -> c
    getEntity' :: flag -> w -> c -> EntityId -> Maybe (EntityComp w c s)

instance (World w, DeriveDefaultWorldPred w flag, Component' flag w c) => Component w c where
         type EntityComp w c = EntityComp' HTrue w c
         setComponent = setComponent' (undefined :: flag)
         getComponent = getComponent' (undefined :: flag)
         getEntity = getEntity' (undefined :: flag)

class DeriveDefaultWorldPred w flag | w->flag where {}
instance DeriveDefaultWorldPred DefaultWorld HFalse
data HTrue
data HFalse

instance (DeriveDefaultWorld w) => Component' HTrue w Position where
         type EntityComp' HTrue w Position = MovableEntity
         setComponent' (undefined) world eId pos = getDefaultDelta world $ setComponent (getDefaultWorld world) eId pos
         getComponent' (undefined) eId w = getComponent eId (getDefaultWorld w)
         getEntity' (undefined::HTrue) w _ eId = getEntity (getDefaultWorld w) (Position (0,0) ::Position) eId

instance (DeriveDefaultWorld w) => Component' HTrue w Rotation where
         type EntityComp' HTrue w Rotation = MovableEntity
         setComponent' (undefined) world eId rot = getDefaultDelta world $ setComponent (getDefaultWorld world) eId rot
         getComponent' (undefined) eId w = getComponent eId (getDefaultWorld w)
         getEntity' (undefined::HTrue) w _ eId = getEntity (getDefaultWorld w) (Rotation 0 ::Rotation) eId

-- getPosition :: (Monad (Delta w), World w, Component w Position) => EntityComp w Position s -> WorldWire w a Position
-- getPosition eId = mkGen_ $ const $ ask >>= return . Right . getComponent eId

getPosition :: (World w, Component w Position) => EntityComp w Position s -> w -> Position
getPosition = getComponent

move :: (Functor (Delta w), Monad (Free (Delta w)), World w, Component w Position) => Position -> WorldWire w (EntityComp w Position s) (EntityComp w Position s)
move pos = mkGen $ \ds eId -> do
    w <- ask
    let dt = realToFrac (dtime ds)
    let Position (dx, dy) = pos
    tell $ setComponent w eId (Position (dx*dt, dy*dt))
    return $ eId `seq` (Right eId, move pos)

rotate :: (Functor (Delta w), Monad (Free (Delta w)), World w, Component w Rotation) => Rotation -> WorldWire w (EntityComp w Rotation s) (EntityComp w Rotation s)
rotate rot = mkGen $ \ds eId -> do w <- ask
                                   tell $ setComponent w eId rot
                                   return $ eId `seq` (Right eId, rotate rot)


-- test :: (HasTime a Time) => WorldWire WorldExtends (MovableEntity s) a
-- test = (W.time W.. W.for 1 W.. move (Position (50, 50) ) W.--> W.time W.. W.for 1 W.. rotate (Rotation 20))

-- test2 :: WorldWire WorldExtends (MovableEntity s) (MovableEntity s)
-- test2 = W.for 2 W.. move (Position (50, 50)) W.. rotate (Rotation 20)

-- test3 :: WorldWire WorldExtends (MovableEntity s) Position
-- test3 = mkGenN $ \eId -> do
--           w <- ask
--           return (Right (getPosition eId w), test3)

-- -- getWire :: Free WorldExtendsDelta  -> IO (WorldWire WorldExtends EntityId ())
-- getWire (Free (Rotated _ _ n)) = print "rotate" >> getWire n
-- getWire (Free (MainDelta (Moved _ _ n))) = print "moved" >> getWire n
-- getWire (Pure (w)) = return w


-- mainTest :: IO ()
-- mainTest = do
--   let session = W.clockSession_
--   let loop session w = do
--                         (dt, session') <- W.stepSession session
--                         let free = runReaderT (stepWire w dt (Right (MovableEntity 0))) (WorldExtends Map.empty (DefaultWorld $ Map.insert 0 (Position (40, 40)) Map.empty))
--                         (e, w') <- getWire free
--                         print e

--                         loop session' w'

--   loop session test3

