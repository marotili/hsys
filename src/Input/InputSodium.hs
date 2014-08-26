{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Input.InputSodium
( initInput
, KeyEvent(..)
, keyDown
, keyUp
, keyIsUp
, keyIsDown
, keyEvent
, runWhenEvent
, keyDownFor
, Time(..)
, syncInput
, InputFRP
, Input
, while, runWhenEvent', replace, both
)

where
        
import           Debug.Trace
import           FRP.Sodium
                 
import           Control.Lens hiding (both)
       
import qualified Graphics.UI.GLFW as GLFW
import           Control.Concurrent.STM (newTChan, readTChan, writeTChan, tryReadTChan, atomically, TChan)
import           Control.Concurrent.Async
import           Control.Applicative
import           Control.Monad.Reader
       
import           Data.Monoid
import           Control.Monad
import           Data.List
                 
import qualified Data.Set as Set
                 
       
data KeyEvent = KeyEvent { eKey :: GLFW.Key
                         , eKeyState :: GLFW.KeyState
                         } deriving (Eq, Ord, Show)

data MouseButtonEvent = MouseButtonEvent { eMouseButton :: GLFW.MouseButton
                                         , eMouseButtonState :: GLFW.MouseButtonState
                                         } deriving (Eq, Ord, Show)

data MousePosEvent = MousePosEvent { eMousePos :: (Double, Double)
                                   } deriving (Eq, Ord, Show)
     
class (Ord e) => InputEvent e where
instance InputEvent KeyEvent where
instance InputEvent MouseButtonEvent where
instance InputEvent MousePosEvent where

flattenEvents :: (InputEvent e) => [e] -> Set.Set e
flattenEvents events = Set.fromList events
       
keyboardCallback :: TChan KeyEvent -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
-- windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
mouseButtonCallback     :: TChan MouseButtonEvent -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TChan MousePosEvent -> GLFW.Window -> Double -> Double                                                 -> IO ()

keyboardCallback chan win key sc keyState modifierKeys = atomically . writeTChan chan $ KeyEvent key keyState
mouseButtonCallback chan win mb mbs modifierKeys = atomically . writeTChan chan $ MouseButtonEvent mb mbs
cursorPosCallback chan win x y = atomically . writeTChan chan $ MousePosEvent (x, y)

registerCallbacks :: GLFW.Window -> IO (TChan KeyEvent, TChan MouseButtonEvent, TChan MousePosEvent)
registerCallbacks win = do
  (keyChan, mouseButtonChan, cursorPosChan) <- atomically $ liftA3 ((,,)) newTChan newTChan newTChan
  GLFW.setKeyCallback win $ Just (keyboardCallback keyChan)
  GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback mouseButtonChan)
  GLFW.setCursorPosCallback win $ Just (cursorPosCallback cursorPosChan)
  return (keyChan, mouseButtonChan, cursorPosChan)
  
handleEvents :: (InputEvent e) => TChan e -> IO (Set.Set e)
handleEvents chan = do
  events <- fmap flattenEvents $ handleEvent chan
  return events
  where handleEvent :: (InputEvent e) => TChan e -> IO [e]
        handleEvent chan = do
          mEvent <- atomically . tryReadTChan $ chan
          case mEvent of
            Just event -> fmap (event :) $ handleEvent chan
            Nothing -> return []
  
newtype Time = Time Double deriving (Num, Eq, Ord, Show)
instance Monoid Time where
  mempty = Time 0
  mappend (Time a) (Time b) = Time $ a + b

accum2 :: (Monoid a) => a -> Event (a -> a) -> Reactive (Behavior a)
accum2 z efa = do
  rec
    s <- hold z $ snapshot ($) (coalesce mappend efa) s
  return s
  
data Input = Input
  { _eKeys :: Event KeyEvent
  }
  
makeLenses ''Input
           
type InputFRP a = ReaderT Input Reactive a
           
syncInput :: Input -> InputFRP a -> IO a
syncInput input i = do
  sync $ runReaderT i input

mkLocalTime :: Event Time -> Reactive (Behavior Time)
mkLocalTime eT = accum2 (Time 0) $ fmap (+) eT
            
keyEvent :: GLFW.Key -> Event KeyEvent -> Event KeyEvent
keyEvent key = filterE (\(KeyEvent k _) -> k == key)
            
keyDown :: GLFW.Key -> InputFRP (Event KeyEvent)
keyDown key = view eKeys >>= return . keyDown' key
keyDown' :: GLFW.Key -> Event KeyEvent -> Event KeyEvent
keyDown' key = filterE (\(KeyEvent k keyState) -> k == key && keyState == GLFW.KeyState'Pressed)
        
keyIsDown :: GLFW.Key -> ReaderT Input Reactive (Behavior Bool)
keyIsDown key = do
    eK <- view eKeys 
    let ek' = keyEvent key eK
    let eKeyFinal = filterE ((/= GLFW.KeyState'Repeating) . eKeyState) ek'
    lift . hold False . fmap (_keyStateDown . eKeyState) $ eKeyFinal
    where 
        _keyStateDown GLFW.KeyState'Pressed = True
        _keyStateDown GLFW.KeyState'Repeating = True
        _keyStateDown GLFW.KeyState'Released = False
    
keyIsUp = fmap (fmap not) . keyIsDown
       
both :: Behavior Bool -> Behavior Bool -> Behavior Bool
both a b = (&&) <$> a <*> b
     
replace :: Functor f => a -> f b -> f a
replace a f = fmap (const a) f
        
keyUp key = view eKeys >>= return . keyDown' key
keyUp' :: GLFW.Key -> Event KeyEvent -> Event KeyEvent
keyUp' key = filterE (\(KeyEvent k keyState) -> k == key && keyState == GLFW.KeyState'Released)
        
--keyDownFor :: Time -> Event Time -> Event KeyEvent -> Event ()
-- keyDownFor :: Time -> Event Time -> Event KeyEvent -> Event (Behavior Time)
   
-- | run reactive when event a occurs
runWhenEvent :: Reactive b -> Event a -> Event b
runWhenEvent r e = runWhenEvent' (const r) e
             
runWhenEvent' :: (a -> Reactive b) -> Event a -> Event b
runWhenEvent' r e = execute $ fmap r e
         
while :: Behavior Bool -> a -> Reactive (Behavior a) -> Reactive (Behavior a)
while behTrigger z beh = do
  let b = pure z
  sB <- hold b $ runWhenEvent' (\b -> if b then beh else return $ pure z) $ updates behTrigger
  switch sB
             
-- hold :: Event a -> Event (Event a) -> Behavior (Event a)
-- runWhenEvent :: Reactive (Event b) -> Event a -> Event (Event a)
-- runWhenEvent :: Reactive (Behavior b) -> Event a -> Event (Behavior a)

-- | create an event that launches after given key is down for time            
keyDownFor :: Time -> Event Time -> GLFW.Key -> InputFRP (Event Time)
keyDownFor t eT key = do
  let e = never
  eKeyDown <- keyDown key
  eKeyUp <- keyUp key
  e' <- lift $ hold e $ runWhenEvent (keyDownAndTimePassedEvent eKeyUp) eKeyDown
  -- e' <- hold e (execute $ fmap (const $ conditions eKeyUp) eKeyDown)
  let e'' = switchE e'
  return e''
  
  where
    keyDownAndTimePassedEvent :: Event a -> Reactive (Event Time)
    keyDownAndTimePassedEvent eKeyUp = do
      e <- runFor t eT
      keyDown <- keyStillDown eKeyUp
      return $ gate e keyDown
    keyStillDown eKeyUp = hold True $ fmap (const False) eKeyUp
    
-- execute $ fmap (const $ mkLocalTime eT) eKey

-- execute $ fmap (const $ runFor t eT) eKey
           
-- | create an event that launches once after time
runFor :: Time -> Event Time -> Reactive (Event Time)
runFor time eT = fmap (itIsTime' . updates) $ (mkLocalTime eT)
  where itIsTime' eTime = once $ (filterE (> time) eTime)
  
initInput :: GLFW.Window -> IO (Input, IO ())
initInput win = do     
  (keyEvents, pushKeyEvent) <- sync newEvent
  (mouseButtonEvents, pushMouseButtonEvent) <- sync newEvent
  (mousePosEvents, pushMousePosEvent) <- sync newEvent

  (keyEventChan, mouseButtonEventChan, mousePosEventChan) <- registerCallbacks win

  --waitQuit <- async $ do
  --  loop win (keyEventChan, mouseButtonEventChan, mousePosEventChan) (pushKeyEvent, pushMouseButtonEvent, pushMousePosEvent)

  return (Input keyEvents, step win (keyEventChan, mouseButtonEventChan, mousePosEventChan) (pushKeyEvent, pushMouseButtonEvent, pushMousePosEvent)) -- (keyEvents, mouseButtonEvents, mousePosEvents))
  
  where step win c@(keyChan, mbChan, mpChan) e@(pushKeyEvent, pushMouseButtonEvent, pushMousePosEvent) = do
            close <- GLFW.windowShouldClose win

            unless close $ do

              GLFW.pollEvents

              events <- handleEvents keyChan
              let escapeEvents = Set.filter (\keyEvent -> eKey keyEvent == GLFW.Key'Escape) events

              when (not . Set.null $ escapeEvents) $ do
                GLFW.setWindowShouldClose win True 
  
              mbEvents <- handleEvents mbChan
              mpEvents <- handleEvents mpChan
              sync $ do
                mapM pushKeyEvent $ Set.toList events
                mapM pushMouseButtonEvent $ Set.toList mbEvents
                mapM pushMousePosEvent $ Set.toList mpEvents
              return ()
  
-- run :: IO ()
-- run = do
--   GLFW.setErrorCallback $ Just simpleErrorCallback
--   m <- GLFW.init
--   Just win <- GLFW.createWindow 100 100 "" Nothing Nothing
  
--   (q, input) <- initInput win
--   syncInput input $ do
--     behK <- keyIsDown GLFW.Key'A
--     lift $ listen (updates behK) print
--   wait q
    
--   GLFW.destroyWindow win
--   GLFW.terminate

--   where simpleErrorCallback e s = print $ unwords [show e, show s]
  
