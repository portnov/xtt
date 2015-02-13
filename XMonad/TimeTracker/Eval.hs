
module XMonad.TimeTracker.Eval where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.Map as M
import Data.Time
import Text.Regex.Posix

import XMonad.TimeTracker.Types
import XMonad.TimeTracker.Syntax

eval :: Expr -> TEvent -> Value
eval (Lit val) _ = val
eval (StringProperty prop) ev = String $ getStringProperty prop ev
eval (Equals e1 e2) ev = Bool $ eval e1 ev == eval e2 ev
eval (Match e regex) ev = Bool $ toString (eval e ev) =~ regex
eval (MatchAny e regexs) ev = Bool $ or [toString (eval e ev) =~ regex | regex <- regexs]
eval (In e es) ev = Bool $ eval e ev `elem` [eval ex ev | ex <- es]
eval (Or e1 e2) ev = Bool $ (toBool $ eval e1 ev) || (toBool $ eval e2 ev)
eval (And e1 e2) ev = Bool $ (toBool $ eval e1 ev) && (toBool $ eval e2 ev)

data TaskInfo =
  TaskInfo {
      tiDuration :: NominalDiffTime
    , tiFields :: M.Map String [Value]
  }
  deriving (Eq, Show)

data ParserState = PS {
      psCurrentTask :: String
    , psStartTime :: Maybe UTCTime
    , psPrevEvent :: Maybe TEvent
    , psResult :: M.Map Value TaskInfo
    } deriving (Eq, Show)

emptyPS :: ParserState
emptyPS = PS {
  psCurrentTask = "Startup",
  psStartTime = Nothing,
  psPrevEvent = Nothing,
  psResult = M.empty }

addTime :: NominalDiffTime -> TaskInfo -> TaskInfo
addTime dt ti = ti {tiDuration = tiDuration ti + dt}

addFields :: [(String, [Value])] -> TaskInfo -> TaskInfo
addFields fsmap ti =
    ti {
      tiFields = updateMany fsmap (tiFields ti)
    }
  where
    updateMany [] fs = fs
    updateMany ((k,vs) : rest) fs =
      updateMany rest $ M.insertWith (++) k vs fs

updateTask :: TaskInfo -> TaskInfo -> TaskInfo
updateTask (TaskInfo dt fields) ti =
  addFields (M.assocs fields) $ addTime dt ti

putData :: Value -> NominalDiffTime -> M.Map String Value -> St.State ParserState ()
putData key dt fields = do
  St.modify $ \st ->
    let newTask   = TaskInfo dt $ M.map (\v -> [v]) fields
        newResult = M.insertWith updateTask key newTask (psResult st)
    in  st {psResult = newResult}

process :: Expr -> Expr -> [(String,Expr)] -> TEvent -> St.State ParserState ()
process selector key fields ev = do
  st <- St.get
  let good = toBool . eval selector
      getKey = eval key
      evaldFields = M.fromList [(n, eval f ev) | (n,f) <- fields]
  case psPrevEvent st of
    Nothing -> return ()
    Just prevEvent -> do
      let dt = diffUTCTime (eTimestamp ev) (eTimestamp prevEvent)
      when (good prevEvent) $
          putData (getKey prevEvent) dt evaldFields
  when (eTask ev /= psCurrentTask st) $ do
      St.put $ st {psCurrentTask = eTask ev, psStartTime = Just (eTimestamp ev)}
  St.modify $ \st -> st {psPrevEvent = Just ev}

processAll :: Expr -> Expr -> [(String,Expr)] -> [TEvent] -> St.State ParserState ()
processAll selector key fields events = mapM_ (process selector key fields) events

runProcess :: Query -> [TEvent] -> M.Map Value TaskInfo
runProcess q events = 
  let st = St.execState (processAll (qWhere q) (qGroupBy q) (qSelect q) events) emptyPS
  in  psResult st

