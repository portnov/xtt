
module XMonad.TimeTracker.Eval where

import Control.Monad
import qualified Control.Monad.State as St
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time
import Text.Regex.Posix

import XMonad.TimeTracker.Types
import XMonad.TimeTracker.Syntax

eval :: [(String, Expr)] -> Expr -> TEvent -> Value
eval vars expr ev = go expr
  where
    go (Lit val) = val
    go (StringProperty prop) = String $ getStringProperty prop ev
    go (Equals e (List es)) = Bool $ go e `elem` [go ex | ex <- es]
    go (Equals e1 e2) = Bool $ go e1 == go e2 
    go (Match e (List regexs)) = Bool $ or [e' =~ regex' | regex' <- concatMap toStrings (map go regexs), 
                                                           e' <- toStrings (go e)]
    go (Match e regex) = Bool $ or [e' =~ regex' | regex' <- toStrings (go regex), e' <- toStrings (go e)]
    go (Or e1 e2) = Bool $ (toBool $ go e1) || (toBool $ go e2)
    go (And e1 e2) = Bool $ (toBool $ go e1) && (toBool $ go e2)
    go (List es) = LitList $ map go es
    go (Identifier i) =
      case lookup i vars of
        Just val -> go val
        Nothing  -> error $ "Unknown identifier: " ++ i
    go (Case pairs def) =
      case [expr | (cond, expr) <- pairs, toBool (go cond)] of
        [] -> go def
        (e:_) -> go e

data TaskInfo =
  TaskInfo {
      tiDuration :: NominalDiffTime
    , tiFields :: M.Map String (M.Map Value NominalDiffTime)
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

addFields :: NominalDiffTime -> [(String, M.Map Value NominalDiffTime)] -> TaskInfo -> TaskInfo
addFields dt fsmap ti =
    ti {
      tiDuration = tiDuration ti + dt,
      tiFields = updateMany fsmap (tiFields ti)
    }
  where
    updateMany :: [(String, M.Map Value NominalDiffTime)] -> M.Map String (M.Map Value NominalDiffTime) -> M.Map String (M.Map Value NominalDiffTime)
    updateMany [] fs = fs
    updateMany ((k,v) : rest) fs =
      updateMany rest $ M.insertWith (M.unionWith (+)) k v fs

updateTask :: TaskInfo -> TaskInfo -> TaskInfo
updateTask (TaskInfo dt fields) ti =
  addFields dt (M.assocs fields) ti

putData :: Value -> NominalDiffTime -> M.Map String Value -> St.State ParserState ()
putData key dt fields = do
  St.modify $ \st ->
    let newTask   = TaskInfo dt $ M.map (\v -> M.singleton v dt) fields
        newResult = M.insertWith updateTask key newTask (psResult st)
    in  st {psResult = newResult}

process :: [VarDefinition] -> Expr -> Expr -> [(String,Expr)] -> TEvent -> St.State ParserState ()
process vars selector key fields ev = do
  st <- St.get
  let vars' = [(name,value) | VarDefinition name value <- vars]
      good = toBool . eval vars' selector
      getKey = eval vars' key
      evalFields e = M.fromList [(n, eval vars' f e) | (n,f) <- fields]
  case psPrevEvent st of
    Nothing -> return ()
    Just prevEvent -> do
      let dt = diffUTCTime (eTimestamp ev) (eTimestamp prevEvent)
      when (good prevEvent) $
          putData (getKey prevEvent) dt (evalFields prevEvent)
  when (eTask ev /= psCurrentTask st) $ do
      St.put $ st {psCurrentTask = eTask ev, psStartTime = Just (eTimestamp ev)}
  St.modify $ \st -> st {psPrevEvent = Just ev}

processAll :: [VarDefinition] -> Expr -> Expr -> [(String,Expr)] -> [TEvent] -> St.State ParserState ()
processAll vars selector key fields events = mapM_ (process vars selector key fields) events

runProcess :: [VarDefinition] -> Query -> [TEvent] -> M.Map Value TaskInfo
runProcess vars q events = 
  let st = St.execState (processAll vars (qWhere q) (qGroupBy q) (qSelect q) events) emptyPS
  in  psResult st

