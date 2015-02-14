{-# LANGUAGE RankNTypes #-}
module XMonad.TimeTracker.Eval where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.State as St
-- import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Dates as D
import Data.Time
import Text.Regex.Posix

import XMonad.TimeTracker.Types
import XMonad.TimeTracker.Syntax

data ParserState = PS {
      psCurrentTask :: String
    , psStartTime :: Maybe UTCTime
    , psPrevEvent :: Maybe TEvent
    , psTotalTime :: NominalDiffTime
    , psVariables :: [(String, Expr)]
    , psTimestamp :: UTCTime
    , psDuration :: NominalDiffTime
    , psResult :: M.Map Value TaskInfo
    } deriving (Eq, Show)

emptyPS :: ParserState
emptyPS = PS {
  psCurrentTask = "Startup",
  psStartTime = Nothing,
  psPrevEvent = Nothing,
  psVariables = [],
  psDuration = 0,
  psTimestamp = undefined,
  psTotalTime = 0,
  psResult = M.empty }

type EvalM a = St.StateT ParserState IO a

-- To be moved to Data.Dates
utcTime2dateTime :: UTCTime -> IO D.DateTime
utcTime2dateTime utc = do
  zone <- getCurrentTimeZone
  let lt = utcToLocalTime zone utc
      date = D.dayToDateTime (localDay lt)
      day = localTimeOfDay lt
      time = D.Time (todHour day) (todMin day) (round $ todSec day)
  return $ D.addTime date time

-- To be moved to Data.Dates
seconds2Time :: Int -> D.Time
seconds2Time x =
  let (y,s) = divMod x 60
      (h,m) = divMod y 60
  in  D.Time h m s

-- To be moved to Data.Dates
nominalDiffTime2Time :: NominalDiffTime -> D.Time
nominalDiffTime2Time dt = seconds2Time $ round dt

eval :: Expr -> TEvent -> EvalM Value
eval expr ev = go expr
  where
    go (Lit val) = return val
    go (StringProperty prop) = return $ String $ getStringProperty prop ev
    go (BinOp op e1 e2) = evalOp op e1 e2
    go (List es) = LitList <$> mapM go es
    go (Not e) = (Bool . not . toBool) <$> go e
    go (Identifier i) = do
      vars <- St.gets psVariables
      case lookup i vars of
        Just val -> go val
        Nothing  -> fail $ "Unknown identifier: " ++ i
    go Timestamp = do
      t <- St.gets psTimestamp
      liftIO $ DateTime <$> utcTime2dateTime t
    go Duration = do
      dt <- St.gets psDuration
      return $ Time $ nominalDiffTime2Time dt
    go (Case pairs def) = do
      pairs' <- filterM (\(cond, expr) -> toBool <$> go cond) pairs
      case map snd pairs' of
        [] -> go def
        (e:_) -> go e

    evalOp Equals e (List es) =
        Bool <$> (elem <$> go e <*> mapM go es)
    evalOp Equals e1 e2 = Bool <$> liftM2 equals (go e1) (go e2)

    evalOp Match (Lit (DateTime d)) (Lit (Time t)) = return $ Bool $ equalsDateTime d t
    evalOp Match (Lit (Time t)) (Lit (DateTime d)) = return $ Bool $ equalsDateTime d t
    evalOp Match (Lit (WeekDay w)) (Lit (DateTime d)) = return $ Bool $ equalsWeekday d w
    evalOp Match (Lit (DateTime d)) (Lit (WeekDay w)) = return $ Bool $ equalsWeekday d w
    evalOp Match (Lit (DateTime d1)) (Lit (DateTime d2)) = return $ Bool $ equalsDate d1 d2

    evalOp Match e (List regexs) = do
        regexs' <- mapM go regexs
        s <- go e
        let res = or [e' =~ regex' | regex' <- concatMap toStrings regexs', 
                                     e' <- toStrings s]
        return $ Bool res
    evalOp Match e regex = do
        r <- go regex
        s <- go e
        case (r,s) of
          (DateTime d, Time t) -> return $ Bool $ equalsDateTime d t
          (Time t, DateTime d) -> return $ Bool $ equalsDateTime d t
          (WeekDay w, DateTime d) -> return $ Bool $ equalsWeekday d w
          (DateTime d, WeekDay w) -> return $ Bool $ equalsWeekday d w
          (DateTime d1, DateTime d2) -> return $ Bool $ equalsDate d1 d2
          _ -> return $ Bool $ or [e' =~ regex' | regex' <- toStrings r, e' <- toStrings s]

    evalOp Cut e (List regexs) = do
        e' <- go e
        regexs' <- mapM go regexs
        return $ LitList [String $ cut (toString e') (toString regex) | regex <- regexs']
    evalOp Cut e regex = do
        e' <- go e
        regex' <- go regex
        return $ String $ cut (toString e') (toString regex')
    evalOp Or e1 e2 = Bool <$> (liftA2 (||) (toBool <$> go e1) (toBool <$> go e2))
    evalOp And e1 e2 = Bool <$> (liftA2 (&&) (toBool <$> go e1) (toBool <$> go e2))
    evalOp Lt e1 e2 = evalCmp (<) e1 e2
    evalOp Gt e1 e2 = evalCmp (>) e1 e2
    evalOp Lte e1 e2 = evalCmp (<=) e1 e2
    evalOp Gte e1 e2 = evalCmp (>=) e1 e2

    -- isDateOnly d = D.hour d == 0 && D.minute d == 0 && D.second d == 0

    equals (DateTime dt1) (DateTime dt2) = equalsDate dt1 dt2
    equals x y = x == y

    evalCmp :: (forall a. Ord a => a -> a -> Bool) -> Expr -> Expr -> EvalM Value
    evalCmp op e1 e2 = do
      e1' <- go e1
      e2' <- go e2
      case (e1', e2') of
        (Time t1, Time t2) -> return $ Bool $ t1 `op` t2
        (DateTime dt1, DateTime dt2) -> return $ Bool $ dt1 `op` dt2
        (x,y) -> fail $ "Cannot compare " ++ show x ++ " with " ++ show y

equalsDate d1 d2 =
    D.year d1 == D.year d2 && D.month d1 == D.month d2 && D.day d1 == D.day d2

equalsDateTime d t = (D.hour d == D.tHour t) &&
                     (D.minute d == D.tMinute t) &&
                     (D.second d == D.tSecond t)

equalsWeekday d w = D.dateWeekDay d == w

cut :: String -> String -> String
cut str regex =
  let (_,result,_) = str =~ regex :: (String, String, String)
  in  result

data TaskInfo =
  TaskInfo {
      tiDuration :: NominalDiffTime
    , tiFields :: M.Map String (M.Map Value NominalDiffTime)
  }
  deriving (Eq, Show)

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

putData :: Value -> NominalDiffTime -> M.Map String Value -> EvalM ()
putData key dt fields = do
  St.modify $ \st ->
    let newTask   = TaskInfo dt $ M.map (\v -> M.singleton v dt) fields
        newResult = M.insertWith updateTask key newTask (psResult st)
    in  st {psResult = newResult}

process :: Expr -> Expr -> [(String,Expr)] -> TEvent -> EvalM ()
process selector key fields ev = do
  st <- St.get
  St.modify $ \st -> st {psTimestamp = eTimestamp ev}
  let good e = toBool <$> eval selector e
      getKey = eval key
      evalFields e = do
          fs <- forM fields $ \(n, f) -> do
                                         f' <- eval f e
                                         return (n, f')
          return $ M.fromList fs
  case psPrevEvent st of
    Nothing -> return ()
    Just prevEvent -> do
      let dt = diffUTCTime (eTimestamp ev) (eTimestamp prevEvent)
      St.modify $ \st -> st {psDuration = dt}
      ok <- good prevEvent
      when ok $ do
          key' <- eval key prevEvent
          fields' <- evalFields prevEvent
          putData key' dt fields'
          St.modify $ \st -> st {psTotalTime = psTotalTime st + dt}
  when (eTask ev /= psCurrentTask st) $ do
      St.put $ st {psCurrentTask = eTask ev, psStartTime = Just (eTimestamp ev)}
  St.modify $ \st -> st {psPrevEvent = Just ev}

processAll :: Expr -> Expr -> [(String,Expr)] -> [TEvent] -> EvalM ()
processAll selector key fields events = mapM_ (process selector key fields) events

runProcess :: [VarDefinition] -> Query -> [TEvent] -> IO (M.Map Value TaskInfo, NominalDiffTime)
runProcess vars q events = do
  let vars' = [(name,value) | VarDefinition name value <- vars]
  st <- St.execStateT (processAll (qWhere q) (qGroupBy q) (qSelect q) events) $
                        emptyPS {psVariables = vars'}
  return (psResult st, psTotalTime st)

