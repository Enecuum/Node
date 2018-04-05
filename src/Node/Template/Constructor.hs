{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Node.Template.Constructor (
    managerMsgFuncList, managerMiningMsgList, managerBootNodeMsgList,
    managerMsgFuncListFull, managerMiningMsgListFull, managerBootNodeMsgListFull,
    managerMsgFuncListData, managerMiningMsgListData, managerBootNodeMsgListData,
    nodeBaseDataList, nodeConfigList, helloMsgList,
    genDataClass, genBazeDataInstance, dataConstruct, msgClass, baseMsgInstance,
    derivativeMsgInstance, makeLensInstance', lensInst
  ) where

import Language.Haskell.TH
import Control.Monad
import Lens.Micro
import Data.Char
import Data.DoList (item, toList, DoListM)


managerMsgFuncList, managerMiningMsgList, managerBootNodeMsgList ::
    [(String, Int)]
managerMsgFuncList      = (_2 %~ length) <$> managerMsgFuncListFull
managerMiningMsgList    = (_2 %~ length) <$> managerMiningMsgListFull
managerBootNodeMsgList  = (_2 %~ length) <$> managerBootNodeMsgListFull


managerMsgFuncListFull, managerMiningMsgListFull, managerBootNodeMsgListFull ::
    [(String, [[String]])]
managerMsgFuncListFull      = toPair <$> managerMsgFuncListData
managerMiningMsgListFull    = toPair <$> managerMiningMsgListData
managerBootNodeMsgListFull  = toPair <$> managerBootNodeMsgListData



managerMsgFuncListData :: [(Bool, String, [[String]])]
managerMsgFuncListData = toList $ do
    "stateRequest"          +: []
    "pingRequestInfo"       +: [["HostAddress"], ["PortNumber"], ["TimeSpec"], ["HostAddress"]]
    "clientIsDisconnected"  +: [["NodeId"], ["Chan", "MsgToSender"]]
    "serverIsDead"          +: []
    "deleteDeadSouls"       +: []
    "datagramMsg"           +: [["B.ByteString"], ["NodeId"]]
    "initDatagram"          +: [["Chan", "MsgToSender"], ["HostAddress"], ["B.ByteString"]]
    "sendDatagram"          +: [["B.ByteString"], ["NodeId"]]
    "connectivityQuery"     +: []
    "disconnectNode"        +: [["NodeId"]]
    "sendInitDatagram"      +: [["HostAddress"], ["PortNumber"], ["NodeId"]]


managerMiningMsgListData :: [(Bool, String, [[String]])]
managerMiningMsgListData = toList $ do
    "newTransaction"                +: [["Transaction"]]
    "sendTargetedTransaction"       +: [["Transaction"], ["NodeId"]]
    "blockMadeMsg"                  +: [["Microblock"]]
    "deleteOldestMsg"               +: []
    "deleteOldestVacantPositions"   +: []
    "sendIAmPublicator"             +: []
    "sendRawData"                   +: [["B.ByteString"]]
    "sendTransactionToPublicator"   +: [["Transaction"]]
    "resendTransactionToPublicator" +: []
    "shardingNodeRequestMsg" +: [["ShardingNodeRequestMsg"]]

managerBootNodeMsgListData :: [(Bool, String, [[String]])]
managerBootNodeMsgListData = toList $ do
    "checkBroadcastNode"    +: [["NodeId"], ["HostAddress"], ["PortNumber"]]
    "checkBroadcastNodes"   +: []


nodeBaseDataList, nodeConfigList, helloMsgList :: [(String, [String])]
nodeConfigList = toList $ do
    "privateNumber" !: ["DH.PrivateNumber"]
    "publicKey"     !: ["ECDSA.PublicKey"]
    "privateKey"    !: ["ECDSA.PrivateKey"]
    "publicPoint"   !: ["DH.PublicPoint"]
    "myNodeId"      !: ["MyNodeId"]
    "portNumber"    !: ["PortNumber"]


nodeBaseDataList = toList $ do
    "nodes"             !: ["M.Map", "NodeId", "Node"]
    "exitChan"          !: ["Chan", "ExitMsg"]
    "bootNodes"         !: ["BootNodeList"]
    "answerChan"        !: ["Chan", "Answer"]
    "broadcastNum"      !: ["Int"]
    "vacantPositions"   !: ["BI.Bimap", "TimeSpec", "IdIpPort"]
    "hostAddress"       !: ["Maybe", "HostAddress"]
    "microblockChan"    !: ["Chan", "Microblock"]


helloMsgList = toList $ do
    "p2pVersion"        !: ["P2pVersion"]
    "clientId"          !: ["ClientId"]
    "listenPort"        !: ["PortNumber"]
    "nodeId"            !: ["NodeId"]
    "caps"              !: ["Caps"]
    "nodeVariantRoles"  !: ["NodeVariantRoles"]

infixl 2 +:
--infixl 2 -:

(+:) :: String -> [[String]] -> DoListM (Bool, String, [[String]]) ()
a +: b = item (True, a, b)
--a -: b = item (False, a, b)

(!:) :: String -> [String] -> DoListM (String, [String]) ()
a !: b = item (a, b)

toPair :: (a, b, c) -> (b, c)
toPair (_, a, b) = (a, b)


itIsFunc :: String -> Int -> DecQ
itIsFunc = itIsFuncMod ""


itIsFuncMod :: String -> String -> Int -> DecQ
itIsFuncMod postf n i = do
    [FunD _ [Clause _ b1 _, cl2]]<- [d|func True = True; func _ = False|]
    let name = n & ix 0 %~ toUpper
        cl1   = Clause [ConP (mkName $ name ++ postf) $ const WildP <$> [1..i]] b1 []
    pure $ FunD (mkName $ "is" ++ name) [cl1, cl2]


constrFunc :: String -> String -> DecQ
constrFunc name clName = funD (mkName name) $
    [clause [] (normalB $ conE $ mkName $ (name & ix 0 %~ toUpper) ++ clName) []]


baseMsgInstance' :: String -> String -> [(String, Int)] -> DecQ
baseMsgInstance' clName tName patts = do
    [ValD _ body _] <- [d|fun = id|]
    instanceD (cxt []) (appT (conT $ mkName clName) (conT $ mkName tName))
        (map (\(a, _) -> constrFunc a "") patts ++
        map (\(a, b)-> itIsFunc a b) patts ++
        [pure $ ValD (VarP $ mkName $ "to" ++ clName) body []])

baseMsgInstance :: String -> String -> [(String, Int)] -> Q [Dec]
baseMsgInstance clName tName patts = sequence
    [baseMsgInstance' clName tName patts]


derivativeMsgInstance :: String -> String -> [(String, Int)] -> Q [Dec]
derivativeMsgInstance clName tName patts = sequence
    [derivativeMsgInstance' clName tName patts]


derivativeMsgInstance' :: String -> String -> [(String, Int)] -> DecQ
derivativeMsgInstance' clName tName patts = instanceD
    (cxt []) (appT (conT $ mkName clName) (conT $ mkName tName))
    (map (\(a, _) -> constrFunc a tName) patts ++
    map (\(a, i) -> itIsFuncMod tName a i) patts ++
    [funD (mkName $ "to" ++ clName)
        [clause [] body []]]
    )
  where

    body = normalB $ lamCaseE $ branches ++
        [match wildP (normalB $ appE (varE $ mkName "error")
            (litE (stringL $ clName ++ " LambdaCase error"))) []]

    branches = map (\(a, b) -> match
        (conP (mkName $ (a & ix 0 %~ toUpper) ++ tName) (map varP $ names b))
        (normalB $ foldl appE (conE $ mkName $ (a & ix 0 %~ toUpper)) (vars b))
        []) patts

    vars :: Int -> [ExpQ]
    vars b = varE <$> names b

    names :: Int -> [Name]
    names b = map (\i -> mkName $ "a" ++ show i) [1..b]


makeLensInstance' :: String -> String -> DecQ
makeLensInstance' clName tpName = instanceD (cxt [])
    (appT (conT $ mkName ((clName & ix 0 %~ toUpper) ++ "Class"))
        (conT $ mkName $ tpName & ix 0 %~ toUpper))
        [(lensGen' clName (tpName ++ (clName & ix 0 %~ toUpper)))]


msgClass' :: [String] -> String -> [(String, [[String]])] -> DecQ
msgClass' par name patt = classD
    (cxt $ map (\a -> appT (conT $ mkName a) (varT $ mkName "a")) par)
    (mkName name) [plainTV $ mkName "a"] []((
        msgConstructorType <$> patt) ++
        ((\(a, _) -> yesMsgFuncsType (a & ix 0 %~ toUpper))
            <$> patt) ++
        [sigD (mkName $ "to" ++ name) (appArrowT [
            varT $ mkName "a", conT $ mkName $ name ++ "Base"])] ++
        ((\(a, _) -> deriveYesFunc a ("to" ++ name)) <$> patt)
    )

deriveYesFunc :: String -> String -> DecQ
deriveYesFunc n n2 = funD funcName [clause [] body []]
  where
    funcName = mkName $ "is" ++ (n & ix 0 %~ toUpper)
    body     = normalB $ uInfixE
        (varE funcName) (varE $ mkName ".") (varE $ mkName n2)


yesMsgFuncsType :: String -> DecQ
yesMsgFuncsType name = sigD (mkName $ "is" ++ name)
    (appArrowT [varT $ mkName "a", conT $ mkName "Bool"])


msgConstructorType :: (String, [[String]]) -> DecQ
msgConstructorType (name, typeLine) = sigD (mkName name) theType
  where
    theType :: TypeQ
    theType = if length typeLine > 0
        then appArrowT $ (foldTypeList <$> typeLine) ++ [varT $ mkName "a"]
        else varT $ mkName "a"

foldTypeList :: [String] -> TypeQ
foldTypeList t = foldl1 appT (conT.mkName <$> t)


appArrowT :: [TypeQ] -> TypeQ
appArrowT (t:[]) = t
appArrowT (t:xs) = appT (appT arrowT t) (appArrowT xs)
appArrowT []     = error "Template.Constructor: appArrowT"


msgClass :: [String] -> String -> [(String, [[String]])] -> Q [Dec]
msgClass par name patt = sequence [msgClass' par name patt]

dataConstruct :: String -> [(Bool, String, [[String]])] -> Q [Dec]
dataConstruct n str = sequence [dataConstruct' n str]

dataConstruct' :: String -> [(Bool, String, [[String]])] -> DecQ
dataConstruct' n str = do
    let name = mkName n
    [DataD c _ v k [NormalC _ [(_, _)]] d] <- [d|data Data = AP Int deriving (Show)|]
    aCons <- forM str $ \(b, n', t) -> toTypes (modifyName b n' n) t
    pure $ DataD c name v k aCons d

modifyName :: Bool -> String -> String -> String
modifyName b n1 n2 = (if b then n1 else n1 ++ n2) & ix 0 %~ toUpper


toTypes :: String -> [[String]] -> ConQ
toTypes n s = do
    [DataD _ _ _ _ [NormalC _ [(b, _)]] _] <- [d|data Data = AP Int|]
    let name = mkName n
    pure $ NormalC name ((b,).foldTypeList' <$> s)


foldTypeList' :: [String] -> Type
foldTypeList' t = foldl1 AppT (ConT . mkName <$> t)


lensInst :: String -> [String] -> [String] -> String -> Q [Dec]
lensInst n t1 t2 get = sequence [lensTypeGen' n t1 t2, lensGen' n get]


lensGen' :: String -> String -> DecQ
lensGen' name get = pure $ ValD (VarP $ mkName name)
    (NormalB (AppE (AppE (var "lens") (var get))
        (LamE [var "md", var "a"]
            (RecUpdE (var "md") [(mkName get, var "a")])))) []

lensTypeGen' :: String -> [String] -> [String] -> DecQ
lensTypeGen' n t1 t2 = pure $ SigD (mkName n)
    (AppT (AppT (ConT $ mkName "Lens'") (foldTypeList' t1)) (foldTypeList' t2))


genDataClass' :: String -> [(String, [String])] -> DecQ
genDataClass' name patt = pure $
    ClassD [] (mkName $ (name ++ "Class") & ix 0 %~ toUpper)
        [PlainTV $ mkName "a"] []
        (
        -- n :: Lens' a t
        ((\(n, t) -> SigD
            (mkName n) (AppT (AppT (ConT $ mkName "Lens'") (VarT $ mkName "a"))
                (foldTypeList' t))) <$> patt) ++
        -- name :: Lens' a Name
        [SigD (mkName name)
            (AppT (AppT (ConT $ mkName "Lens'") (VarT $ mkName "a"))
                (ConT $ mkName $ name & ix 0 %~ toUpper))] ++
        ((\(n, _) -> FunD (mkName n) [Clause []
            (NormalB $ UInfixE (var name) (var ".") (var n))
            []]) <$> patt)
        )
genDataClass :: String -> [(String, [String])] -> Q [Dec]
genDataClass a b = toNorm $ genDataClass' a b

genBazeDataInstance :: String -> [String] -> Q [Dec]
genBazeDataInstance a b = toNorm $ genBazeDataInstance' a b

genBazeDataInstance' :: String -> [String] -> DecQ
genBazeDataInstance' name patt = instanceD (pure [])
    (appT (nameGen $ name ++ "Class") (nameGen name))
    (((\a -> lensGen' a (name ++ (a & ix 0 %~ toUpper))) <$> patt)
    ++ [idFunc])
  where
    nameGen n = conT $ mkName $ n & ix 0 %~ toUpper
    idFunc :: DecQ
    idFunc = funD (mkName name) [clause [] (normalB $ varE $ mkName "idLens") []]


class Var a where
    var :: String -> a
    --con :: String -> a

instance Var Exp where
    var = VarE . mkName

instance Var Pat where
    var = VarP . mkName

instance Var Type where
    var = VarT . mkName

toNorm :: Monad m => m a -> m [a]
toNorm a = sequence [a]
