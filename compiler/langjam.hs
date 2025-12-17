{-# LANGUAGE RecordWildCards, TupleSections, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, BlockArguments, FlexibleContexts #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void (Void)
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import Data.Either (either)
import Data.Functor.Identity (runIdentity)
import Control.Monad.State (evalState, execState, execStateT, modify, gets, get)
import Control.Monad.Random (Rand, StdGen, getRandom, getStdGen, evalRand)
import Control.Monad.Trans (lift)
import qualified Data.UUID as UUID
import Control.DeepSeq (deepseq)
import Control.Monad.Tardis
import Control.Arrow (first, second)
import Data.Foldable (fold)

type Parser = Parsec Void T.Text

data Expr = NumLit Int | VarExpr String deriving (Show)
data Instr = Instr { instrName :: String, instrArgs :: [Expr] } deriving (Show)
data Line = InstrLine Instr | LabelLine String deriving (Show)

data ArgType = RArg | WArg | RWArg | IntArg deriving (Show)
data MacroArg = MacroArg { macroArgType :: ArgType, macroArgName :: String } deriving (Show)
data MacroDecl = MacroDecl { macroName :: String, macroArgs :: [MacroArg], macroBody :: [Line] } deriving (Show)

data Decl = DeclMacro MacroDecl deriving (Show)

data Code = Code [Decl] deriving (Show)

data HardwareRegister = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | VA | VB | VC | VD | VE | VF deriving (Eq, Ord, Enum, Bounded, Show) -- nice

-- TODO I don't trust this:
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

expr :: Parser Expr
expr = (NumLit . read <$> some digitChar) <|> (VarExpr <$> some letterChar)

instr :: Parser Instr
instr = Instr <$> (some letterChar <* space1) <*> (sepBy (lexeme expr) (lexeme $ char ',')) <* char ';'

line :: Parser Line
line = (InstrLine <$> instr) <|> (LabelLine <$> (some letterChar <* char ':'))

argType :: Parser ArgType
argType = (char 'r' >> pure RArg) <|> (char 'w' >> pure WArg) <|> (string "rw" >> pure RWArg)

macroArg :: Parser MacroArg
macroArg = MacroArg <$> (argType <* space1) <*> some letterChar

macroDecl :: Parser MacroDecl
macroDecl = string "macro" >> space1 >> (MacroDecl <$> lexeme (some letterChar) <*> (lexeme (char '(') >> sepBy (lexeme macroArg) (lexeme $ char ',') <* lexeme (char ')') <* lexeme (char '{')) <*> many (lexeme line)) <* char '}'

decl :: Parser Decl
decl = DeclMacro <$> macroDecl

code :: Parser Code
code = Code <$> many (lexeme decl)

class VarGen m where
  genVar :: m String

instance VarGen (Rand StdGen) where
  genVar = UUID.toString <$> getRandom

class ReplaceVars t where
  replaceVars :: (Monad m) => (String -> m Expr) -> t -> m t

getVars :: (ReplaceVars t) => t -> Set.Set String
getVars t = execState (replaceVars (\v -> modify (Set.insert v) >> pure (VarExpr v)) t) Set.empty

instance ReplaceVars Expr where
  replaceVars f (VarExpr v) = f v
  replaceVars _ e = pure e

instance ReplaceVars Instr where
  replaceVars f i = (\l -> i { instrArgs = l }) <$> mapM (replaceVars f) (instrArgs i)

instance ReplaceVars Line where
  replaceVars f (LabelLine l) = g <$> f l where -- TODO hack
    g (VarExpr v) = LabelLine v
  replaceVars f (InstrLine i) = InstrLine <$> replaceVars f i

instance ReplaceVars MacroDecl where
  replaceVars f d = (\b -> d { macroBody = b }) <$> mapM (replaceVars f) (macroBody d)

privateVars :: Set.Set String -> MacroDecl -> Set.Set String
privateVars excl decl = Set.difference (getVars decl) (Set.union excl $ Set.fromList $ macroArgName <$> macroArgs decl)

inlineMacro :: (VarGen m, Monad m) => Set.Set String -> [Expr] -> MacroDecl -> m [Line]
inlineMacro excl args decl = (\localMap -> runIdentity . replaceVars (replaceFunc localMap) <$> macroBody decl) <$> genLocalMap where
  replaceFunc localMap v = case (Map.lookup v argMap, Map.lookup v localMap) of
    (Just argVar, _) -> pure argVar
    (_, Just localVar) -> pure localVar
    _ -> pure (VarExpr v)
  argList = macroArgName <$> macroArgs decl
  argMap = Map.fromList $ zip argList args
  argSet = Set.fromList argList
  localSet = privateVars (excl `Set.union` argSet) decl
  genLocalMap = Map.fromList <$> (sequence $ (\v -> (v,) . VarExpr <$> genVar) <$> Set.toList localSet)

inlineAllMacros :: (VarGen m, Monad m) => Set.Set String -> Set.Set String -> (String -> m MacroDecl) -> MacroDecl -> m MacroDecl
inlineAllMacros builtins globalExcl decls decl = (\newBody -> decl { macroBody = concat newBody }) <$> sequence (replaceLine <$> macroBody decl) where
  replaceLine (InstrLine instr) | not (instrName instr `Set.member` builtins) = decls (instrName instr) >>= inlineMacro globalExcl (instrArgs instr)
  replaceLine line = pure [line]

fullyInlineCodeMacro :: (VarGen m, Monad m) => Set.Set String -> Code -> String -> m MacroDecl
fullyInlineCodeMacro builtins (Code declList) = f where
  f = inlineAllMacros builtins globalExcl f . lookupDeclMap
  lookupDeclMap m = fromJust $ Map.lookup m declMap
  macroDeclList = (\(DeclMacro d) -> d) <$> declList
  declMap = Map.fromList $ (\d -> (macroName d, d)) <$> macroDeclList
  globalExcl = Set.empty

codeToLines :: (VarGen m, Monad m) => Set.Set String -> Code -> m [Line]
codeToLines builtins code = macroBody <$> fullyInlineCodeMacro builtins code "main"

regAllocGatherReadWrites :: Map.Map String [ArgType] -> [Line] -> Map.Map String (Set.Set Int, Set.Set Int)
regAllocGatherReadWrites instrTypes l = snd $ execState (mapM_ helper l) (0, Map.empty) where
  helper (InstrLine (Instr {..})) = do
    let argTypes = fromJust $ Map.lookup instrName instrTypes
    mapM_ processArg $ zip instrArgs argTypes
    modify $ first (+1)
  helper _ = pure ()
  getLineNum = gets fst
  processArg (VarExpr v, RArg) = getLineNum >>= modify . second . modifyMap first v
  processArg (VarExpr v, WArg) = getLineNum >>= modify . second . modifyMap second v
  processArg (v, RWArg) = processArg (v, RArg) >> processArg (v, WArg)
  processArg _ = pure ()
  modifyMap modFn var lineNum = Map.alter (Just . modFn (Set.insert lineNum) . fromMaybe (Set.empty, Set.empty)) var

regAllocGetLivenessOneReg :: Set.Set Int -> Set.Set Int -> Set.Set Int
regAllocGetLivenessOneReg reads writes | Set.null reads && Set.null writes = Set.empty
regAllocGetLivenessOneReg reads writes = Set.fromList . catMaybes $ evalTardis (mapM helper [firstLine..lastLine]) (undefined, ()) where
  readsAndWrites = Set.union reads writes
  firstLine = Set.findMin readsAndWrites
  lastLine = Set.findMax readsAndWrites
  helper lineNum = do
    let read = Set.member lineNum reads
    let write = Set.member lineNum writes
    if read then sendPast True else if write then sendPast False else pure ()
    nextIsRead <- getFuture
    pure $ if (read || write || nextIsRead) then Just lineNum else Nothing

regAllocGetLivenessPerLine :: Map.Map String (Set.Set Int) -> Map.Map Int (Set.Set String)
regAllocGetLivenessPerLine regLivenesses = Map.unionsWith (Set.union) regMaps where
  regMaps = regMap <$> Map.toList regLivenesses
  regMap (reg, lines) = let set = Set.singleton reg in Map.fromList $ (, set) <$> Set.toList lines

regAlloc :: Map.Map String [ArgType] -> [Line] -> Maybe (Map.Map String HardwareRegister)
regAlloc instrTypes l = execStateT (mapM_ handleReg regList) Map.empty where
  readWrites = regAllocGatherReadWrites instrTypes l
  regLivenesses = uncurry regAllocGetLivenessOneReg <$> readWrites
  livenessPerLine = regAllocGetLivenessPerLine regLivenesses
  regList = Map.keys readWrites -- TODO minor hack
  allRegs = Set.fromList [minBound..maxBound]
  handleReg reg = do
    currentMap <- get
    let allLines = fromMaybe Set.empty $ Map.lookup reg regLivenesses
    let allOverlaps = Set.delete reg $ fold $ catMaybes $ flip Map.lookup livenessPerLine <$> Set.toList allLines
    let allOverlapsHardware = Set.fromList $ catMaybes $ flip Map.lookup currentMap <$> Set.toList allOverlaps
    let hardwarePossibilities = Set.difference allRegs allOverlapsHardware
    hardware <- lift $ Set.lookupMin hardwarePossibilities
    modify (Map.insert reg hardware)

class Assembler m where
  emitInstr :: String -> [Either HardwareRegister Int] -> m ()

newtype PrintingAssembler t = PrintingAssembler { runPrintingAssembler :: IO t } deriving (Functor, Applicative, Monad)

instance Assembler PrintingAssembler where
  emitInstr s l = PrintingAssembler $ putStrLn (show s <> " " <> show l)

splitLines :: [Line] -> ([Instr], Map.Map String Int)
splitLines = helper 0 ([], Map.empty) where
  helper _ (is, m) [] = (reverse is, m)
  helper lineNum (is, m) (InstrLine i:t) = helper (lineNum+1) (i:is, m) t
  helper lineNum (is, m) (LabelLine l:t) = helper lineNum (is, Map.insert l lineNum m) t

assembleLine :: (Assembler m, Monad m) => Map.Map String HardwareRegister -> Map.Map String Int -> Instr -> m ()
assembleLine regAllocs labels (Instr {..}) = emitInstr instrName $ processArg <$> instrArgs where
  processArg (NumLit n) = Right n
  processArg (VarExpr v) = case (Map.lookup v regAllocs, Map.lookup v labels) of
    (Just reg, _) -> Left reg
    (_, Just n) -> Right n

assemble :: (Assembler m, Monad m) => Map.Map String HardwareRegister -> [Line] -> m ()
assemble regAllocs lines = let (instrs, labels) = splitLines lines in mapM_ (assembleLine regAllocs labels) instrs

main :: IO ()
main = do
  putStrLn "enter program then ctrl D:"
  userCode <- getContents
  userCode `deepseq` pure ()
  let parsedCode = either (error . errorBundlePretty) id $ parse code "" $ T.pack userCode
  g <- getStdGen
  let lines = evalRand (codeToLines (Set.fromList ["add", "set"]) parsedCode) g
  putStrLn "inlined:"
  print lines
  let regs = fromJust $ regAlloc (Map.fromList [("add", [RWArg, RArg]), ("set", [RWArg, RArg])]) lines
  putStrLn "assembly:"
  runPrintingAssembler $ assemble regs lines
  putStrLn ""
