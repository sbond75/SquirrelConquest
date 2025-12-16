{-# LANGUAGE RecordWildCards, TupleSections, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, BlockArguments #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Either (either)
import Data.Functor.Identity (runIdentity)
import Control.Monad.State (evalState, execState, modify, gets)
import Control.Monad.Random (Rand, StdGen, getRandom, getStdGen, evalRand)
import qualified Data.UUID as UUID
import Control.DeepSeq (deepseq)

type Parser = Parsec Void T.Text

data Expr = NumLit Int | VarExpr String
data Instr = Instr { instrName :: String, instrArgs :: [Expr] }
data Line = InstrLine Instr | LabelLine String

data ArgType = RArg | WArg | RWArg
data MacroArg = MacroArg { macroArgType :: ArgType, macroArgName :: String }
data MacroDecl = MacroDecl { macroName :: String, macroArgs :: [MacroArg], macroBody :: [Line] }

data Decl = DeclMacro MacroDecl

data Code = Code [Decl]

data HardwareRegister = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | VA | VB | VC | VD | VE | VF deriving (Enum, Bounded, Show) -- nice

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

codeToLines :: (VarGen m, Monad m) => Set.Set String -> Code -> m [Line]
codeToLines builtins (Code declList) = macroBody <$> getMacro "main" where
  getMacro = inlineAllMacros builtins globalExcl getMacro . lookupDeclMap
  lookupDeclMap m = fromJust $ Map.lookup m declMap
  macroDeclList = (\(DeclMacro d) -> d) <$> declList
  declMap = Map.fromList $ (\d -> (macroName d, d)) <$> macroDeclList
  globalExcl = Set.empty

regAlloc :: [Line] -> Maybe (Map.Map String HardwareRegister)
regAlloc l = Just $ Map.fromList $ (,V0) <$> concatMap (Set.toList . getVars) l -- TODO lol it just does V0 for everything

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
  userCode `deepseq` (putStrLn "result:")
  let parsedCode = either (error . errorBundlePretty) id $ parse code "" $ T.pack userCode
  g <- getStdGen
  let lines = evalRand (codeToLines (Set.fromList ["add", "set"]) parsedCode) g
  let regs = fromJust $ regAlloc lines
  runPrintingAssembler $ assemble regs lines
  putStrLn ""
