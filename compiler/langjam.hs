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
import Control.Arrow (first, second)
import Data.Foldable (fold)
import Control.Monad (when)
import Data.Word (Word16, Word8)
import Data.Bits (shiftL, shiftR, (.|.))
import Control.Monad.Writer (Writer, runWriter, tell)
import Numeric (showHex)
import qualified Data.ByteString as BS

type Parser = Parsec Void T.Text

data Expr = NumLit Int | VarExpr String deriving (Show)
data Instr = Instr { instrName :: String, instrArgs :: [Expr] } deriving (Show)
data Line = InstrLine Instr | LabelLine String deriving (Show)

data ArgType = RArg | WArg | RWArg | IntArg | LabelArg deriving (Show)
data MacroArg = MacroArg { macroArgType :: ArgType, macroArgName :: String } deriving (Show)
data MacroDecl = MacroDecl { macroName :: String, macroArgs :: [MacroArg], macroBody :: [Line] } deriving (Show)

data Decl = DeclMacro MacroDecl deriving (Show)

data Code = Code [Decl] deriving (Show)

data HardwareRegister = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | VA | VB | VC | VD | VE | VF deriving (Eq, Ord, Enum, Bounded, Show) -- nice

-- TODO I don't trust this:
-- Treat spaces, # line comments, and /* block comments */ as whitespace
spaceConsumer :: Parser ()
spaceConsumer =
  Lexer.space
    space1
    (Lexer.skipLineComment "#")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- Identifier: [A-Za-z_][A-Za-z0-9_]*
identifier :: Parser String
identifier =
  (:) <$> (letterChar <|> char '_')
      <*> many (alphaNumChar <|> char '_')

expr :: Parser Expr
expr = (NumLit . read <$> some digitChar) <|> (VarExpr <$> identifier)

instr :: Parser Instr
instr = Instr <$> (identifier <* space1) <*> (sepBy (lexeme expr) (lexeme $ char ',')) <* char ';'

line :: Parser Line
line =
  try labelLine <|> instrLine
  where
    labelLine = LabelLine <$> (identifier <* char ':')
    instrLine = InstrLine  <$> instr

argType :: Parser ArgType
argType =
  choice
    [ try (string "rw"   >> pure RWArg)
    , string "int"       >> pure IntArg
    , string "label"     >> pure LabelArg
    , char 'r'           >> pure RArg
    , char 'w'           >> pure WArg
    ]

macroArg :: Parser MacroArg
macroArg = MacroArg <$> (argType <* space1) <*> identifier

macroDecl :: Parser MacroDecl
macroDecl = string "macro" >> space1 >> (MacroDecl <$> lexeme identifier <*> (lexeme (char '(') >> sepBy (lexeme macroArg) (lexeme $ char ',') <* lexeme (char ')') <* lexeme (char '{')) <*> many (lexeme line)) <* char '}'

decl :: Parser Decl
decl = DeclMacro <$> macroDecl

code :: Parser Code
code = spaceConsumer *> (Code <$> many (lexeme decl))

class VarGen m where
  genVar :: String -> m String

instance VarGen (Rand StdGen) where
  genVar s = ((s <> "-") <>) . UUID.toString <$> getRandom

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
  genLocalMap = Map.fromList <$> (sequence $ (\v -> (v,) . VarExpr <$> genVar v) <$> Set.toList localSet)

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

regAllocGetInterestsPerLine :: Map.Map Int (Set.Set String) -> Map.Map Int (Set.Set String) -> Map.Map Int (Set.Set Int) -> Map.Map Int (Set.Set Int) -> Int -> Map.Map Int (Set.Set String)
regAllocGetInterestsPerLine reads writes sources targets codeLen = fst $ execState helper (Map.empty, Set.fromList [0..codeLen-1]) where
  helper = do
    q <- gets snd
    maybe (pure ()) helper' $ Set.lookupMax q

  helper' lineNum = do
    let getLine = \l -> gets $ Map.findWithDefault Set.empty l . fst
    modify $ second $ Set.delete lineNum
    interests <- fold <$> mapM getLine (Set.toList $ getTargets lineNum)
    let interests' = Set.union (getReadVars lineNum) (Set.difference interests $ getWrittenVars lineNum)
    interestsChanged <- (interests' /=) <$> getLine lineNum
    when interestsChanged $ do
      modify $ first $ Map.insert lineNum interests'
      modify $ second $ Set.union $ getSources lineNum

  getReadVars = flip (Map.findWithDefault Set.empty) reads
  getWrittenVars = flip (Map.findWithDefault Set.empty) writes
  getSources l = Map.findWithDefault Set.empty l sources
  getTargets l = Map.findWithDefault Set.empty l targets

splitLines :: [Line] -> ([Instr], Map.Map String Int)
splitLines = helper 0 ([], Map.empty) where
  helper _ (is, m) [] = (reverse is, m)
  helper lineNum (is, m) (InstrLine i:t) = helper (lineNum+1) (i:is, m) t
  helper lineNum (is, m) (LabelLine l:t) = helper lineNum (is, Map.insert l lineNum m) t

computeTargets :: Map.Map String ([ArgType], Maybe (Set.Set Int)) -> [Line] -> Map.Map Int (Set.Set Int)
computeTargets instrTypes l = toIntMap $ helper <$> (zip [0..] instrs) where
  toIntMap = Map.fromList . zip [0..]
  (instrs, lbls) = splitLines l
  helper (lineNum, Instr{..}) = let (argTypes, inherent) = instrTypes Map.! instrName in Set.union (Set.fromList $ catMaybes $ fmap labelArg $ zip argTypes instrArgs) (Set.map (lineNum +) $ fromMaybe (Set.singleton 1) inherent)
  labelArg (LabelArg, VarExpr v) = Map.lookup v lbls
  labelArg _ = Nothing

invertGraph :: (Ord a, Ord b) => Map.Map a (Set.Set b) -> Map.Map b (Set.Set a)
invertGraph = Map.unionsWith (<>) . fmap helper . Map.toList where
  helper (x, ys) = Map.fromList $ zip (Set.toList ys) $ fmap Set.singleton $ repeat x

regAlloc :: Map.Map String ([ArgType], Maybe (Set.Set Int)) -> [Line] -> Maybe (Map.Map String HardwareRegister)
regAlloc instrTypes l = execStateT (mapM_ handleReg regList) Map.empty where
  readWrites = regAllocGatherReadWrites (fst <$> instrTypes) l
  lineToReads = invertGraph $ fst <$> readWrites
  lineToWrites = invertGraph $ snd <$> readWrites
  targets = computeTargets instrTypes l
  sources = invertGraph targets
  interestsPerLine = regAllocGetInterestsPerLine lineToReads lineToWrites targets sources (length l)
  livenessPerLine = Map.unionsWith (<>) [interestsPerLine, lineToReads, lineToWrites]
  regLivenesses = invertGraph livenessPerLine
  regList = Map.keys readWrites -- TODO minor hack
  allRegs = Set.fromList [V1 .. VE]
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

newtype AssemblerBase t = AssemblerBase { assemblerBase :: Writer [(String, [Either HardwareRegister Int])] t } deriving (Functor, Applicative, Monad)

instance Assembler AssemblerBase where
  emitInstr s l = AssemblerBase $ tell [(s, l)]

runAssemblerBase :: AssemblerBase () -> [(String, [Either HardwareRegister Int])]
runAssemblerBase = snd . runWriter . assemblerBase

-- TODO endianness
assembleLine
  :: (Assembler m, Monad m)
  => Map.Map String HardwareRegister  -- register allocations
  -> Map.Map String Int               -- label -> instruction index
  -> Instr
  -> m ()
assembleLine regAllocs labels (Instr {..}) =
  emitInstr instrName (map processArg instrArgs)
  where
    -- Where your emulator loads the program in RAM.
    -- For wernsey/chip8-style cores this is normally 0x200.
    programStart :: Int
    programStart = 0x200

    -- Instruction index -> byte address
    labelToAddr :: Int -> Int
    labelToAddr n = programStart + 2 * n

    processArg :: Expr -> Either HardwareRegister Int
    processArg (NumLit n) = Right n
    processArg (VarExpr v) =
      case (Map.lookup v regAllocs, Map.lookup v labels) of
        -- Variable that was given a hardware register
        (Just reg, _) -> Left reg
        -- Label: convert from instruction index to RAM address
        (_, Just n)   -> Right (labelToAddr n)
        -- Should not happen if your macros are well-formed
        _             -> error ("Unknown variable/label: " ++ v)

assemble :: (Assembler m, Monad m) => Map.Map String HardwareRegister -> [Line] -> m ()
assemble regAllocs lines = let (instrs, labels) = splitLines lines in mapM_ (assembleLine regAllocs labels) instrs

data MachineInstr = MachineInstr { baseBytes :: Word16 } | FakeInstr ([Either HardwareRegister Int] -> Word16)

renderMachineInstr :: MachineInstr -> [Either HardwareRegister Int] -> Word16
renderMachineInstr (FakeInstr f) = f
renderMachineInstr (MachineInstr{..}) = foldl (.|.) baseBytes . fmap processArg . zip [0..] where
  -- each increment of argNum we shift 4 less
  -- we start at position 2 ie <<8
  processArg (argNum, Left v) = shiftL (fromIntegral $ fromEnum v) (8 - 4*argNum)
  -- number values are all the way to the right, so we don't need to worry about shifting them; additional space changes their max value, not their shift amt
  processArg (_, Right n) = fromIntegral n

renderMachineInstrs :: Map.Map String MachineInstr -> [(String, [Either HardwareRegister Int])] -> [Word16]
renderMachineInstrs instrs = fmap helper where
  helper (name, args) = renderMachineInstr (instrs Map.! name) args

chip8Instrs :: Map.Map String (([ArgType], Maybe (Set.Set Int)), MachineInstr)
chip8Instrs = Map.fromList
  [ ("clear", (([], Nothing), MachineInstr 0x00E0))
  , ("goto", (([LabelArg], Just Set.empty), MachineInstr 0x1000))
  , ("skipifeqn", (([RArg, IntArg], Just $ Set.fromList [1, 2]), MachineInstr 0x3000))
  , ("skipifneqn", (([RArg, IntArg], Just $ Set.fromList [1, 2]), MachineInstr 0x4000))
  , ("skipifeq", (([RArg, RArg], Just $ Set.fromList [1, 2]), MachineInstr 0x5000))
  , ("setn", (([WArg, IntArg], Nothing), MachineInstr 0x6000))
  , ("addn", (([RWArg, IntArg], Nothing), MachineInstr 0x7000))
  , ("set", (([WArg, RArg], Nothing), MachineInstr 0x8000))
  , ("bor", (([RWArg, RArg], Nothing), MachineInstr 0x8001))
  , ("band", (([RWArg, RArg], Nothing), MachineInstr 0x8002))
  , ("bxor", (([RWArg, RArg], Nothing), MachineInstr 0x8003))
  , ("add", (([RWArg, RArg], Nothing), MachineInstr 0x8004))
  , ("sub", (([RWArg, RArg], Nothing), MachineInstr 0x8005))
  , ("shr", (([RWArg], Nothing), MachineInstr 0x8006)) -- TODO VF
  , ("negsub", (([RWArg, RArg], Nothing), MachineInstr 0x8007))
  , ("shl", (([RWArg], Nothing), MachineInstr 0x800E)) -- TODO VF
  , ("skipifneq", (([RArg, RArg], Just $ Set.fromList [1, 2]), MachineInstr 0x9000))
  , ("seti", (([IntArg], Nothing), MachineInstr 0xA000)) -- TODO I
  , ("rand", (([RArg, IntArg], Nothing), MachineInstr 0xC000)) -- TODO or w?
  , ("draw", (([RArg, RArg, IntArg], Nothing), MachineInstr 0xD000)) -- TODO I
  , ("skipifkey", (([RArg], Just $ Set.fromList [1, 2]), MachineInstr 0xE09E))
  , ("skipifnkey", (([RArg], Just $ Set.fromList [1, 2]), MachineInstr 0xE0A1))
  , ("getdelay", (([WArg], Nothing), MachineInstr 0xF007))
  , ("getkey", (([WArg], Nothing), MachineInstr 0xF00A))
  , ("setdelay", (([RArg], Nothing), MachineInstr 0xF015))
  , ("setsound", (([RArg], Nothing), MachineInstr 0xF018))
  , ("addi", (([RArg], Nothing), MachineInstr 0xF01E)) -- TODO I
  , ("setisprite", (([RArg], Nothing), MachineInstr 0xF029)) -- TODO I; TODO what is this
  , ("bcd", (([RArg], Nothing), MachineInstr 0xF033)) -- TODO I
  -- , ("regdump", ([RArg], Nothing, MachineInstr 0xF055)) -- TODO, this reads a bunch of regs
  -- , ("regload", ([WArg], Nothing, MachineInstr 0xF065)) -- TODO, this writes a bunch of regs
  -- workaround hacks (TODO):
  , ("regdump", (([], Nothing), MachineInstr 0xF055))
  , ("regload", (([], Nothing), MachineInstr 0xF065))
  , ("getzero", (([WArg], Nothing), MachineInstr 0x8000))
  , ("setzero", (([RArg], Nothing), FakeInstr (\[Left reg] -> renderMachineInstr (snd $ chip8Instrs Map.! "set") [Left V0, Left reg])))
  ]

w16to8 :: Word16 -> (Word8, Word8)
w16to8 w = (fromIntegral $ shiftR w 8, fromIntegral w)

w16to8s :: [Word16] -> [Word8]
w16to8s = concatMap ((\(a,b) -> [a,b]) . w16to8)

main :: IO ()
main = do
  putStrLn "enter program then ctrl D:"
  userCode <- getContents
  userCode `deepseq` pure ()
  let parsedCode = either (error . errorBundlePretty) id $ parse code "" $ T.pack userCode
  g <- getStdGen
  let lines = evalRand (codeToLines (Set.fromList $ Map.keys chip8Instrs) parsedCode) g
  putStrLn "inlined:"
  print lines
  let regs = fromJust $ regAlloc (fst <$> chip8Instrs) lines
  putStrLn "register allocation:"
  print $ regs
  putStrLn "assembly:"
  print $ runAssemblerBase $ assemble regs lines
  putStrLn "machine:"
  let machine = w16to8s $ renderMachineInstrs (snd <$> chip8Instrs) $ runAssemblerBase $ assemble regs lines
  print $ fmap (flip showHex "") $ machine
  BS.writeFile "game.ch8" $ BS.pack machine
  putStrLn "wrote file"
  putStrLn ""
