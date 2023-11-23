{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Applicative    ( (<**>) )
import           Control.Monad          ( void )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.State
    ( MonadState(..), StateT, evalState, execStateT, gets, modify, when )

import qualified Data.ByteString        as BS
import           Data.List              ( find, nub )
import           Data.Text              ( Text )
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Text.IO           as Text
import           Data.Void              ( Void )

import qualified Options.Applicative    as OA

import           Text.Megaparsec
    ( (<|>), MonadParsec(notFollowedBy), ParseErrorBundle, ParsecT, anySingle, choice, eof
    , errorBundlePretty, many, manyTill, option, runParserT, try )
import           Text.Megaparsec.Char   ( char, newline, space, spaceChar, string )

class PP a where
    pp :: a -> Text

instance PP Text where
    pp = id

instance PP a => PP [ a ] where
    pp = Text.intercalate ", " . fmap pp

newtype SepNewline a = SepNewline [ a ]

instance PP a => PP (SepNewline a) where
    pp (SepNewline a) = Text.intercalate "\n" (pp <$> a)

data Predicate
    = Predicate { name    :: Text
                , comment :: Text
                }
    deriving ( Eq, Show )

instance PP Predicate where
    pp = comment

data Fact
    = Fact { op     :: Predicate
           , object :: Text
           }
    deriving ( Eq, Show )

instance PP Fact where
    pp (Fact p o) = pp p <> " " <> o

data Rule
    = Rule { cond :: [ Predicate ]
           , cons :: [ Predicate ]
           }
    deriving ( Eq, Show )

instance PP Rule where
    pp (Rule cns cns') = "如果 " <> pp cns <> ", 那么 " <> pp cns'

data Program
    = Program { facts   :: [ Fact ]
              , rules   :: [ Rule ]
              , objects :: [ Text ]
              }
    deriving ( Eq, Show )

instance PP Program where
    pp (Program f r o)
        = "事实:\n"
        <> pp (SepNewline f)
        <> "\n\n规则:\n"
        <> pp (SepNewline r)
        <> "\n\n对象:"
        <> pp o

type DeduceM = StateT Program IO

apply :: Text -> Predicate -> Fact
apply t p = Fact p t

recall :: Fact -> Program -> Bool
recall p prog = p `elem` facts prog

testIf :: Rule -> Text -> Program -> Bool
testIf r obj prog = all ((`recall` prog) . apply obj) (cond r)

remember :: Fact -> DeduceM Bool
remember p = do
    prog <- get
    if p `recall` prog
        then pure False
        else do
            liftIO $ Text.putStrLn ("|= Fact : " <> pp p)
            put
                prog { facts = p : facts prog
                     }
            pure True

useThen :: Rule -> Text -> DeduceM Bool
useThen r obj = or <$> mapM (remember . apply obj) (cons r)

tryRule :: Rule -> Text -> DeduceM Bool
tryRule r obj = do
    prog <- get
    if testIf r obj prog
        then useThen r obj
        else pure False

stepForward :: DeduceM Bool
stepForward = do
    objs <- gets objects
    foldr (\rule m -> foldr (\obj m' -> do
                                 r <- tryRule rule obj
                                 if r
                                     then pure True
                                     else m') m objs) (pure False) =<< gets rules

deduce :: DeduceM ()
deduce = do
    success <- stepForward
    when success deduce

many1 :: MonadParsec e s m => m a -> m [ a ]
many1 p = (:) <$> p <*> many p

parsePred :: MonadState [ Predicate ] m
    => ParsecT Void Text m a
    -> [ Predicate ]
    -> ParsecT Void Text m [ Predicate ]
parsePred end acc = do
    n <- manyTill anySingle (many1 spaceChar)
    r <- lookupPred (Text.pack n)
    case r of
        Just p  -> let
            acc' = p : acc
            in 
                option acc' (notFollowedBy end >> parsePredRest end acc')
        Nothing -> fail ("Unknown predicate: " <> n)

parsePredRest :: MonadState [ Predicate ] m
    => ParsecT Void Text m a
    -> [ Predicate ]
    -> ParsecT Void Text m [ Predicate ]
parsePredRest end acc = do
    _ <- string "AND"
    space
    parsePred end acc

parseComment :: ParsecT Void Text m ()
parseComment = do
    _ <- char '#'
    _ <- manyTill anySingle (void newline <|> eof)
    space
    pure ()

parsePredicate :: MonadState [ Predicate ] m => ParsecT Void Text m ()
parsePredicate = do
    name <- manyTill anySingle (char ':')
    _ <- char ':'
    comment <- manyTill anySingle (void newline <|> eof)
    space
    modify (Predicate (Text.pack name) (Text.pack comment) :)

lookupPred :: MonadState [ Predicate ] m => Text -> m (Maybe Predicate)
lookupPred n = gets (find ((== n) . name))

parseRule :: MonadState [ Predicate ] m => ParsecT Void Text m Rule
parseRule = do
    _ <- string "IF"
    space
    cond <- parsePred (string "THEN") []
    _ <- string "THEN"
    space
    cons <- parsePred (many1 newline) []
    space
    pure $ Rule cond cons

parseFact :: MonadState [ Predicate ] m => ParsecT Void Text m Fact
parseFact = do
    n <- manyTill anySingle (char '(')
    m <- manyTill anySingle (char ')')
    p <- lookupPred (Text.pack n)
    space
    case p of
        Just p' -> pure $ Fact p' (Text.pack m)
        Nothing -> fail ("Unknown predicate: " <> n)

parseProgram :: MonadState [ Predicate ] m => Program -> ParsecT Void Text m Program
parseProgram prog
    = choice
        [ eof >> pure prog
        , parseComment >> parseProgram prog
        , try (parsePredicate >> parseProgram prog)
        , try (parseRule >>= \r -> parseProgram
                   prog { rules = r : rules prog
                        })
        , parseFact >>= \f -> parseProgram
              prog { facts   = f : facts prog
                   , objects = object f : objects prog
                   }
        ]

runParseProgram :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Program
runParseProgram n s = evalState (runParserT (parseProgram (Program [] [] [])) n s) []

argParser :: OA.Parser String
argParser
    = OA.strOption
        (OA.long "filepath"
         <> OA.metavar "FILE"
         <> OA.help "the knowledge database file begin processed")

main :: IO ()
main = do
    baseFile <- OA.execParser
        (OA.info
             (argParser <**> OA.helper)
             (OA.fullDesc
              <> OA.progDesc "Forward inference on expert system"
              <> OA.header "Animal - a forward inference machine"))
    s <- Text.decodeUtf8 <$> BS.readFile baseFile
    let prog = runParseProgram baseFile s
    case prog of
        Left e     -> putStr (errorBundlePretty e)
        Right prog -> do
            let prog'
                    = prog { objects = nub $ objects prog
                           }
            putStrLn "\nInitial knowledge base:"
            Text.putStr $ pp prog'
            putStrLn "\n\nDeduced knowledge base:"
            prog'' <- execStateT deduce prog'
            Text.putStr $ pp prog''
