-- | This module contains message building functions that can be used for the
--   transformation of ASTs.
module HST.Frontend.Transformer.Messages where

import           Polysemy             ( Member, Members, Sem )

import           HST.Effect.InputFile ( InputFile, getInputFile )
import           HST.Effect.Report
  ( Message(Message), Report, Severity(Error, Info), report, reportFatal )
import qualified HST.Frontend.Syntax  as S

-- | Reports a fatal error that the given feature is not supported.
notSupported
  :: Member Report r
  => String -- ^ The name of the feature (plural) that is not supported.
  -> Sem r b
notSupported feature
  = reportFatal $ Message Error $ feature ++ " are not supported!"

-- | Informs the user that the given feature is not supported and the
--   corresponding AST node will be skipped.
skipNotSupported
  :: Member Report r
  => String -- ^ The name of the feature (plural) that is not supported.
  -> Sem r ()
skipNotSupported feature = report
  $ Message Info
  $ feature ++ " are not supported and will be skipped!"

-- | Reports a fatal error that the given feature is not supported. Displays an
--   excerpt of the input code specified by the given source span.
notSupportedWithExcerpt
  :: Members '[InputFile, Report] r
  => String      -- ^ The name of the feature (plural) that is not supported.
  -> S.SrcSpan a -- ^ The source span of the excerpt to display.
  -> Sem r b
notSupportedWithExcerpt feature srcSpan = do
  excerpt <- displayCodeExcerpt srcSpan
  reportFatal $ Message Error $ feature ++ " are not supported!\n" ++ excerpt

-- | Informs the user that the given feature is not supported and the
--   corresponding AST node will be skipped. Displays an excerpt of the input
--   code specified by the given source span.
skipNotSupportedWithExcerpt
  :: Members '[InputFile, Report] r
  => String      -- ^ The name of the feature (plural) that is not supported.
  -> S.SrcSpan a -- ^ The source span of the excerpt to display.
  -> Sem r ()
skipNotSupportedWithExcerpt feature srcSpan = do
  excerpt <- displayCodeExcerpt srcSpan
  report
    $ Message Info
    $ feature ++ " are not supported and will be skipped!\n" ++ excerpt

-- | Displays an excerpt of the input code specified by the given source span.
--
--   The excerpt consists of an introductory line with the file path and the
--   source span numbers, all lines of the input program that are at least
--   partially contained in the given source span, including their line
--   numbers, and marks showing the start and end of the spanned code.
displayCodeExcerpt :: Member InputFile r => S.SrcSpan a -> Sem r String
displayCodeExcerpt S.NoSrcSpan = return "No source span information available!"
displayCodeExcerpt src@S.SrcSpan {} = do
  maybeContent <- getInputFile (S.srcSpanFilePath src)
  return $ case maybeContent of
    Nothing      ->
      "No content was found for the file `" ++ S.srcSpanFilePath src ++ "`!"
    Just content ->
      let ls = getLines (S.srcSpanStartLine src - 1) (S.srcSpanEndLine src)
            (lines content)
      in if not (isValidSrcSpan ls)
           then "The source span "
             ++ srcString
             ++ " of `"
             ++ S.srcSpanFilePath src
             ++ "` cannot be fully displayed!"
           else S.srcSpanFilePath src
             ++ ':' : srcString ++ ":\n" ++ unlines (prettyLines ls)
 where
  -- | Returns a sublist of the given list specified by the given indices.
  --
  --   The first of the zero-based indices is inclusive, the second is
  --   exclusive. Invalid indices do not cause runtime errors.
  --   Example: getLines 1 3 [1, 2, 3, 4, 5] = [2, 3]
  getLines :: Int -> Int -> [a] -> [a]
  getLines i1 i2 = take (i2 - i1) . drop i1

  -- | Tests if a source span is valid by checking if the locations specified
  --   in the source span do exist in the given lines.
  isValidSrcSpan :: [String] -> Bool
  isValidSrcSpan [] = False
  isValidSrcSpan ls
    = (length ls == S.srcSpanEndLine src - S.srcSpanStartLine src + 1)
    && (length (head ls) >= S.srcSpanStartColumn src)
    && (length (last ls) >= S.srcSpanEndColumn src - 1)

  -- | Builds a string displaying the line and column of the source span start
  --   and end.
  srcString :: String
  srcString = show (S.srcSpanStartLine src)
    ++ ':'
    : show (S.srcSpanStartColumn src)
    ++ '-' : show (S.srcSpanEndLine src) ++ ':' : show (S.srcSpanEndColumn src)

  -- | Adds line numbers to each given line and adds marks showing the start
  --   and end of the spanned code.
  prettyLines :: [String] -> [String]
  prettyLines ls
    = let (numbers, maxLineNumLength) = alignedLineNumbers
            (S.srcSpanStartLine src) (S.srcSpanEndLine src)
          ls' = zipWith (\lNum code -> lNum ++ " | " ++ code) numbers ls
      in case ls of
           -- Excerpts consisting of no lines should be exfiltrated by
           -- 'isValidSrcSpan'.
           []        -> []
           -- For single-line excerpts, an additional line below the excerpt
           -- marking the entire spanned code is added.
           [_]       ->
             let lastLine = replicate
                   (maxLineNumLength + 2 + S.srcSpanStartColumn src) ' '
                   ++ replicate
                   (S.srcSpanEndColumn src - S.srcSpanStartColumn src) '^'
             in ls' ++ [lastLine]
           -- For multi-line excerpts, there are two lines added:
           -- A line above the excerpt marking the start of the spanned code.
           -- A line below the excerpt marking the end of the spanned code.
           _ : _ : _ ->
             let maxLineLength = maximum
                   (S.srcSpanEndColumn src - 1 : map length (init ls))
                 minPadding    = minimum
                   (S.srcSpanStartColumn src - 1
                    : map (length . takeWhile (== ' ')) (tail ls))
                 firstLine     = replicate
                   (maxLineNumLength + 2 + S.srcSpanStartColumn src) ' '
                   ++ replicate (maxLineLength - S.srcSpanStartColumn src + 1)
                   'v'
                 lastLine      = replicate (maxLineNumLength + 3 + minPadding)
                   ' '
                   ++ replicate (S.srcSpanEndColumn src - minPadding - 1) '^'
             in firstLine : ls' ++ [lastLine]

  -- | Produces a list of right-aligned line numbers going from the first to
  --   the second given line number (both are inclusive). Also returns the
  --   maximum length of these numbers (the length of the second given number).
  alignedLineNumbers :: Int -> Int -> ([String], Int)
  alignedLineNumbers l1 l2
    = let maxLineNumLength = length (show l2)
      in (map (padLeft maxLineNumLength . show) [l1 .. l2], maxLineNumLength)

  -- | Adds spaces to the left side of the given string so that the given
  --   maximum length is reached.
  padLeft :: Int -> String -> String
  padLeft maxLength s = replicate (maxLength - length s) ' ' ++ s
{- TODO Unfinished. Could be used to colorize the spanned code.
  insertColor :: Int -> Int -> [String] -> Maybe [String]
  insertColor i1 i2 ls = reverse (insertColor i2 (reverse insertColor' i1 ls)))

  insertColor' :: Int -> [String] -> Maybe [String]
  insertColor' c (h:ls') = 
  insertColor' _ [] = Nothing

  insertAt :: Int -> a -> [a] -> Maybe [a]
  insertAt 0 e list = Just (e : list)
  insertAt i e [] = Nothing
  insertAt i e (h:list') | i < 0 = Nothing
                         | otherwise = fmap (h :) (insertAt (i - 1) e list')
  -}
