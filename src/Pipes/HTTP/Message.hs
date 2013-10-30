{-# LANGUAGE RankNTypes #-}

module Pipes.HTTP.Message
 ( parseResponses
   -- * Types
 , module Pipes.HTTP.Message.Types
 ) where

import           Control.Applicative
import           Control.Monad.Trans.Free      (FreeT(..), FreeF(..))
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as B8
import           Data.Word                     (Word)
import qualified Data.HashMap.Strict           as HM
import qualified Network.HTTP.Types            as H
import           Pipes
import qualified Pipes.Attoparsec              as Pa
import qualified Pipes.ByteString              as Pb
import           Pipes.HTTP.Message.Attoparsec (pResponseLine, pHeaders)
import           Pipes.HTTP.Message.Types
import qualified Pipes.Parse                   as Pp


--------------------------------------------------------------------------------

parseResponses
 :: Monad m
 => Producer ByteString m r
 -> FreeT (ResponseF m) m (Either BadHttpMessage (), Producer ByteString m r)
parseResponses p0 = FreeT $ do
    (er1, p1) <- Pp.runStateT parseResponse p0
    return $ case er1 of
      Left  e1   -> Pure (Left e1, p1)
      Right resp ->
        case respBodyShape resp of
          Nothing -> Pure (Left BadBodyShape, p1)
          Just s  -> Free (ResponseF resp $ parseResponses <$> splitBody s p1)


--------------------------------------------------------------------------------
-- Internal stuff below here


-- | Determine the shape of an HTTP message body based on the header values
--
-- This implementation only understands the "Content-Length" header.
bodyShape :: H.HttpVersion -> HttpHeaders -> Maybe BodyShape
bodyShape _version headers = single -- <|> chunked <|> ...
  where
    single = case HM.lookup H.hContentLength headers of
               Just [len] -> Just $ Single $ read $ B8.unpack len
               _          -> Nothing

respBodyShape :: Response -> Maybe BodyShape
respBodyShape (Response l h) = bodyShape (_reslVersion l) h


-- | Forwards downstream a single `BodyShape` from the given `Producer`,
-- returning any leftovers.
splitBody
  :: Monad m
  => BodyShape
  -> Producer  ByteString m r
  -> Producer' ByteString m (Producer ByteString m r)
splitBody shape p = case shape of
    Single len -> Pb.splitAt len p


-- | Attempt to parse the response line and headers from the underlying producer
parseResponse
  :: Monad m
  => Pp.StateT (Producer ByteString m r) m (Either BadHttpMessage Response)
parseResponse = do
    er1 <- Pa.parse pResponseLine
    case er1 of
      Left  _        -> return $ Left BadLeadingLine
      Right (_,line) -> do
        er2 <- Pa.parse pHeaders
        return $ case er2 of
          Left  _        -> Left  BadHeaders
          Right (_,hdrs) -> Right (Response line hdrs)


-- | Shape of the HTTP message body
data BodyShape
  = Single Word
  -- | Chunked ... something ...

