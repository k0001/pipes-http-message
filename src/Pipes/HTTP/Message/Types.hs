{-# LANGUAGE DeriveDataTypeable #-}

module Pipes.HTTP.Message.Types (
    HttpHeaders
  , ResponseLine(..)
  , Response(..)
  , ResponseF(..)
  , RequestLine(..)
  , Request(..)
  , RequestF(..)
  , BadHttpMessage(..)
  ) where

import           Data.ByteString               (ByteString)
import           Data.CaseInsensitive          (CI)
import           Data.Data                     (Data, Typeable)
import qualified Network.HTTP.Types            as H
import           Pipes                         (Producer)
import qualified Data.HashMap.Strict           as HM

--------------------------------------------------------------------------------

type HttpHeaders = HM.HashMap (CI ByteString) [ByteString]

--------------------------------------------------------------------------------

data ResponseLine = ResponseLine
  { _reslVersion    :: !H.HttpVersion
  , _reslStatusCode :: !Int
  , _reslStatusMsg  :: !ByteString
  } deriving (Eq, Ord, Show, Typeable)

data Response = Response
  { _resLine        :: !ResponseLine
  , _resHeaders     :: !HttpHeaders
  } deriving (Eq, Show, Typeable)

data ResponseF m r = ResponseF !Response !(Producer ByteString m r)

instance (Monad m) => Functor (ResponseF m) where
  fmap f (ResponseF r c) = ResponseF r (fmap f c)

--------------------------------------------------------------------------------

data RequestLine = RequestLine
  { _reqlMethod     :: !H.StdMethod
  , _reqlUri        :: !ByteString
  , _reqlVersion    :: !H.HttpVersion
  } deriving (Eq, Ord, Show, Typeable)

data Request = Request
  { _reqLine        :: !RequestLine
  , _reqHeaders     :: !HttpHeaders
  } deriving (Eq, Show, Typeable)

data RequestF m r = RequestF !Request !(Producer ByteString m r)

instance (Monad m) => Functor (RequestF m) where
  fmap f (RequestF r c) = RequestF r (fmap f c)

--------------------------------------------------------------------------------

data BadHttpMessage
  = BadLeadingLine
  | BadHeaders
  | BadBodyShape
  deriving (Eq, Ord, Read, Show, Data, Typeable)

--------------------------------------------------------------------------------
