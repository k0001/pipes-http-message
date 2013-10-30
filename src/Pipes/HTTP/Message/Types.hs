{-# LANGUAGE DeriveDataTypeable #-}

module Pipes.HTTP.Message.Types (
    HttpHeaders
  , ResponseMeta(..)
  , ResponseF(..)
  , RequestMeta(..)
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

data ResponseMeta = ResponseMeta
  { _resmVersion    :: !H.HttpVersion
  , _resmStatusCode :: !Int
  , _resmStatusMsg  :: !ByteString
  , _resmHeaders    :: !HttpHeaders
  } deriving (Eq, Show, Typeable)

data ResponseF m r = ResponseF
  { _resMeta :: !ResponseMeta
  , _resBody :: !(Producer ByteString m r)
  }

instance (Monad m) => Functor (ResponseF m) where
  fmap f (ResponseF r c) = ResponseF r (fmap f c)

--------------------------------------------------------------------------------

data RequestMeta = RequestMeta
  { _reqmMethod  :: !H.StdMethod
  , _reqmUri     :: !ByteString
  , _reqmVersion :: !H.HttpVersion
  , _reqmHeaders :: !HttpHeaders
  } deriving (Eq, Show, Typeable)

data RequestF m r = RequestF
  { _reqMeta :: !RequestMeta
  , _reqBody :: !(Producer ByteString m r)
  }

instance (Monad m) => Functor (RequestF m) where
  fmap f (RequestF r c) = RequestF r (fmap f c)

--------------------------------------------------------------------------------

data BadHttpMessage
  = BadMetadata
  | BadBodyShape
  deriving (Eq, Ord, Read, Show, Data, Typeable)

--------------------------------------------------------------------------------
