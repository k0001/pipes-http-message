{-# LANGUAGE DeriveDataTypeable #-}

module Pipes.HTTP.Message.Types (
    ResponseLine(..)
  , Response(..)
  , RequestLine(..)
  , Request(..)
  , BadHttpMessage(..)
  ) where

import           Data.ByteString               (ByteString)
import           Data.Data                     (Data, Typeable)
import qualified Network.HTTP.Types            as H
import           Pipes                         (Producer)

--------------------------------------------------------------------------------

data ResponseLine = ResponseLine
  { _reslVersion    :: !H.HttpVersion
  , _reslStatusCode :: !Int
  , _reslStatusMsg  :: !ByteString
  } deriving (Eq, Ord, Show, Typeable)

data Response m r = Response
  { _resLine        :: !ResponseLine
  , _resHeaders     :: !H.ResponseHeaders
  , _resContent     :: !(Producer ByteString m r)
  }

instance (Monad m) => Functor (Response m) where
  fmap f (Response l h c) = Response l h (fmap f c)

--------------------------------------------------------------------------------

data RequestLine = RequestLine
  { _reqlMethod     :: !H.StdMethod
  , _reqlUri        :: !ByteString
  , _reqlVersion    :: !H.HttpVersion
  } deriving (Eq, Ord, Show, Typeable)

data Request m r = Request
  { _reqLine        :: !RequestLine
  , _reqHeaders     :: !H.RequestHeaders
  , _reqContent     :: !(Producer ByteString m r)
  }

instance (Monad m) => Functor (Request m) where
  fmap f (Request l h c) = Request l h (fmap f c)

--------------------------------------------------------------------------------

data BadHttpMessage
  = BadLeadingLine
  | BadHeaders
  | BadBodyShape
  deriving (Eq, Read, Show, Data, Typeable)

--------------------------------------------------------------------------------
