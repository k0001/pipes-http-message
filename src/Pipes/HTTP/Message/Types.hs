{-# LANGUAGE DeriveDataTypeable #-}

module Pipes.HTTP.Message.Types (
    ResponseLine(..)
  , Response(..)
  , RequestLine(..)
  , Request(..)
  ) where

import           Data.ByteString               (ByteString)
import           Data.Time                     (UTCTime)
import           Data.Typeable                 (Typeable)
import qualified Network.HTTP.Types            as H
import           Pipes                         (Producer)

--------------------------------------------------------------------------------

data ResponseLine = ResponseLine
  { _reslVersion    :: !H.HttpVersion
  , _reslStatusCode :: !Int
  , _reslStatusMsg  :: !ByteString
  } deriving (Eq, Ord, Show)

data Response m r = Response
  { _resLine        :: !ResponseLine
  , _resHeaders     :: !H.ResponseHeaders
  , _resContent     :: !(Producer ByteString m r)
  }

--------------------------------------------------------------------------------

data RequestLine = RequestLine
  { _reqlMethod     :: !H.StdMethod
  , _reqlUri        :: !ByteString
  , _reqlVersion    :: !H.HttpVersion
  } deriving (Eq, Ord, Show)

data Request m r = Request
  { _reqLine        :: !RequestLine
  , _reqHeaders     :: !H.RequestHeaders
  , _reqContent     :: !(Producer ByteString m r)
  }

--------------------------------------------------------------------------------
