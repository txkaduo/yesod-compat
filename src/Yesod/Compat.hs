module Yesod.Compat
  ( module Yesod.Compat
  , module HandlerCatchMod
  ) where

import ClassyPrelude.Yesod
import Control.Monad.Trans.Except

#if MIN_VERSION_yesod(1, 6, 0)
import Control.Monad.Trans.Resource
#endif

-- but in old yesod, we have MonadCatch and MonadMask.
-- So sometimes we need UnliftIO.tryAny, sometimes we need Control.Monad.Catch.tryAny
#if MIN_VERSION_yesod(1, 6, 0)
import qualified UnliftIO as HandlerCatchMod hiding (fromEitherM)
#else
import qualified Control.Exception.Safe as HandlerCatchMod
#endif


#if MIN_VERSION_yesod(1, 6, 0)

type HandlerOf site = HandlerFor site

type SubHandlerOf sub master = SubHandlerFor sub master

type WidgetOf site = WidgetFor site ()


liftMonadHandler :: MonadHandler m => HandlerFor (HandlerSite m) a -> m a
liftMonadHandler = liftHandler


liftMonadHandlerToSub :: HandlerOf master a -> SubHandlerOf site master a
liftMonadHandlerToSub = liftHandler


getSubYesodCompat :: SubHandlerOf sub master sub
getSubYesodCompat = getSubYesod


#else

type HandlerOf site = HandlerT site IO

type SubHandlerOf sub master = HandlerT sub (HandlerT master IO)

type WidgetOf site = WidgetT site IO ()

liftMonadHandler :: MonadHandler m => HandlerT (HandlerSite m) IO a -> m a
liftMonadHandler = liftHandlerT


liftMonadHandlerToSub :: HandlerOf master a -> SubHandlerOf site master a
liftMonadHandlerToSub = lift


getSubYesodCompat :: SubHandlerOf sub master sub
getSubYesodCompat = getYesod


#endif


#if MIN_VERSION_resourcet(1, 2, 0)
runResourceExceptT :: (MonadUnliftIO m) => ResourceT (ExceptT e m) a -> ExceptT e m a
runResourceExceptT = ExceptT . runResourceT . transResourceT runExceptT
#else
runResourceExceptT :: (MonadBaseControl IO m) => ResourceT (ExceptT e m) a -> ExceptT e m a
runResourceExceptT = runResourceT
#endif


#if MIN_VERSION_conduit(1, 3, 0)

type SourceC m o = ConduitT () o m ()
type SinkC i = ConduitT i Void
type ConduitC i m o = ConduitT i o m ()

#else

type SourceC m o = Source m o
type SinkC i = Sink i
type ConduitC i m o = Conduit i m o

#endif


#if MIN_VERSION_classy_prelude(1, 5, 0)
---------------------------------------------------------------
-- Exception handling functions require MonadUnliftIO instances
---------------------------------------------------------------

type CatchExceptionMonad m = (MonadUnliftIO m)
type MaskExceptionMonad m = (MonadUnliftIO m)

-- Because there is no instance MonadUnliftIO (ExceptT e m)
catchExceptT :: (Exception ex, MonadUnliftIO m)
             => ExceptT e m a
             -> (ex -> ExceptT e m a)
             -> ExceptT e m a
catchExceptT f h =
  lift ((runExceptT f) `catch` (runExceptT . h))
    >>= either throwE return


catchAnyExceptT :: (MonadUnliftIO m)
                => ExceptT e m a
                -> (SomeException -> ExceptT e m a)
                -> ExceptT e m a
catchAnyExceptT f h =
  lift ((runExceptT f) `catchAny` (runExceptT . h))
    >>= either throwE return


#elif MIN_VERSION_classy_prelude(1, 0, 0)
----------------------------------------------------------------------
-- Exception handling functions require MonadCatch/MonadMask instances
----------------------------------------------------------------------

type CatchExceptionMonad m = (MonadCatch m)
type MaskExceptionMonad m = (MonadMask m)

catchExceptT :: (Exception ex, CatchExceptionMonad m)
             => ExceptT e m a
             -> (ex -> ExceptT e m a)
             -> ExceptT e m a
catchExceptT = catch


catchAnyExceptT :: (CatchExceptionMonad m)
                => ExceptT e m a
                -> (SomeException -> ExceptT e m a)
                -> ExceptT e m a
catchAnyExceptT = catch


#endif


-- constraints of runSqlPool has been changed from (MonadBaseControl IO m) to (MonadUnliftIO m)
#if MIN_VERSION_persistent(2, 8, 0)
type RunSqlMonad m = (MonadUnliftIO m)
#else
type RunSqlMonad m = (MonadBaseControl IO m)
#endif


-- In new yesod Handler functions, we have MonadUnliftIO instance, but not MonadCatch/MonadMask
-- but in old yesod, we have MonadCatch and MonadMask.
-- So sometimes we need UnliftIO.tryAny, sometimes we need Control.Monad.Catch.tryAny
#if MIN_VERSION_yesod(1, 6, 0)
type MonadHandlerCatch m = (MonadUnliftIO m)
type MonadHandlerMask m = (MonadUnliftIO m)
#else
type MonadHandlerCatch m = (HandlerCatchMod.MonadCatch m)
type MonadHandlerMask m = (HandlerCatchMod.MonadMask m)
#endif
