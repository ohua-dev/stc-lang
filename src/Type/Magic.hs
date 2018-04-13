{-# LANGUAGE CPP              #-}

module Type.Magic (injectList, extractList, extractFunctor, injectFunctor) where


#if MIN_VERSION_base(4,10,0)
import           Type.Magic.GHC8   as X
#else
import           Type.Magic.OldGHC as X
#endif
