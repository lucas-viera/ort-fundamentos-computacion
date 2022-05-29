{-# LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module TiposFinitos where 

data PCard where {
    Norte :: PCard;
    Sur :: PCard;
    Este :: PCard;
    Oeste :: PCard;
}
