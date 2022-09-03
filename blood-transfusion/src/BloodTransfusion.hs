{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module BloodTransfusion where

import qualified Control.Exception         as CE (Exception (..))
import           Control.Monad             (join, (>=>))
import           Data.Barbie               (AllB, ConstraintsB, ProductB,
                                            TraversableB)
import qualified Data.Barbie               as B
import           Data.Bifoldable           (Bifoldable (..))
import           Data.Bifunctor            (Bifunctor (..), bimap, first)
import           Data.Bitraversable        (Bitraversable (..))
import           Data.Bool                 (bool)
import           Data.Constraint           (Dict (..))
import           Data.Function             (on)
import           Data.Functor.Const        (Const (..))
import           Data.Functor.Identity     (Identity (..))
import           Data.Generic.HKD
import           Data.Kind                 (Constraint, Type)
import           Data.List                 (elem)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Maybe                (maybe, maybeToList)
import           Data.Proxy
import           Data.Tagged
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Typeable             (typeOf)
import           Data.Validation
import           GHC.Generics              (Generic)
import           GHC.TypeLits
import qualified GHC.TypeLits              as TypeLits (ErrorMessage (..))
import           Refined                   hiding (NonEmpty)
import           Refined.These             ()


type family InvalidTreeThatGrows (tree :: Symbol)
                                 (idx  :: Symbol) where

  InvalidTreeThatGrows tree idx =
    TypeError
    (    'TypeLits.Text "Invalid Tree That Grows: "
    ':$$: 'TypeLits.Text tree
    ':<>: 'TypeLits.Text idx
    )

-- Q12.1
data RhDPresence = RhDNegative | RhDPositive
  deriving stock (Generic, Eq, Show)

data ABOBloodType = O | A | B | AB
  deriving stock (Generic, Eq, Show)

data RhABOBloodType = RhABOBloodType
  { canonGetABOBloodType :: ABOBloodType
  , canonGetRhDPresence  :: RhDPresence
  } deriving stock (Generic, Show)

newtype BloodType (idx :: Symbol) = BloodType
  { unBloodType :: BloodTypeX idx
  } deriving stock (Generic)

type family BloodTypeX (idx :: Symbol) :: Type where
  BloodTypeX "Canonical" = RhABOBloodType
  BloodTypeX idx =
    InvalidTreeThatGrows "BloodTypeX" idx

newtype Sex (idx :: Symbol) = Sex
  { unSex :: SexX idx
  } deriving stock (Generic)

type family SexX (idx :: Symbol) :: Type where
  SexX "BasicBinary" = BasicBinarySex
  SexX idx
    = InvalidTreeThatGrows "SexX" idx

data BasicBinarySex = Male | Female
  deriving stock (Generic, Eq, Show)

newtype Age (idx :: Symbol) = Age
  { unAge :: AgeX idx
  } deriving stock (Generic)

type family AgeX (idx :: Symbol) :: Type where
  AgeX "Years" = Refined Positive Int
  AgeX idx
    = InvalidTreeThatGrows "AgeX" idx

newtype Height (idx :: Symbol) = Height
  { unHeight :: HeightX idx
  } deriving stock (Generic)

type family HeightX (idx :: Symbol) :: Type where
  HeightX "Meters" = Refined Positive Float
  HeightX idx
    = InvalidTreeThatGrows "HeightX" idx

newtype Weight (idx :: Symbol) = Weight
  { unWeight :: WeightX idx
  } deriving stock (Generic)

type family WeightX (idx :: Symbol) :: Type where
  WeightX "Kg" = Refined Positive Float
  WeightX idx
    = InvalidTreeThatGrows "WeightX" idx

newtype Nickname = Nickname
  { unNickname :: Text
  } deriving stock (Generic)

newtype Document (idx :: Symbol) = Document
  { unDocument :: DocumentX idx
  } deriving stock (Generic)

type family DocumentX (idx :: Symbol) :: Type where
  DocumentX "Dummy" = ()
  DocumentX idx
    = InvalidTreeThatGrows "DocumentX" idx

data Patient (idx :: Symbol) = Patient
  { patientNickname  :: PNicknameX idx
  , patientDocument  :: PDocumentX idx
  , patientAge       :: PAgeX idx
  , patientSex       :: PSexX idx
  , patientHeight    :: PHeightX idx
  , patientWeight    :: PWeightX idx
  , patientBloodType :: PBloodTypeX idx
  } deriving stock (Generic)

type PatientF (idx :: Symbol) = HKD (Patient idx)

class PatientIndex (idx :: Symbol) where
  type PNicknameX idx  :: *
  type PDocumentX idx  :: *
  type PAgeX idx       :: *
  type PSexX idx       :: *
  type PHeightX idx    :: *
  type PWeightX idx    :: *
  type PBloodTypeX idx :: *

instance (idx ~ "Default") => PatientIndex idx where
  type PNicknameX idx  = Nickname
  type PDocumentX idx  = Document "Dummy"
  type PAgeX idx       = Age "Years"
  type PSexX idx       = Sex "BasicBinary"
  type PHeightX idx    = Height "Meters"
  type PWeightX idx    = Weight "Kg"
  type PBloodTypeX idx = BloodType "Canonical"

patientDefaultRhDPresence :: Patient "Default" -> RhDPresence
patientDefaultRhDPresence =
  canonGetRhDPresence . unBloodType . patientBloodType

patientDefaultABOBloodType :: Patient "Default" -> ABOBloodType
patientDefaultABOBloodType =
  canonGetABOBloodType . unBloodType . patientBloodType

type ValidationNel e = Validation (NonEmpty e)

type MapNel k e = Map k (NonEmpty e)

validationNelMapNel
  :: Ord k => ValidationNel (k, NonEmpty e) a -> ValidationNel (MapNel k e) a
validationNelMapNel = first (pure . M.fromList . NonEmpty.toList)

newtype ValidationTaggedExn e a = ValidationTaggedExn
  { unValidationTaggedExn :: Validation (Tagged '(a, "Exn") e) a
  } deriving stock (Generic, Eq, Show)
    deriving ( Semigroup
             , Monoid
             ) via (Validation e a)
    deriving ( Functor
             , Applicative
             , Foldable
             ) via (Validation e)
    deriving ( Bifunctor
             , Bifoldable
             ) via (Validation)

instance Bitraversable ValidationTaggedExn

type ValidationTaggedExnNel e a = ValidationTaggedExn (NonEmpty e) a

construct'
  :: Construct Identity structure => HKD structure Identity -> structure
construct' = runIdentity . construct

constructSequenced'
  :: ( structureF ~ HKD structure
     , Construct Identity structure
     , TraversableB structureF
     , Applicative f
     )
  => HKD structure f
  -> f structure
constructSequenced' = fmap construct' . B.bsequence'

mkAgeInYears :: Int -> ValidationNel RefineException (Age "Years")
mkAgeInYears = fmap Age . validationNel . refine

mkHeightInMeters :: Float -> ValidationNel RefineException (Height "Meters")
mkHeightInMeters = fmap Height . validationNel . refine

mkWeightInKg :: Float -> ValidationNel RefineException (Weight "Kg")
mkWeightInKg = fmap Weight . validationNel . refine

blabelValidationNel
  :: (structureF ~ HKD structure, ProductB structureF, Label structure)
  => structureF (ValidationNel e)
  -> structureF (ValidationNel (Text, NonEmpty e))
blabelValidationNel =
  B.bzipWith (first . fmap pure . (,) . T.pack . getConst) label

unravel'
  :: ( structureF ~ HKD structure
     , Construct Identity structure
     , ProductB structureF
     , TraversableB structureF
     , Label structure
     )
  => structureF (ValidationNel e)
  -> ValidationNel (MapNel Text e) structure
unravel' = validationNelMapNel . constructSequenced' . blabelValidationNel

unravelValidationTaggedExnNel'
  :: ( structureF ~ HKD structure
     , Construct Identity structure
     , ProductB structureF
     , TraversableB structureF
     , Label structure
     )
  => structureF (ValidationNel e)
  -> ValidationTaggedExnNel (MapNel Text e) structure
unravelValidationTaggedExnNel' = ValidationTaggedExn . first Tagged . unravel'

data BloodDonor a = BloodDonor'
  { _bloodDonorConstraints :: DictN (SingTransfusionPieceI a)
  , unBloodDonor           :: a
  } deriving stock (Generic)

pattern BloodDonor :: SingTransfusionPieceI a => a -> BloodDonor a
pattern BloodDonor a = BloodDonor' DictN a

data BloodReceiver a = BloodReceiver'
  { _bloodReceiverConstraints :: DictN (SingTransfusionPieceI a)
  , unBloodReceiver           :: a
  } deriving stock (Generic)

pattern BloodReceiver :: SingTransfusionPieceI a => a -> BloodReceiver a
pattern BloodReceiver a = BloodReceiver' DictN a

data SingTransfusionPiece a where
  SRhDPresence    :: SingTransfusionPiece RhDPresence
  SABOBloodType   :: SingTransfusionPiece ABOBloodType
  SPatientDefault :: SingTransfusionPiece (Patient "Default")

class SingTransfusionPieceI (a :: *) where
  singTransfusionPieceI :: SingTransfusionPiece a

instance SingTransfusionPieceI RhDPresence where
  singTransfusionPieceI = SRhDPresence

instance SingTransfusionPieceI ABOBloodType where
  singTransfusionPieceI = SABOBloodType

instance SingTransfusionPieceI (Patient "Default") where
  singTransfusionPieceI = SPatientDefault

data Transfusion (type' :: Symbol) (criteria' :: Symbol) = Transfusion'
  { _transfusionConstraints ::
      DictN ( TConstraints type' criteria'
            , Transfusionable type' criteria')

  , transfusionDocument     :: TDocumentX type' criteria'
  , transfusionDonor        :: TDonorX type' criteria'
  , transfusionReceiver     :: TReceiverX type' criteria'
  } deriving stock (Generic)

pattern Transfusion
  :: (TConstraints type' criteria', Transfusionable type' criteria')
  => TDocumentX type' criteria'
  -> TDonorX type' criteria'
  -> TReceiverX type' criteria'
  -> Transfusion type' criteria'
pattern Transfusion doc donor receiver = Transfusion' DictN doc donor receiver

type TransfusionF (type' :: Symbol) (criteria' :: Symbol) =
  HKD (Transfusion type' criteria')

type family TConstraints (type' :: Symbol)
                         (criteria' :: Symbol) :: Constraint where

  TConstraints type' criteria' =
    HasTransfusionMembers (Patient "Default") (Transfusion type' criteria')

class TransfusionIndex (type' :: Symbol) (criteria' :: Symbol) where
  type TDocumentX type' criteria' :: *
  type TDonorX    type' criteria' :: *
  type TReceiverX type' criteria' :: *

instance TransfusionIndex "RedBloodCell" "NonEmergency" where
  type TDocumentX "RedBloodCell" "NonEmergency" = Document "Dummy"
  type TDonorX    "RedBloodCell" "NonEmergency" = BloodDonor (Patient "Default")
  type TReceiverX "RedBloodCell" "NonEmergency" = BloodReceiver (Patient "Default")

instance TransfusionIndex "Plasma" "NonEmergency" where
  type TDocumentX "Plasma" "NonEmergency" = Document "Dummy"
  type TDonorX    "Plasma" "NonEmergency" = BloodDonor (Patient "Default")
  type TReceiverX "Plasma" "NonEmergency" = BloodReceiver (Patient "Default")

class HasTransfusionMembers member a where
  transfusionMembers :: a -> (BloodDonor member, BloodReceiver member)

instance HasTransfusionMembers (Patient "Default")
                               (Transfusion "RedBloodCell" "NonEmergency") where

  transfusionMembers = (,) <$> transfusionDonor <*> transfusionReceiver

instance HasTransfusionMembers (Patient "Default")
                               (Transfusion "Plasma" "NonEmergency") where

  transfusionMembers = (,) <$> transfusionDonor <*> transfusionReceiver

viewWrappedTransfusionPieces
  :: (SingTransfusionPieceI r)
  => (p -> r)
  -> (BloodDonor p, BloodReceiver p)
  -> (BloodDonor r, BloodReceiver r)
viewWrappedTransfusionPieces f =
  bimap (BloodDonor . f . unBloodDonor) (BloodReceiver . f . unBloodReceiver)

instance HasTransfusionMembers RhDPresence
                               (Transfusion "RedBloodCell" "NonEmergency") where

  transfusionMembers =
    viewWrappedTransfusionPieces patientDefaultRhDPresence . transfusionMembers

instance HasTransfusionMembers RhDPresence
                               (Transfusion "Plasma" "NonEmergency") where

  transfusionMembers =
    viewWrappedTransfusionPieces patientDefaultRhDPresence . transfusionMembers

instance HasTransfusionMembers ABOBloodType
                               (Transfusion "RedBloodCell" "NonEmergency") where

  transfusionMembers =
    viewWrappedTransfusionPieces patientDefaultABOBloodType
      . transfusionMembers

instance HasTransfusionMembers ABOBloodType
                               (Transfusion "Plasma" "NonEmergency") where

  transfusionMembers =
    viewWrappedTransfusionPieces patientDefaultABOBloodType
      . transfusionMembers

data RhDCompatibility (criteria' :: Symbol)

instance  ( transfusion ~ Transfusion type' "NonEmergency"
          , HasTransfusionMembers RhDPresence transfusion
          ) =>
          Predicate (RhDCompatibility "NonEmergency") transfusion where

  validate p =
    let
      validateRhDCompatibility = \case
        (BloodDonor RhDPositive, BloodReceiver RhDNegative) ->
          throwRefineOtherException
            (typeOf p)
            "(!) RhD incompatible: Donor is RhD positive, Receiver is RhD negative"
        _ -> return ()
    in
      validateRhDCompatibility . transfusionMembers

data RedBloodCellABOCompatibility (criteria' :: Symbol)

instance  ( transfusion ~ Transfusion type' "NonEmergency"
          , HasTransfusionMembers ABOBloodType transfusion
          ) =>
          Predicate (RedBloodCellABOCompatibility "NonEmergency")
                    transfusion where

  validate p =
    let validateRedBloodCellABOCompatibility = \case
          (BloodDonor O, _               ) -> return ()
          (_           , BloodReceiver AB) -> return ()
          (BloodDonor A, BloodReceiver A ) -> return ()
          (BloodDonor B, BloodReceiver B ) -> return ()
          (donor', receiver') ->
            throwRefineOtherException (typeOf p)
              $  "(!) Red blood cell incompatible: "
              <> "Donor is "
              <> pretty (unBloodDonor donor')
              <> " Receiver is "
              <> pretty (unBloodReceiver receiver')
    in  validateRedBloodCellABOCompatibility . transfusionMembers

data PlasmaABOCompatibility (criteria' :: Symbol)

instance  ( transfusion ~ Transfusion type' "NonEmergency"
          , HasTransfusionMembers ABOBloodType transfusion
          ) =>
          Predicate (PlasmaABOCompatibility "NonEmergency")
                    transfusion where

  validate p =
    let validatePlasmaABOCompatibility = \case
          (BloodDonor AB, _              ) -> return ()
          (_            , BloodReceiver O) -> return ()
          (BloodDonor A , BloodReceiver A) -> return ()
          (BloodDonor B , BloodReceiver B) -> return ()
          (donor', receiver') ->
            throwRefineOtherException (typeOf p)
              $  "(!) Plasma incompatible: "
              <> "Donor is "
              <> pretty (unBloodDonor donor')
              <> " Receiver is "
              <> pretty (unBloodReceiver receiver')
    in  validatePlasmaABOCompatibility . transfusionMembers

type family TransfusionPredicate (type' :: Symbol) (criteria' :: Symbol) where

  TransfusionPredicate "RedBloodCell" criteria' =
    RhDCompatibility criteria' && RedBloodCellABOCompatibility criteria'

  TransfusionPredicate "Plasma" criteria' =
    RhDCompatibility criteria' && PlasmaABOCompatibility criteria'

  TransfusionPredicate type' criteria' =
    TypeError
    (    'TypeLits.Text "Invalid TransfusionPredicate: "
    ':<>: 'TypeLits.Text type'
    ':<>: 'TypeLits.Text " "
    ':<>: 'TypeLits.Text criteria'
    )

class Transfusionable (type' :: Symbol) (criteria' :: Symbol) where

  refineTransfusion
    :: (transfusion ~ Transfusion type' criteria')
    => transfusion
    -> Either
         RefineException
         (Refined (TransfusionPredicate type' criteria') transfusion)

instance Transfusionable "RedBloodCell" "NonEmergency" where
  refineTransfusion = refine

instance Transfusionable "Plasma" "NonEmergency" where
  refineTransfusion = refine

type RefinedTransfusion (type' :: Symbol) (criteria' :: Symbol) =
  Refined (TransfusionPredicate type' criteria')
          (Transfusion type' criteria')

data RefineTransfusionExn where
  RefineTransfusionPrettyExn :: PrettyExn -> RefineTransfusionExn
  RefineTransfusionExn
    :: (transfusion ~ Transfusion type' criteria', Pretty transfusion)
    => transfusion
    -> RefineException
    -> RefineTransfusionExn

instance Show RefineTransfusionExn where
  show = show . pretty

instance CE.Exception RefineTransfusionExn

-- Q12.2
refineExceptionDocs' :: RefineException -> [Doc ann]
refineExceptionDocs' = \case
  RefineNotException _       -> []
  RefineOtherException _ doc -> pure $ PP.unAnnotate doc
  RefineOrException _ l r    -> on (<>) refineExceptionDocs' l r
  RefineAndException _ child -> join bifoldMap refineExceptionDocs' child

instance Pretty RefineTransfusionExn where
  pretty (RefineTransfusionPrettyExn e) = pretty e
  pretty (RefineTransfusionExn transfusion refineExn) =
    let refineExnDoc = PP.indent 2 $ PP.vsep $ refineExceptionDocs' refineExn
    in  "*** Refine exception: "
        <> doubleHardline
        <> refineExnDoc
        <> doubleHardline
        <> "*** Rejected transfusion: "
        <> doubleHardline
        <> (PP.indent 2 $ pretty transfusion)

bpretty
  :: ( structureF ~ HKD structure
     , AllB Pretty structureF
     , Construct Identity structure
     , ConstraintsB structureF
     )
  => structure
  -> structureF (Const (Doc ann))
bpretty =
  B.bmapC @Pretty (Const . pretty . runIdentity) . deconstruct @Identity

bprettyLabeled
  :: ( structureF ~ HKD structure
     , AllB Pretty structureF
     , Construct Identity structure
     , ConstraintsB structureF
     , Label structure
     , ProductB structureF
     )
  => structure
  -> structureF (Const (Text, Doc ann))
bprettyLabeled = B.bzipWith (first . (,) . T.pack . getConst) label . bpretty

assembleLabeled
  :: (((Text, a) -> Maybe (Text, a)) -> t)
  -> Tagged "StripPrefix" Text
  -> Tagged "ExcludeFields" [Text]
  -> t
assembleLabeled f (Tagged prefix) (Tagged excludedFields) =
  let allButExcluded = (flip elem excludedFields) . fst >>= bool
        (Just . first (stripPrefix' prefix))
        (const Nothing)
  in  f allButExcluded

bassembleLabeled
  :: (structureF ~ HKD structure, TraversableB structureF)
  => Tagged "StripPrefix" Text
  -> Tagged "ExcludeFields" [Text]
  -> structureF (Const (Text, a))
  -> [(Text, a)]
bassembleLabeled =
  assembleLabeled (\f -> B.bfoldMap (maybeToList . f . getConst))

massembleLabeled
  :: Tagged "StripPrefix" Text
  -> Tagged "ExcludeFields" [Text]
  -> Map Text a
  -> [(Text, a)]
massembleLabeled =
  assembleLabeled (\f -> M.foldMapWithKey (fmap (maybeToList . f) . (,)))

type PrettyFormLine ann =
  Tagged "Form" [(Text, Doc ann)] -> Text -> Doc ann -> Doc ann

indentedInline :: PrettyFormLine ann
indentedInline (Tagged form) =
  let maxLabelLen = maximum $ T.length . fst <$> form
      prettyLine label' value = PP.width
        (pretty label' <> ": ")
        (\w -> PP.indent ((maxLabelLen + 2) - w) value)
  in  prettyLine

exnFormLine :: PrettyFormLine ann
exnFormLine =
  let prettyLine label' value =
        join PP.surround PP.hardline
          $  "- "
          <> pretty label'
          <> ": "
          <> doubleHardline
          <> PP.indent 2 value
  in  const prettyLine

extraIndent :: Int -> PrettyFormLine ann -> PrettyFormLine ann
extraIndent n prettyLine form label' value =
  prettyLine form label' (PP.indent n value)

prettyFormLines
  :: PrettyFormLine ann -> Tagged "Form" [(Text, Doc ann)] -> [Doc ann]
prettyFormLines prettyLine form = uncurry (prettyLine form) <$> (untag form)

stripPrefix' :: Text -> Text -> Text
stripPrefix' = (flip maybe id <*>) . T.stripPrefix

doubleHardline :: Doc ann
doubleHardline = join (<>) PP.hardline

instance Pretty ABOBloodType where pretty = PP.viaShow
instance Pretty RhDPresence where
  pretty RhDPositive = "+"
  pretty RhDNegative = "-"

instance Pretty RhABOBloodType where
  pretty =
    (<>) <$> pretty . canonGetABOBloodType <*> pretty . canonGetRhDPresence

instance Pretty BasicBinarySex where pretty = PP.viaShow

instance Pretty Nickname where pretty = pretty . unNickname
instance Pretty (Document "Dummy") where pretty = pretty . unDocument
instance Pretty (Sex "BasicBinary") where pretty = pretty . unSex
instance Pretty (Age "Years") where pretty = pretty . unrefine . unAge
instance Pretty (Height "Meters") where
  pretty = (<> " m") . pretty . unrefine . unHeight
instance Pretty (Weight "Kg") where
  pretty = (<> " kg") . pretty . unrefine . unWeight
instance Pretty (BloodType "Canonical") where pretty = pretty . unBloodType

instance Pretty a => Pretty (BloodDonor a) where
  pretty = pretty . unBloodDonor
instance Pretty a => Pretty (BloodReceiver a) where
  pretty = pretty . unBloodReceiver

instance (AllB Pretty (PatientF idx))
         => Pretty (Patient idx) where

  pretty =
    PP.vsep
      . prettyFormLines indentedInline
      . Tagged @"Form"
      . bassembleLabeled (Tagged @"StripPrefix" "patient")
                         (Tagged @"ExcludeFields" ["patientDocument"])
      . bprettyLabeled

-- To help satisfy (AllB c) for constrained hkd
newtype DictN a = DictN'
  { unDictN :: (Dict a)
  }

pattern DictN :: a => DictN a
pattern DictN = DictN' Dict

instance Pretty (DictN a) where
  pretty = const "Pretty (DictN a): This should be internal..."

proxyFromTagged :: Tagged s a -> Proxy s
proxyFromTagged = const Proxy

proxyListHead :: Proxy (x : xs) -> Proxy x
proxyListHead = reproxy

proxyListTail :: Proxy (x : xs) -> Proxy xs
proxyListTail = reproxy

class SymbolVals (xs :: [Symbol]) where
  symbolVals :: Proxy xs -> [Text]

instance SymbolVals '[] where
  symbolVals = const []

instance (KnownSymbol x, SymbolVals xs) => SymbolVals (x : xs) where
  symbolVals =
    (:)
      <$> (T.pack . symbolVal . proxyListHead)
      <*> (symbolVals . proxyListTail)

captionTtg :: Tagged "Root" Text -> Tagged "Labels" [Text] -> Text
captionTtg (Tagged root) (Tagged labels) =
  T.concat [root, " ", T.intercalate " " $ T.pack . show <$> labels]

prettyCaptionTtg :: Tagged "Root" Text -> Tagged "Labels" [Text] -> Doc ann
prettyCaptionTtg = fmap pretty . captionTtg

captionConstructionExn
  :: Tagged "Root" Text -> Tagged "Labels" [Text] -> Doc ann -> Doc ann
captionConstructionExn root labels =
  (<>) $ "* Error constructing `" <> prettyCaptionTtg root labels <> "`: "

constructionExn
  :: SymbolVals xs => Tagged "Root" Text -> Tagged xs a -> Doc ann -> Doc ann
constructionExn root =
  captionConstructionExn root
    <$> Tagged @"Labels"
    .   tail
    .   symbolVals
    .   proxyFromTagged

prettyTaggedMapNelTextExn
  :: (SymbolVals xs, Pretty e)
  => Tagged "Root" Text
  -> Tagged "StripPrefix" Text
  -> Tagged "ExcludeFields" [Text]
  -> Tagged xs (MapNel Text e)
  -> Doc ann
prettyTaggedMapNelTextExn root stripPrefix excludeFields =
  let prettyExn =
        PP.vsep
          . prettyFormLines exnFormLine
          . Tagged @"Form"
          . massembleLabeled stripPrefix excludeFields
          . fmap (foldMap pretty)
          . untag
  in  constructionExn root <*> prettyExn

instance (KnownSymbol idx, Pretty e)
         => Pretty (Tagged '(Patient idx, "Exn") (MapNel Text e)) where

  pretty =
    let retag'
          :: Tagged '(Patient idx, "Exn") a -> Tagged '["PatientExn", idx] a
        retag' = retag
    in  prettyTaggedMapNelTextExn (Tagged @"Root" "Patient")
                                  (Tagged @"StripPrefix" "patient")
                                  (Tagged @"ExcludeFields" [])
          . retag'

instance (KnownSymbol idx, Pretty e)
         => Pretty (Tagged '(Patient idx, "Exn") (NonEmpty (MapNel Text e))) where

  pretty = foldMap pretty . sequenceA

instance (KnownSymbol type', KnownSymbol criteria', Pretty e)
         => Pretty (Tagged '(Transfusion type' criteria', "Exn")
                           (MapNel Text e)) where

  pretty =
    let retag'
          :: Tagged '(Transfusion type' criteria', "Exn") a
          -> Tagged '["TransfusionExn", type', criteria'] a
        retag' = retag
    in  prettyTaggedMapNelTextExn
            (Tagged @"Root" "Transfusion")
            (Tagged @"StripPrefix" "transfusion")
            (Tagged @"ExcludeFields" ["_transfusionConstraints"])
          . retag'

instance (KnownSymbol type', KnownSymbol criteria', Pretty e)
         => Pretty (Tagged '(Transfusion type' criteria', "Exn")
                           (NonEmpty (MapNel Text e))) where

  pretty = foldMap pretty . sequenceA

data PrettyExn where
  PrettyExn :: Pretty e => e -> PrettyExn

validationNelPretty'
  :: Pretty (Tagged '(a, "Exn") e)
  => ValidationTaggedExnNel e a
  -> ValidationNel PrettyExn a
validationNelPretty' =
  first (fmap PrettyExn . sequenceA) . unValidationTaggedExn

instance Pretty PrettyExn where
  pretty (PrettyExn e) = pretty e

prettyValidationTaggedExnNel'
  :: (Pretty a, Pretty (Tagged '(a, "Exn") (NonEmpty e)))
  => ValidationTaggedExnNel e a
  -> Doc ann
prettyValidationTaggedExnNel' =
  validation pretty pretty . unValidationTaggedExn

transfusionSymbols :: Transfusion type' criteria' -> Proxy '[type', criteria']
transfusionSymbols = const Proxy

instance ( KnownSymbol type'
         , KnownSymbol criteria'
         , AllB Pretty (TransfusionF type' criteria')
         ) =>
         Pretty (Transfusion type' criteria') where

  pretty =
    let caption labels = (<>) $ prettyCaptionTtg
          (Tagged @"Root" "Blood transfusion")
          (Tagged @"Labels" labels) <> ": " <> doubleHardline
        pretty' =
          PP.concatWith (PP.surround doubleHardline)
            . prettyFormLines (extraIndent 2 indentedInline)
            . Tagged @"Form"
            . bassembleLabeled
                (Tagged @"StripPrefix" "transfusion")
                (Tagged @"ExcludeFields"
                        ["_transfusionConstraints", "transfusionDocument"]
                )
            . bprettyLabeled
    in  caption <$> symbolVals . transfusionSymbols <*> pretty'

unravelRefinedTransfusion'
  :: ( Transfusionable type' criteria'
     , Pretty (Transfusion type' criteria')
     , Pretty (Tagged '(Transfusion type' criteria', "Exn") (NonEmpty e))
     )
  => ValidationTaggedExnNel e (Transfusion type' criteria')
  -> Either RefineTransfusionExn (RefinedTransfusion type' criteria')
unravelRefinedTransfusion' =
  toEither
    .   first (RefineTransfusionPrettyExn . PrettyExn)
    .   unValidationTaggedExn
    >=> first
    <$> RefineTransfusionExn
    <*> refineTransfusion

--------------------------------------------------------------------------------
p0 :: ValidationTaggedExnNel (MapNel Text RefineException) (Patient "Default")
p0 = unravelValidationTaggedExnNel' $ build
  @(Patient "Default")
  (pure $ Nickname "Leo")
  (pure $ Document ())
  (mkAgeInYears 25)
  (pure $ Sex Male)
  (mkHeightInMeters 1.87)
  (mkWeightInKg 104)
  (pure $ BloodType $ RhABOBloodType A RhDPositive)

p1 :: ValidationTaggedExnNel (MapNel Text RefineException) (Patient "Default")
p1 = unravelValidationTaggedExnNel' $ build
  @(Patient "Default")
  (pure $ Nickname "Bob Sacamano")
  (pure $ Document ())
  (mkAgeInYears 42)
  (pure $ Sex Male)
  (mkHeightInMeters 1.75)
  (mkWeightInKg 82)
  (pure $ BloodType $ RhABOBloodType O RhDNegative)

t0
  :: Either
       RefineTransfusionExn
       (RefinedTransfusion "RedBloodCell" "NonEmergency")
t0 = unravelRefinedTransfusion' $ unravelValidationTaggedExnNel' $ build
  @(Transfusion "RedBloodCell" "NonEmergency")
  (pure DictN)
  (pure $ Document ())
  (BloodDonor <$> validationNelPretty' p0)
  (BloodReceiver <$> validationNelPretty' p1)

-- either pretty (pretty . unrefine) t0
