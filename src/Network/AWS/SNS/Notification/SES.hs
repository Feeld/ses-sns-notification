{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Network.AWS.SNS.Notification.SES where


import           Control.Lens     (Lens', lens)
import           Data.Aeson       (FromJSON (..), Object, Value (String),
                                   withObject, withText, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import           Data.Text        (Text, unpack)
import           Data.Time        (UTCTime)
import           GHC.Generics     (Generic)

data Notification = Notification Mail NotificationStatus
  deriving (Eq, Show, Generic)

data NotificationStatus
  = StatusBounce    Bounce
  | StatusComplaint Complaint
  | StatusDelivery  Delivery
  deriving (Eq, Show, Generic)


instance FromJSON Notification where
  parseJSON = withObject "Notification" $ \o ->
    o .: "notificationType" >>= \case
      "Delivery"  -> Notification <$> o .: "mail" <*> (StatusDelivery  <$> o .: "delivery")
      "Bounce"    -> Notification <$> o .: "mail" <*> (StatusBounce    <$> o .: "bounce")
      "Complaint" -> Notification <$> o .: "mail" <*> (StatusComplaint <$> o .: "complaint")
      (other :: Text) -> fail $ unpack $ "Unknown notificationType: " <> other

mail :: Lens' Notification Mail
mail = lens (\(Notification m _) -> m) (\(Notification _ x) v -> Notification v x)
{-# INLINE mail #-}

notificationStatus :: Lens' Notification NotificationStatus
notificationStatus = lens (\(Notification _ x) -> x) (\(Notification x _) v -> Notification x v)
{-# INLINE notificationStatus #-}

--
-- Mail
--

data Mail = Mail
  { timestamp        :: UTCTime
  , messageId        :: Text
  , source           :: Text
  , sourceArn        :: Text
  , sourceIp         :: Text
  , sendingAccountId :: Text
  , destination      :: [Text]
  , headersTruncated :: Maybe Bool
  , headers          :: Maybe [Header]
  , commonHeaders    :: Maybe CommonHeaders
  }
  deriving (Eq, Show, Generic, FromJSON)

data CommonHeaders = CommonHeaders
  { from      :: [Text]
  , date      :: Maybe Text
  , to        :: [Text]
  , messageId :: Maybe Text
  , subject   :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

data Header = Header
  { name  :: Text
  , value :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

--
-- Bounce
--

data Bounce = Bounce
  { bounceType        :: BounceType
  , bouncedRecipients :: [BouncedRecipient]
  , timestamp         :: UTCTime
  , feedbackId        :: Text
  , remoteMtaIp       :: Maybe Text
  , reportingMTA      :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON Bounce where
  parseJSON = withObject "Bounce" $ \o -> do
    bounceType <- parseBounceType o
    bouncedRecipients <- o .: "bouncedRecipients"
    timestamp <- o .: "timestamp"
    feedbackId <- o .: "feedbackId"
    remoteMtaIp <- o .:? "remoteMtaIp"
    reportingMTA <- o .:? "reportingMTA"
    pure Bounce
      { bounceType
      , bouncedRecipients
      , timestamp
      , feedbackId
      , remoteMtaIp
      , reportingMTA
      }

-- This type combines both bounceType and bounceSubType so only valid
-- combinations can be represented
-- https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notification-contents.html#bounce-types
data BounceType
  = Undetermined
  | Permanent PermanentType
  | Transient TransientType
  deriving (Eq, Show, Generic)

data PermanentType
  = PermanentGeneral
  | PermanentNoEmail
  | PermanentSuppressed
  deriving (Eq, Show, Generic)

data TransientType
  = TransientGeneral
  | TransientMailboxFull
  | TransientMessageTooLarge
  | TransientContentRejected
  | TransientAttachmentRejected
  deriving (Eq, Show, Generic)


parseBounceType :: Object -> Parser BounceType
parseBounceType o = do
  type_ <- o .: "bounceType"
  case type_ of
    "Undetermined" -> pure Undetermined
    "Permanent" -> Permanent <$> (do
      subtype_ <- o .: "bounceSubType"
      case subtype_ of
        "General" -> pure PermanentGeneral
        "NoEmail" -> pure PermanentNoEmail
        "Suppressed" -> pure PermanentSuppressed
        String other -> fail $ unpack $ "Unknown Permanent subtype: " <> other
        _ -> fail "bounceSubType should be a string"
        )
    "Transient" -> Transient <$> (do
      subtype_ <- o .: "bounceSubType"
      case subtype_ of
        "General" -> pure TransientGeneral
        "MailboxFull" -> pure TransientMailboxFull
        "MessageTooLarge" -> pure TransientMessageTooLarge
        "ContentRejected" -> pure TransientContentRejected
        "AttachmentRejected" -> pure TransientAttachmentRejected
        String other -> fail $ unpack $ "Unknown Permanent subtype: " <> other
        _ -> fail "bounceSubType should be a string"
        )
    String other -> fail $ unpack $ "Unknown bounceType: " <> other
    _ -> fail "bounceType should be a string"

data BouncedRecipient = BouncedRecipient
  { emailAddress   :: Text
  , action         :: Maybe Text
  , status         :: Maybe Text
  , diagnosticCode :: Maybe Text
  } deriving (Eq, Show, Generic, FromJSON)




--
-- Complaint
--

data Complaint = Complaint
  { complainedRecipients  :: [ComplainedRecipient]
  , timestamp             :: UTCTime
  , feedbackId            :: Text
  , userAgent             :: Maybe Text
  , complaintFeedbackType :: Maybe ComplaintType
  , arrivalDate           :: Maybe UTCTime
  } deriving (Eq, Show, Generic, FromJSON)


newtype ComplainedRecipient = ComplainedRecipient
  { emailAddress :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ComplaintType
  = Abuse
  | AuthFailure
  | Fraud
  | NotSpam
  | Other
  | Virus
  deriving (Eq, Show, Generic)

instance FromJSON ComplaintType where
  parseJSON = withText "ComplaintType" $ \case
    "abuse" -> pure Abuse
    "auth-failure" -> pure AuthFailure
    "fraud" -> pure Fraud
    "not-spam" -> pure NotSpam
    "other" -> pure Other
    "virus" -> pure Virus
    _ -> pure Other -- Note that we fallback to Other to be a bit more robust to unexpected additions

--
-- Delivery
--

data Delivery = Delivery
  { timestamp            :: UTCTime
  , processingTimeMillis :: Int
  , recipients           :: [Text]
  , smtpResponse         :: Text
  , reportingMTA         :: Text
  , remoteMtaIp          :: Text
  } deriving (Eq, Show, Generic, FromJSON)
