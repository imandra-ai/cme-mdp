
type t_Asset = char list
type t_CFICode = char list
type t_CHAR = char
type t_Currency = char list
type t_InstAttribType = int
type t_Int16 = int
type t_Int32 = int32
type t_Int32NULL = int32 option
type t_Int8 = int
type t_Int8NULL = int option
type t_LocalMktDate = int option
type t_MDEntryTypeChannelReset = char
type t_MDEntryTypeLimits = char
type t_MDEntryTypeTrade = char
type t_MDEntryTypeVol = char
type t_MDFeedType = char list
type t_MDUpdateActionNew = int
type t_MDUpdateTypeNew = int
type t_QuoteReqId = char list
type t_SecurityExchange = char list
type t_SecurityGroup = char list
type t_SecurityIDSource = char
type t_SecuritySubType = char list
type t_SecurityType = char list
type t_Symbol = char list
type t_Text = char list
type t_UnderlyingSymbol = char list
type t_UnitOfMeasure = char list
type t_UserDefinedInstrument = char
type t_uInt32 = int32
type t_uInt32NULL = int32 option
type t_uInt64 = int64
type t_uInt8 = int
type t_uInt8NULL = int option
type t_DecimalQty = {
     f_DecimalQty_mantissa : int32 option;
     f_DecimalQty_exponent : int
}
type t_FLOAT = {
     f_FLOAT_mantissa : int64;
     f_FLOAT_exponent : int
}
type t_MaturityMonthYear = {
     f_MaturityMonthYear_year : int option;
     f_MaturityMonthYear_month : int option;
     f_MaturityMonthYear_day : int option;
     f_MaturityMonthYear_week : int option
}
type t_PRICE = {
     f_PRICE_mantissa : int64;
     f_PRICE_exponent : int
}
type t_PRICENULL = {
     f_PRICENULL_mantissa : int64 option;
     f_PRICENULL_exponent : int
}
type t_groupSize = {
     f_groupSize_blockLength : int;
     f_groupSize_numInGroup : int
}
type t_groupSize8Byte = {
     f_groupSize8Byte_blockLength : int;
     f_groupSize8Byte_numInGroup : int
}
type t_groupSizeEncoding = {
     f_groupSizeEncoding_blockLength : int;
     f_groupSizeEncoding_numInGroup : int
}
type t_messageHeader = {
     f_messageHeader_blockLength : int;
     f_messageHeader_templateId : int;
     f_messageHeader_schemaId : int;
     f_messageHeader_version : int
}
type t_AggressorSide =
     | V_AggressorSide_NoAggressor
     | V_AggressorSide_Buy
     | V_AggressorSide_Sell
     | V_AggressorSide_Null
type t_EventType =
     | V_EventType_Activation
     | V_EventType_LastEligibleTradeDate
type t_HaltReason =
     | V_HaltReason_GroupSchedule
     | V_HaltReason_SurveillanceIntervention
     | V_HaltReason_MarketEvent
     | V_HaltReason_InstrumentActivation
     | V_HaltReason_InstrumentExpiration
     | V_HaltReason_Unknown
     | V_HaltReason_RecoveryInProcess
type t_LegSide =
     | V_LegSide_BuySide
     | V_LegSide_SellSide
type t_MDEntryType =
     | V_MDEntryType_Bid
     | V_MDEntryType_Offer
     | V_MDEntryType_Trade
     | V_MDEntryType_OpenPrice
     | V_MDEntryType_SettlementPrice
     | V_MDEntryType_TradingSessionHighPrice
     | V_MDEntryType_TradingSessionLowPrice
     | V_MDEntryType_ClearedVolume
     | V_MDEntryType_OpenInterest
     | V_MDEntryType_ImpliedBid
     | V_MDEntryType_ImpliedOffer
     | V_MDEntryType_BookReset
     | V_MDEntryType_SessionHighBid
     | V_MDEntryType_SessionLowOffer
     | V_MDEntryType_FixingPrice
     | V_MDEntryType_ElectronicVolume
     | V_MDEntryType_ThresholdLimitsandPriceBandVariation
type t_MDEntryTypeBook =
     | V_MDEntryTypeBook_Bid
     | V_MDEntryTypeBook_Offer
     | V_MDEntryTypeBook_ImpliedBid
     | V_MDEntryTypeBook_ImpliedOffer
     | V_MDEntryTypeBook_BookReset
type t_MDEntryTypeDailyStatistics =
     | V_MDEntryTypeDailyStatistics_SettlementPrice
     | V_MDEntryTypeDailyStatistics_ClearedVolume
     | V_MDEntryTypeDailyStatistics_OpenInterest
     | V_MDEntryTypeDailyStatistics_FixingPrice
type t_MDEntryTypeStatistics =
     | V_MDEntryTypeStatistics_OpenPrice
     | V_MDEntryTypeStatistics_HighTrade
     | V_MDEntryTypeStatistics_LowTrade
     | V_MDEntryTypeStatistics_HighestBid
     | V_MDEntryTypeStatistics_LowestOffer
type t_MDUpdateAction =
     | V_MDUpdateAction_New
     | V_MDUpdateAction_Change
     | V_MDUpdateAction_Delete
     | V_MDUpdateAction_DeleteThru
     | V_MDUpdateAction_DeleteFrom
     | V_MDUpdateAction_Overlay
type t_OpenCloseSettlFlag =
     | V_OpenCloseSettlFlag_DailyOpenPrice
     | V_OpenCloseSettlFlag_IndicativeOpeningPrice
     | V_OpenCloseSettlFlag_Null
type t_PutOrCall =
     | V_PutOrCall_Put
     | V_PutOrCall_Call
type t_SecurityTradingEvent =
     | V_SecurityTradingEvent_NoEvent
     | V_SecurityTradingEvent_NoCancel
     | V_SecurityTradingEvent_ResetStatistics
     | V_SecurityTradingEvent_ImpliedMatchingON
     | V_SecurityTradingEvent_ImpliedMatchingOFF
type t_SecurityTradingStatus =
     | V_SecurityTradingStatus_TradingHalt
     | V_SecurityTradingStatus_Close
     | V_SecurityTradingStatus_NewPriceIndication
     | V_SecurityTradingStatus_ReadyToTrade
     | V_SecurityTradingStatus_NotAvailableForTrading
     | V_SecurityTradingStatus_UnknownorInvalid
     | V_SecurityTradingStatus_PreOpen
     | V_SecurityTradingStatus_PreCross
     | V_SecurityTradingStatus_Cross
     | V_SecurityTradingStatus_PostClose
     | V_SecurityTradingStatus_NoChange
     | V_SecurityTradingStatus_Null
type t_SecurityUpdateAction =
     | V_SecurityUpdateAction_Add
     | V_SecurityUpdateAction_Delete
     | V_SecurityUpdateAction_Modify
type t_InstAttribValue = { 
    r_InstAttribValue_ElectronicMatchEligible : bool;
    r_InstAttribValue_OrderCrossEligible : bool;
    r_InstAttribValue_BlockTradeEligible : bool;
    r_InstAttribValue_EFPEligible : bool;
    r_InstAttribValue_EBFEligible : bool;
    r_InstAttribValue_EFSEligible : bool;
    r_InstAttribValue_EFREligible : bool;
    r_InstAttribValue_OTCEligible : bool;
    r_InstAttribValue_iLinkIndicativeMassQuotingEligible : bool;
    r_InstAttribValue_NegativeStrikeEligible : bool;
    r_InstAttribValue_NegativePriceOutrightEligible : bool;
    r_InstAttribValue_IsFractional : bool;
    r_InstAttribValue_VolatilityQuotedOption : bool;
    r_InstAttribValue_RFQCrossEligible : bool;
    r_InstAttribValue_ZeroPriceOutrightEligible : bool;
    r_InstAttribValue_DecayingProductEligibility : bool;
    r_InstAttribValue_VariableProductEligibility : bool;
    r_InstAttribValue_DailyProductEligibility : bool;
    r_InstAttribValue_GTOrdersEligibility : bool;
    r_InstAttribValue_ImpliedMatchingEligibility : bool
}
type t_MatchEventIndicator = { 
    r_MatchEventIndicator_LastTradeMsg : bool;
    r_MatchEventIndicator_LastVolumeMsg : bool;
    r_MatchEventIndicator_LastQuoteMsg : bool;
    r_MatchEventIndicator_LastStatsMsg : bool;
    r_MatchEventIndicator_LastImpliedMsg : bool;
    r_MatchEventIndicator_RecoveryMsg : bool;
    r_MatchEventIndicator_Reserved : bool;
    r_MatchEventIndicator_EndOfEvent : bool
}
type t_SettlPriceType = { 
    r_SettlPriceType_Final : bool;
    r_SettlPriceType_Actual : bool;
    r_SettlPriceType_Rounded : bool;
    r_SettlPriceType_Intraday : bool;
    r_SettlPriceType_ReservedBits : bool;
    r_SettlPriceType_NullValue : bool
}
type g_ChannelReset4_NoMDEntries = {
    f_ChannelReset4_NoMDEntries_MDUpdateAction: t_MDUpdateTypeNew;
    f_ChannelReset4_NoMDEntries_MDEntryType: t_MDEntryTypeChannelReset;
    f_ChannelReset4_NoMDEntries_ApplID: t_Int16
}

type msg_ChannelReset4 = { 
    f_ChannelReset4_TransactTime: t_uInt64;
    f_ChannelReset4_MatchEventIndicator: t_MatchEventIndicator;
    f_ChannelReset4_NoMDEntries: g_ChannelReset4_NoMDEntries list
}
type msg_AdminHeartbeat12 = unit 


type msg_AdminLogin15 = { 
    f_AdminLogin15_HeartBtInt: t_Int8
}

type msg_AdminLogout16 = { 
    f_AdminLogout16_Text: t_Text
}
type g_MDInstrumentDefinitionFuture27_NoEvents = {
    f_MDInstrumentDefinitionFuture27_NoEvents_EventType: t_EventType;
    f_MDInstrumentDefinitionFuture27_NoEvents_EventTime: t_uInt64
}
type g_MDInstrumentDefinitionFuture27_NoMDFeedTypes = {
    f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MDFeedType: t_MDFeedType;
    f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MarketDepth: t_Int8
}
type g_MDInstrumentDefinitionFuture27_NoInstAttrib = {
    f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribType: t_InstAttribType;
    f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribValue: t_InstAttribValue
}
type g_MDInstrumentDefinitionFuture27_NoLotTypeRules = {
    f_MDInstrumentDefinitionFuture27_NoLotTypeRules_LotType: t_Int8;
    f_MDInstrumentDefinitionFuture27_NoLotTypeRules_MinLotSize: t_DecimalQty
}

type msg_MDInstrumentDefinitionFuture27 = { 
    f_MDInstrumentDefinitionFuture27_MatchEventIndicator: t_MatchEventIndicator;
    f_MDInstrumentDefinitionFuture27_TotNumReports: t_uInt32NULL;
    f_MDInstrumentDefinitionFuture27_SecurityUpdateAction: t_SecurityUpdateAction;
    f_MDInstrumentDefinitionFuture27_LastUpdateTime: t_uInt64;
    f_MDInstrumentDefinitionFuture27_MDSecurityTradingStatus: t_SecurityTradingStatus;
    f_MDInstrumentDefinitionFuture27_ApplID: t_Int16;
    f_MDInstrumentDefinitionFuture27_MarketSegmentID: t_uInt8;
    f_MDInstrumentDefinitionFuture27_UnderlyingProduct: t_uInt8;
    f_MDInstrumentDefinitionFuture27_SecurityExchange: t_SecurityExchange;
    f_MDInstrumentDefinitionFuture27_SecurityGroup: t_SecurityGroup;
    f_MDInstrumentDefinitionFuture27_Asset: t_Asset;
    f_MDInstrumentDefinitionFuture27_Symbol: t_Symbol;
    f_MDInstrumentDefinitionFuture27_SecurityID: t_Int32;
    f_MDInstrumentDefinitionFuture27_SecurityIDSource: t_SecurityIDSource;
    f_MDInstrumentDefinitionFuture27_SecurityType: t_SecurityType;
    f_MDInstrumentDefinitionFuture27_CFICode: t_CFICode;
    f_MDInstrumentDefinitionFuture27_MaturityMonthYear: t_MaturityMonthYear;
    f_MDInstrumentDefinitionFuture27_Currency: t_Currency;
    f_MDInstrumentDefinitionFuture27_SettlCurrency: t_Currency;
    f_MDInstrumentDefinitionFuture27_MatchAlgorithm: t_CHAR;
    f_MDInstrumentDefinitionFuture27_MinTradeVol: t_uInt32;
    f_MDInstrumentDefinitionFuture27_MaxTradeVol: t_uInt32;
    f_MDInstrumentDefinitionFuture27_MinPriceIncrement: t_PRICE;
    f_MDInstrumentDefinitionFuture27_DisplayFactor: t_FLOAT;
    f_MDInstrumentDefinitionFuture27_MainFraction: t_uInt8NULL;
    f_MDInstrumentDefinitionFuture27_SubFraction: t_uInt8NULL;
    f_MDInstrumentDefinitionFuture27_PriceDisplayFormat: t_uInt8NULL;
    f_MDInstrumentDefinitionFuture27_UnitOfMeasure: t_UnitOfMeasure;
    f_MDInstrumentDefinitionFuture27_UnitOfMeasureQty: t_PRICENULL;
    f_MDInstrumentDefinitionFuture27_TradingReferencePrice: t_PRICENULL;
    f_MDInstrumentDefinitionFuture27_SettlPriceType: t_SettlPriceType;
    f_MDInstrumentDefinitionFuture27_OpenInterestQty: t_Int32NULL;
    f_MDInstrumentDefinitionFuture27_ClearedVolume: t_Int32NULL;
    f_MDInstrumentDefinitionFuture27_HighLimitPrice: t_PRICENULL;
    f_MDInstrumentDefinitionFuture27_LowLimitPrice: t_PRICENULL;
    f_MDInstrumentDefinitionFuture27_MaxPriceVariation: t_PRICENULL;
    f_MDInstrumentDefinitionFuture27_DecayQuantity: t_Int32NULL;
    f_MDInstrumentDefinitionFuture27_DecayStartDate: t_LocalMktDate;
    f_MDInstrumentDefinitionFuture27_OriginalContractSize: t_Int32NULL;
    f_MDInstrumentDefinitionFuture27_ContractMultiplier: t_Int32NULL;
    f_MDInstrumentDefinitionFuture27_ContractMultiplierUnit: t_Int8NULL;
    f_MDInstrumentDefinitionFuture27_FlowScheduleType: t_Int8NULL;
    f_MDInstrumentDefinitionFuture27_MinPriceIncrementAmount: t_PRICENULL;
    f_MDInstrumentDefinitionFuture27_UserDefinedInstrument: t_UserDefinedInstrument;
    f_MDInstrumentDefinitionFuture27_TradingReferenceDate: t_LocalMktDate;
    f_MDInstrumentDefinitionFuture27_NoEvents: g_MDInstrumentDefinitionFuture27_NoEvents list;
    f_MDInstrumentDefinitionFuture27_NoMDFeedTypes: g_MDInstrumentDefinitionFuture27_NoMDFeedTypes list;
    f_MDInstrumentDefinitionFuture27_NoInstAttrib: g_MDInstrumentDefinitionFuture27_NoInstAttrib list;
    f_MDInstrumentDefinitionFuture27_NoLotTypeRules: g_MDInstrumentDefinitionFuture27_NoLotTypeRules list
}
type g_MDInstrumentDefinitionSpread29_NoEvents = {
    f_MDInstrumentDefinitionSpread29_NoEvents_EventType: t_EventType;
    f_MDInstrumentDefinitionSpread29_NoEvents_EventTime: t_uInt64
}
type g_MDInstrumentDefinitionSpread29_NoMDFeedTypes = {
    f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MDFeedType: t_MDFeedType;
    f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MarketDepth: t_Int8
}
type g_MDInstrumentDefinitionSpread29_NoInstAttrib = {
    f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribType: t_InstAttribType;
    f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribValue: t_InstAttribValue
}
type g_MDInstrumentDefinitionSpread29_NoLotTypeRules = {
    f_MDInstrumentDefinitionSpread29_NoLotTypeRules_LotType: t_Int8;
    f_MDInstrumentDefinitionSpread29_NoLotTypeRules_MinLotSize: t_DecimalQty
}
type g_MDInstrumentDefinitionSpread29_NoLegs = {
    f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityID: t_Int32;
    f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityIDSource: t_SecurityIDSource;
    f_MDInstrumentDefinitionSpread29_NoLegs_LegSide: t_LegSide;
    f_MDInstrumentDefinitionSpread29_NoLegs_LegRatioQty: t_Int8;
    f_MDInstrumentDefinitionSpread29_NoLegs_LegPrice: t_PRICENULL;
    f_MDInstrumentDefinitionSpread29_NoLegs_LegOptionDelta: t_DecimalQty
}

type msg_MDInstrumentDefinitionSpread29 = { 
    f_MDInstrumentDefinitionSpread29_MatchEventIndicator: t_MatchEventIndicator;
    f_MDInstrumentDefinitionSpread29_TotNumReports: t_uInt32NULL;
    f_MDInstrumentDefinitionSpread29_SecurityUpdateAction: t_SecurityUpdateAction;
    f_MDInstrumentDefinitionSpread29_LastUpdateTime: t_uInt64;
    f_MDInstrumentDefinitionSpread29_MDSecurityTradingStatus: t_SecurityTradingStatus;
    f_MDInstrumentDefinitionSpread29_ApplID: t_Int16;
    f_MDInstrumentDefinitionSpread29_MarketSegmentID: t_uInt8;
    f_MDInstrumentDefinitionSpread29_UnderlyingProduct: t_uInt8NULL;
    f_MDInstrumentDefinitionSpread29_SecurityExchange: t_SecurityExchange;
    f_MDInstrumentDefinitionSpread29_SecurityGroup: t_SecurityGroup;
    f_MDInstrumentDefinitionSpread29_Asset: t_Asset;
    f_MDInstrumentDefinitionSpread29_Symbol: t_Symbol;
    f_MDInstrumentDefinitionSpread29_SecurityID: t_Int32;
    f_MDInstrumentDefinitionSpread29_SecurityIDSource: t_SecurityIDSource;
    f_MDInstrumentDefinitionSpread29_SecurityType: t_SecurityType;
    f_MDInstrumentDefinitionSpread29_CFICode: t_CFICode;
    f_MDInstrumentDefinitionSpread29_MaturityMonthYear: t_MaturityMonthYear;
    f_MDInstrumentDefinitionSpread29_Currency: t_Currency;
    f_MDInstrumentDefinitionSpread29_SecuritySubType: t_SecuritySubType;
    f_MDInstrumentDefinitionSpread29_UserDefinedInstrument: t_UserDefinedInstrument;
    f_MDInstrumentDefinitionSpread29_MatchAlgorithm: t_CHAR;
    f_MDInstrumentDefinitionSpread29_MinTradeVol: t_uInt32;
    f_MDInstrumentDefinitionSpread29_MaxTradeVol: t_uInt32;
    f_MDInstrumentDefinitionSpread29_MinPriceIncrement: t_PRICE;
    f_MDInstrumentDefinitionSpread29_DisplayFactor: t_FLOAT;
    f_MDInstrumentDefinitionSpread29_PriceDisplayFormat: t_uInt8NULL;
    f_MDInstrumentDefinitionSpread29_PriceRatio: t_PRICENULL;
    f_MDInstrumentDefinitionSpread29_TickRule: t_Int8NULL;
    f_MDInstrumentDefinitionSpread29_UnitOfMeasure: t_UnitOfMeasure;
    f_MDInstrumentDefinitionSpread29_TradingReferencePrice: t_PRICENULL;
    f_MDInstrumentDefinitionSpread29_SettlPriceType: t_SettlPriceType;
    f_MDInstrumentDefinitionSpread29_OpenInterestQty: t_Int32NULL;
    f_MDInstrumentDefinitionSpread29_ClearedVolume: t_Int32NULL;
    f_MDInstrumentDefinitionSpread29_HighLimitPrice: t_PRICENULL;
    f_MDInstrumentDefinitionSpread29_LowLimitPrice: t_PRICENULL;
    f_MDInstrumentDefinitionSpread29_MaxPriceVariation: t_PRICENULL;
    f_MDInstrumentDefinitionSpread29_MainFraction: t_uInt8NULL;
    f_MDInstrumentDefinitionSpread29_SubFraction: t_uInt8NULL;
    f_MDInstrumentDefinitionSpread29_TradingReferenceDate: t_LocalMktDate;
    f_MDInstrumentDefinitionSpread29_NoEvents: g_MDInstrumentDefinitionSpread29_NoEvents list;
    f_MDInstrumentDefinitionSpread29_NoMDFeedTypes: g_MDInstrumentDefinitionSpread29_NoMDFeedTypes list;
    f_MDInstrumentDefinitionSpread29_NoInstAttrib: g_MDInstrumentDefinitionSpread29_NoInstAttrib list;
    f_MDInstrumentDefinitionSpread29_NoLotTypeRules: g_MDInstrumentDefinitionSpread29_NoLotTypeRules list;
    f_MDInstrumentDefinitionSpread29_NoLegs: g_MDInstrumentDefinitionSpread29_NoLegs list
}

type msg_SecurityStatus30 = { 
    f_SecurityStatus30_TransactTime: t_uInt64;
    f_SecurityStatus30_SecurityGroup: t_SecurityGroup;
    f_SecurityStatus30_Asset: t_Asset;
    f_SecurityStatus30_SecurityID: t_Int32NULL;
    f_SecurityStatus30_TradeDate: t_LocalMktDate;
    f_SecurityStatus30_MatchEventIndicator: t_MatchEventIndicator;
    f_SecurityStatus30_SecurityTradingStatus: t_SecurityTradingStatus;
    f_SecurityStatus30_HaltReason: t_HaltReason;
    f_SecurityStatus30_SecurityTradingEvent: t_SecurityTradingEvent
}
type g_MDIncrementalRefreshBook32_NoMDEntries = {
    f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx: t_PRICENULL;
    f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize: t_Int32NULL;
    f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID: t_Int32;
    f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq: t_uInt32;
    f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders: t_Int32NULL;
    f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel: t_uInt8;
    f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction: t_MDUpdateAction;
    f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType: t_MDEntryTypeBook
}

type msg_MDIncrementalRefreshBook32 = { 
    f_MDIncrementalRefreshBook32_TransactTime: t_uInt64;
    f_MDIncrementalRefreshBook32_MatchEventIndicator: t_MatchEventIndicator;
    f_MDIncrementalRefreshBook32_NoMDEntries: g_MDIncrementalRefreshBook32_NoMDEntries list
}
type g_MDIncrementalRefreshDailyStatistics33_NoMDEntries = {
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryPx: t_PRICENULL;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntrySize: t_Int32NULL;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SecurityID: t_Int32;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_RptSeq: t_uInt32;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_TradingReferenceDate: t_LocalMktDate;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SettlPriceType: t_SettlPriceType;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDUpdateAction: t_MDUpdateAction;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryType: t_MDEntryTypeDailyStatistics
}

type msg_MDIncrementalRefreshDailyStatistics33 = { 
    f_MDIncrementalRefreshDailyStatistics33_TransactTime: t_uInt64;
    f_MDIncrementalRefreshDailyStatistics33_MatchEventIndicator: t_MatchEventIndicator;
    f_MDIncrementalRefreshDailyStatistics33_NoMDEntries: g_MDIncrementalRefreshDailyStatistics33_NoMDEntries list
}
type g_MDIncrementalRefreshLimitsBanding34_NoMDEntries = {
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_HighLimitPrice: t_PRICENULL;
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_LowLimitPrice: t_PRICENULL;
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MaxPriceVariation: t_PRICENULL;
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_SecurityID: t_Int32;
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_RptSeq: t_uInt32;
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDUpdateAction: t_MDUpdateActionNew;
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDEntryType: t_MDEntryTypeLimits
}

type msg_MDIncrementalRefreshLimitsBanding34 = { 
    f_MDIncrementalRefreshLimitsBanding34_TransactTime: t_uInt64;
    f_MDIncrementalRefreshLimitsBanding34_MatchEventIndicator: t_MatchEventIndicator;
    f_MDIncrementalRefreshLimitsBanding34_NoMDEntries: g_MDIncrementalRefreshLimitsBanding34_NoMDEntries list
}
type g_MDIncrementalRefreshSessionStatistics35_NoMDEntries = {
    f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryPx: t_PRICE;
    f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_SecurityID: t_Int32;
    f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_RptSeq: t_uInt32;
    f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_OpenCloseSettlFlag: t_OpenCloseSettlFlag;
    f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDUpdateAction: t_MDUpdateAction;
    f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryType: t_MDEntryTypeStatistics
}

type msg_MDIncrementalRefreshSessionStatistics35 = { 
    f_MDIncrementalRefreshSessionStatistics35_TransactTime: t_uInt64;
    f_MDIncrementalRefreshSessionStatistics35_MatchEventIndicator: t_MatchEventIndicator;
    f_MDIncrementalRefreshSessionStatistics35_NoMDEntries: g_MDIncrementalRefreshSessionStatistics35_NoMDEntries list
}
type g_MDIncrementalRefreshTrade36_NoMDEntries = {
    f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryPx: t_PRICE;
    f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntrySize: t_Int32;
    f_MDIncrementalRefreshTrade36_NoMDEntries_SecurityID: t_Int32;
    f_MDIncrementalRefreshTrade36_NoMDEntries_RptSeq: t_uInt32;
    f_MDIncrementalRefreshTrade36_NoMDEntries_NumberOfOrders: t_Int32NULL;
    f_MDIncrementalRefreshTrade36_NoMDEntries_TradeID: t_Int32;
    f_MDIncrementalRefreshTrade36_NoMDEntries_AggressorSide: t_AggressorSide;
    f_MDIncrementalRefreshTrade36_NoMDEntries_MDUpdateAction: t_MDUpdateAction;
    f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryType: t_MDEntryTypeTrade
}

type msg_MDIncrementalRefreshTrade36 = { 
    f_MDIncrementalRefreshTrade36_TransactTime: t_uInt64;
    f_MDIncrementalRefreshTrade36_MatchEventIndicator: t_MatchEventIndicator;
    f_MDIncrementalRefreshTrade36_NoMDEntries: g_MDIncrementalRefreshTrade36_NoMDEntries list
}
type g_MDIncrementalRefreshVolume37_NoMDEntries = {
    f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntrySize: t_Int32;
    f_MDIncrementalRefreshVolume37_NoMDEntries_SecurityID: t_Int32;
    f_MDIncrementalRefreshVolume37_NoMDEntries_RptSeq: t_uInt32;
    f_MDIncrementalRefreshVolume37_NoMDEntries_MDUpdateAction: t_MDUpdateAction;
    f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntryType: t_MDEntryTypeVol
}

type msg_MDIncrementalRefreshVolume37 = { 
    f_MDIncrementalRefreshVolume37_TransactTime: t_uInt64;
    f_MDIncrementalRefreshVolume37_MatchEventIndicator: t_MatchEventIndicator;
    f_MDIncrementalRefreshVolume37_NoMDEntries: g_MDIncrementalRefreshVolume37_NoMDEntries list
}
type g_SnapshotFullRefresh38_NoMDEntries = {
    f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx: t_PRICENULL;
    f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize: t_Int32NULL;
    f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders: t_Int32NULL;
    f_SnapshotFullRefresh38_NoMDEntries_MDPriceLevel: t_Int8NULL;
    f_SnapshotFullRefresh38_NoMDEntries_TradingReferenceDate: t_LocalMktDate;
    f_SnapshotFullRefresh38_NoMDEntries_OpenCloseSettlFlag: t_OpenCloseSettlFlag;
    f_SnapshotFullRefresh38_NoMDEntries_SettlPriceType: t_SettlPriceType;
    f_SnapshotFullRefresh38_NoMDEntries_MDEntryType: t_MDEntryType
}

type msg_SnapshotFullRefresh38 = { 
    f_SnapshotFullRefresh38_LastMsgSeqNumProcessed: t_uInt32;
    f_SnapshotFullRefresh38_TotNumReports: t_uInt32;
    f_SnapshotFullRefresh38_SecurityID: t_Int32;
    f_SnapshotFullRefresh38_RptSeq: t_uInt32;
    f_SnapshotFullRefresh38_TransactTime: t_uInt64;
    f_SnapshotFullRefresh38_LastUpdateTime: t_uInt64;
    f_SnapshotFullRefresh38_TradeDate: t_LocalMktDate;
    f_SnapshotFullRefresh38_MDSecurityTradingStatus: t_SecurityTradingStatus;
    f_SnapshotFullRefresh38_HighLimitPrice: t_PRICENULL;
    f_SnapshotFullRefresh38_LowLimitPrice: t_PRICENULL;
    f_SnapshotFullRefresh38_MaxPriceVariation: t_PRICENULL;
    f_SnapshotFullRefresh38_NoMDEntries: g_SnapshotFullRefresh38_NoMDEntries list
}
type g_QuoteRequest39_NoRelatedSym = {
    f_QuoteRequest39_NoRelatedSym_Symbol: t_Symbol;
    f_QuoteRequest39_NoRelatedSym_SecurityID: t_Int32;
    f_QuoteRequest39_NoRelatedSym_OrderQty: t_Int32NULL;
    f_QuoteRequest39_NoRelatedSym_QuoteType: t_Int8;
    f_QuoteRequest39_NoRelatedSym_Side: t_Int8NULL
}

type msg_QuoteRequest39 = { 
    f_QuoteRequest39_TransactTime: t_uInt64;
    f_QuoteRequest39_QuoteReqID: t_QuoteReqId;
    f_QuoteRequest39_MatchEventIndicator: t_MatchEventIndicator;
    f_QuoteRequest39_NoRelatedSym: g_QuoteRequest39_NoRelatedSym list
}
type g_MDInstrumentDefinitionOption41_NoEvents = {
    f_MDInstrumentDefinitionOption41_NoEvents_EventType: t_EventType;
    f_MDInstrumentDefinitionOption41_NoEvents_EventTime: t_uInt64
}
type g_MDInstrumentDefinitionOption41_NoMDFeedTypes = {
    f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MDFeedType: t_MDFeedType;
    f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MarketDepth: t_Int8
}
type g_MDInstrumentDefinitionOption41_NoInstAttrib = {
    f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribType: t_InstAttribType;
    f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribValue: t_InstAttribValue
}
type g_MDInstrumentDefinitionOption41_NoLotTypeRules = {
    f_MDInstrumentDefinitionOption41_NoLotTypeRules_LotType: t_Int8;
    f_MDInstrumentDefinitionOption41_NoLotTypeRules_MinLotSize: t_DecimalQty
}
type g_MDInstrumentDefinitionOption41_NoUnderlyings = {
    f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityID: t_Int32;
    f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityIDSource: t_SecurityIDSource;
    f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSymbol: t_UnderlyingSymbol
}

type msg_MDInstrumentDefinitionOption41 = { 
    f_MDInstrumentDefinitionOption41_MatchEventIndicator: t_MatchEventIndicator;
    f_MDInstrumentDefinitionOption41_TotNumReports: t_uInt32NULL;
    f_MDInstrumentDefinitionOption41_SecurityUpdateAction: t_SecurityUpdateAction;
    f_MDInstrumentDefinitionOption41_LastUpdateTime: t_uInt64;
    f_MDInstrumentDefinitionOption41_MDSecurityTradingStatus: t_SecurityTradingStatus;
    f_MDInstrumentDefinitionOption41_ApplID: t_Int16;
    f_MDInstrumentDefinitionOption41_MarketSegmentID: t_uInt8;
    f_MDInstrumentDefinitionOption41_UnderlyingProduct: t_uInt8;
    f_MDInstrumentDefinitionOption41_SecurityExchange: t_SecurityExchange;
    f_MDInstrumentDefinitionOption41_SecurityGroup: t_SecurityGroup;
    f_MDInstrumentDefinitionOption41_Asset: t_Asset;
    f_MDInstrumentDefinitionOption41_Symbol: t_Symbol;
    f_MDInstrumentDefinitionOption41_SecurityID: t_Int32;
    f_MDInstrumentDefinitionOption41_SecurityIDSource: t_SecurityIDSource;
    f_MDInstrumentDefinitionOption41_SecurityType: t_SecurityType;
    f_MDInstrumentDefinitionOption41_CFICode: t_CFICode;
    f_MDInstrumentDefinitionOption41_PutOrCall: t_PutOrCall;
    f_MDInstrumentDefinitionOption41_MaturityMonthYear: t_MaturityMonthYear;
    f_MDInstrumentDefinitionOption41_Currency: t_Currency;
    f_MDInstrumentDefinitionOption41_StrikePrice: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_StrikeCurrency: t_Currency;
    f_MDInstrumentDefinitionOption41_SettlCurrency: t_Currency;
    f_MDInstrumentDefinitionOption41_MinCabPrice: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_MatchAlgorithm: t_CHAR;
    f_MDInstrumentDefinitionOption41_MinTradeVol: t_uInt32;
    f_MDInstrumentDefinitionOption41_MaxTradeVol: t_uInt32;
    f_MDInstrumentDefinitionOption41_MinPriceIncrement: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_MinPriceIncrementAmount: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_DisplayFactor: t_FLOAT;
    f_MDInstrumentDefinitionOption41_TickRule: t_Int8NULL;
    f_MDInstrumentDefinitionOption41_MainFraction: t_uInt8NULL;
    f_MDInstrumentDefinitionOption41_SubFraction: t_uInt8NULL;
    f_MDInstrumentDefinitionOption41_PriceDisplayFormat: t_uInt8NULL;
    f_MDInstrumentDefinitionOption41_UnitOfMeasure: t_UnitOfMeasure;
    f_MDInstrumentDefinitionOption41_UnitOfMeasureQty: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_TradingReferencePrice: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_SettlPriceType: t_SettlPriceType;
    f_MDInstrumentDefinitionOption41_ClearedVolume: t_Int32NULL;
    f_MDInstrumentDefinitionOption41_OpenInterestQty: t_Int32NULL;
    f_MDInstrumentDefinitionOption41_LowLimitPrice: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_HighLimitPrice: t_PRICENULL;
    f_MDInstrumentDefinitionOption41_UserDefinedInstrument: t_UserDefinedInstrument;
    f_MDInstrumentDefinitionOption41_TradingReferenceDate: t_LocalMktDate;
    f_MDInstrumentDefinitionOption41_NoEvents: g_MDInstrumentDefinitionOption41_NoEvents list;
    f_MDInstrumentDefinitionOption41_NoMDFeedTypes: g_MDInstrumentDefinitionOption41_NoMDFeedTypes list;
    f_MDInstrumentDefinitionOption41_NoInstAttrib: g_MDInstrumentDefinitionOption41_NoInstAttrib list;
    f_MDInstrumentDefinitionOption41_NoLotTypeRules: g_MDInstrumentDefinitionOption41_NoLotTypeRules list;
    f_MDInstrumentDefinitionOption41_NoUnderlyings: g_MDInstrumentDefinitionOption41_NoUnderlyings list
}
type g_MDIncrementalRefreshTradeSummary42_NoMDEntries = {
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryPx: t_PRICE;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntrySize: t_Int32;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_SecurityID: t_Int32;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_RptSeq: t_uInt32;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_NumberOfOrders: t_Int32NULL;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_AggressorSide: t_AggressorSide;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDUpdateAction: t_MDUpdateAction;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryType: t_MDEntryTypeTrade
}
type g_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries = {
    f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_OrderID: t_uInt64;
    f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_LastQty: t_Int32
}

type msg_MDIncrementalRefreshTradeSummary42 = { 
    f_MDIncrementalRefreshTradeSummary42_TransactTime: t_uInt64;
    f_MDIncrementalRefreshTradeSummary42_MatchEventIndicator: t_MatchEventIndicator;
    f_MDIncrementalRefreshTradeSummary42_NoMDEntries: g_MDIncrementalRefreshTradeSummary42_NoMDEntries list;
    f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries: g_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries list
}
type message = 
    | M_ChannelReset4 of msg_ChannelReset4
    | M_AdminHeartbeat12 of msg_AdminHeartbeat12
    | M_AdminLogin15 of msg_AdminLogin15
    | M_AdminLogout16 of msg_AdminLogout16
    | M_MDInstrumentDefinitionFuture27 of msg_MDInstrumentDefinitionFuture27
    | M_MDInstrumentDefinitionSpread29 of msg_MDInstrumentDefinitionSpread29
    | M_SecurityStatus30 of msg_SecurityStatus30
    | M_MDIncrementalRefreshBook32 of msg_MDIncrementalRefreshBook32
    | M_MDIncrementalRefreshDailyStatistics33 of msg_MDIncrementalRefreshDailyStatistics33
    | M_MDIncrementalRefreshLimitsBanding34 of msg_MDIncrementalRefreshLimitsBanding34
    | M_MDIncrementalRefreshSessionStatistics35 of msg_MDIncrementalRefreshSessionStatistics35
    | M_MDIncrementalRefreshTrade36 of msg_MDIncrementalRefreshTrade36
    | M_MDIncrementalRefreshVolume37 of msg_MDIncrementalRefreshVolume37
    | M_SnapshotFullRefresh38 of msg_SnapshotFullRefresh38
    | M_QuoteRequest39 of msg_QuoteRequest39
    | M_MDInstrumentDefinitionOption41 of msg_MDInstrumentDefinitionOption41
    | M_MDIncrementalRefreshTradeSummary42 of msg_MDIncrementalRefreshTradeSummary42