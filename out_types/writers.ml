open Message_types
let write_Asset bits v = List.fold_left Binparser.write_char bits v ;;
let write_CFICode bits v = List.fold_left Binparser.write_char bits v ;;
let write_CHAR bits v = Binparser.write_char bits v ;;
let string_to_CHAR v = Binparser.string_to_char v ;;
let write_bits_CHAR = Binparser.write_bits_char ;;
let write_Currency bits v = List.fold_left Binparser.write_char bits v ;;
let write_InstAttribType bits v = bits ;;
let write_Int16 bits v = Binparser.write_int16 bits v ;;
let string_to_Int16 v = Binparser.string_to_int16 v ;;
let write_bits_Int16 = Binparser.write_bits_int16 ;;
let write_Int32 bits v = Binparser.write_int32 bits v ;;
let string_to_Int32 v = Binparser.string_to_int32 v ;;
let write_bits_Int32 = Binparser.write_bits_int32 ;;
let write_Int32NULL bits v = match v with | Some x -> Binparser.write_int32 bits x | None -> "2147483647" |> Binparser.string_to_int32 |> Binparser.write_int32 bits ;;
let string_to_Int32NULL v = match v with | "2147483647" -> None | x -> Some ( Binparser.string_to_int32 x ) ;;
let write_bits_Int32NULL = Binparser.write_bits_int32 ;;
let write_Int8 bits v = Binparser.write_int8 bits v ;;
let string_to_Int8 v = Binparser.string_to_int8 v ;;
let write_bits_Int8 = Binparser.write_bits_int8 ;;
let write_Int8NULL bits v = match v with | Some x -> Binparser.write_int8 bits x | None -> "127" |> Binparser.string_to_int8 |> Binparser.write_int8 bits ;;
let string_to_Int8NULL v = match v with | "127" -> None | x -> Some ( Binparser.string_to_int8 x ) ;;
let write_bits_Int8NULL = Binparser.write_bits_int8 ;;
let write_LocalMktDate bits v = match v with | Some x -> Binparser.write_uint16 bits x | None -> "65535" |> Binparser.string_to_uint16 |> Binparser.write_uint16 bits ;;
let string_to_LocalMktDate v = match v with | "65535" -> None | x -> Some ( Binparser.string_to_uint16 x ) ;;
let write_bits_LocalMktDate = Binparser.write_bits_uint16 ;;
let write_MDEntryTypeChannelReset bits v = bits ;;
let write_MDEntryTypeLimits bits v = bits ;;
let write_MDEntryTypeTrade bits v = bits ;;
let write_MDEntryTypeVol bits v = bits ;;
let write_MDFeedType bits v = List.fold_left Binparser.write_char bits v ;;
let write_MDUpdateActionNew bits v = bits ;;
let write_MDUpdateTypeNew bits v = bits ;;
let write_QuoteReqId bits v = List.fold_left Binparser.write_char bits v ;;
let write_SecurityExchange bits v = List.fold_left Binparser.write_char bits v ;;
let write_SecurityGroup bits v = List.fold_left Binparser.write_char bits v ;;
let write_SecurityIDSource bits v = bits ;;
let write_SecuritySubType bits v = List.fold_left Binparser.write_char bits v ;;
let write_SecurityType bits v = List.fold_left Binparser.write_char bits v ;;
let write_Symbol bits v = List.fold_left Binparser.write_char bits v ;;
let write_Text bits v = List.fold_left Binparser.write_char bits v ;;
let write_UnderlyingSymbol bits v = List.fold_left Binparser.write_char bits v ;;
let write_UnitOfMeasure bits v = List.fold_left Binparser.write_char bits v ;;
let write_UserDefinedInstrument bits v = Binparser.write_char bits v ;;
let string_to_UserDefinedInstrument v = Binparser.string_to_char v ;;
let write_bits_UserDefinedInstrument = Binparser.write_bits_char ;;
let write_uInt32 bits v = Binparser.write_uint32 bits v ;;
let string_to_uInt32 v = Binparser.string_to_uint32 v ;;
let write_bits_uInt32 = Binparser.write_bits_uint32 ;;
let write_uInt32NULL bits v = match v with | Some x -> Binparser.write_uint32 bits x | None -> "4294967295" |> Binparser.string_to_uint32 |> Binparser.write_uint32 bits ;;
let string_to_uInt32NULL v = match v with | "4294967295" -> None | x -> Some ( Binparser.string_to_uint32 x ) ;;
let write_bits_uInt32NULL = Binparser.write_bits_uint32 ;;
let write_uInt64 bits v = Binparser.write_uint64 bits v ;;
let string_to_uInt64 v = Binparser.string_to_uint64 v ;;
let write_bits_uInt64 = Binparser.write_bits_uint64 ;;
let write_uInt8 bits v = Binparser.write_uint8 bits v ;;
let string_to_uInt8 v = Binparser.string_to_uint8 v ;;
let write_bits_uInt8 = Binparser.write_bits_uint8 ;;
let write_uInt8NULL bits v = match v with | Some x -> Binparser.write_uint8 bits x | None -> "255" |> Binparser.string_to_uint8 |> Binparser.write_uint8 bits ;;
let string_to_uInt8NULL v = match v with | "255" -> None | x -> Some ( Binparser.string_to_uint8 x ) ;;
let write_bits_uInt8NULL = Binparser.write_bits_uint8 ;;
let write_DecimalQty bits v = 
     let bits = match v.f_DecimalQty_mantissa with | Some x -> Binparser.write_int32 bits x | None -> "2147483647" |> Binparser.string_to_int32 |> Binparser.write_int32 bits in
     let bits = bits in
    bits
let write_FLOAT bits v = 
     let bits = Binparser.write_int64 bits v.f_FLOAT_mantissa in
     let bits = bits in
    bits
let write_MaturityMonthYear bits v = 
     let bits = match v.f_MaturityMonthYear_year with | Some x -> Binparser.write_uint16 bits x | None -> "65535" |> Binparser.string_to_uint16 |> Binparser.write_uint16 bits in
     let bits = match v.f_MaturityMonthYear_month with | Some x -> Binparser.write_uint8 bits x | None -> "255" |> Binparser.string_to_uint8 |> Binparser.write_uint8 bits in
     let bits = match v.f_MaturityMonthYear_day with | Some x -> Binparser.write_uint8 bits x | None -> "255" |> Binparser.string_to_uint8 |> Binparser.write_uint8 bits in
     let bits = match v.f_MaturityMonthYear_week with | Some x -> Binparser.write_uint8 bits x | None -> "255" |> Binparser.string_to_uint8 |> Binparser.write_uint8 bits in
    bits
let write_PRICE bits v = 
     let bits = Binparser.write_int64 bits v.f_PRICE_mantissa in
     let bits = bits in
    bits
let write_PRICENULL bits v = 
     let bits = match v.f_PRICENULL_mantissa with | Some x -> Binparser.write_int64 bits x | None -> "9223372036854775807" |> Binparser.string_to_int64 |> Binparser.write_int64 bits in
     let bits = bits in
    bits
let write_groupSize bits v = 
     let bits = Binparser.write_uint16 bits v.f_groupSize_blockLength in
     let bits = Binparser.write_uint8 bits v.f_groupSize_numInGroup in
    bits
let write_groupSize8Byte bits v = 
     let bits = Binparser.write_uint16 bits v.f_groupSize8Byte_blockLength in
     let bits = Binparser.write_uint8 bits v.f_groupSize8Byte_numInGroup in
    bits
let write_groupSizeEncoding bits v = 
     let bits = Binparser.write_uint16 bits v.f_groupSizeEncoding_blockLength in
     let bits = Binparser.write_uint16 bits v.f_groupSizeEncoding_numInGroup in
    bits
let write_messageHeader bits v = 
     let bits = Binparser.write_uint16 bits v.f_messageHeader_blockLength in
     let bits = Binparser.write_uint16 bits v.f_messageHeader_templateId in
     let bits = Binparser.write_uint16 bits v.f_messageHeader_schemaId in
     let bits = Binparser.write_uint16 bits v.f_messageHeader_version in
    bits
let write_AggressorSide bits v = 
     match v with 
     | V_AggressorSide_NoAggressor -> "0" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_AggressorSide_Buy -> "1" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_AggressorSide_Sell -> "2" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_AggressorSide_Null -> "255" |> string_to_uInt8NULL |> write_uInt8NULL bits 

let write_EventType bits v = 
     match v with 
     | V_EventType_Activation -> "5" |> string_to_uInt8 |> write_uInt8 bits
     | V_EventType_LastEligibleTradeDate -> "7" |> string_to_uInt8 |> write_uInt8 bits

let write_HaltReason bits v = 
     match v with 
     | V_HaltReason_GroupSchedule -> "0" |> string_to_uInt8 |> write_uInt8 bits
     | V_HaltReason_SurveillanceIntervention -> "1" |> string_to_uInt8 |> write_uInt8 bits
     | V_HaltReason_MarketEvent -> "2" |> string_to_uInt8 |> write_uInt8 bits
     | V_HaltReason_InstrumentActivation -> "3" |> string_to_uInt8 |> write_uInt8 bits
     | V_HaltReason_InstrumentExpiration -> "4" |> string_to_uInt8 |> write_uInt8 bits
     | V_HaltReason_Unknown -> "5" |> string_to_uInt8 |> write_uInt8 bits
     | V_HaltReason_RecoveryInProcess -> "6" |> string_to_uInt8 |> write_uInt8 bits

let write_LegSide bits v = 
     match v with 
     | V_LegSide_BuySide -> "1" |> string_to_uInt8 |> write_uInt8 bits
     | V_LegSide_SellSide -> "2" |> string_to_uInt8 |> write_uInt8 bits

let write_MDEntryType bits v = 
     match v with 
     | V_MDEntryType_Bid -> "0" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_Offer -> "1" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_Trade -> "2" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_OpenPrice -> "4" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_SettlementPrice -> "6" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_TradingSessionHighPrice -> "7" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_TradingSessionLowPrice -> "8" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_ClearedVolume -> "B" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_OpenInterest -> "C" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_ImpliedBid -> "E" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_ImpliedOffer -> "F" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_BookReset -> "J" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_SessionHighBid -> "N" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_SessionLowOffer -> "O" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_FixingPrice -> "W" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_ElectronicVolume -> "e" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryType_ThresholdLimitsandPriceBandVariation -> "g" |> string_to_CHAR |> write_CHAR bits

let write_MDEntryTypeBook bits v = 
     match v with 
     | V_MDEntryTypeBook_Bid -> "0" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeBook_Offer -> "1" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeBook_ImpliedBid -> "E" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeBook_ImpliedOffer -> "F" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeBook_BookReset -> "J" |> string_to_CHAR |> write_CHAR bits

let write_MDEntryTypeDailyStatistics bits v = 
     match v with 
     | V_MDEntryTypeDailyStatistics_SettlementPrice -> "6" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeDailyStatistics_ClearedVolume -> "B" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeDailyStatistics_OpenInterest -> "C" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeDailyStatistics_FixingPrice -> "W" |> string_to_CHAR |> write_CHAR bits

let write_MDEntryTypeStatistics bits v = 
     match v with 
     | V_MDEntryTypeStatistics_OpenPrice -> "4" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeStatistics_HighTrade -> "7" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeStatistics_LowTrade -> "8" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeStatistics_HighestBid -> "N" |> string_to_CHAR |> write_CHAR bits
     | V_MDEntryTypeStatistics_LowestOffer -> "O" |> string_to_CHAR |> write_CHAR bits

let write_MDUpdateAction bits v = 
     match v with 
     | V_MDUpdateAction_New -> "0" |> string_to_uInt8 |> write_uInt8 bits
     | V_MDUpdateAction_Change -> "1" |> string_to_uInt8 |> write_uInt8 bits
     | V_MDUpdateAction_Delete -> "2" |> string_to_uInt8 |> write_uInt8 bits
     | V_MDUpdateAction_DeleteThru -> "3" |> string_to_uInt8 |> write_uInt8 bits
     | V_MDUpdateAction_DeleteFrom -> "4" |> string_to_uInt8 |> write_uInt8 bits
     | V_MDUpdateAction_Overlay -> "5" |> string_to_uInt8 |> write_uInt8 bits

let write_OpenCloseSettlFlag bits v = 
     match v with 
     | V_OpenCloseSettlFlag_DailyOpenPrice -> "0" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_OpenCloseSettlFlag_IndicativeOpeningPrice -> "5" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_OpenCloseSettlFlag_Null -> "255" |> string_to_uInt8NULL |> write_uInt8NULL bits 

let write_PutOrCall bits v = 
     match v with 
     | V_PutOrCall_Put -> "0" |> string_to_uInt8 |> write_uInt8 bits
     | V_PutOrCall_Call -> "1" |> string_to_uInt8 |> write_uInt8 bits

let write_SecurityTradingEvent bits v = 
     match v with 
     | V_SecurityTradingEvent_NoEvent -> "0" |> string_to_uInt8 |> write_uInt8 bits
     | V_SecurityTradingEvent_NoCancel -> "1" |> string_to_uInt8 |> write_uInt8 bits
     | V_SecurityTradingEvent_ResetStatistics -> "4" |> string_to_uInt8 |> write_uInt8 bits
     | V_SecurityTradingEvent_ImpliedMatchingON -> "5" |> string_to_uInt8 |> write_uInt8 bits
     | V_SecurityTradingEvent_ImpliedMatchingOFF -> "6" |> string_to_uInt8 |> write_uInt8 bits

let write_SecurityTradingStatus bits v = 
     match v with 
     | V_SecurityTradingStatus_TradingHalt -> "2" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_Close -> "4" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_NewPriceIndication -> "15" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_ReadyToTrade -> "17" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_NotAvailableForTrading -> "18" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_UnknownorInvalid -> "20" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_PreOpen -> "21" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_PreCross -> "24" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_Cross -> "25" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_PostClose -> "26" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_NoChange -> "103" |> string_to_uInt8NULL |> write_uInt8NULL bits
     | V_SecurityTradingStatus_Null -> "255" |> string_to_uInt8NULL |> write_uInt8NULL bits 

let write_SecurityUpdateAction bits v = 
     match v with 
     | V_SecurityUpdateAction_Add -> "A" |> string_to_CHAR |> write_CHAR bits
     | V_SecurityUpdateAction_Delete -> "D" |> string_to_CHAR |> write_CHAR bits
     | V_SecurityUpdateAction_Modify -> "M" |> string_to_CHAR |> write_CHAR bits

let write_InstAttribValue bits v = 
    write_bits_uInt32 bits [v.r_InstAttribValue_ElectronicMatchEligible;
    v.r_InstAttribValue_OrderCrossEligible;
    v.r_InstAttribValue_BlockTradeEligible;
    v.r_InstAttribValue_EFPEligible;
    v.r_InstAttribValue_EBFEligible;
    v.r_InstAttribValue_EFSEligible;
    v.r_InstAttribValue_EFREligible;
    v.r_InstAttribValue_OTCEligible;
    v.r_InstAttribValue_iLinkIndicativeMassQuotingEligible;
    v.r_InstAttribValue_NegativeStrikeEligible;
    v.r_InstAttribValue_NegativePriceOutrightEligible;
    v.r_InstAttribValue_IsFractional;
    v.r_InstAttribValue_VolatilityQuotedOption;
    v.r_InstAttribValue_RFQCrossEligible;
    v.r_InstAttribValue_ZeroPriceOutrightEligible;
    v.r_InstAttribValue_DecayingProductEligibility;
    v.r_InstAttribValue_VariableProductEligibility;
    v.r_InstAttribValue_DailyProductEligibility;
    v.r_InstAttribValue_GTOrdersEligibility;
    v.r_InstAttribValue_ImpliedMatchingEligibility]
let write_MatchEventIndicator bits v = 
    write_bits_uInt8 bits [v.r_MatchEventIndicator_LastTradeMsg;
    v.r_MatchEventIndicator_LastVolumeMsg;
    v.r_MatchEventIndicator_LastQuoteMsg;
    v.r_MatchEventIndicator_LastStatsMsg;
    v.r_MatchEventIndicator_LastImpliedMsg;
    v.r_MatchEventIndicator_RecoveryMsg;
    v.r_MatchEventIndicator_Reserved;
    v.r_MatchEventIndicator_EndOfEvent]
let write_SettlPriceType bits v = 
    write_bits_uInt8 bits [v.r_SettlPriceType_Final;
    v.r_SettlPriceType_Actual;
    v.r_SettlPriceType_Rounded;
    v.r_SettlPriceType_Intraday;
    v.r_SettlPriceType_ReservedBits;
    false;
    false;
    v.r_SettlPriceType_NullValue]
let write_g_ChannelReset4_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_MDUpdateTypeNew nbits v.f_ChannelReset4_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeChannelReset nbits v.f_ChannelReset4_NoMDEntries_MDEntryType in
    let nbits = write_Int16 nbits v.f_ChannelReset4_NoMDEntries_ApplID in
    Binparser.append_padded bits nbits n 


let write_msg_ChannelReset4 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_ChannelReset4_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_ChannelReset4_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_ChannelReset4_NoMDEntries , 2 ) in
         List.fold_left ( write_g_ChannelReset4_NoMDEntries 2 ) gbits v.f_ChannelReset4_NoMDEntries in
    bits, gbits
let write_msg_AdminHeartbeat12 x = Binparser.create_out (), Binparser.create_out () 


let write_msg_AdminLogin15 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_Int8 bits v.f_AdminLogin15_HeartBtInt in
    bits, gbits

let write_msg_AdminLogout16 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_Text bits v.f_AdminLogout16_Text in
    bits, gbits
let write_g_MDInstrumentDefinitionFuture27_NoEvents n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_EventType nbits v.f_MDInstrumentDefinitionFuture27_NoEvents_EventType in
    let nbits = write_uInt64 nbits v.f_MDInstrumentDefinitionFuture27_NoEvents_EventTime in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionFuture27_NoMDFeedTypes n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_MDFeedType nbits v.f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MDFeedType in
    let nbits = write_Int8 nbits v.f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MarketDepth in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionFuture27_NoInstAttrib n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_InstAttribType nbits v.f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribType in
    let nbits = write_InstAttribValue nbits v.f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribValue in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionFuture27_NoLotTypeRules n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_Int8 nbits v.f_MDInstrumentDefinitionFuture27_NoLotTypeRules_LotType in
    let nbits = write_DecimalQty nbits v.f_MDInstrumentDefinitionFuture27_NoLotTypeRules_MinLotSize in
    Binparser.append_padded bits nbits n 


let write_msg_MDInstrumentDefinitionFuture27 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_MatchEventIndicator bits v.f_MDInstrumentDefinitionFuture27_MatchEventIndicator in
    let bits = write_uInt32NULL bits v.f_MDInstrumentDefinitionFuture27_TotNumReports in
    let bits = write_SecurityUpdateAction bits v.f_MDInstrumentDefinitionFuture27_SecurityUpdateAction in
    let bits = write_uInt64 bits v.f_MDInstrumentDefinitionFuture27_LastUpdateTime in
    let bits = write_SecurityTradingStatus bits v.f_MDInstrumentDefinitionFuture27_MDSecurityTradingStatus in
    let bits = write_Int16 bits v.f_MDInstrumentDefinitionFuture27_ApplID in
    let bits = write_uInt8 bits v.f_MDInstrumentDefinitionFuture27_MarketSegmentID in
    let bits = write_uInt8 bits v.f_MDInstrumentDefinitionFuture27_UnderlyingProduct in
    let bits = write_SecurityExchange bits v.f_MDInstrumentDefinitionFuture27_SecurityExchange in
    let bits = write_SecurityGroup bits v.f_MDInstrumentDefinitionFuture27_SecurityGroup in
    let bits = write_Asset bits v.f_MDInstrumentDefinitionFuture27_Asset in
    let bits = write_Symbol bits v.f_MDInstrumentDefinitionFuture27_Symbol in
    let bits = write_Int32 bits v.f_MDInstrumentDefinitionFuture27_SecurityID in
    let bits = write_SecurityIDSource bits v.f_MDInstrumentDefinitionFuture27_SecurityIDSource in
    let bits = write_SecurityType bits v.f_MDInstrumentDefinitionFuture27_SecurityType in
    let bits = write_CFICode bits v.f_MDInstrumentDefinitionFuture27_CFICode in
    let bits = write_MaturityMonthYear bits v.f_MDInstrumentDefinitionFuture27_MaturityMonthYear in
    let bits = write_Currency bits v.f_MDInstrumentDefinitionFuture27_Currency in
    let bits = write_Currency bits v.f_MDInstrumentDefinitionFuture27_SettlCurrency in
    let bits = write_CHAR bits v.f_MDInstrumentDefinitionFuture27_MatchAlgorithm in
    let bits = write_uInt32 bits v.f_MDInstrumentDefinitionFuture27_MinTradeVol in
    let bits = write_uInt32 bits v.f_MDInstrumentDefinitionFuture27_MaxTradeVol in
    let bits = write_PRICE bits v.f_MDInstrumentDefinitionFuture27_MinPriceIncrement in
    let bits = write_FLOAT bits v.f_MDInstrumentDefinitionFuture27_DisplayFactor in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionFuture27_MainFraction in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionFuture27_SubFraction in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionFuture27_PriceDisplayFormat in
    let bits = write_UnitOfMeasure bits v.f_MDInstrumentDefinitionFuture27_UnitOfMeasure in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionFuture27_UnitOfMeasureQty in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionFuture27_TradingReferencePrice in
    let bits = write_SettlPriceType bits v.f_MDInstrumentDefinitionFuture27_SettlPriceType in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionFuture27_OpenInterestQty in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionFuture27_ClearedVolume in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionFuture27_HighLimitPrice in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionFuture27_LowLimitPrice in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionFuture27_MaxPriceVariation in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionFuture27_DecayQuantity in
    let bits = write_LocalMktDate bits v.f_MDInstrumentDefinitionFuture27_DecayStartDate in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionFuture27_OriginalContractSize in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionFuture27_ContractMultiplier in
    let bits = write_Int8NULL bits v.f_MDInstrumentDefinitionFuture27_ContractMultiplierUnit in
    let bits = write_Int8NULL bits v.f_MDInstrumentDefinitionFuture27_FlowScheduleType in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionFuture27_MinPriceIncrementAmount in
    let bits = write_UserDefinedInstrument bits v.f_MDInstrumentDefinitionFuture27_UserDefinedInstrument in
    let bits = write_LocalMktDate bits v.f_MDInstrumentDefinitionFuture27_TradingReferenceDate in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionFuture27_NoEvents , 9 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionFuture27_NoEvents 9 ) gbits v.f_MDInstrumentDefinitionFuture27_NoEvents in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionFuture27_NoMDFeedTypes , 4 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionFuture27_NoMDFeedTypes 4 ) gbits v.f_MDInstrumentDefinitionFuture27_NoMDFeedTypes in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionFuture27_NoInstAttrib , 4 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionFuture27_NoInstAttrib 4 ) gbits v.f_MDInstrumentDefinitionFuture27_NoInstAttrib in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionFuture27_NoLotTypeRules , 5 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionFuture27_NoLotTypeRules 5 ) gbits v.f_MDInstrumentDefinitionFuture27_NoLotTypeRules in
    bits, gbits
let write_g_MDInstrumentDefinitionSpread29_NoEvents n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_EventType nbits v.f_MDInstrumentDefinitionSpread29_NoEvents_EventType in
    let nbits = write_uInt64 nbits v.f_MDInstrumentDefinitionSpread29_NoEvents_EventTime in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionSpread29_NoMDFeedTypes n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_MDFeedType nbits v.f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MDFeedType in
    let nbits = write_Int8 nbits v.f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MarketDepth in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionSpread29_NoInstAttrib n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_InstAttribType nbits v.f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribType in
    let nbits = write_InstAttribValue nbits v.f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribValue in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionSpread29_NoLotTypeRules n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_Int8 nbits v.f_MDInstrumentDefinitionSpread29_NoLotTypeRules_LotType in
    let nbits = write_DecimalQty nbits v.f_MDInstrumentDefinitionSpread29_NoLotTypeRules_MinLotSize in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionSpread29_NoLegs n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_Int32 nbits v.f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityID in
    let nbits = write_SecurityIDSource nbits v.f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityIDSource in
    let nbits = write_LegSide nbits v.f_MDInstrumentDefinitionSpread29_NoLegs_LegSide in
    let nbits = write_Int8 nbits v.f_MDInstrumentDefinitionSpread29_NoLegs_LegRatioQty in
    let nbits = write_PRICENULL nbits v.f_MDInstrumentDefinitionSpread29_NoLegs_LegPrice in
    let nbits = write_DecimalQty nbits v.f_MDInstrumentDefinitionSpread29_NoLegs_LegOptionDelta in
    Binparser.append_padded bits nbits n 


let write_msg_MDInstrumentDefinitionSpread29 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_MatchEventIndicator bits v.f_MDInstrumentDefinitionSpread29_MatchEventIndicator in
    let bits = write_uInt32NULL bits v.f_MDInstrumentDefinitionSpread29_TotNumReports in
    let bits = write_SecurityUpdateAction bits v.f_MDInstrumentDefinitionSpread29_SecurityUpdateAction in
    let bits = write_uInt64 bits v.f_MDInstrumentDefinitionSpread29_LastUpdateTime in
    let bits = write_SecurityTradingStatus bits v.f_MDInstrumentDefinitionSpread29_MDSecurityTradingStatus in
    let bits = write_Int16 bits v.f_MDInstrumentDefinitionSpread29_ApplID in
    let bits = write_uInt8 bits v.f_MDInstrumentDefinitionSpread29_MarketSegmentID in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionSpread29_UnderlyingProduct in
    let bits = write_SecurityExchange bits v.f_MDInstrumentDefinitionSpread29_SecurityExchange in
    let bits = write_SecurityGroup bits v.f_MDInstrumentDefinitionSpread29_SecurityGroup in
    let bits = write_Asset bits v.f_MDInstrumentDefinitionSpread29_Asset in
    let bits = write_Symbol bits v.f_MDInstrumentDefinitionSpread29_Symbol in
    let bits = write_Int32 bits v.f_MDInstrumentDefinitionSpread29_SecurityID in
    let bits = write_SecurityIDSource bits v.f_MDInstrumentDefinitionSpread29_SecurityIDSource in
    let bits = write_SecurityType bits v.f_MDInstrumentDefinitionSpread29_SecurityType in
    let bits = write_CFICode bits v.f_MDInstrumentDefinitionSpread29_CFICode in
    let bits = write_MaturityMonthYear bits v.f_MDInstrumentDefinitionSpread29_MaturityMonthYear in
    let bits = write_Currency bits v.f_MDInstrumentDefinitionSpread29_Currency in
    let bits = write_SecuritySubType bits v.f_MDInstrumentDefinitionSpread29_SecuritySubType in
    let bits = write_UserDefinedInstrument bits v.f_MDInstrumentDefinitionSpread29_UserDefinedInstrument in
    let bits = write_CHAR bits v.f_MDInstrumentDefinitionSpread29_MatchAlgorithm in
    let bits = write_uInt32 bits v.f_MDInstrumentDefinitionSpread29_MinTradeVol in
    let bits = write_uInt32 bits v.f_MDInstrumentDefinitionSpread29_MaxTradeVol in
    let bits = write_PRICE bits v.f_MDInstrumentDefinitionSpread29_MinPriceIncrement in
    let bits = write_FLOAT bits v.f_MDInstrumentDefinitionSpread29_DisplayFactor in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionSpread29_PriceDisplayFormat in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionSpread29_PriceRatio in
    let bits = write_Int8NULL bits v.f_MDInstrumentDefinitionSpread29_TickRule in
    let bits = write_UnitOfMeasure bits v.f_MDInstrumentDefinitionSpread29_UnitOfMeasure in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionSpread29_TradingReferencePrice in
    let bits = write_SettlPriceType bits v.f_MDInstrumentDefinitionSpread29_SettlPriceType in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionSpread29_OpenInterestQty in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionSpread29_ClearedVolume in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionSpread29_HighLimitPrice in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionSpread29_LowLimitPrice in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionSpread29_MaxPriceVariation in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionSpread29_MainFraction in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionSpread29_SubFraction in
    let bits = write_LocalMktDate bits v.f_MDInstrumentDefinitionSpread29_TradingReferenceDate in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionSpread29_NoEvents , 9 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionSpread29_NoEvents 9 ) gbits v.f_MDInstrumentDefinitionSpread29_NoEvents in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionSpread29_NoMDFeedTypes , 4 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionSpread29_NoMDFeedTypes 4 ) gbits v.f_MDInstrumentDefinitionSpread29_NoMDFeedTypes in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionSpread29_NoInstAttrib , 4 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionSpread29_NoInstAttrib 4 ) gbits v.f_MDInstrumentDefinitionSpread29_NoInstAttrib in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionSpread29_NoLotTypeRules , 5 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionSpread29_NoLotTypeRules 5 ) gbits v.f_MDInstrumentDefinitionSpread29_NoLotTypeRules in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionSpread29_NoLegs , 18 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionSpread29_NoLegs 18 ) gbits v.f_MDInstrumentDefinitionSpread29_NoLegs in
    bits, gbits

let write_msg_SecurityStatus30 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_SecurityStatus30_TransactTime in
    let bits = write_SecurityGroup bits v.f_SecurityStatus30_SecurityGroup in
    let bits = write_Asset bits v.f_SecurityStatus30_Asset in
    let bits = write_Int32NULL bits v.f_SecurityStatus30_SecurityID in
    let bits = write_LocalMktDate bits v.f_SecurityStatus30_TradeDate in
    let bits = write_MatchEventIndicator bits v.f_SecurityStatus30_MatchEventIndicator in
    let bits = write_SecurityTradingStatus bits v.f_SecurityStatus30_SecurityTradingStatus in
    let bits = write_HaltReason bits v.f_SecurityStatus30_HaltReason in
    let bits = write_SecurityTradingEvent bits v.f_SecurityStatus30_SecurityTradingEvent in
    bits, gbits
let write_g_MDIncrementalRefreshBook32_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_PRICENULL nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx in
    let nbits = write_Int32NULL nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID in
    let nbits = write_uInt32 nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq in
    let nbits = write_Int32NULL nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders in
    let nbits = write_uInt8 nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel in
    let nbits = write_MDUpdateAction nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeBook nbits v.f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 


let write_msg_MDIncrementalRefreshBook32 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_MDIncrementalRefreshBook32_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_MDIncrementalRefreshBook32_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshBook32_NoMDEntries , 32 ) in
         List.fold_left ( write_g_MDIncrementalRefreshBook32_NoMDEntries 32 ) gbits v.f_MDIncrementalRefreshBook32_NoMDEntries in
    bits, gbits
let write_g_MDIncrementalRefreshDailyStatistics33_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_PRICENULL nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryPx in
    let nbits = write_Int32NULL nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntrySize in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SecurityID in
    let nbits = write_uInt32 nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_RptSeq in
    let nbits = write_LocalMktDate nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_TradingReferenceDate in
    let nbits = write_SettlPriceType nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SettlPriceType in
    let nbits = write_MDUpdateAction nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeDailyStatistics nbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 


let write_msg_MDIncrementalRefreshDailyStatistics33 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_MDIncrementalRefreshDailyStatistics33_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_MDIncrementalRefreshDailyStatistics33_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries , 32 ) in
         List.fold_left ( write_g_MDIncrementalRefreshDailyStatistics33_NoMDEntries 32 ) gbits v.f_MDIncrementalRefreshDailyStatistics33_NoMDEntries in
    bits, gbits
let write_g_MDIncrementalRefreshLimitsBanding34_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_PRICENULL nbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_HighLimitPrice in
    let nbits = write_PRICENULL nbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_LowLimitPrice in
    let nbits = write_PRICENULL nbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MaxPriceVariation in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_SecurityID in
    let nbits = write_uInt32 nbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_RptSeq in
    let nbits = write_MDUpdateActionNew nbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeLimits nbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 


let write_msg_MDIncrementalRefreshLimitsBanding34 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_MDIncrementalRefreshLimitsBanding34_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_MDIncrementalRefreshLimitsBanding34_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries , 32 ) in
         List.fold_left ( write_g_MDIncrementalRefreshLimitsBanding34_NoMDEntries 32 ) gbits v.f_MDIncrementalRefreshLimitsBanding34_NoMDEntries in
    bits, gbits
let write_g_MDIncrementalRefreshSessionStatistics35_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_PRICE nbits v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryPx in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_SecurityID in
    let nbits = write_uInt32 nbits v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_RptSeq in
    let nbits = write_OpenCloseSettlFlag nbits v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_OpenCloseSettlFlag in
    let nbits = write_MDUpdateAction nbits v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeStatistics nbits v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 


let write_msg_MDIncrementalRefreshSessionStatistics35 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_MDIncrementalRefreshSessionStatistics35_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_MDIncrementalRefreshSessionStatistics35_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries , 24 ) in
         List.fold_left ( write_g_MDIncrementalRefreshSessionStatistics35_NoMDEntries 24 ) gbits v.f_MDIncrementalRefreshSessionStatistics35_NoMDEntries in
    bits, gbits
let write_g_MDIncrementalRefreshTrade36_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_PRICE nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryPx in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntrySize in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_SecurityID in
    let nbits = write_uInt32 nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_RptSeq in
    let nbits = write_Int32NULL nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_NumberOfOrders in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_TradeID in
    let nbits = write_AggressorSide nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_AggressorSide in
    let nbits = write_MDUpdateAction nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeTrade nbits v.f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 


let write_msg_MDIncrementalRefreshTrade36 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_MDIncrementalRefreshTrade36_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_MDIncrementalRefreshTrade36_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshTrade36_NoMDEntries , 32 ) in
         List.fold_left ( write_g_MDIncrementalRefreshTrade36_NoMDEntries 32 ) gbits v.f_MDIncrementalRefreshTrade36_NoMDEntries in
    bits, gbits
let write_g_MDIncrementalRefreshVolume37_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntrySize in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshVolume37_NoMDEntries_SecurityID in
    let nbits = write_uInt32 nbits v.f_MDIncrementalRefreshVolume37_NoMDEntries_RptSeq in
    let nbits = write_MDUpdateAction nbits v.f_MDIncrementalRefreshVolume37_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeVol nbits v.f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 


let write_msg_MDIncrementalRefreshVolume37 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_MDIncrementalRefreshVolume37_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_MDIncrementalRefreshVolume37_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshVolume37_NoMDEntries , 16 ) in
         List.fold_left ( write_g_MDIncrementalRefreshVolume37_NoMDEntries 16 ) gbits v.f_MDIncrementalRefreshVolume37_NoMDEntries in
    bits, gbits
let write_g_SnapshotFullRefresh38_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_PRICENULL nbits v.f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx in
    let nbits = write_Int32NULL nbits v.f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize in
    let nbits = write_Int32NULL nbits v.f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders in
    let nbits = write_Int8NULL nbits v.f_SnapshotFullRefresh38_NoMDEntries_MDPriceLevel in
    let nbits = write_LocalMktDate nbits v.f_SnapshotFullRefresh38_NoMDEntries_TradingReferenceDate in
    let nbits = write_OpenCloseSettlFlag nbits v.f_SnapshotFullRefresh38_NoMDEntries_OpenCloseSettlFlag in
    let nbits = write_SettlPriceType nbits v.f_SnapshotFullRefresh38_NoMDEntries_SettlPriceType in
    let nbits = write_MDEntryType nbits v.f_SnapshotFullRefresh38_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 


let write_msg_SnapshotFullRefresh38 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt32 bits v.f_SnapshotFullRefresh38_LastMsgSeqNumProcessed in
    let bits = write_uInt32 bits v.f_SnapshotFullRefresh38_TotNumReports in
    let bits = write_Int32 bits v.f_SnapshotFullRefresh38_SecurityID in
    let bits = write_uInt32 bits v.f_SnapshotFullRefresh38_RptSeq in
    let bits = write_uInt64 bits v.f_SnapshotFullRefresh38_TransactTime in
    let bits = write_uInt64 bits v.f_SnapshotFullRefresh38_LastUpdateTime in
    let bits = write_LocalMktDate bits v.f_SnapshotFullRefresh38_TradeDate in
    let bits = write_SecurityTradingStatus bits v.f_SnapshotFullRefresh38_MDSecurityTradingStatus in
    let bits = write_PRICENULL bits v.f_SnapshotFullRefresh38_HighLimitPrice in
    let bits = write_PRICENULL bits v.f_SnapshotFullRefresh38_LowLimitPrice in
    let bits = write_PRICENULL bits v.f_SnapshotFullRefresh38_MaxPriceVariation in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_SnapshotFullRefresh38_NoMDEntries , 22 ) in
         List.fold_left ( write_g_SnapshotFullRefresh38_NoMDEntries 22 ) gbits v.f_SnapshotFullRefresh38_NoMDEntries in
    bits, gbits
let write_g_QuoteRequest39_NoRelatedSym n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_Symbol nbits v.f_QuoteRequest39_NoRelatedSym_Symbol in
    let nbits = write_Int32 nbits v.f_QuoteRequest39_NoRelatedSym_SecurityID in
    let nbits = write_Int32NULL nbits v.f_QuoteRequest39_NoRelatedSym_OrderQty in
    let nbits = write_Int8 nbits v.f_QuoteRequest39_NoRelatedSym_QuoteType in
    let nbits = write_Int8NULL nbits v.f_QuoteRequest39_NoRelatedSym_Side in
    Binparser.append_padded bits nbits n 


let write_msg_QuoteRequest39 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_QuoteRequest39_TransactTime in
    let bits = write_QuoteReqId bits v.f_QuoteRequest39_QuoteReqID in
    let bits = write_MatchEventIndicator bits v.f_QuoteRequest39_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_QuoteRequest39_NoRelatedSym , 32 ) in
         List.fold_left ( write_g_QuoteRequest39_NoRelatedSym 32 ) gbits v.f_QuoteRequest39_NoRelatedSym in
    bits, gbits
let write_g_MDInstrumentDefinitionOption41_NoEvents n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_EventType nbits v.f_MDInstrumentDefinitionOption41_NoEvents_EventType in
    let nbits = write_uInt64 nbits v.f_MDInstrumentDefinitionOption41_NoEvents_EventTime in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionOption41_NoMDFeedTypes n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_MDFeedType nbits v.f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MDFeedType in
    let nbits = write_Int8 nbits v.f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MarketDepth in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionOption41_NoInstAttrib n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_InstAttribType nbits v.f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribType in
    let nbits = write_InstAttribValue nbits v.f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribValue in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionOption41_NoLotTypeRules n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_Int8 nbits v.f_MDInstrumentDefinitionOption41_NoLotTypeRules_LotType in
    let nbits = write_DecimalQty nbits v.f_MDInstrumentDefinitionOption41_NoLotTypeRules_MinLotSize in
    Binparser.append_padded bits nbits n 

let write_g_MDInstrumentDefinitionOption41_NoUnderlyings n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_Int32 nbits v.f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityID in
    let nbits = write_SecurityIDSource nbits v.f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityIDSource in
    let nbits = write_UnderlyingSymbol nbits v.f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSymbol in
    Binparser.append_padded bits nbits n 


let write_msg_MDInstrumentDefinitionOption41 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_MatchEventIndicator bits v.f_MDInstrumentDefinitionOption41_MatchEventIndicator in
    let bits = write_uInt32NULL bits v.f_MDInstrumentDefinitionOption41_TotNumReports in
    let bits = write_SecurityUpdateAction bits v.f_MDInstrumentDefinitionOption41_SecurityUpdateAction in
    let bits = write_uInt64 bits v.f_MDInstrumentDefinitionOption41_LastUpdateTime in
    let bits = write_SecurityTradingStatus bits v.f_MDInstrumentDefinitionOption41_MDSecurityTradingStatus in
    let bits = write_Int16 bits v.f_MDInstrumentDefinitionOption41_ApplID in
    let bits = write_uInt8 bits v.f_MDInstrumentDefinitionOption41_MarketSegmentID in
    let bits = write_uInt8 bits v.f_MDInstrumentDefinitionOption41_UnderlyingProduct in
    let bits = write_SecurityExchange bits v.f_MDInstrumentDefinitionOption41_SecurityExchange in
    let bits = write_SecurityGroup bits v.f_MDInstrumentDefinitionOption41_SecurityGroup in
    let bits = write_Asset bits v.f_MDInstrumentDefinitionOption41_Asset in
    let bits = write_Symbol bits v.f_MDInstrumentDefinitionOption41_Symbol in
    let bits = write_Int32 bits v.f_MDInstrumentDefinitionOption41_SecurityID in
    let bits = write_SecurityIDSource bits v.f_MDInstrumentDefinitionOption41_SecurityIDSource in
    let bits = write_SecurityType bits v.f_MDInstrumentDefinitionOption41_SecurityType in
    let bits = write_CFICode bits v.f_MDInstrumentDefinitionOption41_CFICode in
    let bits = write_PutOrCall bits v.f_MDInstrumentDefinitionOption41_PutOrCall in
    let bits = write_MaturityMonthYear bits v.f_MDInstrumentDefinitionOption41_MaturityMonthYear in
    let bits = write_Currency bits v.f_MDInstrumentDefinitionOption41_Currency in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_StrikePrice in
    let bits = write_Currency bits v.f_MDInstrumentDefinitionOption41_StrikeCurrency in
    let bits = write_Currency bits v.f_MDInstrumentDefinitionOption41_SettlCurrency in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_MinCabPrice in
    let bits = write_CHAR bits v.f_MDInstrumentDefinitionOption41_MatchAlgorithm in
    let bits = write_uInt32 bits v.f_MDInstrumentDefinitionOption41_MinTradeVol in
    let bits = write_uInt32 bits v.f_MDInstrumentDefinitionOption41_MaxTradeVol in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_MinPriceIncrement in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_MinPriceIncrementAmount in
    let bits = write_FLOAT bits v.f_MDInstrumentDefinitionOption41_DisplayFactor in
    let bits = write_Int8NULL bits v.f_MDInstrumentDefinitionOption41_TickRule in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionOption41_MainFraction in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionOption41_SubFraction in
    let bits = write_uInt8NULL bits v.f_MDInstrumentDefinitionOption41_PriceDisplayFormat in
    let bits = write_UnitOfMeasure bits v.f_MDInstrumentDefinitionOption41_UnitOfMeasure in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_UnitOfMeasureQty in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_TradingReferencePrice in
    let bits = write_SettlPriceType bits v.f_MDInstrumentDefinitionOption41_SettlPriceType in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionOption41_ClearedVolume in
    let bits = write_Int32NULL bits v.f_MDInstrumentDefinitionOption41_OpenInterestQty in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_LowLimitPrice in
    let bits = write_PRICENULL bits v.f_MDInstrumentDefinitionOption41_HighLimitPrice in
    let bits = write_UserDefinedInstrument bits v.f_MDInstrumentDefinitionOption41_UserDefinedInstrument in
    let bits = write_LocalMktDate bits v.f_MDInstrumentDefinitionOption41_TradingReferenceDate in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionOption41_NoEvents , 9 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionOption41_NoEvents 9 ) gbits v.f_MDInstrumentDefinitionOption41_NoEvents in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionOption41_NoMDFeedTypes , 4 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionOption41_NoMDFeedTypes 4 ) gbits v.f_MDInstrumentDefinitionOption41_NoMDFeedTypes in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionOption41_NoInstAttrib , 4 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionOption41_NoInstAttrib 4 ) gbits v.f_MDInstrumentDefinitionOption41_NoInstAttrib in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionOption41_NoLotTypeRules , 5 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionOption41_NoLotTypeRules 5 ) gbits v.f_MDInstrumentDefinitionOption41_NoLotTypeRules in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDInstrumentDefinitionOption41_NoUnderlyings , 24 ) in
         List.fold_left ( write_g_MDInstrumentDefinitionOption41_NoUnderlyings 24 ) gbits v.f_MDInstrumentDefinitionOption41_NoUnderlyings in
    bits, gbits
let write_g_MDIncrementalRefreshTradeSummary42_NoMDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_PRICE nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryPx in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntrySize in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_SecurityID in
    let nbits = write_uInt32 nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_RptSeq in
    let nbits = write_Int32NULL nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_NumberOfOrders in
    let nbits = write_AggressorSide nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_AggressorSide in
    let nbits = write_MDUpdateAction nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDUpdateAction in
    let nbits = write_MDEntryTypeTrade nbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryType in
    Binparser.append_padded bits nbits n 

let write_g_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries n bits v = 
    let nbits = Binparser.create_out () in 
    let nbits = write_uInt64 nbits v.f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_OrderID in
    let nbits = write_Int32 nbits v.f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_LastQty in
    Binparser.append_padded bits nbits n 


let write_msg_MDIncrementalRefreshTradeSummary42 v = 
    let bits, gbits = Binparser.create_out (), Binparser.create_out () in
    let bits = write_uInt64 bits v.f_MDIncrementalRefreshTradeSummary42_TransactTime in
    let bits = write_MatchEventIndicator bits v.f_MDIncrementalRefreshTradeSummary42_MatchEventIndicator in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries , 32 ) in
         List.fold_left ( write_g_MDIncrementalRefreshTradeSummary42_NoMDEntries 32 ) gbits v.f_MDIncrementalRefreshTradeSummary42_NoMDEntries in
    let gbits = 
         let gbits = Binparser.write_group_info  gbits ( List.length v.f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries , 16 ) in
         List.fold_left ( write_g_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries 16 ) gbits v.f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries in
    bits, gbits
let write_message bits md msg =
    let fields, groups = match msg with
    | M_ChannelReset4 x ->  ( write_msg_ChannelReset4 x )
    | M_AdminHeartbeat12 x ->  ( write_msg_AdminHeartbeat12 x )
    | M_AdminLogin15 x ->  ( write_msg_AdminLogin15 x )
    | M_AdminLogout16 x ->  ( write_msg_AdminLogout16 x )
    | M_MDInstrumentDefinitionFuture27 x ->  ( write_msg_MDInstrumentDefinitionFuture27 x )
    | M_MDInstrumentDefinitionSpread29 x ->  ( write_msg_MDInstrumentDefinitionSpread29 x )
    | M_SecurityStatus30 x ->  ( write_msg_SecurityStatus30 x )
    | M_MDIncrementalRefreshBook32 x ->  ( write_msg_MDIncrementalRefreshBook32 x )
    | M_MDIncrementalRefreshDailyStatistics33 x ->  ( write_msg_MDIncrementalRefreshDailyStatistics33 x )
    | M_MDIncrementalRefreshLimitsBanding34 x ->  ( write_msg_MDIncrementalRefreshLimitsBanding34 x )
    | M_MDIncrementalRefreshSessionStatistics35 x ->  ( write_msg_MDIncrementalRefreshSessionStatistics35 x )
    | M_MDIncrementalRefreshTrade36 x ->  ( write_msg_MDIncrementalRefreshTrade36 x )
    | M_MDIncrementalRefreshVolume37 x ->  ( write_msg_MDIncrementalRefreshVolume37 x )
    | M_SnapshotFullRefresh38 x ->  ( write_msg_SnapshotFullRefresh38 x )
    | M_QuoteRequest39 x ->  ( write_msg_QuoteRequest39 x )
    | M_MDInstrumentDefinitionOption41 x ->  ( write_msg_MDInstrumentDefinitionOption41 x )
    | M_MDIncrementalRefreshTradeSummary42 x ->  ( write_msg_MDIncrementalRefreshTradeSummary42 x )
    in Binparser.write_message bits ( md, fields, groups ) 