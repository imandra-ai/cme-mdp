open Message_types                                          
let rec foldi (i,b) f bits =                                
    if i <= 0 then                                          
        [], bits                                            
    else                                                    
        let block, bits = Binparser.cut_block bits b in     
        let v,  _  = f block in                             
        let tl, bits = foldi (i-1, b) f bits in             
        v::tl, bits                                         
let read_Asset bits = Binparser.repeat 6 Binparser.read_char bits ;;
let string_of_Asset v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_CFICode bits = Binparser.repeat 6 Binparser.read_char bits ;;
let string_of_CFICode v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_CHAR bits = Binparser.read_char bits ;;
let string_of_CHAR v = Binparser.string_of_char v ;;
let bit_CHAR = Binparser.bit_char ;;
let read_Currency bits = Binparser.repeat 3 Binparser.read_char bits ;;
let string_of_Currency v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_InstAttribType bits = (Binparser.string_to_int8 "24") , bits ;;
let string_of_InstAttribType v = "24" ;;
let read_Int16 bits = Binparser.read_int16 bits ;;
let string_of_Int16 v = Binparser.string_of_int16 v ;;
let bit_Int16 = Binparser.bit_int16 ;;
let read_Int32 bits = Binparser.read_int32 bits ;;
let string_of_Int32 v = Binparser.string_of_int32 v ;;
let bit_Int32 = Binparser.bit_int32 ;;
let read_Int32NULL bits = Binparser.nullable_to_option ( Binparser.string_to_int32 "2147483647") ( Binparser.read_int32 bits ) ;;
let string_of_Int32NULL v = match v with | Some x -> Binparser.string_of_int32 x | None -> "2147483647" |> Binparser.string_to_int32 |> Binparser.string_of_int32 ;;
let read_Int8 bits = Binparser.read_int8 bits ;;
let string_of_Int8 v = Binparser.string_of_int8 v ;;
let bit_Int8 = Binparser.bit_int8 ;;
let read_Int8NULL bits = Binparser.nullable_to_option ( Binparser.string_to_int8 "127") ( Binparser.read_int8 bits ) ;;
let string_of_Int8NULL v = match v with | Some x -> Binparser.string_of_int8 x | None -> "127" |> Binparser.string_to_int8 |> Binparser.string_of_int8 ;;
let read_LocalMktDate bits = Binparser.nullable_to_option ( Binparser.string_to_uint16 "65535") ( Binparser.read_uint16 bits ) ;;
let string_of_LocalMktDate v = match v with | Some x -> Binparser.string_of_uint16 x | None -> "65535" |> Binparser.string_to_uint16 |> Binparser.string_of_uint16 ;;
let read_MDEntryTypeChannelReset bits = (Binparser.string_to_char "J") , bits ;;
let string_of_MDEntryTypeChannelReset v = "J" ;;
let read_MDEntryTypeLimits bits = (Binparser.string_to_char "g") , bits ;;
let string_of_MDEntryTypeLimits v = "g" ;;
let read_MDEntryTypeTrade bits = (Binparser.string_to_char "2") , bits ;;
let string_of_MDEntryTypeTrade v = "2" ;;
let read_MDEntryTypeVol bits = (Binparser.string_to_char "e") , bits ;;
let string_of_MDEntryTypeVol v = "e" ;;
let read_MDFeedType bits = Binparser.repeat 3 Binparser.read_char bits ;;
let string_of_MDFeedType v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_MDUpdateActionNew bits = (Binparser.string_to_int8 "0") , bits ;;
let string_of_MDUpdateActionNew v = "0" ;;
let read_MDUpdateTypeNew bits = (Binparser.string_to_int8 "0") , bits ;;
let string_of_MDUpdateTypeNew v = "0" ;;
let read_QuoteReqId bits = Binparser.repeat 23 Binparser.read_char bits ;;
let string_of_QuoteReqId v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_SecurityExchange bits = Binparser.repeat 4 Binparser.read_char bits ;;
let string_of_SecurityExchange v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_SecurityGroup bits = Binparser.repeat 6 Binparser.read_char bits ;;
let string_of_SecurityGroup v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_SecurityIDSource bits = (Binparser.string_to_char "8") , bits ;;
let string_of_SecurityIDSource v = "8" ;;
let read_SecuritySubType bits = Binparser.repeat 5 Binparser.read_char bits ;;
let string_of_SecuritySubType v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_SecurityType bits = Binparser.repeat 6 Binparser.read_char bits ;;
let string_of_SecurityType v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_Symbol bits = Binparser.repeat 20 Binparser.read_char bits ;;
let string_of_Symbol v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_Text bits = Binparser.repeat 180 Binparser.read_char bits ;;
let string_of_Text v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_UnderlyingSymbol bits = Binparser.repeat 20 Binparser.read_char bits ;;
let string_of_UnderlyingSymbol v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_UnitOfMeasure bits = Binparser.repeat 30 Binparser.read_char bits ;;
let string_of_UnitOfMeasure v = v |> List.map Binparser.string_of_char |> String.concat "" ;;
let read_UserDefinedInstrument bits = Binparser.read_char bits ;;
let string_of_UserDefinedInstrument v = Binparser.string_of_char v ;;
let bit_UserDefinedInstrument = Binparser.bit_char ;;
let read_uInt32 bits = Binparser.read_uint32 bits ;;
let string_of_uInt32 v = Binparser.string_of_uint32 v ;;
let bit_uInt32 = Binparser.bit_uint32 ;;
let read_uInt32NULL bits = Binparser.nullable_to_option ( Binparser.string_to_uint32 "4294967295") ( Binparser.read_uint32 bits ) ;;
let string_of_uInt32NULL v = match v with | Some x -> Binparser.string_of_uint32 x | None -> "4294967295" |> Binparser.string_to_uint32 |> Binparser.string_of_uint32 ;;
let read_uInt64 bits = Binparser.read_uint64 bits ;;
let string_of_uInt64 v = Binparser.string_of_uint64 v ;;
let bit_uInt64 = Binparser.bit_uint64 ;;
let read_uInt8 bits = Binparser.read_uint8 bits ;;
let string_of_uInt8 v = Binparser.string_of_uint8 v ;;
let bit_uInt8 = Binparser.bit_uint8 ;;
let read_uInt8NULL bits = Binparser.nullable_to_option ( Binparser.string_to_uint8 "255") ( Binparser.read_uint8 bits ) ;;
let string_of_uInt8NULL v = match v with | Some x -> Binparser.string_of_uint8 x | None -> "255" |> Binparser.string_to_uint8 |> Binparser.string_of_uint8 ;;
let read_DecimalQty bits = 
     let f_DecimalQty_mantissa , bits = Binparser.nullable_to_option ( Binparser.string_to_int32 "2147483647") ( Binparser.read_int32 bits ) in
     let f_DecimalQty_exponent , bits = (Binparser.string_to_int8 "-4") , bits in
    { f_DecimalQty_mantissa;f_DecimalQty_exponent}, bits
let read_FLOAT bits = 
     let f_FLOAT_mantissa , bits = Binparser.read_int64 bits in
     let f_FLOAT_exponent , bits = (Binparser.string_to_int8 "-7") , bits in
    { f_FLOAT_mantissa;f_FLOAT_exponent}, bits
let read_MaturityMonthYear bits = 
     let f_MaturityMonthYear_year , bits = Binparser.nullable_to_option ( Binparser.string_to_uint16 "65535") ( Binparser.read_uint16 bits ) in
     let f_MaturityMonthYear_month , bits = Binparser.nullable_to_option ( Binparser.string_to_uint8 "255") ( Binparser.read_uint8 bits ) in
     let f_MaturityMonthYear_day , bits = Binparser.nullable_to_option ( Binparser.string_to_uint8 "255") ( Binparser.read_uint8 bits ) in
     let f_MaturityMonthYear_week , bits = Binparser.nullable_to_option ( Binparser.string_to_uint8 "255") ( Binparser.read_uint8 bits ) in
    { f_MaturityMonthYear_year;f_MaturityMonthYear_month;f_MaturityMonthYear_day;f_MaturityMonthYear_week}, bits
let read_PRICE bits = 
     let f_PRICE_mantissa , bits = Binparser.read_int64 bits in
     let f_PRICE_exponent , bits = (Binparser.string_to_int8 "-7") , bits in
    { f_PRICE_mantissa;f_PRICE_exponent}, bits
let read_PRICENULL bits = 
     let f_PRICENULL_mantissa , bits = Binparser.nullable_to_option ( Binparser.string_to_int64 "9223372036854775807") ( Binparser.read_int64 bits ) in
     let f_PRICENULL_exponent , bits = (Binparser.string_to_int8 "-7") , bits in
    { f_PRICENULL_mantissa;f_PRICENULL_exponent}, bits
let read_groupSize bits = 
     let f_groupSize_blockLength , bits = Binparser.read_uint16 bits in
     let f_groupSize_numInGroup , bits = Binparser.read_uint8 bits in
    { f_groupSize_blockLength;f_groupSize_numInGroup}, bits
let read_groupSize8Byte bits = 
     let f_groupSize8Byte_blockLength , bits = Binparser.read_uint16 bits in
     let f_groupSize8Byte_numInGroup , bits = Binparser.read_uint8 bits in
    { f_groupSize8Byte_blockLength;f_groupSize8Byte_numInGroup}, bits
let read_groupSizeEncoding bits = 
     let f_groupSizeEncoding_blockLength , bits = Binparser.read_uint16 bits in
     let f_groupSizeEncoding_numInGroup , bits = Binparser.read_uint16 bits in
    { f_groupSizeEncoding_blockLength;f_groupSizeEncoding_numInGroup}, bits
let read_messageHeader bits = 
     let f_messageHeader_blockLength , bits = Binparser.read_uint16 bits in
     let f_messageHeader_templateId , bits = Binparser.read_uint16 bits in
     let f_messageHeader_schemaId , bits = Binparser.read_uint16 bits in
     let f_messageHeader_version , bits = Binparser.read_uint16 bits in
    { f_messageHeader_blockLength;f_messageHeader_templateId;f_messageHeader_schemaId;f_messageHeader_version}, bits
let read_AggressorSide bits = 
     let caseid,bits = read_uInt8NULL bits in 
     match string_of_uInt8NULL caseid with 
     | "0" -> V_AggressorSide_NoAggressor , bits
     | "1" -> V_AggressorSide_Buy , bits
     | "2" -> V_AggressorSide_Sell , bits
     | "255" -> V_AggressorSide_Null , bits 
     | _ -> raise (Failure ("Match error in t_AggressorSide"))
let read_EventType bits = 
     let caseid,bits = read_uInt8 bits in 
     match string_of_uInt8 caseid with 
     | "5" -> V_EventType_Activation , bits
     | "7" -> V_EventType_LastEligibleTradeDate , bits
     | _ -> raise (Failure ("Match error in t_EventType"))
let read_HaltReason bits = 
     let caseid,bits = read_uInt8 bits in 
     match string_of_uInt8 caseid with 
     | "0" -> V_HaltReason_GroupSchedule , bits
     | "1" -> V_HaltReason_SurveillanceIntervention , bits
     | "2" -> V_HaltReason_MarketEvent , bits
     | "3" -> V_HaltReason_InstrumentActivation , bits
     | "4" -> V_HaltReason_InstrumentExpiration , bits
     | "5" -> V_HaltReason_Unknown , bits
     | "6" -> V_HaltReason_RecoveryInProcess , bits
     | _ -> raise (Failure ("Match error in t_HaltReason"))
let read_LegSide bits = 
     let caseid,bits = read_uInt8 bits in 
     match string_of_uInt8 caseid with 
     | "1" -> V_LegSide_BuySide , bits
     | "2" -> V_LegSide_SellSide , bits
     | _ -> raise (Failure ("Match error in t_LegSide"))
let read_MDEntryType bits = 
     let caseid,bits = read_CHAR bits in 
     match string_of_CHAR caseid with 
     | "0" -> V_MDEntryType_Bid , bits
     | "1" -> V_MDEntryType_Offer , bits
     | "2" -> V_MDEntryType_Trade , bits
     | "4" -> V_MDEntryType_OpenPrice , bits
     | "6" -> V_MDEntryType_SettlementPrice , bits
     | "7" -> V_MDEntryType_TradingSessionHighPrice , bits
     | "8" -> V_MDEntryType_TradingSessionLowPrice , bits
     | "B" -> V_MDEntryType_ClearedVolume , bits
     | "C" -> V_MDEntryType_OpenInterest , bits
     | "E" -> V_MDEntryType_ImpliedBid , bits
     | "F" -> V_MDEntryType_ImpliedOffer , bits
     | "J" -> V_MDEntryType_BookReset , bits
     | "N" -> V_MDEntryType_SessionHighBid , bits
     | "O" -> V_MDEntryType_SessionLowOffer , bits
     | "W" -> V_MDEntryType_FixingPrice , bits
     | "e" -> V_MDEntryType_ElectronicVolume , bits
     | "g" -> V_MDEntryType_ThresholdLimitsandPriceBandVariation , bits
     | _ -> raise (Failure ("Match error in t_MDEntryType"))
let read_MDEntryTypeBook bits = 
     let caseid,bits = read_CHAR bits in 
     match string_of_CHAR caseid with 
     | "0" -> V_MDEntryTypeBook_Bid , bits
     | "1" -> V_MDEntryTypeBook_Offer , bits
     | "E" -> V_MDEntryTypeBook_ImpliedBid , bits
     | "F" -> V_MDEntryTypeBook_ImpliedOffer , bits
     | "J" -> V_MDEntryTypeBook_BookReset , bits
     | _ -> raise (Failure ("Match error in t_MDEntryTypeBook"))
let read_MDEntryTypeDailyStatistics bits = 
     let caseid,bits = read_CHAR bits in 
     match string_of_CHAR caseid with 
     | "6" -> V_MDEntryTypeDailyStatistics_SettlementPrice , bits
     | "B" -> V_MDEntryTypeDailyStatistics_ClearedVolume , bits
     | "C" -> V_MDEntryTypeDailyStatistics_OpenInterest , bits
     | "W" -> V_MDEntryTypeDailyStatistics_FixingPrice , bits
     | _ -> raise (Failure ("Match error in t_MDEntryTypeDailyStatistics"))
let read_MDEntryTypeStatistics bits = 
     let caseid,bits = read_CHAR bits in 
     match string_of_CHAR caseid with 
     | "4" -> V_MDEntryTypeStatistics_OpenPrice , bits
     | "7" -> V_MDEntryTypeStatistics_HighTrade , bits
     | "8" -> V_MDEntryTypeStatistics_LowTrade , bits
     | "N" -> V_MDEntryTypeStatistics_HighestBid , bits
     | "O" -> V_MDEntryTypeStatistics_LowestOffer , bits
     | _ -> raise (Failure ("Match error in t_MDEntryTypeStatistics"))
let read_MDUpdateAction bits = 
     let caseid,bits = read_uInt8 bits in 
     match string_of_uInt8 caseid with 
     | "0" -> V_MDUpdateAction_New , bits
     | "1" -> V_MDUpdateAction_Change , bits
     | "2" -> V_MDUpdateAction_Delete , bits
     | "3" -> V_MDUpdateAction_DeleteThru , bits
     | "4" -> V_MDUpdateAction_DeleteFrom , bits
     | "5" -> V_MDUpdateAction_Overlay , bits
     | _ -> raise (Failure ("Match error in t_MDUpdateAction"))
let read_OpenCloseSettlFlag bits = 
     let caseid,bits = read_uInt8NULL bits in 
     match string_of_uInt8NULL caseid with 
     | "0" -> V_OpenCloseSettlFlag_DailyOpenPrice , bits
     | "5" -> V_OpenCloseSettlFlag_IndicativeOpeningPrice , bits
     | "255" -> V_OpenCloseSettlFlag_Null , bits 
     | _ -> raise (Failure ("Match error in t_OpenCloseSettlFlag"))
let read_PutOrCall bits = 
     let caseid,bits = read_uInt8 bits in 
     match string_of_uInt8 caseid with 
     | "0" -> V_PutOrCall_Put , bits
     | "1" -> V_PutOrCall_Call , bits
     | _ -> raise (Failure ("Match error in t_PutOrCall"))
let read_SecurityTradingEvent bits = 
     let caseid,bits = read_uInt8 bits in 
     match string_of_uInt8 caseid with 
     | "0" -> V_SecurityTradingEvent_NoEvent , bits
     | "1" -> V_SecurityTradingEvent_NoCancel , bits
     | "4" -> V_SecurityTradingEvent_ResetStatistics , bits
     | "5" -> V_SecurityTradingEvent_ImpliedMatchingON , bits
     | "6" -> V_SecurityTradingEvent_ImpliedMatchingOFF , bits
     | _ -> raise (Failure ("Match error in t_SecurityTradingEvent"))
let read_SecurityTradingStatus bits = 
     let caseid,bits = read_uInt8NULL bits in 
     match string_of_uInt8NULL caseid with 
     | "2" -> V_SecurityTradingStatus_TradingHalt , bits
     | "4" -> V_SecurityTradingStatus_Close , bits
     | "15" -> V_SecurityTradingStatus_NewPriceIndication , bits
     | "17" -> V_SecurityTradingStatus_ReadyToTrade , bits
     | "18" -> V_SecurityTradingStatus_NotAvailableForTrading , bits
     | "20" -> V_SecurityTradingStatus_UnknownorInvalid , bits
     | "21" -> V_SecurityTradingStatus_PreOpen , bits
     | "24" -> V_SecurityTradingStatus_PreCross , bits
     | "25" -> V_SecurityTradingStatus_Cross , bits
     | "26" -> V_SecurityTradingStatus_PostClose , bits
     | "103" -> V_SecurityTradingStatus_NoChange , bits
     | "255" -> V_SecurityTradingStatus_Null , bits 
     | _ -> raise (Failure ("Match error in t_SecurityTradingStatus"))
let read_SecurityUpdateAction bits = 
     let caseid,bits = read_CHAR bits in 
     match string_of_CHAR caseid with 
     | "A" -> V_SecurityUpdateAction_Add , bits
     | "D" -> V_SecurityUpdateAction_Delete , bits
     | "M" -> V_SecurityUpdateAction_Modify , bits
     | _ -> raise (Failure ("Match error in t_SecurityUpdateAction"))
let read_InstAttribValue bits = 
    let v, bits = read_uInt32 bits in 
    {
        r_InstAttribValue_ElectronicMatchEligible = bit_uInt32 v 0;
        r_InstAttribValue_OrderCrossEligible = bit_uInt32 v 1;
        r_InstAttribValue_BlockTradeEligible = bit_uInt32 v 2;
        r_InstAttribValue_EFPEligible = bit_uInt32 v 3;
        r_InstAttribValue_EBFEligible = bit_uInt32 v 4;
        r_InstAttribValue_EFSEligible = bit_uInt32 v 5;
        r_InstAttribValue_EFREligible = bit_uInt32 v 6;
        r_InstAttribValue_OTCEligible = bit_uInt32 v 7;
        r_InstAttribValue_iLinkIndicativeMassQuotingEligible = bit_uInt32 v 8;
        r_InstAttribValue_NegativeStrikeEligible = bit_uInt32 v 9;
        r_InstAttribValue_NegativePriceOutrightEligible = bit_uInt32 v 10;
        r_InstAttribValue_IsFractional = bit_uInt32 v 11;
        r_InstAttribValue_VolatilityQuotedOption = bit_uInt32 v 12;
        r_InstAttribValue_RFQCrossEligible = bit_uInt32 v 13;
        r_InstAttribValue_ZeroPriceOutrightEligible = bit_uInt32 v 14;
        r_InstAttribValue_DecayingProductEligibility = bit_uInt32 v 15;
        r_InstAttribValue_VariableProductEligibility = bit_uInt32 v 16;
        r_InstAttribValue_DailyProductEligibility = bit_uInt32 v 17;
        r_InstAttribValue_GTOrdersEligibility = bit_uInt32 v 18;
        r_InstAttribValue_ImpliedMatchingEligibility = bit_uInt32 v 19
    }, bits
let read_MatchEventIndicator bits = 
    let v, bits = read_uInt8 bits in 
    {
        r_MatchEventIndicator_LastTradeMsg = bit_uInt8 v 0;
        r_MatchEventIndicator_LastVolumeMsg = bit_uInt8 v 1;
        r_MatchEventIndicator_LastQuoteMsg = bit_uInt8 v 2;
        r_MatchEventIndicator_LastStatsMsg = bit_uInt8 v 3;
        r_MatchEventIndicator_LastImpliedMsg = bit_uInt8 v 4;
        r_MatchEventIndicator_RecoveryMsg = bit_uInt8 v 5;
        r_MatchEventIndicator_Reserved = bit_uInt8 v 6;
        r_MatchEventIndicator_EndOfEvent = bit_uInt8 v 7
    }, bits
let read_SettlPriceType bits = 
    let v, bits = read_uInt8 bits in 
    {
        r_SettlPriceType_Final = bit_uInt8 v 0;
        r_SettlPriceType_Actual = bit_uInt8 v 1;
        r_SettlPriceType_Rounded = bit_uInt8 v 2;
        r_SettlPriceType_Intraday = bit_uInt8 v 3;
        r_SettlPriceType_ReservedBits = bit_uInt8 v 4;
        r_SettlPriceType_NullValue = bit_uInt8 v 7
    }, bits
let read_g_ChannelReset4_NoMDEntries bits = 
    let f_ChannelReset4_NoMDEntries_MDUpdateAction, bits = read_MDUpdateTypeNew bits in
    let f_ChannelReset4_NoMDEntries_MDEntryType, bits = read_MDEntryTypeChannelReset bits in
    let f_ChannelReset4_NoMDEntries_ApplID, bits = read_Int16 bits in
    {f_ChannelReset4_NoMDEntries_MDUpdateAction; f_ChannelReset4_NoMDEntries_MDEntryType; f_ChannelReset4_NoMDEntries_ApplID}, bits

let read_msg_ChannelReset4 (bits, gbits) = 
    let f_ChannelReset4_TransactTime, bits = read_uInt64 bits in
    let f_ChannelReset4_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_ChannelReset4_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_ChannelReset4_NoMDEntries gbits in
    { f_ChannelReset4_TransactTime; f_ChannelReset4_MatchEventIndicator; f_ChannelReset4_NoMDEntries } 
let read_msg_AdminHeartbeat12 (bits, gbits) = () 


let read_msg_AdminLogin15 (bits, gbits) = 
    let f_AdminLogin15_HeartBtInt, bits = read_Int8 bits in
    { f_AdminLogin15_HeartBtInt } 

let read_msg_AdminLogout16 (bits, gbits) = 
    let f_AdminLogout16_Text, bits = read_Text bits in
    { f_AdminLogout16_Text } 
let read_g_MDInstrumentDefinitionFuture27_NoEvents bits = 
    let f_MDInstrumentDefinitionFuture27_NoEvents_EventType, bits = read_EventType bits in
    let f_MDInstrumentDefinitionFuture27_NoEvents_EventTime, bits = read_uInt64 bits in
    {f_MDInstrumentDefinitionFuture27_NoEvents_EventType; f_MDInstrumentDefinitionFuture27_NoEvents_EventTime}, bits
let read_g_MDInstrumentDefinitionFuture27_NoMDFeedTypes bits = 
    let f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MDFeedType, bits = read_MDFeedType bits in
    let f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MarketDepth, bits = read_Int8 bits in
    {f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MDFeedType; f_MDInstrumentDefinitionFuture27_NoMDFeedTypes_MarketDepth}, bits
let read_g_MDInstrumentDefinitionFuture27_NoInstAttrib bits = 
    let f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribType, bits = read_InstAttribType bits in
    let f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribValue, bits = read_InstAttribValue bits in
    {f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribType; f_MDInstrumentDefinitionFuture27_NoInstAttrib_InstAttribValue}, bits
let read_g_MDInstrumentDefinitionFuture27_NoLotTypeRules bits = 
    let f_MDInstrumentDefinitionFuture27_NoLotTypeRules_LotType, bits = read_Int8 bits in
    let f_MDInstrumentDefinitionFuture27_NoLotTypeRules_MinLotSize, bits = read_DecimalQty bits in
    {f_MDInstrumentDefinitionFuture27_NoLotTypeRules_LotType; f_MDInstrumentDefinitionFuture27_NoLotTypeRules_MinLotSize}, bits

let read_msg_MDInstrumentDefinitionFuture27 (bits, gbits) = 
    let f_MDInstrumentDefinitionFuture27_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDInstrumentDefinitionFuture27_TotNumReports, bits = read_uInt32NULL bits in
    let f_MDInstrumentDefinitionFuture27_SecurityUpdateAction, bits = read_SecurityUpdateAction bits in
    let f_MDInstrumentDefinitionFuture27_LastUpdateTime, bits = read_uInt64 bits in
    let f_MDInstrumentDefinitionFuture27_MDSecurityTradingStatus, bits = read_SecurityTradingStatus bits in
    let f_MDInstrumentDefinitionFuture27_ApplID, bits = read_Int16 bits in
    let f_MDInstrumentDefinitionFuture27_MarketSegmentID, bits = read_uInt8 bits in
    let f_MDInstrumentDefinitionFuture27_UnderlyingProduct, bits = read_uInt8 bits in
    let f_MDInstrumentDefinitionFuture27_SecurityExchange, bits = read_SecurityExchange bits in
    let f_MDInstrumentDefinitionFuture27_SecurityGroup, bits = read_SecurityGroup bits in
    let f_MDInstrumentDefinitionFuture27_Asset, bits = read_Asset bits in
    let f_MDInstrumentDefinitionFuture27_Symbol, bits = read_Symbol bits in
    let f_MDInstrumentDefinitionFuture27_SecurityID, bits = read_Int32 bits in
    let f_MDInstrumentDefinitionFuture27_SecurityIDSource, bits = read_SecurityIDSource bits in
    let f_MDInstrumentDefinitionFuture27_SecurityType, bits = read_SecurityType bits in
    let f_MDInstrumentDefinitionFuture27_CFICode, bits = read_CFICode bits in
    let f_MDInstrumentDefinitionFuture27_MaturityMonthYear, bits = read_MaturityMonthYear bits in
    let f_MDInstrumentDefinitionFuture27_Currency, bits = read_Currency bits in
    let f_MDInstrumentDefinitionFuture27_SettlCurrency, bits = read_Currency bits in
    let f_MDInstrumentDefinitionFuture27_MatchAlgorithm, bits = read_CHAR bits in
    let f_MDInstrumentDefinitionFuture27_MinTradeVol, bits = read_uInt32 bits in
    let f_MDInstrumentDefinitionFuture27_MaxTradeVol, bits = read_uInt32 bits in
    let f_MDInstrumentDefinitionFuture27_MinPriceIncrement, bits = read_PRICE bits in
    let f_MDInstrumentDefinitionFuture27_DisplayFactor, bits = read_FLOAT bits in
    let f_MDInstrumentDefinitionFuture27_MainFraction, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionFuture27_SubFraction, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionFuture27_PriceDisplayFormat, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionFuture27_UnitOfMeasure, bits = read_UnitOfMeasure bits in
    let f_MDInstrumentDefinitionFuture27_UnitOfMeasureQty, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionFuture27_TradingReferencePrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionFuture27_SettlPriceType, bits = read_SettlPriceType bits in
    let f_MDInstrumentDefinitionFuture27_OpenInterestQty, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionFuture27_ClearedVolume, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionFuture27_HighLimitPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionFuture27_LowLimitPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionFuture27_MaxPriceVariation, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionFuture27_DecayQuantity, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionFuture27_DecayStartDate, bits = read_LocalMktDate bits in
    let f_MDInstrumentDefinitionFuture27_OriginalContractSize, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionFuture27_ContractMultiplier, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionFuture27_ContractMultiplierUnit, bits = read_Int8NULL bits in
    let f_MDInstrumentDefinitionFuture27_FlowScheduleType, bits = read_Int8NULL bits in
    let f_MDInstrumentDefinitionFuture27_MinPriceIncrementAmount, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionFuture27_UserDefinedInstrument, bits = read_UserDefinedInstrument bits in
    let f_MDInstrumentDefinitionFuture27_TradingReferenceDate, bits = read_LocalMktDate bits in
    let f_MDInstrumentDefinitionFuture27_NoEvents, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionFuture27_NoEvents gbits in
    let f_MDInstrumentDefinitionFuture27_NoMDFeedTypes, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionFuture27_NoMDFeedTypes gbits in
    let f_MDInstrumentDefinitionFuture27_NoInstAttrib, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionFuture27_NoInstAttrib gbits in
    let f_MDInstrumentDefinitionFuture27_NoLotTypeRules, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionFuture27_NoLotTypeRules gbits in
    { f_MDInstrumentDefinitionFuture27_MatchEventIndicator; f_MDInstrumentDefinitionFuture27_TotNumReports; f_MDInstrumentDefinitionFuture27_SecurityUpdateAction; f_MDInstrumentDefinitionFuture27_LastUpdateTime; f_MDInstrumentDefinitionFuture27_MDSecurityTradingStatus; f_MDInstrumentDefinitionFuture27_ApplID; f_MDInstrumentDefinitionFuture27_MarketSegmentID; f_MDInstrumentDefinitionFuture27_UnderlyingProduct; f_MDInstrumentDefinitionFuture27_SecurityExchange; f_MDInstrumentDefinitionFuture27_SecurityGroup; f_MDInstrumentDefinitionFuture27_Asset; f_MDInstrumentDefinitionFuture27_Symbol; f_MDInstrumentDefinitionFuture27_SecurityID; f_MDInstrumentDefinitionFuture27_SecurityIDSource; f_MDInstrumentDefinitionFuture27_SecurityType; f_MDInstrumentDefinitionFuture27_CFICode; f_MDInstrumentDefinitionFuture27_MaturityMonthYear; f_MDInstrumentDefinitionFuture27_Currency; f_MDInstrumentDefinitionFuture27_SettlCurrency; f_MDInstrumentDefinitionFuture27_MatchAlgorithm; f_MDInstrumentDefinitionFuture27_MinTradeVol; f_MDInstrumentDefinitionFuture27_MaxTradeVol; f_MDInstrumentDefinitionFuture27_MinPriceIncrement; f_MDInstrumentDefinitionFuture27_DisplayFactor; f_MDInstrumentDefinitionFuture27_MainFraction; f_MDInstrumentDefinitionFuture27_SubFraction; f_MDInstrumentDefinitionFuture27_PriceDisplayFormat; f_MDInstrumentDefinitionFuture27_UnitOfMeasure; f_MDInstrumentDefinitionFuture27_UnitOfMeasureQty; f_MDInstrumentDefinitionFuture27_TradingReferencePrice; f_MDInstrumentDefinitionFuture27_SettlPriceType; f_MDInstrumentDefinitionFuture27_OpenInterestQty; f_MDInstrumentDefinitionFuture27_ClearedVolume; f_MDInstrumentDefinitionFuture27_HighLimitPrice; f_MDInstrumentDefinitionFuture27_LowLimitPrice; f_MDInstrumentDefinitionFuture27_MaxPriceVariation; f_MDInstrumentDefinitionFuture27_DecayQuantity; f_MDInstrumentDefinitionFuture27_DecayStartDate; f_MDInstrumentDefinitionFuture27_OriginalContractSize; f_MDInstrumentDefinitionFuture27_ContractMultiplier; f_MDInstrumentDefinitionFuture27_ContractMultiplierUnit; f_MDInstrumentDefinitionFuture27_FlowScheduleType; f_MDInstrumentDefinitionFuture27_MinPriceIncrementAmount; f_MDInstrumentDefinitionFuture27_UserDefinedInstrument; f_MDInstrumentDefinitionFuture27_TradingReferenceDate; f_MDInstrumentDefinitionFuture27_NoEvents; f_MDInstrumentDefinitionFuture27_NoMDFeedTypes; f_MDInstrumentDefinitionFuture27_NoInstAttrib; f_MDInstrumentDefinitionFuture27_NoLotTypeRules } 
let read_g_MDInstrumentDefinitionSpread29_NoEvents bits = 
    let f_MDInstrumentDefinitionSpread29_NoEvents_EventType, bits = read_EventType bits in
    let f_MDInstrumentDefinitionSpread29_NoEvents_EventTime, bits = read_uInt64 bits in
    {f_MDInstrumentDefinitionSpread29_NoEvents_EventType; f_MDInstrumentDefinitionSpread29_NoEvents_EventTime}, bits
let read_g_MDInstrumentDefinitionSpread29_NoMDFeedTypes bits = 
    let f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MDFeedType, bits = read_MDFeedType bits in
    let f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MarketDepth, bits = read_Int8 bits in
    {f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MDFeedType; f_MDInstrumentDefinitionSpread29_NoMDFeedTypes_MarketDepth}, bits
let read_g_MDInstrumentDefinitionSpread29_NoInstAttrib bits = 
    let f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribType, bits = read_InstAttribType bits in
    let f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribValue, bits = read_InstAttribValue bits in
    {f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribType; f_MDInstrumentDefinitionSpread29_NoInstAttrib_InstAttribValue}, bits
let read_g_MDInstrumentDefinitionSpread29_NoLotTypeRules bits = 
    let f_MDInstrumentDefinitionSpread29_NoLotTypeRules_LotType, bits = read_Int8 bits in
    let f_MDInstrumentDefinitionSpread29_NoLotTypeRules_MinLotSize, bits = read_DecimalQty bits in
    {f_MDInstrumentDefinitionSpread29_NoLotTypeRules_LotType; f_MDInstrumentDefinitionSpread29_NoLotTypeRules_MinLotSize}, bits
let read_g_MDInstrumentDefinitionSpread29_NoLegs bits = 
    let f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityID, bits = read_Int32 bits in
    let f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityIDSource, bits = read_SecurityIDSource bits in
    let f_MDInstrumentDefinitionSpread29_NoLegs_LegSide, bits = read_LegSide bits in
    let f_MDInstrumentDefinitionSpread29_NoLegs_LegRatioQty, bits = read_Int8 bits in
    let f_MDInstrumentDefinitionSpread29_NoLegs_LegPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionSpread29_NoLegs_LegOptionDelta, bits = read_DecimalQty bits in
    {f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityID; f_MDInstrumentDefinitionSpread29_NoLegs_LegSecurityIDSource; f_MDInstrumentDefinitionSpread29_NoLegs_LegSide; f_MDInstrumentDefinitionSpread29_NoLegs_LegRatioQty; f_MDInstrumentDefinitionSpread29_NoLegs_LegPrice; f_MDInstrumentDefinitionSpread29_NoLegs_LegOptionDelta}, bits

let read_msg_MDInstrumentDefinitionSpread29 (bits, gbits) = 
    let f_MDInstrumentDefinitionSpread29_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDInstrumentDefinitionSpread29_TotNumReports, bits = read_uInt32NULL bits in
    let f_MDInstrumentDefinitionSpread29_SecurityUpdateAction, bits = read_SecurityUpdateAction bits in
    let f_MDInstrumentDefinitionSpread29_LastUpdateTime, bits = read_uInt64 bits in
    let f_MDInstrumentDefinitionSpread29_MDSecurityTradingStatus, bits = read_SecurityTradingStatus bits in
    let f_MDInstrumentDefinitionSpread29_ApplID, bits = read_Int16 bits in
    let f_MDInstrumentDefinitionSpread29_MarketSegmentID, bits = read_uInt8 bits in
    let f_MDInstrumentDefinitionSpread29_UnderlyingProduct, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionSpread29_SecurityExchange, bits = read_SecurityExchange bits in
    let f_MDInstrumentDefinitionSpread29_SecurityGroup, bits = read_SecurityGroup bits in
    let f_MDInstrumentDefinitionSpread29_Asset, bits = read_Asset bits in
    let f_MDInstrumentDefinitionSpread29_Symbol, bits = read_Symbol bits in
    let f_MDInstrumentDefinitionSpread29_SecurityID, bits = read_Int32 bits in
    let f_MDInstrumentDefinitionSpread29_SecurityIDSource, bits = read_SecurityIDSource bits in
    let f_MDInstrumentDefinitionSpread29_SecurityType, bits = read_SecurityType bits in
    let f_MDInstrumentDefinitionSpread29_CFICode, bits = read_CFICode bits in
    let f_MDInstrumentDefinitionSpread29_MaturityMonthYear, bits = read_MaturityMonthYear bits in
    let f_MDInstrumentDefinitionSpread29_Currency, bits = read_Currency bits in
    let f_MDInstrumentDefinitionSpread29_SecuritySubType, bits = read_SecuritySubType bits in
    let f_MDInstrumentDefinitionSpread29_UserDefinedInstrument, bits = read_UserDefinedInstrument bits in
    let f_MDInstrumentDefinitionSpread29_MatchAlgorithm, bits = read_CHAR bits in
    let f_MDInstrumentDefinitionSpread29_MinTradeVol, bits = read_uInt32 bits in
    let f_MDInstrumentDefinitionSpread29_MaxTradeVol, bits = read_uInt32 bits in
    let f_MDInstrumentDefinitionSpread29_MinPriceIncrement, bits = read_PRICE bits in
    let f_MDInstrumentDefinitionSpread29_DisplayFactor, bits = read_FLOAT bits in
    let f_MDInstrumentDefinitionSpread29_PriceDisplayFormat, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionSpread29_PriceRatio, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionSpread29_TickRule, bits = read_Int8NULL bits in
    let f_MDInstrumentDefinitionSpread29_UnitOfMeasure, bits = read_UnitOfMeasure bits in
    let f_MDInstrumentDefinitionSpread29_TradingReferencePrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionSpread29_SettlPriceType, bits = read_SettlPriceType bits in
    let f_MDInstrumentDefinitionSpread29_OpenInterestQty, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionSpread29_ClearedVolume, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionSpread29_HighLimitPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionSpread29_LowLimitPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionSpread29_MaxPriceVariation, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionSpread29_MainFraction, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionSpread29_SubFraction, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionSpread29_TradingReferenceDate, bits = read_LocalMktDate bits in
    let f_MDInstrumentDefinitionSpread29_NoEvents, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionSpread29_NoEvents gbits in
    let f_MDInstrumentDefinitionSpread29_NoMDFeedTypes, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionSpread29_NoMDFeedTypes gbits in
    let f_MDInstrumentDefinitionSpread29_NoInstAttrib, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionSpread29_NoInstAttrib gbits in
    let f_MDInstrumentDefinitionSpread29_NoLotTypeRules, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionSpread29_NoLotTypeRules gbits in
    let f_MDInstrumentDefinitionSpread29_NoLegs, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionSpread29_NoLegs gbits in
    { f_MDInstrumentDefinitionSpread29_MatchEventIndicator; f_MDInstrumentDefinitionSpread29_TotNumReports; f_MDInstrumentDefinitionSpread29_SecurityUpdateAction; f_MDInstrumentDefinitionSpread29_LastUpdateTime; f_MDInstrumentDefinitionSpread29_MDSecurityTradingStatus; f_MDInstrumentDefinitionSpread29_ApplID; f_MDInstrumentDefinitionSpread29_MarketSegmentID; f_MDInstrumentDefinitionSpread29_UnderlyingProduct; f_MDInstrumentDefinitionSpread29_SecurityExchange; f_MDInstrumentDefinitionSpread29_SecurityGroup; f_MDInstrumentDefinitionSpread29_Asset; f_MDInstrumentDefinitionSpread29_Symbol; f_MDInstrumentDefinitionSpread29_SecurityID; f_MDInstrumentDefinitionSpread29_SecurityIDSource; f_MDInstrumentDefinitionSpread29_SecurityType; f_MDInstrumentDefinitionSpread29_CFICode; f_MDInstrumentDefinitionSpread29_MaturityMonthYear; f_MDInstrumentDefinitionSpread29_Currency; f_MDInstrumentDefinitionSpread29_SecuritySubType; f_MDInstrumentDefinitionSpread29_UserDefinedInstrument; f_MDInstrumentDefinitionSpread29_MatchAlgorithm; f_MDInstrumentDefinitionSpread29_MinTradeVol; f_MDInstrumentDefinitionSpread29_MaxTradeVol; f_MDInstrumentDefinitionSpread29_MinPriceIncrement; f_MDInstrumentDefinitionSpread29_DisplayFactor; f_MDInstrumentDefinitionSpread29_PriceDisplayFormat; f_MDInstrumentDefinitionSpread29_PriceRatio; f_MDInstrumentDefinitionSpread29_TickRule; f_MDInstrumentDefinitionSpread29_UnitOfMeasure; f_MDInstrumentDefinitionSpread29_TradingReferencePrice; f_MDInstrumentDefinitionSpread29_SettlPriceType; f_MDInstrumentDefinitionSpread29_OpenInterestQty; f_MDInstrumentDefinitionSpread29_ClearedVolume; f_MDInstrumentDefinitionSpread29_HighLimitPrice; f_MDInstrumentDefinitionSpread29_LowLimitPrice; f_MDInstrumentDefinitionSpread29_MaxPriceVariation; f_MDInstrumentDefinitionSpread29_MainFraction; f_MDInstrumentDefinitionSpread29_SubFraction; f_MDInstrumentDefinitionSpread29_TradingReferenceDate; f_MDInstrumentDefinitionSpread29_NoEvents; f_MDInstrumentDefinitionSpread29_NoMDFeedTypes; f_MDInstrumentDefinitionSpread29_NoInstAttrib; f_MDInstrumentDefinitionSpread29_NoLotTypeRules; f_MDInstrumentDefinitionSpread29_NoLegs } 

let read_msg_SecurityStatus30 (bits, gbits) = 
    let f_SecurityStatus30_TransactTime, bits = read_uInt64 bits in
    let f_SecurityStatus30_SecurityGroup, bits = read_SecurityGroup bits in
    let f_SecurityStatus30_Asset, bits = read_Asset bits in
    let f_SecurityStatus30_SecurityID, bits = read_Int32NULL bits in
    let f_SecurityStatus30_TradeDate, bits = read_LocalMktDate bits in
    let f_SecurityStatus30_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_SecurityStatus30_SecurityTradingStatus, bits = read_SecurityTradingStatus bits in
    let f_SecurityStatus30_HaltReason, bits = read_HaltReason bits in
    let f_SecurityStatus30_SecurityTradingEvent, bits = read_SecurityTradingEvent bits in
    { f_SecurityStatus30_TransactTime; f_SecurityStatus30_SecurityGroup; f_SecurityStatus30_Asset; f_SecurityStatus30_SecurityID; f_SecurityStatus30_TradeDate; f_SecurityStatus30_MatchEventIndicator; f_SecurityStatus30_SecurityTradingStatus; f_SecurityStatus30_HaltReason; f_SecurityStatus30_SecurityTradingEvent } 
let read_g_MDIncrementalRefreshBook32_NoMDEntries bits = 
    let f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx, bits = read_PRICENULL bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize, bits = read_Int32NULL bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq, bits = read_uInt32 bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders, bits = read_Int32NULL bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel, bits = read_uInt8 bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction, bits = read_MDUpdateAction bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType, bits = read_MDEntryTypeBook bits in
    {f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryPx; f_MDIncrementalRefreshBook32_NoMDEntries_MDEntrySize; f_MDIncrementalRefreshBook32_NoMDEntries_SecurityID; f_MDIncrementalRefreshBook32_NoMDEntries_RptSeq; f_MDIncrementalRefreshBook32_NoMDEntries_NumberOfOrders; f_MDIncrementalRefreshBook32_NoMDEntries_MDPriceLevel; f_MDIncrementalRefreshBook32_NoMDEntries_MDUpdateAction; f_MDIncrementalRefreshBook32_NoMDEntries_MDEntryType}, bits

let read_msg_MDIncrementalRefreshBook32 (bits, gbits) = 
    let f_MDIncrementalRefreshBook32_TransactTime, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshBook32_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDIncrementalRefreshBook32_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshBook32_NoMDEntries gbits in
    { f_MDIncrementalRefreshBook32_TransactTime; f_MDIncrementalRefreshBook32_MatchEventIndicator; f_MDIncrementalRefreshBook32_NoMDEntries } 
let read_g_MDIncrementalRefreshDailyStatistics33_NoMDEntries bits = 
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryPx, bits = read_PRICENULL bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntrySize, bits = read_Int32NULL bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SecurityID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_RptSeq, bits = read_uInt32 bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_TradingReferenceDate, bits = read_LocalMktDate bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SettlPriceType, bits = read_SettlPriceType bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDUpdateAction, bits = read_MDUpdateAction bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryType, bits = read_MDEntryTypeDailyStatistics bits in
    {f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryPx; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntrySize; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SecurityID; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_RptSeq; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_TradingReferenceDate; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_SettlPriceType; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDUpdateAction; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries_MDEntryType}, bits

let read_msg_MDIncrementalRefreshDailyStatistics33 (bits, gbits) = 
    let f_MDIncrementalRefreshDailyStatistics33_TransactTime, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshDailyStatistics33_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDIncrementalRefreshDailyStatistics33_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshDailyStatistics33_NoMDEntries gbits in
    { f_MDIncrementalRefreshDailyStatistics33_TransactTime; f_MDIncrementalRefreshDailyStatistics33_MatchEventIndicator; f_MDIncrementalRefreshDailyStatistics33_NoMDEntries } 
let read_g_MDIncrementalRefreshLimitsBanding34_NoMDEntries bits = 
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_HighLimitPrice, bits = read_PRICENULL bits in
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_LowLimitPrice, bits = read_PRICENULL bits in
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MaxPriceVariation, bits = read_PRICENULL bits in
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_SecurityID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_RptSeq, bits = read_uInt32 bits in
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDUpdateAction, bits = read_MDUpdateActionNew bits in
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDEntryType, bits = read_MDEntryTypeLimits bits in
    {f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_HighLimitPrice; f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_LowLimitPrice; f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MaxPriceVariation; f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_SecurityID; f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_RptSeq; f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDUpdateAction; f_MDIncrementalRefreshLimitsBanding34_NoMDEntries_MDEntryType}, bits

let read_msg_MDIncrementalRefreshLimitsBanding34 (bits, gbits) = 
    let f_MDIncrementalRefreshLimitsBanding34_TransactTime, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshLimitsBanding34_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDIncrementalRefreshLimitsBanding34_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshLimitsBanding34_NoMDEntries gbits in
    { f_MDIncrementalRefreshLimitsBanding34_TransactTime; f_MDIncrementalRefreshLimitsBanding34_MatchEventIndicator; f_MDIncrementalRefreshLimitsBanding34_NoMDEntries } 
let read_g_MDIncrementalRefreshSessionStatistics35_NoMDEntries bits = 
    let f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryPx, bits = read_PRICE bits in
    let f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_SecurityID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_RptSeq, bits = read_uInt32 bits in
    let f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_OpenCloseSettlFlag, bits = read_OpenCloseSettlFlag bits in
    let f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDUpdateAction, bits = read_MDUpdateAction bits in
    let f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryType, bits = read_MDEntryTypeStatistics bits in
    {f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryPx; f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_SecurityID; f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_RptSeq; f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_OpenCloseSettlFlag; f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDUpdateAction; f_MDIncrementalRefreshSessionStatistics35_NoMDEntries_MDEntryType}, bits

let read_msg_MDIncrementalRefreshSessionStatistics35 (bits, gbits) = 
    let f_MDIncrementalRefreshSessionStatistics35_TransactTime, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshSessionStatistics35_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDIncrementalRefreshSessionStatistics35_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshSessionStatistics35_NoMDEntries gbits in
    { f_MDIncrementalRefreshSessionStatistics35_TransactTime; f_MDIncrementalRefreshSessionStatistics35_MatchEventIndicator; f_MDIncrementalRefreshSessionStatistics35_NoMDEntries } 
let read_g_MDIncrementalRefreshTrade36_NoMDEntries bits = 
    let f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryPx, bits = read_PRICE bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntrySize, bits = read_Int32 bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_SecurityID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_RptSeq, bits = read_uInt32 bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_NumberOfOrders, bits = read_Int32NULL bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_TradeID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_AggressorSide, bits = read_AggressorSide bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_MDUpdateAction, bits = read_MDUpdateAction bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryType, bits = read_MDEntryTypeTrade bits in
    {f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryPx; f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntrySize; f_MDIncrementalRefreshTrade36_NoMDEntries_SecurityID; f_MDIncrementalRefreshTrade36_NoMDEntries_RptSeq; f_MDIncrementalRefreshTrade36_NoMDEntries_NumberOfOrders; f_MDIncrementalRefreshTrade36_NoMDEntries_TradeID; f_MDIncrementalRefreshTrade36_NoMDEntries_AggressorSide; f_MDIncrementalRefreshTrade36_NoMDEntries_MDUpdateAction; f_MDIncrementalRefreshTrade36_NoMDEntries_MDEntryType}, bits

let read_msg_MDIncrementalRefreshTrade36 (bits, gbits) = 
    let f_MDIncrementalRefreshTrade36_TransactTime, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshTrade36_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDIncrementalRefreshTrade36_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshTrade36_NoMDEntries gbits in
    { f_MDIncrementalRefreshTrade36_TransactTime; f_MDIncrementalRefreshTrade36_MatchEventIndicator; f_MDIncrementalRefreshTrade36_NoMDEntries } 
let read_g_MDIncrementalRefreshVolume37_NoMDEntries bits = 
    let f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntrySize, bits = read_Int32 bits in
    let f_MDIncrementalRefreshVolume37_NoMDEntries_SecurityID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshVolume37_NoMDEntries_RptSeq, bits = read_uInt32 bits in
    let f_MDIncrementalRefreshVolume37_NoMDEntries_MDUpdateAction, bits = read_MDUpdateAction bits in
    let f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntryType, bits = read_MDEntryTypeVol bits in
    {f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntrySize; f_MDIncrementalRefreshVolume37_NoMDEntries_SecurityID; f_MDIncrementalRefreshVolume37_NoMDEntries_RptSeq; f_MDIncrementalRefreshVolume37_NoMDEntries_MDUpdateAction; f_MDIncrementalRefreshVolume37_NoMDEntries_MDEntryType}, bits

let read_msg_MDIncrementalRefreshVolume37 (bits, gbits) = 
    let f_MDIncrementalRefreshVolume37_TransactTime, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshVolume37_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDIncrementalRefreshVolume37_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshVolume37_NoMDEntries gbits in
    { f_MDIncrementalRefreshVolume37_TransactTime; f_MDIncrementalRefreshVolume37_MatchEventIndicator; f_MDIncrementalRefreshVolume37_NoMDEntries } 
let read_g_SnapshotFullRefresh38_NoMDEntries bits = 
    let f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx, bits = read_PRICENULL bits in
    let f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize, bits = read_Int32NULL bits in
    let f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders, bits = read_Int32NULL bits in
    let f_SnapshotFullRefresh38_NoMDEntries_MDPriceLevel, bits = read_Int8NULL bits in
    let f_SnapshotFullRefresh38_NoMDEntries_TradingReferenceDate, bits = read_LocalMktDate bits in
    let f_SnapshotFullRefresh38_NoMDEntries_OpenCloseSettlFlag, bits = read_OpenCloseSettlFlag bits in
    let f_SnapshotFullRefresh38_NoMDEntries_SettlPriceType, bits = read_SettlPriceType bits in
    let f_SnapshotFullRefresh38_NoMDEntries_MDEntryType, bits = read_MDEntryType bits in
    {f_SnapshotFullRefresh38_NoMDEntries_MDEntryPx; f_SnapshotFullRefresh38_NoMDEntries_MDEntrySize; f_SnapshotFullRefresh38_NoMDEntries_NumberOfOrders; f_SnapshotFullRefresh38_NoMDEntries_MDPriceLevel; f_SnapshotFullRefresh38_NoMDEntries_TradingReferenceDate; f_SnapshotFullRefresh38_NoMDEntries_OpenCloseSettlFlag; f_SnapshotFullRefresh38_NoMDEntries_SettlPriceType; f_SnapshotFullRefresh38_NoMDEntries_MDEntryType}, bits

let read_msg_SnapshotFullRefresh38 (bits, gbits) = 
    let f_SnapshotFullRefresh38_LastMsgSeqNumProcessed, bits = read_uInt32 bits in
    let f_SnapshotFullRefresh38_TotNumReports, bits = read_uInt32 bits in
    let f_SnapshotFullRefresh38_SecurityID, bits = read_Int32 bits in
    let f_SnapshotFullRefresh38_RptSeq, bits = read_uInt32 bits in
    let f_SnapshotFullRefresh38_TransactTime, bits = read_uInt64 bits in
    let f_SnapshotFullRefresh38_LastUpdateTime, bits = read_uInt64 bits in
    let f_SnapshotFullRefresh38_TradeDate, bits = read_LocalMktDate bits in
    let f_SnapshotFullRefresh38_MDSecurityTradingStatus, bits = read_SecurityTradingStatus bits in
    let f_SnapshotFullRefresh38_HighLimitPrice, bits = read_PRICENULL bits in
    let f_SnapshotFullRefresh38_LowLimitPrice, bits = read_PRICENULL bits in
    let f_SnapshotFullRefresh38_MaxPriceVariation, bits = read_PRICENULL bits in
    let f_SnapshotFullRefresh38_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_SnapshotFullRefresh38_NoMDEntries gbits in
    { f_SnapshotFullRefresh38_LastMsgSeqNumProcessed; f_SnapshotFullRefresh38_TotNumReports; f_SnapshotFullRefresh38_SecurityID; f_SnapshotFullRefresh38_RptSeq; f_SnapshotFullRefresh38_TransactTime; f_SnapshotFullRefresh38_LastUpdateTime; f_SnapshotFullRefresh38_TradeDate; f_SnapshotFullRefresh38_MDSecurityTradingStatus; f_SnapshotFullRefresh38_HighLimitPrice; f_SnapshotFullRefresh38_LowLimitPrice; f_SnapshotFullRefresh38_MaxPriceVariation; f_SnapshotFullRefresh38_NoMDEntries } 
let read_g_QuoteRequest39_NoRelatedSym bits = 
    let f_QuoteRequest39_NoRelatedSym_Symbol, bits = read_Symbol bits in
    let f_QuoteRequest39_NoRelatedSym_SecurityID, bits = read_Int32 bits in
    let f_QuoteRequest39_NoRelatedSym_OrderQty, bits = read_Int32NULL bits in
    let f_QuoteRequest39_NoRelatedSym_QuoteType, bits = read_Int8 bits in
    let f_QuoteRequest39_NoRelatedSym_Side, bits = read_Int8NULL bits in
    {f_QuoteRequest39_NoRelatedSym_Symbol; f_QuoteRequest39_NoRelatedSym_SecurityID; f_QuoteRequest39_NoRelatedSym_OrderQty; f_QuoteRequest39_NoRelatedSym_QuoteType; f_QuoteRequest39_NoRelatedSym_Side}, bits

let read_msg_QuoteRequest39 (bits, gbits) = 
    let f_QuoteRequest39_TransactTime, bits = read_uInt64 bits in
    let f_QuoteRequest39_QuoteReqID, bits = read_QuoteReqId bits in
    let f_QuoteRequest39_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_QuoteRequest39_NoRelatedSym, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_QuoteRequest39_NoRelatedSym gbits in
    { f_QuoteRequest39_TransactTime; f_QuoteRequest39_QuoteReqID; f_QuoteRequest39_MatchEventIndicator; f_QuoteRequest39_NoRelatedSym } 
let read_g_MDInstrumentDefinitionOption41_NoEvents bits = 
    let f_MDInstrumentDefinitionOption41_NoEvents_EventType, bits = read_EventType bits in
    let f_MDInstrumentDefinitionOption41_NoEvents_EventTime, bits = read_uInt64 bits in
    {f_MDInstrumentDefinitionOption41_NoEvents_EventType; f_MDInstrumentDefinitionOption41_NoEvents_EventTime}, bits
let read_g_MDInstrumentDefinitionOption41_NoMDFeedTypes bits = 
    let f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MDFeedType, bits = read_MDFeedType bits in
    let f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MarketDepth, bits = read_Int8 bits in
    {f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MDFeedType; f_MDInstrumentDefinitionOption41_NoMDFeedTypes_MarketDepth}, bits
let read_g_MDInstrumentDefinitionOption41_NoInstAttrib bits = 
    let f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribType, bits = read_InstAttribType bits in
    let f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribValue, bits = read_InstAttribValue bits in
    {f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribType; f_MDInstrumentDefinitionOption41_NoInstAttrib_InstAttribValue}, bits
let read_g_MDInstrumentDefinitionOption41_NoLotTypeRules bits = 
    let f_MDInstrumentDefinitionOption41_NoLotTypeRules_LotType, bits = read_Int8 bits in
    let f_MDInstrumentDefinitionOption41_NoLotTypeRules_MinLotSize, bits = read_DecimalQty bits in
    {f_MDInstrumentDefinitionOption41_NoLotTypeRules_LotType; f_MDInstrumentDefinitionOption41_NoLotTypeRules_MinLotSize}, bits
let read_g_MDInstrumentDefinitionOption41_NoUnderlyings bits = 
    let f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityID, bits = read_Int32 bits in
    let f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityIDSource, bits = read_SecurityIDSource bits in
    let f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSymbol, bits = read_UnderlyingSymbol bits in
    {f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityID; f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSecurityIDSource; f_MDInstrumentDefinitionOption41_NoUnderlyings_UnderlyingSymbol}, bits

let read_msg_MDInstrumentDefinitionOption41 (bits, gbits) = 
    let f_MDInstrumentDefinitionOption41_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDInstrumentDefinitionOption41_TotNumReports, bits = read_uInt32NULL bits in
    let f_MDInstrumentDefinitionOption41_SecurityUpdateAction, bits = read_SecurityUpdateAction bits in
    let f_MDInstrumentDefinitionOption41_LastUpdateTime, bits = read_uInt64 bits in
    let f_MDInstrumentDefinitionOption41_MDSecurityTradingStatus, bits = read_SecurityTradingStatus bits in
    let f_MDInstrumentDefinitionOption41_ApplID, bits = read_Int16 bits in
    let f_MDInstrumentDefinitionOption41_MarketSegmentID, bits = read_uInt8 bits in
    let f_MDInstrumentDefinitionOption41_UnderlyingProduct, bits = read_uInt8 bits in
    let f_MDInstrumentDefinitionOption41_SecurityExchange, bits = read_SecurityExchange bits in
    let f_MDInstrumentDefinitionOption41_SecurityGroup, bits = read_SecurityGroup bits in
    let f_MDInstrumentDefinitionOption41_Asset, bits = read_Asset bits in
    let f_MDInstrumentDefinitionOption41_Symbol, bits = read_Symbol bits in
    let f_MDInstrumentDefinitionOption41_SecurityID, bits = read_Int32 bits in
    let f_MDInstrumentDefinitionOption41_SecurityIDSource, bits = read_SecurityIDSource bits in
    let f_MDInstrumentDefinitionOption41_SecurityType, bits = read_SecurityType bits in
    let f_MDInstrumentDefinitionOption41_CFICode, bits = read_CFICode bits in
    let f_MDInstrumentDefinitionOption41_PutOrCall, bits = read_PutOrCall bits in
    let f_MDInstrumentDefinitionOption41_MaturityMonthYear, bits = read_MaturityMonthYear bits in
    let f_MDInstrumentDefinitionOption41_Currency, bits = read_Currency bits in
    let f_MDInstrumentDefinitionOption41_StrikePrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_StrikeCurrency, bits = read_Currency bits in
    let f_MDInstrumentDefinitionOption41_SettlCurrency, bits = read_Currency bits in
    let f_MDInstrumentDefinitionOption41_MinCabPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_MatchAlgorithm, bits = read_CHAR bits in
    let f_MDInstrumentDefinitionOption41_MinTradeVol, bits = read_uInt32 bits in
    let f_MDInstrumentDefinitionOption41_MaxTradeVol, bits = read_uInt32 bits in
    let f_MDInstrumentDefinitionOption41_MinPriceIncrement, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_MinPriceIncrementAmount, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_DisplayFactor, bits = read_FLOAT bits in
    let f_MDInstrumentDefinitionOption41_TickRule, bits = read_Int8NULL bits in
    let f_MDInstrumentDefinitionOption41_MainFraction, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionOption41_SubFraction, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionOption41_PriceDisplayFormat, bits = read_uInt8NULL bits in
    let f_MDInstrumentDefinitionOption41_UnitOfMeasure, bits = read_UnitOfMeasure bits in
    let f_MDInstrumentDefinitionOption41_UnitOfMeasureQty, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_TradingReferencePrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_SettlPriceType, bits = read_SettlPriceType bits in
    let f_MDInstrumentDefinitionOption41_ClearedVolume, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionOption41_OpenInterestQty, bits = read_Int32NULL bits in
    let f_MDInstrumentDefinitionOption41_LowLimitPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_HighLimitPrice, bits = read_PRICENULL bits in
    let f_MDInstrumentDefinitionOption41_UserDefinedInstrument, bits = read_UserDefinedInstrument bits in
    let f_MDInstrumentDefinitionOption41_TradingReferenceDate, bits = read_LocalMktDate bits in
    let f_MDInstrumentDefinitionOption41_NoEvents, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionOption41_NoEvents gbits in
    let f_MDInstrumentDefinitionOption41_NoMDFeedTypes, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionOption41_NoMDFeedTypes gbits in
    let f_MDInstrumentDefinitionOption41_NoInstAttrib, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionOption41_NoInstAttrib gbits in
    let f_MDInstrumentDefinitionOption41_NoLotTypeRules, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionOption41_NoLotTypeRules gbits in
    let f_MDInstrumentDefinitionOption41_NoUnderlyings, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDInstrumentDefinitionOption41_NoUnderlyings gbits in
    { f_MDInstrumentDefinitionOption41_MatchEventIndicator; f_MDInstrumentDefinitionOption41_TotNumReports; f_MDInstrumentDefinitionOption41_SecurityUpdateAction; f_MDInstrumentDefinitionOption41_LastUpdateTime; f_MDInstrumentDefinitionOption41_MDSecurityTradingStatus; f_MDInstrumentDefinitionOption41_ApplID; f_MDInstrumentDefinitionOption41_MarketSegmentID; f_MDInstrumentDefinitionOption41_UnderlyingProduct; f_MDInstrumentDefinitionOption41_SecurityExchange; f_MDInstrumentDefinitionOption41_SecurityGroup; f_MDInstrumentDefinitionOption41_Asset; f_MDInstrumentDefinitionOption41_Symbol; f_MDInstrumentDefinitionOption41_SecurityID; f_MDInstrumentDefinitionOption41_SecurityIDSource; f_MDInstrumentDefinitionOption41_SecurityType; f_MDInstrumentDefinitionOption41_CFICode; f_MDInstrumentDefinitionOption41_PutOrCall; f_MDInstrumentDefinitionOption41_MaturityMonthYear; f_MDInstrumentDefinitionOption41_Currency; f_MDInstrumentDefinitionOption41_StrikePrice; f_MDInstrumentDefinitionOption41_StrikeCurrency; f_MDInstrumentDefinitionOption41_SettlCurrency; f_MDInstrumentDefinitionOption41_MinCabPrice; f_MDInstrumentDefinitionOption41_MatchAlgorithm; f_MDInstrumentDefinitionOption41_MinTradeVol; f_MDInstrumentDefinitionOption41_MaxTradeVol; f_MDInstrumentDefinitionOption41_MinPriceIncrement; f_MDInstrumentDefinitionOption41_MinPriceIncrementAmount; f_MDInstrumentDefinitionOption41_DisplayFactor; f_MDInstrumentDefinitionOption41_TickRule; f_MDInstrumentDefinitionOption41_MainFraction; f_MDInstrumentDefinitionOption41_SubFraction; f_MDInstrumentDefinitionOption41_PriceDisplayFormat; f_MDInstrumentDefinitionOption41_UnitOfMeasure; f_MDInstrumentDefinitionOption41_UnitOfMeasureQty; f_MDInstrumentDefinitionOption41_TradingReferencePrice; f_MDInstrumentDefinitionOption41_SettlPriceType; f_MDInstrumentDefinitionOption41_ClearedVolume; f_MDInstrumentDefinitionOption41_OpenInterestQty; f_MDInstrumentDefinitionOption41_LowLimitPrice; f_MDInstrumentDefinitionOption41_HighLimitPrice; f_MDInstrumentDefinitionOption41_UserDefinedInstrument; f_MDInstrumentDefinitionOption41_TradingReferenceDate; f_MDInstrumentDefinitionOption41_NoEvents; f_MDInstrumentDefinitionOption41_NoMDFeedTypes; f_MDInstrumentDefinitionOption41_NoInstAttrib; f_MDInstrumentDefinitionOption41_NoLotTypeRules; f_MDInstrumentDefinitionOption41_NoUnderlyings } 
let read_g_MDIncrementalRefreshTradeSummary42_NoMDEntries bits = 
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryPx, bits = read_PRICE bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntrySize, bits = read_Int32 bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_SecurityID, bits = read_Int32 bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_RptSeq, bits = read_uInt32 bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_NumberOfOrders, bits = read_Int32NULL bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_AggressorSide, bits = read_AggressorSide bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDUpdateAction, bits = read_MDUpdateAction bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryType, bits = read_MDEntryTypeTrade bits in
    {f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryPx; f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntrySize; f_MDIncrementalRefreshTradeSummary42_NoMDEntries_SecurityID; f_MDIncrementalRefreshTradeSummary42_NoMDEntries_RptSeq; f_MDIncrementalRefreshTradeSummary42_NoMDEntries_NumberOfOrders; f_MDIncrementalRefreshTradeSummary42_NoMDEntries_AggressorSide; f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDUpdateAction; f_MDIncrementalRefreshTradeSummary42_NoMDEntries_MDEntryType}, bits
let read_g_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries bits = 
    let f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_OrderID, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_LastQty, bits = read_Int32 bits in
    {f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_OrderID; f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries_LastQty}, bits

let read_msg_MDIncrementalRefreshTradeSummary42 (bits, gbits) = 
    let f_MDIncrementalRefreshTradeSummary42_TransactTime, bits = read_uInt64 bits in
    let f_MDIncrementalRefreshTradeSummary42_MatchEventIndicator, bits = read_MatchEventIndicator bits in
    let f_MDIncrementalRefreshTradeSummary42_NoMDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshTradeSummary42_NoMDEntries gbits in
    let f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries, gbits = 
         let nent, bsz, gbits = Binparser.get_group_info gbits in
         foldi (nent, bsz) read_g_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries gbits in
    { f_MDIncrementalRefreshTradeSummary42_TransactTime; f_MDIncrementalRefreshTradeSummary42_MatchEventIndicator; f_MDIncrementalRefreshTradeSummary42_NoMDEntries; f_MDIncrementalRefreshTradeSummary42_NoOrderIDEntries } 
let read_message bits md =
    let gbits = Binparser.skip bits md.Binparser.block_length in
    match md.Binparser.template_id with
    | 4 -> M_ChannelReset4 ( read_msg_ChannelReset4 (bits, gbits))
    | 12 -> M_AdminHeartbeat12 ( read_msg_AdminHeartbeat12 (bits, gbits))
    | 15 -> M_AdminLogin15 ( read_msg_AdminLogin15 (bits, gbits))
    | 16 -> M_AdminLogout16 ( read_msg_AdminLogout16 (bits, gbits))
    | 27 -> M_MDInstrumentDefinitionFuture27 ( read_msg_MDInstrumentDefinitionFuture27 (bits, gbits))
    | 29 -> M_MDInstrumentDefinitionSpread29 ( read_msg_MDInstrumentDefinitionSpread29 (bits, gbits))
    | 30 -> M_SecurityStatus30 ( read_msg_SecurityStatus30 (bits, gbits))
    | 32 -> M_MDIncrementalRefreshBook32 ( read_msg_MDIncrementalRefreshBook32 (bits, gbits))
    | 33 -> M_MDIncrementalRefreshDailyStatistics33 ( read_msg_MDIncrementalRefreshDailyStatistics33 (bits, gbits))
    | 34 -> M_MDIncrementalRefreshLimitsBanding34 ( read_msg_MDIncrementalRefreshLimitsBanding34 (bits, gbits))
    | 35 -> M_MDIncrementalRefreshSessionStatistics35 ( read_msg_MDIncrementalRefreshSessionStatistics35 (bits, gbits))
    | 36 -> M_MDIncrementalRefreshTrade36 ( read_msg_MDIncrementalRefreshTrade36 (bits, gbits))
    | 37 -> M_MDIncrementalRefreshVolume37 ( read_msg_MDIncrementalRefreshVolume37 (bits, gbits))
    | 38 -> M_SnapshotFullRefresh38 ( read_msg_SnapshotFullRefresh38 (bits, gbits))
    | 39 -> M_QuoteRequest39 ( read_msg_QuoteRequest39 (bits, gbits))
    | 41 -> M_MDInstrumentDefinitionOption41 ( read_msg_MDInstrumentDefinitionOption41 (bits, gbits))
    | 42 -> M_MDIncrementalRefreshTradeSummary42 ( read_msg_MDIncrementalRefreshTradeSummary42 (bits, gbits))