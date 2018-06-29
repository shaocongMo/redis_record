-module(redis_record_handler).

%% ====================================================================
%% Include files  
%% ====================================================================


%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_key/2, parse_value/1, parse_value/2, parse_field/1]).
-export([construct_record/3]).


-spec parse_key(Record::tuple(), 
                PrimaryFields::list({atom(), integer()})) -> list().
parse_key(Record, PrimaryFields) ->
    [RecordName | _ValueList] = erlang:tuple_to_list(Record),
    parse_key(PrimaryFields, Record, [RecordName]).

parse_key([], _Record, KeyList) ->
    lists:concat(lists:reverse(KeyList));
parse_key([{Field, FieldIndex} | PrimaryFields], Record, KeyList) ->
    FieldValue = erlang:element(FieldIndex, Record),
    parse_key(PrimaryFields, Record, [FieldValue, Field | KeyList]).

-spec parse_value(Record::tuple()) -> list().
-spec parse_value(Record::tuple(),
                  IgnoreFieldIndexs::list(integer())) -> list().
parse_value(Record) ->
    parse_value(Record, []).

parse_value(Record, IgnoreFieldIndexs) ->
    [_RecordName | ValueList] = erlang:tuple_to_list(Record),
    parse_value(ValueList, 2, IgnoreFieldIndexs, []).

parse_value([], _FieldIndex, _IgnoreFieldIndexs, RedisFieldValues) ->
    lists:reverse(RedisFieldValues);
parse_value([Value | ValueList], FieldIndex, IgnoreFieldIndexs, RedisFieldValues) ->
    case lists:member(FieldIndex, IgnoreFieldIndexs) of
        false ->
            case is_float(Value) of
                true ->
                    NewValue = float_to_list(Value);
                _ ->
                    NewValue = Value
            end,
            NewRedisFieldValues = [NewValue, FieldIndex | RedisFieldValues];
        _ ->
            NewRedisFieldValues = RedisFieldValues
    end,
    parse_value(ValueList, FieldIndex + 1, IgnoreFieldIndexs, NewRedisFieldValues).

-spec parse_field(DefaultRecord::tuple()) -> tuple().
parse_field(DefaultRecord) ->
    [RecordName | DefaultValueList] = erlang:tuple_to_list(DefaultRecord),
    FieldList = parse_field(DefaultValueList, 2, []),
    {RecordName, FieldList}.

parse_field([], _FieldIndex, FieldList) ->
    FieldList;
parse_field([FieldDefaultValue | DefaultValueList], FieldIndex, FieldList) ->
    FieldType = parse_field_type(FieldDefaultValue),
    parse_field(DefaultValueList, FieldIndex + 1, [{FieldIndex, FieldType} | FieldList]).

-spec construct_record(ValueList::list(), FieldList::list(), Record::tuple()) -> Record::tuple().
construct_record([], _FieldList, Record) ->
    Record;
construct_record([FieldIndexBinary, FieldValueBinary | ValueList], FieldList, Record) ->
    FieldIndex = erlang:binary_to_integer(FieldIndexBinary),
    case lists:keyfind(FieldIndex, 1, FieldList) of
        {FieldIndex, FieldType} ->
            Value = construct_record_value(FieldType, FieldValueBinary),
            NewRecord = erlang:setelement(FieldIndex, Record, Value),
            construct_record(ValueList, FieldList, NewRecord);
        _ ->
            construct_record(ValueList, FieldList, Record)
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================
parse_field_type(Value) when is_integer(Value) -> 
    integer;
parse_field_type(Value) when is_float(Value) ->
    float;
parse_field_type(Value) when is_list(Value) ->
    list;
parse_field_type(Value) when is_binary(Value) ->
    binary;
parse_field_type(_Value) ->
    undefined.

construct_record_value(integer, Value) ->
    erlang:binary_to_integer(Value);
construct_record_value(float, Value) ->
    erlang:binary_to_float(Value);
construct_record_value(binary, Value) ->
    Value;
construct_record_value(list, Value) ->
    erlang:binary_to_list(Value);
construct_record_value(undefined, Value) ->
    try
        erlang:binary_to_integer(Value)
    catch 
        _:_Reason ->
            Value
    end;
construct_record_value(_FieldType, Value) ->
    Value.
