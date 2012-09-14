-module(nhttp_date).


% @doc: While generating date, the format must always be as suggested by RFC 1123.
http_date( localtime ) -> datestr( calendar:localtime() ).
datestr({{Y,M,D},{H,Mi,S}}) ->
    io_lib:format(
        "~3B, ~2B ~3B ~4B ~2B:~2B:~2B",
        [calendar:day_of_the_week(Y,M,D),D,val_to_month(M),Y,H,Mi,S] ).


% @doc: If date-format is not know before-hand, then apply one pattern after
% another to find a matching format.
parse_date(Data) ->
    case parse_date( rfc1123, Data ) of
        {match, _}=X -> X;
        _ -> case parse_date( rfc1036, Data ) of
                {match, _}=X -> X;
                _ -> case parse_date( asctime, Data ) of
                        {match, _}=X -> X;
                        _ -> nomatch
    end.


% @doc: Parse date in RFC1123 format
parse_date(rfc1123, Data) -> 
    parse_date( Data,
                ?RE_DATETIME_RFC1123,
                fun([_, Day,D,M,Y,H,Mi,S]) -> {Y,M,D,H,Mi,S} end );

% @doc: Parse date in RFC1036 format
parse_date(rfc1036, Data) ->
    parse_date( Data,
                ?RE_DATETIME_RFC1036,
                fun([_, Day,D,M,Y,H,Mi,S]) -> {Y,M,D,H,Mi,S} end );

% @doc: Parse date in ASCTIME format
parse_date(asctime, Data) ->
    parse_date( Data,
                ?RE_DATETIME_ASCTIME,
                fun([_, Day,M,D,H,Mi,S,Y]) -> {Y,M,D,H,Mi,S} end ).


% Common logic for all three date formats.
parse_date(Data, R, Fn) ->
    case re:run( Data, R, [{capture, all, list}] ) of
        {match, X} -> {match, date_to_tuple( Fn( X ))};
        _ -> nomatch
    end.

% @doc: convert date components in string tokens to tuple of date and time.
% Similar to the convention used in `calendar` module.
date_to_tuple({Y,M,D,H,Mi,S}) ->
    {{ string:to_integer( D ),
       string:to_integer( month_to_val( M )),
       string:to_integer( Y )
     },
     { string:to_integer( H ),
       string:to_integer( Mi ),
       string:to_integer( S )
     }}.

month_to_val("Jan"++X) -> 1;
month_to_val("Feb"++X) -> 2;
month_to_val("Mar"++X) -> 3;
month_to_val("Apr"++X) -> 4;
month_to_val("May"++X) -> 5;
month_to_val("Jun"++X) -> 6;
month_to_val("Jul"++X) -> 7;
month_to_val("Aug"++X) -> 8;
month_to_val("Sep"++X) -> 9;
month_to_val("Oct"++X) -> 10;
month_to_val("Nov"++X) -> 11;
month_to_val("Dec"++X) -> 12.

val_to_month(1) -> "Jan";
val_to_month(2) -> "Feb";
val_to_month(3) -> "Mar";
val_to_month(4) -> "Apr";
val_to_month(5) -> "May";
val_to_month(6) -> "Jun";
val_to_month(7) -> "Jul";
val_to_month(8) -> "Aug";
val_to_month(9) -> "Sep";
val_to_month(10) -> "Oct";
val_to_month(11) -> "Nov";
val_to_month(12) -> "Dec".

dayof(1) -> "Mon";
dayof(2) -> "Tue";
dayof(3) -> "Wed";
dayof(4) -> "Thu";
dayof(5) -> "Fri";
dayof(6) -> "Sat";
dayof(7) -> "Sun".
