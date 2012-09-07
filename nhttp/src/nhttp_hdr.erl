-module(nhttp_hdr).


% Multiple message-header fields with the same field-name MAY be
% present in a message if and only if the entire field-value for that
% header field is defined as a comma-separated list [i.e., #(values)].

merge_headers([])       -> [];
merge_headers([H | Hs]) -> merge_headers(H, Hs, []).

merge_headers(H, [], Acc) -> lists:reverse([H|Acc]);
merge_headers(H, Rs, Acc) ->
    case merge_header(H, Rs, []) of
        {NewH, []} -> lists:reverse([NewH | Acc]);
        {NewH, NewRs} -> merge_headers( hd(NewRs), tl(NewRs), [NewH | Acc])
    end.

merge_header({F,V}, [], Hs) -> {{F,V}, lists:reverse(Hs)};
merge_header({F,V1}, [{F,V2} | Rs], Hs) ->
    merge_header({F, string:join([V1, V2], ",")}, Rs, Hs);
merge_header({F,V}, [R | Rs], Hs) ->
    merge_header({F,V}, Rs, [R | Hs]).
