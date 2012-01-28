%% `changeset` record is a list of hunks that consititute a patch.
%%  A hunk can be - chgval, addprops, delprops, chgprop
%%
%%  Every changeset is identified by a unique hash generated from
%%  its hunks.
-record( changeset, { id, hunks=[] }).

%% `chgval` tuple is a hunk for changes done to simple term values like
%% string, number, true, false, null inside a more complex terms like list and
%% property list.
-record( chgval,    { old, new }).

%% `addprops` tuple is a hunk of property additions.
%% `from`, `to` specify the start and end position inside the eventual 
%% document-state. `ins` is a list of `diffprop` records.
-record( addprops,  { from, to, ins=[] }).

%% `delprops` tuple is a hunk of property deletions.
%% `from`, `to` specify the start and end position inside the present
%% document-state. `outs` is a list of `diffprop` records.
-record( delprops,  { from, to, outs=[] }).

%% `chgprop` tuple is a hunk of single property change in property list. 
%% `offthis` and `offthat` specify the position of property inside current
%% document-state and eventual document-state.
-record( chgprop,   { offthis, offthat, changeset }).

%% `addems` tuple is a hunk of elements to add inside a list.
%% `from` and `to` specify the start and end position inside the eventual
%% document-state.
-record( addems,    { from, to, ins=[] }).

%% `delems` tuple is a hunk of elements to delete from a list.
%% `from` and `to` specify the start and end position inside the present
%% document-state.
-record( delems,    { from, to, outs=[] }).

%% `chgems` tuple is a hunk of elements to replace from present document-state,
%% with new set of elements leading to an eventual document-state.
-record( chgems,    { ofrom, oto, outs=[], ins=[], ifrom, ito }).

%% `chgem` tuple is a hunk of elements to replace from present document-state,
%% with new set of elements leading to an eventual document-state.
-record( chgem,     { offthis, offthat, changeset }).


%% `diffprop` record specifies what `prop` {key,value} tuple to add or delete
%% in a property list. `off` specifies the offset, that starts from 1, inside
%% the property list of either the present document-state (for addprops) or 
%% eventual document-state (for delprops).
-record( diffprop,  { off, key, value }).

%% `difflist` record specifies what element, or term, to add or delete in a 
%% list. `off` specifies the offset, that starts from 1, inside the property
%% list of either the present document-state (for addems) or eventual 
%% document-state (for delems).
-record( diffem,    { off, value }).
