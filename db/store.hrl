%% Commutable patches :
%%      A patch is a collection of commutable changesets, where each changeset 
%% pertains to a single document and all changesets in a patch pertaining to
%% several documents are to be atomically applied to the database. If one 
%% changeset fails entire patch fails to apply.
%%      A patch is considered commutable only if all changesets inside the
%% patch is commutable with respect to previous patch. 
%% If a patch is non-commutable with respect to previous patch, then both
%% patches are deemed as dependants and 
%%
%% A changeset can be one of the following,
%%      hunk, adoc, rdoc
-record( patch,     { sha, backref, context, changesets=[], effect }).

%% `hunk` record is a list of diffs that constitute a changeset between two
%%  versions of a document.
%%  A hunk can be,
%%      cv, ap, dp, np, al, dl, cl, nl
%%  Every hunk is identified by unique document id and hash generated from its
%%  diffs.
-record( hunk,      { uid, sha, backref, diffs=[] }).
%% Every document is identified by a unique id. Along the way, if user wishes
%% to change the unique id, it can be done so using `rdoc` changeset
-record( adoc,      { uid }).
-record( rdoc,      { uid, backref }).

%% `cv` tuple is diff of changes done to simple term values like,
%%      string, number, true, false, null
%% inside a more complex terms like list and property list.
-record( cv,        { old, new }).

%% `ap` tuple is `diffprop` of property additions. {from,to} specify the start 
%% and end position inside the eventual document-state.
-record( ap,        { from, to, ins=[] }).

%% `dp` tuple is `diffprop` of property deletions. {from,to} specify the start
%% and end position inside the present document-state.
-record( dp,        { from, to, outs=[] }).

%% `np` tuple is diff of single property change in property list. 
%% {offthis,offthat} specify the position of property inside current
%% document-state and eventual document-state, respectively.
-record( np,        { offthis, offthat, changeset }).

%% `ae` tuple is diff of elements to add inside a list. {from,to} specify 
%% the start and end position inside the eventual document-state.
-record( ae,        { from, to, ins=[] }).

%% `delems` tuple is diff of elements to delete from a list.
%% `from` and `to` specify the start and end position inside the present
%% document-state.
-record( de,        { from, to, outs=[] }).

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
