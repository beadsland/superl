%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and 
%% Distribution License, Version 1.0 (the "License"); you may not use 
%% this file except in compliance with the License.  You should have 
%% received a copy of the Common Development and Distribution License 
%% along with this software.  If not, it can be retrieved online at 
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Simple style checker for Erlang modules.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

-module(superl).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

-record(lineinfo, {	max, total, bigfunc, curfunc, hlines, clines } ).

start() ->
	Src = filename:absname("src"),
	{ok, Listing} = file:list_dir(Src),
	review(Src, Listing).

%%
%% Local Functions
%%

review(_Dir, []) -> ok;
review(Dir, [Head | Tail]) ->
	{ok, MP} = re:compile("\.[he]rl$"),
	case re:run(Head, MP, [{capture, none}]) of
		nomatch	-> review(Dir, Tail);
		match	-> test_lines(Dir, Head), review(Dir, Tail)
	end.
	
test_lines(Dir, File) ->
	{ok, FileID} = file:open(Dir ++ "/" ++ File, [read]),
	{ok, MP} = re:compile("^(.*)\.[he]rl$"),
	{match, [Module]} = re:run(File, MP, [{capture, [1], list}]),
	Info = line_info(FileID),
	Ratio = round(Info#lineinfo.hlines / Info#lineinfo.clines * 100),
	if Info#lineinfo.max > 78 ->
			io:format("~s: avoid long lines (~p char line detected)~n", 
					[Module, Info#lineinfo.max]);
	   Info#lineinfo.total > 380 ->
	   		io:format("~s: avoid long modules (~p lines detected)~n",
	   				[Module, Info#lineinfo.total]);
	   Info#lineinfo.bigfunc > 18 ->
	   		io:format("~s: avoid long functions (~p line function detected)~n", 
	   				[Module, Info#lineinfo.bigfunc]);
	   Ratio < 20 ->
	   		io:format("~s: document code (~p% CtC ratio)~n", 
	   				[Module, Ratio]);
	   true -> false
	end.
	
line_info(FileID) ->
	case file:read_line(FileID) of
		eof			-> #lineinfo{	max = 0,
									total = 0,
									curfunc = 0,
									bigfunc = 0,
									hlines = 0,
									clines = 0 };
		{ok, Line} 	-> Info = line_info(FileID),
					   
					   NewMax = max(string:len(Line), Info#lineinfo.max),
					   NewTotal = Info#lineinfo.total + 1,
					   {NewCurFunc, LineType} = 
					   		function_length(Info#lineinfo.curfunc, Line),
					   NewBigFunc = max(Info#lineinfo.bigfunc, NewCurFunc),
					   if LineType == header ->
					   		NewHLine = Info#lineinfo.hlines + 1,
					   		NewCLine = Info#lineinfo.clines;
					   	  LineType == code ->
					   	    NewHLine = Info#lineinfo.hlines,
					   	    NewCLine = Info#lineinfo.clines + 1;
					   	  true ->	% LineType == blank
					   	    NewHLine = Info#lineinfo.hlines,
					   	    NewCLine = Info#lineinfo.clines
					   end,
					   Info#lineinfo{	max = NewMax, 
					   					total = NewTotal,
					   					curfunc = NewCurFunc,
					   					bigfunc = NewBigFunc,
					   					hlines = NewHLine,
					   					clines = NewCLine	}
	end.
	
function_length(Count, Line) ->
	{ok, MP} = re:compile("^[^\\%]+\\.\\ *$"),
	case re:run(Line, MP, [{capture, none}]) of
		match	-> {1, code};
		nomatch -> case Line of
						[$%, $% | _More]	-> {Count, header};
						"\n"				-> {Count, blank};
						_Else				-> {Count + 1, code}
				   end
	end.
