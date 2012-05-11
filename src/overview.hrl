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

%% @doc This is the superly good style checker for Erlang modules.
%% See {@link superl} module documentation for features and usage.
%%
%% ==Installation==
%%
%% Superl is relatively lightweight, and be included as a project
%% dependency via `rebar.config':
%%
%% `{deps, [
%%    {pose, ".*",
%%      {git, "git://github.com/beadsland/pose", {branch, master}}}
%%   ]}'
%%
%% Following `rebar get-deps', the style checker can be invoked easily
%% from an erl command-line:
%%
%% `> erl -pa deps/superl/ebin -s superl -s init stop'
%%
%% It is recommended that the above be incorporated into a `good' rule
%% in project makefiles, resulting in an invocation of:
%%
%% `> make good'
%% @end