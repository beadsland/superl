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

%% @todo add dependency soft-linking option to rebar

%% @doc This is the superly good style checker for Erlang modules.
%% See {@link superl} module documentation for features and usage.
%%
%% The `superl' module includes the
%% <a href="http://github.com/beadsland/pose">`pose'</a> interface,
%% allowing it to be run as a command in the
%% <a href="http://github.com/beadsland/nosh">`nosh'</a> Bourne shell
%% emulator.
%%
%% <ul>
%% <li> {@section Installation} </li>
%% <li> {@section Converting Tabs} </li>
%% </ul>
%%
%% ==Installation==
%%
%% `superl' is relatively lightweight, and be included as a project
%% dependency via `rebar.config':
%%
%% <blockquote>
%% `{deps, [
%%    {superl, ".*",
%%      {git, "git://github.com/beadsland/superl", {branch, master}}}
%%   ]}'
%% </blockquote>
%%
%% Following `rebar get-deps', the style checker can be invoked easily
%% from an erl command-line:
%%
%% <blockquote>
%% `> erl -pa deps/superl/ebin -s superl -s init stop'
%% </blockquote>
%%
%% It is recommended that the above be incorporated into a `good' rule
%% in project makefiles, resulting in an invocation of:
%%
%% <blockquote>
%% `> make good'
%% </blockquote>
%%
%% ==Converting Tabs==
%%
%% The first test `superl' runs on any file is to check of there are
%% leading tabs on any line.  The number of spaces tabs are rendered
%% at in any given editor are not guaranteed, and thus they should be
%% converted to spaces to ensure stylistic indentation is portable across
%% development environments.
%%
%% If using the Eclipse IDE, one way to handle this is with the AnyEdit
%% Tools plugin [http://andrei.gmxhome.de/anyedit/], available in the
%% Eclipse Marketplace.
%% @end