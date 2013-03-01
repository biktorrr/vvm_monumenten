%!/usr/bin/swipl -s

:- initialization
   cp_server.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file provides a skeleton startup file.  It can be localized by running

    % ./configure			(Unix)
    % Double-clicking setup.pl		(Windows)

After  that,  the  system  may  be  customized  by  copying  or  linking
customization  files  from  config-available    to  config-enabled.  See
config-enabled/README.txt for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% Setup search path for cliopatria.

file_search_path(cliopatria, 'c:/users/victor/cliopatria').

% Make loading files silent. Comment if you want verbose loading.

:- set_prolog_flag(verbose, silent).

		 /*******************************
		 *	      LOAD CODE		*
		 *******************************/

% Use the ClioPatria help system.  May   be  commented to disable online
% help on the source-code.

:- use_module(cliopatria('applications/help/load')).

% Load ClioPatria itself.  Better keep this line.

:- use_module(cliopatria(cliopatria)).

% Load package manager

:- use_module(library(cpack/cpack)).

% Load the remainder of the  configuration. The directory config-enabled
% can also be used to  load   additional  (plugin)  functionality.

:- use_module(library(conf_d)).

:- load_conf_d([ 'config-enabled' ], []).

% Get back normal verbosity of the toplevel.

:- set_prolog_flag(verbose, normal).


