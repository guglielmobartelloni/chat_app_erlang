%%%-------------------------------------------------------------------
%% @doc chat_app_miniclip public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app_miniclip_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chat_app_miniclip_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
