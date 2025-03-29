%%%-------------------------------------------------------------------
%% @doc chat_app top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_app_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [#{
        id => chat_app_server,
        start => {chat_app, start, [5555]},
        restart => permanent,
        shutdown => 5555,
        type => worker,
        modules => [chat_app]
    }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
