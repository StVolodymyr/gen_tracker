%%%-------------------------------------------------------------------
%%% @author stark
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2016 3:58 PM
%%%-------------------------------------------------------------------
-module(gen_tracker_sup).
-author("stark").

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_child(Zone, Spec) ->
  supervisor:start_child(Zone, Spec).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Zone :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Zone) ->
  supervisor:start_link({local, Zone}, ?MODULE, [Zone]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  {ok, {{one_for_one, 1000, 1000}, []}};

init([Zone]) ->
  TrackerSup = list_to_atom(atom_to_list(Zone) ++ "_tracker_sup"),
  Tracker = list_to_atom(atom_to_list(Zone) ++ "_tracker"),
  Children = [
    #{
      id => Tracker,
      start => {gen_tracker, start_link, [Zone]},
      restart => permanent,
      shutdown => 5000,
      modules => []
    },
    #{
      id => TrackerSup,
      start => {supervisor, start_link, [{local, TrackerSup}, ?MODULE, []]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => []
    }
  ],

  {ok, {{one_for_all, 1000, 3600}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
