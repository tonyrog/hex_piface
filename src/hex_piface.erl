%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Hex piface/gpio plugin 
%%% @end
%%% Created :  24 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_piface).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal(), Cb::function()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal, Cb) ->
    hex_piface_server:add_event(Flags, Signal, Cb).

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(Ref) ->
    hex_piface_server:del_event(Ref).
%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, Env) ->
    Pin = proplists:get_value(pin, Flags),
    %% pin_reg is not used yes, but could since technically 
    %% input is register 0 and output is register 1 on chipset level
    %% now we pretend to change direction dynamically
    _PinReg = proplists:get_value(pin_reg, Flags, 0),
    Value = case value(Flags, undefined) of
		undefined -> value(Env, undefined);
		V -> V
	    end,
    Polarity = proplists:get_value(polarity, Flags, false),
    case Value =/= Polarity of
	false -> piface:gpio_clr(Pin);
	true -> piface:gpio_set(Pin)
    end.

value(Flags,Default) ->
    case proplists:get_value(value, Flags) of
	undefined -> Default;
	1 -> true;
	0 -> false;
	Bool -> Bool
    end.

%%
%% init_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Reason}
%% validate_event is assumed to have been run before init !
init_event(in,_Flags) ->
    ok;
init_event(out,Flags) ->
    Pin    = proplists:get_value(pin, Flags),
    InitValue  = case proplists:get_value(init_value, Flags) of
		     high -> true;
		     low  -> false;
		     out  -> false;
		     undefined -> false
		 end,
    Polarity = proplists:get_value(polarity, Flags, false),
    case InitValue =/= Polarity of
	false -> piface:gpio_clr(Pin);
	true -> piface:gpio_set(Pin)
    end.

%%
%% validate_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Error}
%%
validate_event(in, Flags) ->
    hex:validate_flags(Flags, input_spec());
validate_event(out, Flags) ->
    hex:validate_flags(Flags, output_spec()).

output_spec() ->
    [{pin, mandatory, {integer,0,7}, undefined},
     {pin_reg, optional, {integer,0,1}, 0},
     {value, optional, {alt,[boolean,unsigned1]}, undefined},
     {init_value, optional, {alt,[{const,high},
				  {const,low},
				  {const,out}]}, out},
     {polarity, optional, boolean, false},
     {direct, optional, boolean, false}
    ].

input_spec() ->
   [ {pin, mandatory, {integer,0,7}, undefined},
     {pin_reg, optional, {integer,0,1}, 0},
     {interrupt, optional, {alt,[{const,none},
				 {const,rising},
				 {const,falling},
				 {const,both}]}, none},
     {polarity, optional, boolean, false},
     {direct, optional, boolean, false}
    ].
