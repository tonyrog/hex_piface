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
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
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

mod_event(in, _Flags) ->
    ok;
mod_event(out, Flags) ->
    %% fixme: send to hex_piface_server
    init_event(out, Flags).

%%
%% validate_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Error}
%%
validate_event(Dir, Flags) ->
    hex:validate_flags(Flags, event_spec(Dir)).

event_spec(out) ->
    [{leaf,pin,[{type,uint8,[{range,[{0,7}],[]}]},{mandatory,true,[]}]},
     {leaf,pin_reg,[{type,uint8,[{range,[{0,1}],[]}]},{default,0,[]}]},
     {leaf,value,[{type,uint8,[{range,[{0,1}],[]}]}]}, %% boolean?
     {leaf,init_value,[{type,enumeration,
			[{enum,high,[]},
			 {enum,low,[]},
			 {enum,out,[]}]},
		       {default, out, []}]},
     {leaf,polarity,[{type,boolean,[]},
		     {default,false,[]}]},
     {leaf,direct,[{type,boolean,[]},
		   {default,false,[]}]}
    ];
event_spec(in) ->
    [{leaf,pin,[{type,uint8,[{range,[{0,7}],[]}]},{mandatory,true,[]}]},
     {leaf,pin_reg,[{type,uint8,[{range,[{0,1}],[]}]},{default,0,[]}]},
     {leaf,interrupt,[{type,enumeration,
		      [{enum,none,[]},
		       {enum,rising,[]},
		       {enum,falling,[]},
		       {enum,both,[]}]},
		      {default,none,[]}]},
     {leaf,polarity,[{type,boolean,[]},
		     {default,false,[]}]},
     {leaf,direct,[{type,boolean,[]},
		   {default,false,[]}]}
    ].
