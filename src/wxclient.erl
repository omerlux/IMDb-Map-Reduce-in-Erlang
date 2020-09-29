%%%-------------------------------------------------------------------
%%% @author Ilay
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. ספט׳ 2020 20:39
%%%-------------------------------------------------------------------
-module(wxclient).
-author("Ilay").

%% API
-export([start/0]).
-include_lib("wx/include/wx.hrl").


%% Will get the pid of server
%% will send the information on button pressing
start() ->

  %%note: Frame and components build
  WX = wx:new(),
  Frame = wxFrame:new(wx:null(), 1, "Disco Map-Reduce Project"),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  SubSizer1 = wxBoxSizer:new(?wxVERTICAL),
  SubSizer2 = wxBoxSizer:new(?wxVERTICAL),

  TopTxt = wxStaticText:new(Frame, ?wxID_ANY, "Query Window"),
  Headline = wxStaticText:new(Frame, ?wxID_ANY, "Insert Value:"),
  TextCtrl  = wxTextCtrl:new(Frame, 1, [{value, ""}, {style, ?wxDEFAULT}]),

  wxSizer:add(SubSizer1, TopTxt, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, Headline, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),
  wxSizer:add(SubSizer1, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}, {border, 8}]),

  Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL,[{underlined, true}]),
%%  wxFont:setUnderlined(Font,true),
  wxTextCtrl:setFont(TopTxt, Font),
  wxSizer:add(MainSizer, SubSizer1, [{flag, ?wxALIGN_LEFT}, {border, 5}]),
  wxSizer:add(MainSizer, SubSizer2, [{flag, ?wxALIGN_RIGHT}, {border, 5}]),

  wxWindow:setSizer(Frame, MainSizer),
  wxFrame:show(Frame).
