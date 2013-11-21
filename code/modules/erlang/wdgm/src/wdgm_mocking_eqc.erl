-module(wdgm_mocking_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-include_lib("wdgm_config.hrl").

-compile(export_all).

api_spec() ->
  #api_spec{
     language = c,
     mocking = eqc_mocking_c,
     modules =
       [ #api_module{
            name = 'WdgM',
            functions =
              []
           }]
    }.

%api_spec() ->
%  #api_spec{
%     language = c,
%     mocking = eqc_mocking_c,
%     modules =
%       [ #api_module{
%            name = 'WdgM',
%            functions =
%              [#api_fun_c{ name='WdgM_DeInit', ret=void,
%                           args=[]
%                         }
%              ]
%           }]
%    }.
